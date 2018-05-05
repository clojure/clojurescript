;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl.nashorn
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.stacktrace]
            [clojure.data.json :as json]
            [cljs.analyzer :as ana]
            [cljs.env :as env]
            [cljs.util :as util]
            [cljs.repl :as repl]
            [cljs.cli :as cli]
            [cljs.compiler :as comp]
            [cljs.closure :as closure]
            [cljs.stacktrace :as st])
  (:import [javax.script ScriptEngine ScriptEngineManager ScriptException ScriptEngineFactory]))

(util/compile-if (Class/forName "jdk.nashorn.api.scripting.NashornException")
  (do
    (import 'jdk.nashorn.api.scripting.NashornException)
    ;; Implementation

    (defn create-engine
      ([] (create-engine nil))
      ([{:keys [code-cache] :or {code-cache true}}]
       (let [args (when code-cache ["-pcc"])
             factories (.getEngineFactories (ScriptEngineManager.))
             factory (get (zipmap (map #(.getEngineName %) factories) factories) "Oracle Nashorn")]
         (if-let [engine (if-not (empty? args)
                           (.getScriptEngine ^ScriptEngineFactory factory (into-array args))
                           (.getScriptEngine ^ScriptEngineFactory factory))]
           (let [context (.getContext engine)]
             (.setWriter context *out*)
             (.setErrorWriter context *err*)
             engine)
           (throw (IllegalArgumentException.
                    "Cannot find the Nashorn script engine, use a JDK version 8 or higher."))))))

    (defn eval-str [^ScriptEngine engine ^String s]
      (.eval engine s))

    (defn eval-resource
      "Evaluate a file on the classpath in the engine."
      [engine path debug]
      (let [r (io/resource path)]
        (eval-str engine (slurp r))
        (when debug (println "loaded: " path))))

    (defn init-engine [engine {:keys [output-dir] :as opts} debug]
      (eval-str engine (format "var CLJS_DEBUG = %s;" (boolean debug)))
      (eval-str engine (format "var CLJS_OUTPUT_DIR = \"%s\";" output-dir))
      (eval-resource engine "goog/base.js" debug)
      (eval-resource engine "goog/deps.js" debug)
      (eval-resource engine "cljs/bootstrap_nashorn.js" debug)
      (eval-str engine
        (format "goog.global.CLOSURE_UNCOMPILED_DEFINES = %s;"
          (json/write-str (:closure-defines opts))))
      engine)

    (defn tear-down-engine [engine]
      (eval-str engine "nashorn_tear_down();"))

    (defn load-js-file [engine file]
      (eval-str engine (format "nashorn_load(\"%s\");" file)))

    ;; Create a minimal build of Clojurescript from the core library.
    ;; Copied from clj.cljs.repl.node.
    (defn bootstrap-repl [engine output-dir opts]
      (env/ensure
        (let [deps-file ".nashorn_repl_deps.js"
              core (io/resource "cljs/core.cljs")
              core-js (closure/compile core
                        (assoc opts :output-file
                          (closure/src-file->target-file
                            core (dissoc opts :output-dir))))
              deps (closure/add-dependencies opts core-js)]
          ;; output unoptimized code and the deps file
          ;; for all compiled namespaces
          (apply closure/output-unoptimized
            (assoc opts :output-to (.getPath (io/file output-dir deps-file)))
            deps)
          ;; load the deps file so we can goog.require cljs.core etc.
          (load-js-file engine deps-file))))

    (defn load-ns [engine ns]
      (eval-str engine
        (format "goog.require(\"%s\");" (comp/munge (first ns)))))

    ;; Nashorn script stacktraces have a relative path which includes the output-dir
    (defn- strip-file-name [^String file-name output-dir]
      (let [with-slash (str output-dir "/")]
        (if (.startsWith file-name with-slash)
          (string/replace-first file-name with-slash "")
          file-name)))

    (def repl-filename "<cljs repl>")

    (defrecord NashornEnv [engine debug]
      repl/IReplEnvOptions
      (-repl-options [this]
        {:output-dir ".cljs_nashorn_repl"
         :target :nashorn})
      repl/IJavaScriptEnv
      (-setup [this {:keys [output-dir bootstrap output-to] :as opts}]
        (init-engine engine opts debug)
        (let [env (ana/empty-env)]
          (if output-to
            (load-js-file engine output-to)
            (bootstrap-repl engine output-dir opts))
          (repl/evaluate-form this env repl-filename
            '(.require js/goog "cljs.core"))
          ;; monkey-patch goog.isProvided_ to suppress useless errors
          (repl/evaluate-form this env repl-filename
            '(set! js/goog.isProvided_ (fn [ns] false)))
          ;; monkey-patch goog.require to be more sensible
          (repl/evaluate-form this env repl-filename
            '(do
               (set! *loaded-libs* #{"cljs.core"})
               (set! (.-require js/goog)
                 (fn [name reload]
                   (when (or (not (contains? *loaded-libs* name)) reload)
                     (set! *loaded-libs* (conj (or *loaded-libs* #{}) name))
                     (js/CLOSURE_IMPORT_SCRIPT
                       (if (some? goog/debugLoader_)
                         (.getPathFromDeps_ goog/debugLoader_ name)
                         (goog.object/get (.. js/goog -dependencies_ -nameToPath) name))))))))))
      (-evaluate [{engine :engine :as this} filename line js]
        (when debug (println "Evaluating: " js))
        (try
          {:status :success
           :value (if-let [r (eval-str engine js)] (.toString r) "")}
          (catch ScriptException e
            (let [^Throwable root-cause (clojure.stacktrace/root-cause e)]
              {:status :exception
               :value (.getMessage root-cause)
               :stacktrace (NashornException/getScriptStackString root-cause)}))
          (catch Throwable e
            (let [^Throwable root-cause (clojure.stacktrace/root-cause e)]
              {:status :exception
               :value (.getMessage root-cause)
               :stacktrace
               (apply str
                 (interpose "\n"
                   (map str
                     (.getStackTrace root-cause))))}))))
      (-load [{engine :engine :as this} ns url]
        (load-ns engine ns))
      (-tear-down [this]
        (tear-down-engine engine))
      repl/IParseStacktrace
      (-parse-stacktrace [this frames-str ret opts]
        (st/parse-stacktrace this frames-str
          (assoc ret :ua-product :nashorn) opts))
      repl/IParseError
      (-parse-error [_ err _]
        (update-in err [:stacktrace]
          (fn [st]
            (string/join "\n" (drop 1 (string/split st #"\n")))))))

    (defn repl-env* [{:keys [debug] :as opts}]
      (let [engine (create-engine opts)]
        (merge
          (NashornEnv. engine debug)
          opts)))

    (defn repl-env
      "Create a Nashorn repl-env for use with the repl/repl* method in Clojurescript."
      [& {:as opts}]
      (repl-env* opts))

    ;; -------------------------------------------------------------------------
    ;; Command Line Support

    (defn -main [& args]
      (apply cli/main repl-env args)))

  (do
    (defn repl-env* [{:keys [debug] :as opts}]
      (throw (ex-info "Nashorn not supported" {:type :repl-error})))

    (defn repl-env
      "Create a Nashorn repl-env for use with the repl/repl* method in Clojurescript."
      [& {:as opts}]
      (throw (ex-info "Nashorn not available under this Java runtime" {:type :repl-error})))

    (defn -main []
      (throw (ex-info "Nashorn not available under this Java runtime" {:type :repl-error})))))



