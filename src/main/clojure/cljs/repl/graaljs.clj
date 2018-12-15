;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl.graaljs
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
  (:import [javax.script ScriptEngine ScriptException]))

(defn- js-opt-key? [k]
  (and (string? k)
       (string/starts-with? k "js.")))

(defn- form-js-opts [opts]
  (for [[k v] opts
        :when (js-opt-key? k)]
    `(.option ~k ~v)))

(defn create-engine [opts]
  ;; In order to support AOT compilation by JVMs that don't have
  ;; GraalVM available, we load and execute engine creation code
  ;; here at runtime.
  (import '(com.oracle.truffle.js.scriptengine GraalJSScriptEngine))
  (import '(org.graalvm.polyglot Context))
  (let [engine  (eval `(GraalJSScriptEngine/create nil
                         (-> (Context/newBuilder (make-array String 0))
                           ~@(form-js-opts opts)
                           (.allowAllAccess true)
                           (.allowNativeAccess true))))
        context (.getContext engine)]
    (.setWriter context *out*)
    (.setErrorWriter context *err*)
    engine))

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
  (eval-resource engine "cljs/bootstrap_graaljs.js" debug)
  (eval-str engine
    (format "goog.global.CLOSURE_UNCOMPILED_DEFINES = %s;"
      (json/write-str (:closure-defines opts))))
  engine)

(defn tear-down-engine [engine]
  (eval-str engine "graaljs_tear_down();"))

(defn load-js-file [engine file]
  (eval-str engine (format "graaljs_load(\"%s\");" file)))

;; Create a minimal build of ClojureScript from the core library.
;; Copied from clj.cljs.repl.node.
(defn bootstrap-repl [engine output-dir opts]
  (env/ensure
    (let [deps-file ".graaljs_repl_deps.js"
          core      (io/resource "cljs/core.cljs")
          core-js   (closure/compile core
                      (assoc opts :output-file
                                  (closure/src-file->target-file
                                    core (dissoc opts :output-dir))))
          deps      (closure/add-dependencies opts core-js)]
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

(def repl-filename "<cljs repl>")

(def ^:private skip-types #{"com.oracle.truffle.api.interop.java.TruffleMap"
                            "com.oracle.truffle.api.interop.java.TruffleMap$FunctionTruffleMap"})

(defn- safe-to-string
  "A safe version that avoids calling .toString on types known to cause stack overflow.
  Also has a guard to return an unreadable containing the type if this is encountered."
  [x]
  (let [type-str (pr-str (type x))]
    (try
      (if (contains? skip-types type-str)
        (str #"<" type-str ">")
        (.toString x))
      (catch StackOverflowError _
        (str "#<stackoverflow " type-str ">")))))

(defrecord GraalJSEnv [engine debug]
  repl/IReplEnvOptions
  (-repl-options [this]
    {:output-dir ".cljs_graaljs_repl"
     :target     :graaljs})
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
       :value  (if-let [r (eval-str engine js)] (safe-to-string r) "")}
      (catch ScriptException e
        {:status :exception
         :value  (eval-str engine "cljs.repl.error__GT_str(cljs.core._STAR_e)")})
      (catch Throwable e
        (let [^Throwable root-cause (clojure.stacktrace/root-cause e)]
          {:status :exception
           :value  (cljs.repl/ex-str (cljs.repl/ex-triage (Throwable->map root-cause)))}))))
  (-load [{engine :engine :as this} ns url]
    (load-ns engine ns))
  (-tear-down [this]
    (tear-down-engine engine))
  repl/IParseStacktrace
  (-parse-stacktrace [this frames-str ret opts]
    (st/parse-stacktrace this frames-str
      (assoc ret :ua-product :graaljs) opts))
  repl/IParseError
  (-parse-error [_ err _]
    (update-in err [:stacktrace]
      (fn [st]
        (string/join "\n" (drop 1 (string/split st #"\n")))))))

(def ^:private default-js-opts
  {"js.timer-resolution" "1"})

(defn repl-env* [{:keys [debug] :as opts}]
  (let [opts (merge default-js-opts opts)
        engine (create-engine opts)]
    (merge
      (GraalJSEnv. engine debug)
      opts)))

(defn repl-env
  "Create a Graal.JS repl-env for use with the repl/repl* method in ClojureScript."
  [& {:as opts}]
  (repl-env* opts))

;; -------------------------------------------------------------------------
;; Command Line Support

(defn -main [& args]
  (apply cli/main repl-env args))
