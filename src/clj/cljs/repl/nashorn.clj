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
            [cljs.analyzer :as ana]
            [cljs.env :as env]
            [cljs.repl :as repl]
            [cljs.compiler :as comp]
            [cljs.closure :as closure])
  (:import [javax.script ScriptEngine ScriptEngineManager ScriptException]
           [jdk.nashorn.api.scripting NashornException]))

;; Nashorn Clojurescript repl binding.
;;
;; Uses the Nashorn load() function to load Javascript files into the script engine.
;;
;; Nashorn's load() function docs:
;; http://docs.oracle.com/javase/8/docs/technotes/guides/scripting/nashorn/shell.html
;;
;; Some functions are copied from: https://github.com/bodil/cljs-nashorn and the node.js repl code

;;
;; ** Usage from Leiningen:
;;
;; Create a file init_repl_test.clj containing (adjust :output-dir to your cljsbuild settings):

(comment
  (ns init-repl-test
    (:require [cljs.repl]
              [cljs.repl.nashorn]))

  (cljs.repl/repl (cljs.repl.nashorn/repl-env)
    :output-dir "resources/public/compiled"
    :cache-analysis true)
  )

;;
;; Invoke it with:
;; lein trampoline run -m clojure.main src-clj/init_repl_test.clj

;;
;; ** Usage from nrepl / piggieback, execute the following at the nrepl prompt:
;;    (adjust :output-dir to your cljsbuild settings)

(comment
  (ns init-repl-piggieback
    (:require [cljs.repl.nashorn]
              [cemerick.piggieback]))

  (cemerick.piggieback/cljs-repl
    :repl-env (cljs.repl.nashorn/repl-env)
    :output-dir "resources/public/compiled"
    :cache-analysis true)
  )

;; Implementation

(defn create-engine []
  (if-let [engine (.getEngineByName (ScriptEngineManager.) "nashorn")]
    (let [context (.getContext engine)]
      (.setWriter context *out*)
      (.setErrorWriter context *err*)
      engine)
    (throw (IllegalArgumentException.
            "Cannot find the Nashorn script engine, use a JDK version 8 or higher."))))

(defn eval-str [^ScriptEngine engine ^String s]
  (.eval engine s))

(defn eval-resource 
  "Evaluate a file on the classpath in the engine."
  [engine path debug]
  (let [r (io/resource path)]
    (eval-str engine (slurp r))
    (when debug (println "loaded: " path))))

(defn init-engine [engine output-dir debug]
  (eval-resource engine "goog/base.js" debug)
  (eval-resource engine "goog/deps.js" debug)
  (eval-str engine "var global = this") ; required by React
  (eval-str engine
    (format
      (str "var nashorn_load = function(path) {"
           "  var outputPath = \"%s\" + \"/\" + path;"
           (when debug "  print(\"loading: \" + outputPath) ; ")
           "  load(outputPath);"
           "};")
      output-dir))
  (eval-str engine
    (str "goog.global.CLOSURE_IMPORT_SCRIPT = function(path) {"
         " nashorn_load(\"goog/\" + path);"
         " return true;"
         "};"))
  (eval-str engine "goog.global.isProvided_ = function(name) { return false; };")
  engine)

(defn load-js-file [engine file]
  (eval-str engine (format "nashorn_load(\"%s\");" file)))

;; Create a minimal build of Clojurescript from the core library.
;; Copied from clj.cljs.repl.node.
(defn bootstrap-repl [engine output-dir opts]
  (env/ensure
    (let [deps-file ".nashorn_repl_deps.js"
          core (io/resource "cljs/core.cljs")
          core-js (closure/compile core
                    (assoc opts
                      :output-file (closure/src-file->target-file core)))
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

(defn- convert-stacktrace-element [^StackTraceElement el]
  {:function (.getMethodName el)
   :file (.getFileName el)
   :line (.getLineNumber el)
   :column 0})

(def repl-filename "<cljs repl>")

(defrecord NashornEnv [engine debug]
  repl/IReplEnvOptions
  (-repl-options [this] {})
  repl/IJavaScriptEnv
  (-setup [this  {:keys [output-dir bootstrap output-to] :as opts}]
    (init-engine engine output-dir debug)
    (let [env (ana/empty-env)]
      (if output-to
        (load-js-file engine output-to)
        (bootstrap-repl engine output-dir opts))
      (repl/evaluate-form this env repl-filename
        '(do
           (.require js/goog "cljs.core")
           (set! *print-newline* false)
           (set! *print-fn* js/print)))
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
                   (aget (.. js/goog -dependencies_ -nameToPath) name)))))))))
  (-evaluate [{engine :engine :as this} filename line js]
    (when debug (println "Evaluating: " js))
    (try
      {:status :success
       :value (if-let [r (eval-str engine js)] (.toString r) "")}

      ;; Stringify the stacktrace to edn for easy parsing in -parse-stacktrace
      (catch ScriptException e
        (let [^Throwable root-cause (clojure.stacktrace/root-cause e)]
          {:status :exception
           :value (.getMessage root-cause)
           :stacktrace (pr-str (map convert-stacktrace-element
                                    (NashornException/getScriptFrames root-cause)))}))
      (catch Throwable e
        (let [^Throwable root-cause (clojure.stacktrace/root-cause e)]
          {:status :exception
           :value (.getMessage root-cause)
           :stacktrace (pr-str (map convert-stacktrace-element (.getStackTrace root-cause)))}))))
  (-load [{engine :engine :as this} ns url]
    (load-ns engine ns))
  (-tear-down [this])
  repl/IParseStacktrace
  (-parse-stacktrace [this frames-str ret {output-dir :output-dir}]
    (when-let [frames (read-string frames-str)]
      (vec (map #(update-in %1 [:file] (fn [s] (strip-file-name s output-dir))) frames)))))

(defn repl-env 
  "Create a Nashorn repl-env for use with the repl/repl* method in Clojurescript and as the
   :repl-env argument to piggieback/cljs-repl. Besides the usual repl options (e.g. :source-map),
   opts has the following extra parameters:
   
   :output-dir  the directory of the compiled files, e.g. \"resources/public/my-app\" (mandatory).
   :output-to   load this file initially into Nashorn, relative to output-dir.
                Use a minimal bootstrapped cljs.core environment if not specified."
  [& {:keys [debug] :as opts}]
  (let [engine (create-engine)
        compiler-env (env/default-compiler-env)]
    (merge (NashornEnv. engine debug)
      {:cljs.env/compiler compiler-env}  ; required by cider middleware ?
      opts)))
