;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl.rhino
  (:refer-clojure :exclude [load-file])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [cljs.compiler :as comp]
            [cljs.closure :as closure]
            [cljs.analyzer :as ana]
            [cljs.repl :as repl]
            [cljs.util :as util])
  (:import [java.io File Reader]
           [org.mozilla.javascript Context ScriptableObject
                                   RhinoException Undefined]))

(def ^String bootjs
  (str "var global = this;\n"
       "CLOSURE_IMPORT_SCRIPT = function(src) {\n"
       "    var ns = \"cljs.repl.rhino\","
       "        name = \"load-file\","
       "        loadFile = Packages.clojure.lang.RT[\"var\"](ns,name);\n"
       "    if(src) loadFile.invoke(___repl_env, __repl_opts, src);\n"
       "};\n"))

;; =============================================================================
;; Protocols

(defprotocol IEval
  (-eval [this env filename line]))

(extend-protocol IEval
  String
  (-eval [this {:keys [cx scope]} filename line]
    (.evaluateString cx scope this filename line nil))
  
  Reader
  (-eval [this {:keys [cx scope]} filename line]
    (.evaluateReader cx scope this filename line nil)))

;; =============================================================================
;; Stacktrace & eval support

(defmulti stacktrace class)

(defmethod stacktrace :default [e]
  (apply str
    (interpose "\n"
      (map #(str "        " (.toString %))
        (.getStackTrace e)))))

(defmethod stacktrace RhinoException [^RhinoException e]
  (.getScriptStackTrace e))

(defmulti eval-result class)

(defmethod eval-result :default [r]
  (.toString r))

(defmethod eval-result nil [_] "")

(defmethod eval-result Undefined [_] "")

;; =============================================================================

(defn rhino-eval
  [repl-env filename line js]
  (try
    (let [linenum (or line Integer/MIN_VALUE)]
      {:status :success
       :value (eval-result (-eval js repl-env filename linenum))})
    (catch Throwable ex
      {:status :exception
       :value (.toString ex)
       :stacktrace (stacktrace ex)})))

(defn load-file
  "Load a JavaScript. This is needed to load JavaScript files before the Rhino
   environment is bootstrapped. After bootstrapping load-javascript will be
   used."
  [repl-env opts src]
  (let [goog-path (io/file (util/output-directory opts) "goog" src)]
    (rhino-eval repl-env (.getPath goog-path) 1 (slurp goog-path))))

(defn load-javascript [repl-env ns url]
  (try
    (with-open [reader (io/reader url)]
      (-eval reader repl-env (.toString url) 1))
    ;; TODO: don't show errors for goog/base.js line number 105
    (catch Throwable ex (println (.getMessage ex)))))

(defn rhino-setup [repl-env opts]
  (let [opts    (merge {:output-dir ".cljs_rhino_repl"} opts)
        scope   (:scope repl-env)
        env     (ana/empty-env)
        core    (io/resource "cljs/core.cljs")
        base-js (slurp (io/resource "goog/base.js"))
        core-js (closure/compile core
                  (assoc opts
                    :output-file
                    (closure/src-file->target-file core)))
        deps    (closure/add-dependencies opts core-js)
        output-dir (util/output-directory opts)
        repl-deps (io/file output-dir "rhino_repl_deps.js")]
    ;; emit core and deps
    (apply closure/output-unoptimized
      (assoc opts :output-to (.getPath repl-deps)) deps)

    ;; setup back references & output stream
    (ScriptableObject/putProperty scope
      "___repl_env" (Context/javaToJS repl-env scope))
    (ScriptableObject/putProperty scope "__repl_opts"
      (Context/javaToJS opts scope))
    (ScriptableObject/putProperty scope
      "out" (Context/javaToJS *out* scope))

    ;; define file loading, load goog.base, load repl deps
    (rhino-eval repl-env "bootjs" 1 bootjs)
    (rhino-eval repl-env "goog/base.js" 1 base-js)
    (rhino-eval repl-env "rhino_repl_deps.js" 1 (slurp repl-deps))

    ;; === Bootstrap ===
    ;; load cljs.core, setup printing
    (repl/evaluate-form repl-env env "<cljs repl>"
      '(do
         (.require js/goog "cljs.core")
         (set! *print-fn* (fn [x] (.write js/out x)))))

    ;; allow namespace reloading
    (repl/evaluate-form repl-env env "<cljs repl>"
      '(set! js/goog.isProvided_ (fn [x] false)))

    ;; monkey-patch goog.require
    (repl/evaluate-form repl-env env "<cljs repl>"
      '(do
         (set! *loaded-libs* #{"cljs.core"})
         (set! (.-require js/goog)
           (fn [name reload]
             (when (or (not (contains? *loaded-libs* name)) reload)
               (set! *loaded-libs* (conj (or *loaded-libs* #{}) name))
               (js/CLOSURE_IMPORT_SCRIPT
                 (aget (.. js/goog -dependencies_ -nameToPath) name)))))))))

(defrecord RhinoEnv []
  repl/IReplEnvOptions
  (-repl-options [this]
    {:require-foreign true})
  repl/IJavaScriptEnv
  (-setup [this opts]
    (rhino-setup this opts))
  (-evaluate [this filename line js]
    (rhino-eval this filename line js))
  (-load [this ns url]
    (load-javascript this ns url))
  (-tear-down [_] (Context/exit)))

(defn repl-env
  "Returns a fresh JS environment, suitable for passing to repl.
  Hang on to return for use across repl calls."
  []
  (let [cx (Context/enter)]
    ;; just avoid the 64K method limit
    ;; Rhino is slow even with optimizations enabled
    (.setOptimizationLevel cx -1)
    (merge (RhinoEnv.)
      {:cx cx
       :scope (.initStandardObjects cx)})))

(comment

  (require '[cljs.repl :as repl])
  (require '[cljs.repl.rhino :as rhino])
  (def env (rhino/repl-env))
  (repl/repl env)
  (+ 1 1)
  "hello"
  {:a "hello"}
  (:a {:a "hello"})
  (:a {:a :b})
  (reduce + [1 2 3 4 5])
  (time (reduce + [1 2 3 4 5]))
  (even? :a)
  (throw (js/Error. "There was an error"))
  (clojure.core/load-file "clojure/string.cljs")
  (clojure.string/triml "   hello")
  (clojure.string/reverse "   hello")

  (load-namespace 'clojure.set)

  (ns test.crypt
    (:require [goog.crypt :as c]))
  (c/stringToByteArray "Hello")

  (load-namespace 'goog.date.Date)
  (goog.date.Date.)
 
  )
