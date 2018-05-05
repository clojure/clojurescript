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
            [clojure.data.json :as json]
            [cljs.compiler :as comp]
            [cljs.closure :as closure]
            [cljs.analyzer :as ana]
            [cljs.repl :as repl]
            [cljs.cli :as cli]
            [cljs.util :as util]
            [cljs.stacktrace :as st])
  (:import [java.io File Reader]
           [org.mozilla.javascript Context ScriptableObject
                                   RhinoException Undefined]))

(def ^String bootjs
  (str "var global = this;\n"
       "var CLOSURE_IMPORT_SCRIPT = function(src) {\n"
       "    var ns = \"cljs.repl.rhino\","
       "        name = \"load-file\","
       "        loadFile = Packages.clojure.lang.RT[\"var\"](ns,name);\n"
       "    if(src) loadFile.invoke(___repl_env, __repl_opts, src);\n"
       "    return true;\n"
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
  [{:keys [scope] :as repl-env} filename line js]
  (try
    (let [linenum (or line Integer/MIN_VALUE)]
      {:status :success
       :value (eval-result (-eval js repl-env filename linenum))})
    (catch Throwable ex
      ;; manually set *e
      (let [top-level (-> scope
                        (ScriptableObject/getProperty "cljs")
                        (ScriptableObject/getProperty "core"))]
        (ScriptableObject/putProperty top-level "_STAR_e"
          (Context/javaToJS ex scope))
        {:status :exception
         :value (.toString ex)
         :stacktrace (stacktrace ex)}))))

(defn load-file
  "Load a JavaScript. This is needed to load JavaScript files before the Rhino
   environment is bootstrapped. After bootstrapping load-javascript will be
   used."
  [repl-env opts src]
  (let [goog-path (io/file (util/output-directory opts) "goog" src)]
    (rhino-eval repl-env (.getPath goog-path) 1 (io/reader goog-path))))

(defn load-javascript [repl-env ns url]
  (try
    (with-open [reader (io/reader url)]
      (-eval reader repl-env (.toString url) 1))
    ;; TODO: don't show errors for goog/base.js line number 105
    (catch Throwable ex (println (.getMessage ex)))))

(defn rhino-setup [repl-env opts]
  (let [scope   (:scope repl-env)
        env     (ana/empty-env)
        core    (io/resource "cljs/core.cljs")
        base-js (io/resource "goog/base.js")
        core-js (closure/compile core
                  (assoc opts :output-file
                    (closure/src-file->target-file
                      core (dissoc opts :output-dir))))
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
    (ScriptableObject/putProperty scope
      "err" (Context/javaToJS *err* scope))

    ;; define file loading, load goog.base, load repl deps
    (rhino-eval repl-env "bootjs" 1 bootjs)
    (rhino-eval repl-env "goog/base.js" 1 (io/reader base-js))
    (rhino-eval repl-env "rhino_repl_deps.js" 1 (io/reader repl-deps))

    ;; === Bootstrap ===
    ;; load cljs.core, setup printing
    (repl/evaluate-form repl-env env "<cljs repl>"
      '(do
         (.require js/goog "cljs.core")
         (set! *print-fn* (fn [x] (.write js/out x)))
         (set! *print-err-fn* (fn [x] (.write js/err x)))))

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
                 (if (some? goog/debugLoader_)
                   (.getPathFromDeps_ goog/debugLoader_ name)
                   (goog.object/get (.. js/goog -dependencies_ -nameToPath) name))))))))

    ;; set closure-defines
    (rhino-eval repl-env "CLOSURE_UNCOMPILED_DEFINES" 1
      (str "goog.global.CLOSURE_UNCOMPILED_DEFINES = "
        (json/write-str (:closure-defines opts)) ";"))))

;; Catching errors and rethrowing in Rhino swallows the original trace
;; https://groups.google.com/d/msg/mozilla.dev.tech.js-engine.rhino/inMyVKhPq6M/cY39hX20_z8J
(defn wrap-fn [form]
  (cond
    (and (seq? form)
      (#{'ns 'require 'require-macros
         'use 'use-macros 'import 'refer-clojure} (first form)))
    identity

    ('#{*1 *2 *3 *e} form) (fn [x] `(cljs.core.pr-str ~x))

    :else
    (fn [x]
      `(cljs.core.pr-str
         (let [ret# ~x]
           (set! *3 *2)
           (set! *2 *1)
           (set! *1 ret#)
           ret#)))))

(defrecord RhinoEnv []
  repl/IReplEnvOptions
  (-repl-options [this]
    {:output-dir ".cljs_rhino_repl"
     :wrap wrap-fn})
  repl/IParseStacktrace
  (-parse-stacktrace [this frames-str ret opts]
    (st/parse-stacktrace this frames-str
      (assoc ret :ua-product :rhino) opts))
  repl/IGetError
  (-get-error [this e env opts]
    (let [{:keys [scope]} this
          ex (-> scope
               (ScriptableObject/getProperty "cljs")
               (ScriptableObject/getProperty "core")
               (ScriptableObject/getProperty "_STAR_e")
               .unwrap)]
      {:status :exception
       :value (.toString ex)
       :stacktrace (stacktrace ex)}))
  repl/IJavaScriptEnv
  (-setup [this opts]
    (rhino-setup this opts))
  (-evaluate [this filename line js]
    (rhino-eval this filename line js))
  (-load [this ns url]
    (load-javascript this ns url))
  (-tear-down [_] (Context/exit)))

(defn repl-env*
  [opts]
  (let [cx (Context/enter)]
    ;; just avoid the 64K method limit
    ;; Rhino is slow even with optimizations enabled
    (.setOptimizationLevel cx -1)
    (merge (RhinoEnv.)
      {:cx cx
       :scope (.initStandardObjects cx)})))

(defn repl-env
  "Returns a fresh JS environment, suitable for passing to repl.
  Hang on to return for use across repl calls."
  [& {:as opts}]
  (repl-env* opts))

(defn -main [& args]
  (apply cli/main repl-env args))

(comment

  (repl/-parse-stacktrace (repl-env)
    "\tat .cljs_rhino_repl/goog/../cljs/core.js:4215 (seq)
\tat .cljs_rhino_repl/goog/../cljs/core.js:4245 (first)
\tat .cljs_rhino_repl/goog/../cljs/core.js:5295 (ffirst)
\tat <cljs repl>:1
\tat <cljs repl>:1"
    nil
    {:output-dir ".cljs_rhino_repl"})

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
