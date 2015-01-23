;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl.rhino
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.repl :as repl]
            [cljs.util :as util])
  (:import [java.io File Reader]
           [org.mozilla.javascript Context ScriptableObject
                                   RhinoException Undefined]))

(def ^String bootjs
  (str "var global = this;"
       "goog.require = function(rule){"
       "Packages.clojure.lang.RT[\"var\"](\"cljs.repl.rhino\",\"goog-require\")"
       ".invoke(___repl_env, __repl_opts, rule);}"))

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
    (.setOptimizationLevel ^Context cx -1)
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

(defn goog-require [repl-env opts rule]
  (let [path        (string/replace (comp/munge rule) \. File/separatorChar)
        output-dir  (util/output-directory opts)
        cljsc-path  (str output-dir File/separator (str path ".js"))
        cljs-path   (str path ".cljs")
        gpath       (-eval (str "goog.dependencies_.nameToPath['" rule "']")
                      repl-env "<cljs repl>" 1)
        js-path     (str "goog/" gpath)
        js-out-path (io/file (str output-dir "/goog/" gpath))]
    (let [compiled (io/file cljsc-path)]
      (if (.exists compiled)
        ;; TODO: only take this path if analysis cache is available
        ;; - David
        (do
          (with-open [reader (io/reader compiled)]
            (-eval reader repl-env cljsc-path 1)))
        (if-let [res (io/resource cljs-path)]
          (binding [ana/*cljs-ns* 'cljs.user]
            (repl/load-stream repl-env cljs-path res))
          (if-let [res (io/resource js-path)]
            (with-open [reader (io/reader res)]
              (-eval reader repl-env js-path 1))
            (if (.exists js-out-path)
              (with-open [reader (io/reader js-out-path)]
                (-eval reader repl-env js-path 1))
              (throw
               (Exception.
                 (str "Cannot find "
                   cljs-path " or "
                   js-path " or "
                   (.getName js-out-path) " in classpath"))))))))))

(defn load-javascript [repl-env ns url]
  (try
    (with-open [reader (io/reader url)]
      (-eval reader repl-env (.toString url) 1))
    ;; TODO: don't show errors for goog/base.js line number 105
    (catch Throwable ex (println (.getMessage ex)))))

(defn rhino-setup [repl-env opts]
  (let [env   (ana/empty-env)
        scope (:scope repl-env)]
    (ScriptableObject/putProperty scope "__repl_opts"
      (Context/javaToJS opts scope))
    (repl/load-file repl-env "cljs/core.cljs" opts)
    (repl/evaluate-form repl-env env "<cljs repl>"
      '(ns cljs.user))
    (ScriptableObject/putProperty scope
      "out" (Context/javaToJS *out* scope))
    (binding [ana/*cljs-ns* 'cljs.core]
      (repl/evaluate-form repl-env env "<cljs repl>"
        '(do
           (set! (.-isProvided_ js/goog) (fn [_] false))
           (set! *print-fn* (fn [x] (.write js/out x))))))))

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
  (let [cx (Context/enter)
        scope (.initStandardObjects cx)
        base (io/resource "goog/base.js")
        deps (io/resource "goog/deps.js")
        new-repl-env (merge (RhinoEnv.) {:cx cx :scope scope})]
    (assert base "Can't find goog/base.js in classpath")
    (assert deps "Can't find goog/deps.js in classpath")
    (ScriptableObject/putProperty scope
      "___repl_env" (Context/javaToJS new-repl-env scope))
    (with-open [r (io/reader base)]
      (-eval r new-repl-env "goog/base.js" 1))
    (-eval bootjs new-repl-env "bootjs" 1)
    ;; Load deps.js line-by-line to avoid 64K method limit
    (with-open [reader (io/reader deps)]
      (doseq [^String line (line-seq reader)]
        (-eval line new-repl-env "goog/deps.js" 1)))
    new-repl-env))

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
  (load-file "clojure/string.cljs")
  (clojure.string/triml "   hello")
  (clojure.string/reverse "   hello")

  (load-namespace 'clojure.set)

  (ns test.crypt
    (:require [goog.crypt :as c]))
  (c/stringToByteArray "Hello")

  (load-namespace 'goog.date.Date)
  (goog.date.Date.)
 
  )
