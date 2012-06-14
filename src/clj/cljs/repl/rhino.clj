;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl.rhino
  (:refer-clojure :exclude [loaded-libs])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [cljs.compiler :as comp]
            [cljs.repl :as repl])
  (:import cljs.repl.IJavaScriptEnv
           [org.mozilla.javascript Context ScriptableObject]))

(def current-repl-env (atom nil))
(def loaded-libs (atom #{}))

;;todo - move to core.cljs, using js
(def ^String bootjs (str "goog.require = function(rule){"
                         "Packages.clojure.lang.RT[\"var\"](\"cljs.repl.rhino\",\"goog-require\")"
                         ".invoke(rule);}"))

(defprotocol IEval
  (-eval [this env filename line]))

(extend-protocol IEval
  
  java.lang.String
  (-eval [this {:keys [cx scope]} filename line]
    (.evaluateString cx scope this filename line nil))
  
  java.io.Reader
  (-eval [this {:keys [cx scope]} filename line]
    (.evaluateReader cx scope this filename line nil))
  )

(defmulti stacktrace class)

(defmethod stacktrace :default [e]
  (apply str (interpose "\n" (map #(str "        " (.toString %)) (.getStackTrace e)))))

(defmethod stacktrace org.mozilla.javascript.RhinoException [e]
  (.getScriptStackTrace e))

(defmulti eval-result class)

(defmethod eval-result :default [r]
  (.toString r))

(defmethod eval-result nil [_] "")

(defmethod eval-result org.mozilla.javascript.Undefined [_] "")

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

(defn goog-require [rule]
  (when-not (contains? @loaded-libs rule)
    (let [repl-env @current-repl-env
          path (string/replace (comp/munge rule) \. java.io.File/separatorChar)
          cljs-path (str path ".cljs")
          js-path (str "goog/"
                       (-eval (str "goog.dependencies_.nameToPath['" rule "']")
                              repl-env
                              "<cljs repl>"
                              1))]
      (if-let [res (io/resource cljs-path)]
        (binding [comp/*cljs-ns* 'cljs.user]
          (repl/load-stream repl-env res))
        (if-let [res (io/resource js-path)]
          (-eval (io/reader res) repl-env js-path 1)
          (throw (Exception. (str "Cannot find " cljs-path " or " js-path " in classpath")))))
      (swap! loaded-libs conj rule))))

(defn load-javascript [repl-env ns url]
  (let [missing (remove #(contains? @loaded-libs %) ns)]
    (when (seq missing)
      (do (try 
            (-eval (io/reader url) repl-env (.toString url) 1)
            ;; TODO: don't show errors for goog/base.js line number 105
            (catch Throwable ex (println (.getMessage ex))))
          (swap! loaded-libs (partial apply conj) missing)))))

(defn rhino-setup [repl-env]
  (let [env (comp/empty-env)
        scope (:scope repl-env)]
    (repl/load-file repl-env "cljs/core.cljs")
    (swap! loaded-libs conj "cljs.core")
    (repl/evaluate-form repl-env
                        env
                        "<cljs repl>"
                        '(ns cljs.user))
    (ScriptableObject/putProperty scope
                                  "out"
                                  (Context/javaToJS System/out scope))
    (repl/evaluate-form repl-env
                        env
                        "<cljs repl>"
                        '(set! *print-fn* (fn [x] (.print js/out x))))))

(extend-protocol repl/IJavaScriptEnv
  clojure.lang.IPersistentMap
  (-setup [this]
    (rhino-setup this))
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
        new-repl-env {:cx cx :scope scope}]
    (assert base "Can't find goog/base.js in classpath")
    (assert deps "Can't find goog/deps.js in classpath")
    (swap! current-repl-env (fn [old] new-repl-env))
    (with-open [r (io/reader base)]
      (-eval r new-repl-env "goog/base.js" 1))
    (-eval bootjs new-repl-env "bootjs" 1)
    ;; Load deps.js line-by-line to avoid 64K method limit
    (doseq [^String line (line-seq (io/reader deps))]
      (-eval line new-repl-env "goog/deps.js" 1))
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
