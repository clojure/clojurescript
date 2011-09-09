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
  (:import cljs.repl.IJavaScriptEnv))

;;todo - move to core.cljs, using js
(def ^String bootjs "
goog.require = function(rule){Packages.clojure.lang.RT[\"var\"](\"cljs.repl.rhino\",\"goog-require\").invoke(goog.global.cljs_javascript_engine, rule);}
")

(defn rhino-eval
  [repl-env line js]
  (try
    (let [jse ^javax.script.ScriptEngine (:jse repl-env)
          filename (.get jse javax.script.ScriptEngine/FILENAME)
          linenum (or line Integer/MIN_VALUE)
          ctx (sun.org.mozilla.javascript.internal.Context/enter)]
      (try
        {:status :success
         :value (.evaluateString ctx (:global repl-env) js filename linenum nil)} 
        (finally
         (sun.org.mozilla.javascript.internal.Context/exit))))
    (catch Throwable ex
      {:status :exception
       :value (.getMessage ex)
       :stacktrace (apply str (interpose "\n" (map #(.toString %) (.getStackTrace ex))))})))

(def loaded-libs (atom #{}))

(defn goog-require [repl-env rule]
  (when-not (contains? @loaded-libs rule)
    (let [jse ^javax.script.ScriptEngine (:jse repl-env)
          path (string/replace (comp/munge rule) \. java.io.File/separatorChar)
          cljs-path (str path ".cljs")
          js-path (str "goog/" (.eval jse (str "goog.dependencies_.nameToPath['" rule "']")))]
      (if-let [res (io/resource cljs-path)]
        (binding [comp/*cljs-ns* 'cljs.user]
          (repl/load-stream repl-env res))
        (if-let [res (io/resource js-path)]
          (.eval jse (io/reader res))
          (throw (Exception. (str "Cannot find " cljs-path " or " js-path " in classpath")))))
      (swap! loaded-libs conj rule))))

(defn load-javascript [repl-env ns url]
  (let [missing (remove #(contains? @loaded-libs %) ns)]
    (when (seq missing)
      (let [jse ^javax.script.ScriptEngine (:jse repl-env)]
        (try 
          (.eval jse (io/reader url))
          ;; TODO: don't show errors for goog/base.js line number 105
          (catch Throwable ex (println (.getMessage ex))))
        (swap! loaded-libs (partial apply conj) missing)))))

(defn rhino-setup [repl-env]
  (let [env {:context :statement :locals {}}]
    (repl/load-file repl-env "cljs/core.cljs")
    (repl/evaluate-form repl-env
                        (assoc env :ns (@comp/namespaces comp/*cljs-ns*))
                        '(ns cljs.user))
    (.put ^javax.script.ScriptEngine (:jse repl-env)
          javax.script.ScriptEngine/FILENAME "<cljs repl>")))

(extend-protocol repl/IJavaScriptEnv
  clojure.lang.IPersistentMap
  (-setup [this]
    (rhino-setup this))
  (-evaluate [this line js]
    (rhino-eval this line js))
  (-load [this ns url]
    (load-javascript this ns url))
  (-put [this k v]
    (case k
      :filename (.put ^javax.script.ScriptEngine (:jse this)
                      javax.script.ScriptEngine/FILENAME v)))
  (-tear-down [_] nil))

(defn repl-env
  "Returns a fresh JS environment, suitable for passing to repl.
  Hang on to return for use across repl calls."
  []
  (let [jse (-> (javax.script.ScriptEngineManager.) (.getEngineByName "JavaScript"))
        base (io/resource "goog/base.js")
        deps (io/resource "goog/deps.js")
        new-repl-env {:jse jse :global (.eval jse "this")}]
    (assert base "Can't find goog/base.js in classpath")
    (assert deps "Can't find goog/deps.js in classpath")
    (.put jse javax.script.ScriptEngine/FILENAME "goog/base.js")
    (.put jse "cljs_javascript_engine" new-repl-env)
    (with-open [r (io/reader base)]
      (.eval jse r))
    (.eval jse bootjs)
    ;; Load deps.js line-by-line to avoid 64K method limit
    (doseq [^String line (line-seq (io/reader deps))]
      (.eval jse line))
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
  (throw (java.lang.Exception. "hello"))
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
