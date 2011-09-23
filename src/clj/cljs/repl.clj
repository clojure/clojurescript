;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl
  (:refer-clojure :exclude [load-file])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [cljs.compiler :as comp]
            [cljs.closure :as cljsc]))

(def ^:dynamic *cljs-verbose* false)

(defprotocol IJavaScriptEnv
  (-setup [this] "initialize the environment")
  (-evaluate [this line js] "evaluate a javascript string")
  (-load [this ns url] "load code at url into the environment")
  (-put [this k f] "set mutable state in the environment")
  (-tear-down [this] "dispose of the environment"))

(defn load-namespace
  "Load a namespace and all of its dependencies into the evaluation environment.
  The environment is responsible for ensuring that each namespace is loaded once and
  only once."
  [repl-env sym]
  (let [sym (if (and (seq? sym)
                     (= (first sym) 'quote))
              (second sym)
              sym)
        opts {:output-dir (get repl-env :working-dir ".repl")}
        deps (->> (cljsc/add-dependencies opts {:requires [(name sym)] :type :seed})
                  (remove (comp #{["goog"]} :provides))
                  (remove (comp #{:seed} :type))
                  (map #(select-keys % [:provides :url])))]
    (doseq [{:keys [url provides]} deps]
      (-load repl-env provides url))))

(defn- load-dependencies
  [repl-env requires]
  (doseq [ns requires]
    (load-namespace repl-env ns)))

(defn- display-error
  ([ret form js]
     (display-error ret form js (constantly nil)))
  ([ret form js f]
     (when-not (and (seq? form) (= 'ns (first form)))
       (f)
       (println (:value ret))
       (when-let [st (:stacktrace ret)]
         (println st)))))

(defn evaluate-form
  "Evaluate a ClojureScript form in the JavaScript environment. Returns a
  string which is the ClojureScript return value. This string may or may
  not be readable by the Clojure reader."
  [repl-env env form]
  (try
    (let [ast (comp/analyze env form)
          js (comp/emits ast)]
      (when (= (:op ast) :ns)
        (load-dependencies repl-env (vals (:requires ast))))
      (when *cljs-verbose*
        (print js))
      (let [ret (-evaluate repl-env (:line (meta form)) js)]
        (case (:status ret)
          ;;we eat ns errors because we know goog.provide() will throw when reloaded
          ;;TODO - file bug with google, this is bs error
          ;;this is what you get when you try to 'teach new developers'
          ;;via errors (goog/base.js 104)
          :error (display-error ret form js)
          :exception (display-error ret form js
                       #(prn "Error evaluating:" form :as js))
          :success (:value ret))))
    (catch Throwable ex
      (.printStackTrace ex)
      (println (str ex)))))

(defn load-stream [repl-env stream]
  (with-open [r (io/reader stream)]
    (let [env {:ns (@comp/namespaces comp/*cljs-ns*) :context :statement :locals {}}
          pbr (clojure.lang.LineNumberingPushbackReader. r)
          eof (Object.)]
      (loop [r (read pbr false eof false)]
        (let [env (assoc env :ns (@comp/namespaces comp/*cljs-ns*))]
          (when-not (identical? eof r)
            (evaluate-form repl-env env r)
            (recur (read pbr false eof false))))))))

(defn load-file
  [repl-env f]
  (binding [comp/*cljs-ns* 'cljs.user]
    (let [res (if (= \/ (first f)) f (io/resource f))]
      (assert res (str "Can't find " f " in classpath"))
      (-put repl-env :filename f)
      (load-stream repl-env res))))

(defn- eval-and-print [repl-env env form]
  (let [ret (evaluate-form repl-env
                           (assoc env :ns (@comp/namespaces comp/*cljs-ns*))
                           (if (and (seq? form) (= 'ns (first form)))
                             form
                             (list 'cljs.core.pr-str form)))]
    (try (prn (read-string ret))
         (catch Exception e
           (if (string? ret)
             (println ret)
             (prn nil))))))

(defn- read-next-form []
  (try {:status :success :form (binding [*ns* (create-ns comp/*cljs-ns*)]
                                 (read))}
       (catch Exception e
         (println (.getMessage e))
         {:status :error})))

(defn repl
  "Note - repl will reload core.cljs every time, even if supplied old repl-env"
  [repl-env & {:keys [verbose warn-on-undeclared]}]
  (prn "Type: " :cljs/quit " to quit")
  (binding [comp/*cljs-ns* 'cljs.user
            *cljs-verbose* verbose
            comp/*cljs-warn-on-undeclared* warn-on-undeclared]
    (let [env {:context :statement :locals {}}]
      (-setup repl-env)
      (loop []
        (print (str "ClojureScript:" comp/*cljs-ns* "> "))
        (flush)
        (let [{:keys [status form]} (read-next-form)]
          (cond
           (= form :cljs/quit) :quit
           
           (= status :error) (recur)
           
           (and (seq? form) (= (first form) 'in-ns))
           (do (set! comp/*cljs-ns* (second (second form))) (newline) (recur))
           
           (and (seq? form) ('#{load-file clojure.core/load-file} (first form)))
           (do (load-file repl-env (second form)) (newline) (recur))
           
           (and (seq? form) ('#{load-namespace} (first form)))
           (do (load-namespace repl-env (second form)) (newline) (recur))
           
           :else
           (do (eval-and-print repl-env env form) (recur)))))
      (-tear-down repl-env))))

