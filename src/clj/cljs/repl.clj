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
  (-evaluate [this filename line js] "evaluate a javascript string")
  (-load [this ns url] "load code at url into the environment")
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
  ([ret form]
     (display-error ret form (constantly nil)))
  ([ret form f]
     (when-not (and (seq? form) (= 'ns (first form)))
       (f)
       (println (:value ret))
       (when-let [st (:stacktrace ret)]
         (println st)))))

(defn evaluate-form
  "Evaluate a ClojureScript form in the JavaScript environment. Returns a
  string which is the ClojureScript return value. This string may or may
  not be readable by the Clojure reader."
  ([repl-env env filename form]
     (evaluate-form repl-env env filename form identity))
  ([repl-env env filename form wrap]
     (try
       (let [ast (comp/analyze env form)
             js (comp/emits ast)
             wrap-js (comp/emits (binding [comp/*cljs-warn-on-undeclared* false
                                           comp/*cljs-warn-on-redef* false
                                           comp/*cljs-warn-on-dynamic* false
                                           comp/*cljs-warn-on-fn-var* false]
                                   (comp/analyze env (wrap form))))]
         (when (= (:op ast) :ns)
           (load-dependencies repl-env (into (vals (:requires ast))
                                             (distinct (vals (:uses ast))))))
         (when *cljs-verbose*
           (print js))
         (let [ret (-evaluate repl-env filename (:line (meta form)) wrap-js)]
           (case (:status ret)
             ;;we eat ns errors because we know goog.provide() will throw when reloaded
             ;;TODO - file bug with google, this is bs error
             ;;this is what you get when you try to 'teach new developers'
             ;;via errors (goog/base.js 104)
             :error (display-error ret form)
             :exception (display-error ret form
                          #(prn "Error evaluating:" form :as js))
             :success (:value ret))))
       (catch Throwable ex
         (.printStackTrace ex)
         (println (str ex))))))

(defn load-stream [repl-env filename stream]
  (with-open [r (io/reader stream)]
    (let [env {:ns (@comp/namespaces comp/*cljs-ns*) :context :statement :locals {}}
          pbr (clojure.lang.LineNumberingPushbackReader. r)
          eof (Object.)]
      (loop [r (read pbr false eof false)]
        (let [env (assoc env :ns (@comp/namespaces comp/*cljs-ns*))]
          (when-not (identical? eof r)
            (evaluate-form repl-env env filename r)
            (recur (read pbr false eof false))))))))

(defn load-file
  [repl-env f]
  (binding [comp/*cljs-ns* 'cljs.user]
    (let [res (if (= \/ (first f)) f (io/resource f))]
      (assert res (str "Can't find " f " in classpath"))
      (load-stream repl-env f res))))

(defn- wrap-fn [form]
  (cond (and (seq? form) (= 'ns (first form))) identity
        ('#{*1 *2 *3} form) (fn [x] `(cljs.core.pr-str ~x))
        :else (fn [x] `(cljs.core.pr-str
                       (let [ret# ~x]
                         (do (set! *3 *2)
                             (set! *2 *1)
                             (set! *1 ret#)
                             ret#))))))

(defn- eval-and-print [repl-env env form]
  (let [ret (evaluate-form repl-env
                           (assoc env :ns (@comp/namespaces comp/*cljs-ns*))
                           "<cljs repl>"
                           form
                           (wrap-fn form))]
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

(def default-special-fns
  (let [load-file-fn (fn [repl-env file] (load-file repl-env file))]
    {'in-ns (fn [_ quoted-ns]
              (let [ns-name (second quoted-ns)]
                (when-not (@comp/namespaces ns-name)
                  (swap! comp/namespaces assoc ns-name {:name ns-name}))
                (set! comp/*cljs-ns* ns-name)))
     'load-file load-file-fn
     'clojure.core/load-file load-file-fn
     'load-namespace (fn [repl-env ns] (load-namespace repl-env ns))}))

(defn repl
  "Note - repl will reload core.cljs every time, even if supplied old repl-env"
  [repl-env & {:keys [verbose warn-on-undeclared special-fns]}]
  (prn "Type: " :cljs/quit " to quit")
  (binding [comp/*cljs-ns* 'cljs.user
            *cljs-verbose* verbose
            comp/*cljs-warn-on-undeclared* warn-on-undeclared]
    (let [env {:context :statement :locals {}}
          special-fns (merge default-special-fns special-fns)
          is-special-fn? (set (keys special-fns))]
      (-setup repl-env)
      (loop []
        (print (str "ClojureScript:" comp/*cljs-ns* "> "))
        (flush)
        (let [{:keys [status form]} (read-next-form)]
          (cond
           (= form :cljs/quit) :quit
           
           (= status :error) (recur)
           
           (and (seq? form) (is-special-fn? (first form)))
           (do (apply (get special-fns (first form)) repl-env (rest form)) (newline) (recur))
           
           :else
           (do (eval-and-print repl-env env form) (recur)))))
      (-tear-down repl-env))))

