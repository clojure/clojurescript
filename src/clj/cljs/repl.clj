;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.repl
  (:refer-clojure :exclude [load-file])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [cljs.compiler :as comp]))

(def ^:dynamic *cljs-verbose* false)
(def ^:dynamic *cljs-warn-on-undeclared* false)

(defprotocol IEvaluator
  (-setup [this])
  (-evaluate [this line js])
  (-put [this k f])
  (-tear-down [this]))

(defn evaluate-form
  [repl-env env form]
  (try
    (let [ast (comp/analyze env form)
          js (comp/emits ast)]
      (when *cljs-verbose*
        (print js))
      (let [ret (-evaluate repl-env (:line (meta form)) js)]
        (if (= (:type ret) :error)
          ;;we eat ns errors because we know goog.provide() will throw when reloaded
          ;;TODO - file bug with google, this is bs error
          ;;this is what you get when you try to 'teach new developers'
          ;;via errors (goog/base.js 104)
          (when-not (and (seq? form) (= 'ns (first form)))
            (prn "Error evaluating:" form :as js)
            (println (:stacktrace ret)))
          (:value ret))))
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

(defn repl
  "Note - repl will reload core.cljs every time, even if supplied old repl-env"
  [repl-env & {:keys [verbose warn-on-undeclared]}]
  (prn "Type: " :cljs/quit " to quit")
  (binding [comp/*cljs-ns* 'cljs.user
            *cljs-verbose* verbose
            *cljs-warn-on-undeclared* warn-on-undeclared]
    (let [env {:context :statement :locals {}}]
      (-setup repl-env)
      (loop []
        (print (str "ClojureScript:" comp/*cljs-ns* "> "))
        (flush)
        (let [form (read)]
          (cond
           (= form :cljs/quit) :quit
           
           (and (seq? form) (= (first form) 'in-ns))
           (do (set! comp/*cljs-ns* (second (second form))) (newline) (recur))

           (and (seq? form) ('#{load-file clojure.core/load-file} (first form)))
           (do (load-file repl-env (second form)) (newline) (recur))
           
           :else
           (let [ret (evaluate-form repl-env
                                    (assoc env :ns (@comp/namespaces comp/*cljs-ns*))
                                    (if (and (seq? form) (= 'ns (first form)))
                                      form
                                      (list 'cljs.core.pr-str form)))]
             (try (prn (if (empty? ret) nil (read-string ret)))
                  (catch Exception e (prn nil)))
             (recur)))))
      (-tear-down repl-env))))

