;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl.node
  (:refer-clojure :exclude [loaded-libs])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.repl :as repl]
            [cljs.closure :as cljsc])
  (:import cljs.repl.IJavaScriptEnv
           java.net.Socket
           java.lang.StringBuilder
           [java.io BufferedReader BufferedWriter]))

(defn socket [host port]
  (let [socket (Socket. host port)
        in     (io/reader socket)
        out    (io/writer socket)]
    {:socket socket :in in :out out}))

(defn close-socket [s]
  (.close (:in s))
  (.close (:out s))
  (.close (:socket s)))

(defn write [^BufferedWriter out ^String js]
  (.write out js)
  (.write out (int 0)) ;; terminator
  (.flush out))

(defn read-response [^BufferedReader in]
  (let [sb (StringBuilder.)]
    (loop [sb sb c (.read in)]
      (cond
       (= c 1) (let [ret (str sb)]
                 (print ret)
                 (recur (StringBuilder.) (.read in)))
       (= c 0) (str sb)
       :else (do
               (.append sb (char c))
               (recur sb (.read in)))))))

(defn node-eval [{{:keys [in out]} :socket :as repl-env} js]
  (write out js)
  {:status :success :value (read-response in)})

(defn load-javascript [repl-env ns url]
  (node-eval repl-env (slurp url)))

(defn setup [repl-env]
  (let [env   (ana/empty-env)
        scope (:scope repl-env)]
    (repl/load-file repl-env "cljs/core.cljs")
    (swap! (:loaded-libs repl-env) conj "cljs.core")
    (repl/evaluate-form repl-env
      env "<cljs repl>"
      '(ns cljs.user))
    (repl/evaluate-form repl-env
      env "<cljs repl>"
      '(set! *print-fn* (fn [x] (js/node_repl_print (pr-str x)))))))

(defrecord NodeEnv [socket loaded-libs]
  repl/IJavaScriptEnv
  (-setup [this]
    (setup this))
  (-evaluate [this filename line js]
    (node-eval this js))
  (-load [this ns url]
    (load-javascript this ns url))
  (-tear-down [this]
    (close-socket socket)))

(defn repl-env
  [& {:keys [host port] :or {host "localhost" port 5001}}]
  (let [repl-env (NodeEnv. (socket host port) (atom {}))
        base     (io/resource "goog/base.js")
        deps     (io/resource "goog/deps.js")]
    (node-eval repl-env (slurp (io/reader base)))
    (node-eval repl-env (slurp (io/reader deps)))
    repl-env))
