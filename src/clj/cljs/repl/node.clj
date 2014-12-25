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

(def current-repl-env (atom nil))
(def loaded-libs (atom #{}))

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

(defn node-eval [{:keys [in out]} js]
  (write out js)
  {:status :success :value (read-response in)})

(defn load-javascript [ctx ns url]
  (node-eval ctx (slurp url)))

(defn setup [repl-env]
  (let [env   (ana/empty-env)
        scope (:scope repl-env)]
    (repl/load-file repl-env "cljs/core.cljs")
    (swap! loaded-libs conj "cljs.core")
    (repl/evaluate-form repl-env
      env "<cljs repl>"
      '(ns cljs.user))
    (repl/evaluate-form repl-env
      env "<cljs repl>"
      '(set! *print-fn* (fn [x] (js/node_repl_print (pr-str x)))))))

(extend-protocol repl/IJavaScriptEnv
  clojure.lang.IPersistentMap
  (-setup [this] (setup this))
  (-evaluate [this filename line js] (node-eval this js))
  (-load [this ns url] (load-javascript this ns url))
  (-tear-down [this] (close-socket this)))

;; do we need to implement our own version of goog.require ? - David

(defn repl-env
  [& {:keys [host port] :or {host "localhost" port 5001}}]
  (let [repl-env (socket host port)
        base     (io/resource "goog/base.js")
        deps     (io/resource "goog/deps.js")]
    (node-eval repl-env (slurp (io/reader base)))
    (node-eval repl-env (slurp (io/reader deps)))
    repl-env))
