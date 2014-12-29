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
            [cljs.closure :as cljsc]
            [cljs.closure :as closure]
            [cljs.util :as util])
  (:import cljs.repl.IJavaScriptEnv
           java.net.Socket
           java.lang.StringBuilder
           [java.io File BufferedReader BufferedWriter]
           [java.lang ProcessBuilder UNIXProcess ProcessBuilder$Redirect]))

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

(defn node-eval
  "Evaluate a JavaScript string in the Node REPL process."
  [repl-env js]
  (let [{:keys [in out]} @(:socket repl-env)]
    (write out js)
    {:status :success
     :value (read-response in)}))

(defn load-javascript
  "Load a JavaScript file into the Node REPL process."
  [repl-env ns url]
  (node-eval repl-env (slurp url)))

(defn setup
  ([repl-env] (setup repl-env nil))
  ([repl-env opts]
    (let [output-dir (io/file (:output-dir opts))
          _    (.mkdirs output-dir)
          bldr (ProcessBuilder. (into-array ["node"]))
          _    (-> bldr
                 (.redirectInput
                   (io/file (io/resource "cljs/repl/node_repl.js")))
                 (.redirectOutput ProcessBuilder$Redirect/INHERIT)
                 (.redirectError ProcessBuilder$Redirect/INHERIT))
          proc (.start bldr)
          env  (ana/empty-env)
          core (io/resource "cljs/core.cljs")]
      ;; TODO: temporary hack, should wait till we can read the start string
      ;; from the process - David
      (Thread/sleep 1000)
      (reset! (:socket repl-env)
        (socket (:host repl-env) (:port repl-env)))
      ;; compile cljs.core & its dependencies, goog/base.js must be available
      ;; for bootstrap to load
      (let [core-js (closure/compile-file core
                      (assoc opts
                        :output-file (closure/src-file->target-file core)))
            deps    (closure/add-dependencies opts core-js)]
        (apply closure/output-unoptimized opts deps))
      ;; bootstrap, replace __dirname as __dirname won't be set
      ;; properly due to how we are running it - David
      (node-eval repl-env
        (string/replace
          (slurp (io/resource "cljs/bootstrap_node.js"))
          "__dirname"
          (str "\""
            (.getName (.getCanonicalFile output-dir))
            File/separator "goog" File/separator "bootstrap\"")))
      ;(repl/load-file repl-env core)
      ;(repl/evaluate-form repl-env
      ;  env "<cljs repl>"
      ;  '(set! *print-fn* (fn [x] (js/node_repl_print (pr-str x)))))
      )))

(defrecord NodeEnv [host port socket loaded-libs]
  repl/IJavaScriptEnv
  (-setup [this]
    (setup this))
  (-setup [this opts]
    (setup this opts))
  (-evaluate [this filename line js]
    (node-eval this js))
  (-load [this ns url]
    (load-javascript this ns url))
  (-tear-down [this]
    (close-socket socket)))

(defn repl-env* [{:keys [host port] :or {host "localhost" port 5001}}]
  (NodeEnv. host port (atom nil) (atom {})))

(defn repl-env
  [& options]
  (repl-env* options))

(comment

  (def bldr (ProcessBuilder. (into-array ["node"])))


  )