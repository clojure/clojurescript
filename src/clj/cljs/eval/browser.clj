;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.eval.browser
  (:require [clojure.string :as str])
  (:import java.io.BufferedReader
           java.io.BufferedWriter
           java.io.InputStreamReader
           java.io.OutputStreamWriter
           java.net.Socket
           java.net.ServerSocket
           cljs.repl.IEvaluator))

(defonce server-state (atom {:socket nil
                             :connection nil
                             :promised-conn nil
                             :return-value-fn nil}))

(defn connection
  "Promise to return a connection when one is available. If a
  connection is not available, store the promise in server-state."
  []
  (let [p (promise)
        conn (:connection @server-state)]
    (if (and conn (not (.isClosed conn)))
      (do (deliver p conn)
          p)
      (do (swap! server-state (fn [old] (assoc old :promised-conn p)))
          p))))

(defn set-connection
  "Given a new available connection, either use it to deliver the
  connection which was promised or store the connection for later
  use."
  [conn]
  (if-let [promised-conn (:promised-conn @server-state)]
    (do (swap! server-state (fn [old] (-> old
                                         (assoc :connection nil)
                                         (assoc :promised-conn nil))))
        (deliver promised-conn conn))
    (swap! server-state (fn [old] (assoc old :connection conn)))))

(defn set-return-value-fn
  "Save the return value function which will be called when the next
  return value is received."
  [f]
  (swap! server-state (fn [old] (assoc old :return-value-fn f))))

(defn send-and-close
  "Use the passed connection to send a form to the browser. Send a
  proper HTTP response."
  [conn form]
  (let [utf-8-form (.getBytes form "UTF-8")
        content-length (count utf-8-form)
        headers (map #(.getBytes (str % "\r\n"))
                     ["HTTP/1.1 200 OK"
                      "Server: ClojureScript REPL"
                      "Content-Type: text/html; charset=utf_8"
                      (str "Content-Length: " content-length)
                      ""])]
    (with-open [os (.getOutputStream conn)]
      (do (doseq [header headers]
            (.write os header 0 (count header)))
          (.write os utf-8-form 0 content-length)
          (.flush os)
          (.close conn)))))

(defn send-for-eval
  "Given a form and a return value function, send the form to the
  browser for evaluation. The return value function will be called
  when the return value is received."
  [form return-value-fn]
  (do (set-return-value-fn return-value-fn)
      (send-and-close @(connection) form)))

(defn return-value
  "Called by the server when a return value is received."
  [val]
  (when-let [f (:return-value-fn @server-state)]
    (f val)))

(defn parse-headers
  "Parse the headers of an HTTP POST request."
  [header-lines]
  (apply hash-map
   (mapcat
    (fn [line]
      (let [[k v] (str/split line #":" 2)]
        [(keyword (str/lower-case k)) (str/triml v)]))
    header-lines)))

(comment

  (parse-headers
   ["Host: www.mysite.com"
    "User-Agent: Mozilla/4.0"
    "Content-Length: 27"
    "Content-Type: application/x-www-form-urlencoded"])
)

;;; assumes first line already consumed
(defn read-headers [rdr]
  (loop [next-line (.readLine rdr)
         header-lines []]
    (if (= "" next-line)
      header-lines                      ;we're done reading headers
      (recur (.readLine rdr) (conj header-lines next-line)))))

(defn read-post [rdr]
  (let [headers (parse-headers (read-headers rdr))
        content-length (Integer/parseInt (:content-length headers))
        content (char-array content-length)]
    (io! (.read rdr content 0 content-length)
         (String. content))))

(defn read-request [reader]
  (let [line (.readLine reader)]
    (if (.startsWith line "POST")
      (read-post reader)
      line)))

(defn- handle-connection
  [conn]
  (let [rdr (BufferedReader. (InputStreamReader. (.getInputStream conn)))]
    (if-let [message (read-request rdr)]
      (do (when (not= message "ready")
            (return-value message))
          (set-connection conn)))))

(defn- server-loop
  [server-socket]
  (let [conn (.accept server-socket)]
    (do (.setKeepAlive conn true)
        (future (handle-connection conn))
        (recur server-socket))))

(defn start-server
  "Start the server on the specified port."
  [port]
  (do (println "Starting Server on Port:" port)
      (let [ss (ServerSocket. port)]
        (future (server-loop ss))
        (swap! server-state (fn [old] {:socket ss :port port})))))

(defn stop-server
  []
  (.close (:socket @server-state)))

(defn browser-eval
  [form]
  (let [return-value (promise)]
    (send-for-eval form
                   (fn [val] (deliver return-value val)))
    @return-value))

(defrecord BrowserEvaluator [port]
  IEvaluator
  (setup [this]
    (start-server port))
  (evaluate [this form]
    (browser-eval form))
  (tear-down [this]
    (do (stop-server)
        (reset! server-state {}))))

(defn create-eval-env [port]
  (BrowserEvaluator. port))

(comment
  
  ;; Try it out

  (use 'cljs.repl)
  (use 'cljs.eval.browser)
  (def repl-env (create-eval-env 9000))
  (repl repl-env)
  ;; curl -v -d "ready" http://127.0.0.1:9000
  ClojureScript:> (+ 1 1)
  ;; curl -v -d "2" http://127.0.0.1:9000
  )
