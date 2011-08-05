;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.browser-repl
  (:import java.io.BufferedReader
           java.io.BufferedWriter
           java.io.InputStreamReader
           java.io.OutputStreamWriter
           java.net.Socket
           java.net.ServerSocket))

(defonce server-state (atom {:socket nil
                             :port nil
                             :connection nil
                             :next-message nil}))

(defn send-and-close
  "Send this message on the connection and close it."
  [conn message]
  (with-open [writer (BufferedWriter.
                      (OutputStreamWriter. (.getOutputStream conn)))]
    (do (.write writer message)
        (.write writer "\r\n")
        (.flush writer)
        (.close conn))))

(defn return [message]
  (println message))

(defn evaluate
  "If a connection is available then send this form, if not then
  put the message in the next-message slot."
  [message]
  (if-let [conn (:connection @server-state)]
    (send-and-close conn message)
    (swap! server-state (fn [old] (assoc old :next-message message)))))

(defn send-message
  "Check to see if there is a message to send and if there is send
  it."
  []
  (let [conn (:connection @server-state)]
    (when (and conn (not (.isClosed conn)))
      (when-let [message (:next-message @server-state)]
        (do (send-and-close conn message)
            (swap! server-state (fn [old] (assoc old :next-message nil))))))))

;; POST /login.jsp HTTP/1.1
;; Host: www.mysite.com
;; User-Agent: Mozilla/4.0
;; Content-Length: 27
;; Content-Type: application/x-www-form-urlencoded
;;
;; userid=joe&password=guessme

#_(defn read-post [line reader]
  (loop [next-line (.readLine reader)
         content-length 0
         request [line]]
    (if next-line
      (recur (.readLine reader) (conj request next-line))
      request)))

(defn read-request [reader]
  (let [line (.readLine reader)]
    line
    #_(if (.startsWith line "POST")
      (read-post line reader)
      line)))

(defn- handle-connection
  [conn]
  (let [reader (BufferedReader. (InputStreamReader. (.getInputStream conn)))]
    (if-let [message (read-request reader)]
      (do (return message)
          (swap! server-state (fn [old] (assoc old :connection conn)))
          (send-message)))))

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

