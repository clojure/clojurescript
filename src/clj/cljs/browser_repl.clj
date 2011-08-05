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
           java.net.ServerSocket)
  (:require [clojure.string :as str]))

(defonce server-state (atom {:socket nil
                             :port nil
                             :connection nil}))

(defonce pending-forms (atom []))

(defn send-and-close
  "Send this message on the connection and close it."
  [conn message]
  (println "the message is: " message)
  (with-open [writer (BufferedWriter.
                      (OutputStreamWriter. (.getOutputStream conn)))]
    (do (.write writer message)
        (.write writer "\r\n")
        (.flush writer)
        (.close conn))))

(defn send-for-eval
  "If a connection is available then send this form."
  [message]
  (when-let [conn (:connection @server-state)]
    (println "sending for eval")
    (send-and-close conn message)))

(defn process-form []
  (when-let [first-form (ffirst @pending-forms)]
    (println "first-form: " first-form)
    (send-for-eval first-form)))

(defn return [return-value]
  (let [[[_ first-promise]] @pending-forms]
    (swap! pending-forms drop 1)
    (deliver first-promise return-value)
    (process-form)))

(defn send-forms [forms]
  (let [pending (reset! pending-forms
                        (map #(vector % (promise)) forms))]
    (println "pending forms: " pending)
    (process-form)
    ;; TODO: (map second pending) should be consumed by a real REPL
    (doseq [return-value (map second pending)]
      (println @return-value))))

;; POST /login.jsp HTTP/1.1
;; Host: www.mysite.com
;; User-Agent: Mozilla/4.0
;; Content-Length: 27
;; Content-Type: application/x-www-form-urlencoded
;;
;; userid=joe&password=guessme

(comment

  (parse-headers
   ["Host: www.mysite.com"
    "User-Agent: Mozilla/4.0"
    "Content-Length: 27"
    "Content-Type: application/x-www-form-urlencoded"])
)

(defn parse-headers [header-lines]
  (apply hash-map
   (mapcat
    (fn [line]
      (let [[k v] (str/split line #":" 2)]
        [(keyword (str/lower-case k)) (str/triml v)]))
    header-lines)))

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
  (let [reader (BufferedReader. (InputStreamReader. (.getInputStream conn)))]
    (if-let [message (read-request reader)]
      (do (when (not= message "ready")
            (return message))
          (swap! server-state (fn [old] (assoc old :connection conn)))))))

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

;;; (in-ns 'cljs.browser-repl)
;;; (start-server 9000)
;;; curl -v -d "ready" http://127.0.0.1:9000
;;; (send-forms ["test"])
;;; curl -v -d "test-result" http://127.0.0.1:9000