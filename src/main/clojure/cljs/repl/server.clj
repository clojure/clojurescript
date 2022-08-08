;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl.server
  (:refer-clojure :exclude [loaded-libs])
  (:require [clojure.string :as str])
  (:import java.io.BufferedReader
           java.io.InputStreamReader
           java.io.ByteArrayOutputStream
           java.util.zip.GZIPOutputStream
           java.net.ServerSocket
           [java.util LinkedList]))

(def ^:dynamic state nil)
(def connq (LinkedList.))
(def promiseq (LinkedList.))
(def lock (Object.))

(defn connection
  "Promise to return a connection when one is available. If no connection is
   available put the promise into a FIFO queue to get the next available
   connection."
  []
  (locking lock
    (let [p (promise)
          conn (.poll connq)]
      (if (and conn (not (.isClosed conn)))
        (deliver p conn)
        (.offer promiseq p))
      p)))

(defn set-connection
  "Given a new available connection, poll the promise queue for and deliver
   the connection. Otherwise put the connection into a FIFO queue."
  [conn]
  (locking lock
    (if-let [p (.poll promiseq)]
      (deliver p conn)
      (.offer connq conn))))

(defonce handlers (atom {}))

(defn dispatch-on
  "Registers a handler to be dispatched based on a request method and a
  predicate.

  pred should be a function that accepts an options map, a connection,
  and a request map and returns a boolean value based on whether or not
  that request should be dispatched to the related handler."
  ([method pred handler]
    (dispatch-on method {:pred pred :handler handler}))
  ([method {:as m}]
    (swap! handlers
      (fn [old]
        (update-in old [method] #(conj (vec %) m))))))

(defn parse-file-parts [file]
  ;; This is a port of java.net.URL.Parts, which is package private.
  (let [ref-idx (str/index-of file "#")
        [file ref] (if ref-idx
                     [(subs file 0 ref-idx) (subs file (inc ref-idx))]
                     [file nil])
        q-idx (str/last-index-of file \?)]
    (merge {:ref ref}
           (if q-idx
             {:path (subs file 0 q-idx)
              :query-str (subs file (inc q-idx))}
             {:path file}))))

;;; assumes first line already consumed
(defn parse-headers
  "Parse the headers of an HTTP POST request."
  [header-lines]
  (apply hash-map
    (mapcat
      (fn [line]
        (let [[k v] (str/split line #":" 2)]
          [(keyword (str/lower-case k)) (str/triml v)]))
      header-lines)))

(defn read-headers [rdr]
  (loop [next-line (.readLine rdr) header-lines []]
    (if (= "" next-line)
      header-lines ;; we're done reading headers
      (recur
        (.readLine rdr)
        (conj header-lines next-line)))))

(defn read-post [line rdr]
  (let [[_ file _] (str/split line #" ")
        {:keys [path ref query-str]} (parse-file-parts file)
        headers (parse-headers (read-headers rdr))
        content-length (Integer/parseInt (:content-length headers))
        content (char-array content-length)]
    (io! (.read rdr content 0 content-length)
      {:method :post
       :path path
       :ref ref
       :query-str query-str
       :headers headers
       :content (String. content)})))

(defn read-get [line rdr]
  (let [[_ file _] (str/split line #" ")
        {:keys [path ref query-str]} (parse-file-parts file)
        headers (parse-headers (read-headers rdr))]
    {:method :get
     :path path
     :ref ref
     :query-str query-str
     :headers headers}))

(defn read-request [rdr]
  (if-let [line (.readLine rdr)]
    (cond
      (.startsWith line "POST") (read-post line rdr)
      (.startsWith line "GET") (read-get line rdr)
      :else {:method :unknown :content line})
    {:method :unknown :content nil}))

(defn- status-line [status]
  (case status
    200 "HTTP/1.1 200 OK"
    404 "HTTP/1.1 404 Not Found"
    "HTTP/1.1 500 Error"))

(defn ^bytes gzip [^bytes bytes]
  (let [baos (ByteArrayOutputStream. (count bytes))]
    (try
      (let [gzos (GZIPOutputStream. baos)]
        (try
          (.write gzos bytes)
          (finally
            (.close gzos))))
      (finally
        (.close baos)))
    (.toByteArray baos)))

(defn send-and-close
  "Use the passed connection to send a form to the browser. Send a
  proper HTTP response."
  ([conn status form]
    (send-and-close conn status form "text/html"))
  ([conn status form content-type]
    (send-and-close conn status form content-type "UTF-8"))
  ([conn status form content-type encoding]
    (send-and-close conn status form content-type encoding false))
  ([conn status form content-type encoding gzip?]
    (let [byte-form (cond-> (.getBytes form encoding) gzip? gzip)
          content-length (count byte-form)
          headers (map #(.getBytes (str % "\r\n"))
                    (cond->
                      [(status-line status)
                       "Server: ClojureScript REPL"
                       (if (not= content-type "application/wasm")
                         (str "Content-Type: "
                              content-type
                              "; charset=" encoding)
                         (str "Content-Type: "
                              content-type))
                       (str "Content-Length: " content-length)]
                      gzip? (conj "Content-Encoding: gzip")
                      true (conj "")))]
      (with-open [os (.getOutputStream conn)]
        (doseq [header headers]
          (.write os header 0 (count header)))
        (.write os byte-form 0 content-length)
        (.flush os)
        (.close conn)))))

(defn send-404 [conn path]
  (send-and-close conn 404
    (str
      "<html><body>"
      "<h2>Page not found</h2>"
      "No page " path " found on this server."
      "</body></html>")
    "text/html"))

(defn- dispatch-request [request conn opts]
  (try
    (if-let [handlers ((:method request) @handlers)]
      (if-let [handler
               (some (fn [{:keys [pred handler]}]
                       (when (pred request conn opts)
                         handler))
                     handlers)]
        (if (= :post (:method request))
          (handler (read-string (:content request)) conn opts )
          (handler request conn opts))
        (send-404 conn (:path request))))
    (catch Throwable t
      (try
        (send-and-close conn 500 (str "<html><body>"
                                      "<h2>Exception while handling</h2>"
                                      "</body></html>"))
        (catch Throwable _))
      (throw t))))

(defn- handle-connection
  [opts conn]
  (let [rdr (BufferedReader. (InputStreamReader. (.getInputStream conn)))]
    (if-let [request (read-request rdr)]
      (dispatch-request request conn opts)
      (.close conn))))

(defn- server-loop
  [opts server-socket]
  (when-let [conn (try (.accept server-socket) (catch Throwable _))]
    (.setKeepAlive conn true)
    (.start
      (Thread.
        ((ns-resolve 'clojure.core 'binding-conveyor-fn)
          (fn [] (handle-connection opts conn)))))
    (recur opts server-socket)))

(defn start
  "Start the server on the specified port."
  [opts]
  (let [ss (ServerSocket. (:port opts))]
    (.start
      (Thread.
        ((ns-resolve 'clojure.core 'binding-conveyor-fn)
          (fn [] (server-loop opts ss)))))
    (swap! state (fn [old] (assoc old :socket ss :port (:port opts))))))

(defn stop []
  (when-let [sock (:socket @state)]
    (when-not (.isClosed sock)
      (.close sock))))
