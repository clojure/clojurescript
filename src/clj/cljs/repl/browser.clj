;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.repl.browser
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cljs.compiler :as comp]
            [cljs.repl :as repl])
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

(defn status-line [status]
  (case status
    200 "HTTP/1.1 200 OK"
    404 "HTTP/1.1 404 Not Found"
    "HTTP/1.1 500 Error"))

(defn send-and-close
  "Use the passed connection to send a form to the browser. Send a
  proper HTTP response."
  [conn status form]
  (let [utf-8-form (.getBytes form "UTF-8")
        content-length (count utf-8-form)
        headers (map #(.getBytes (str % "\r\n"))
                     [(status-line status)
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

(defn send-404 [conn path]
  (send-and-close conn 404
                  (str "<html><body>"
                       "<h2>Page not found</h2>"
                       "No page " path " found on this server."
                       "</body></html>")))

(defn send-for-eval
  "Given a form and a return value function, send the form to the
  browser for evaluation. The return value function will be called
  when the return value is received."
  [form return-value-fn]
  (do (set-return-value-fn return-value-fn)
      (send-and-close @(connection) 200 form)))

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

(defn read-post [line rdr]
  (let [[_ path _] (str/split line #" ")
        headers (parse-headers (read-headers rdr))
        content-length (Integer/parseInt (:content-length headers))
        content (char-array content-length)]
    (io! (.read rdr content 0 content-length)
         {:method :post
          :path path
          :headers headers
          :content (String. content)})))

(defn read-get [line rdr]
  (let [[_ path _] (str/split line #" ")
        headers (parse-headers (read-headers rdr))]
    {:method :get
     :path path
     :headers headers}))

(defn read-request [rdr]
  (let [line (.readLine rdr)]
    (cond (.startsWith line "POST") (read-post line rdr)
          (.startsWith line "GET") (read-get line rdr)
          :else {:method :unknown :content line})))

(defn handle-get [root conn request]
  (let [file (io/file (str root (:path request)))]
    (if (.exists file)
      (send-and-close conn 200 (slurp file))
      (send-404 conn (:path request)))))

(defn- handle-connection
  [root conn]
  (let [rdr (BufferedReader. (InputStreamReader. (.getInputStream conn)))]
    (if-let [request (read-request rdr)]
      (case (:method request)
        :get (handle-get root conn request)
        :post (do (when-not (= (:content request) "ready")
                    (return-value (:content request)))
                  (set-connection conn))
        (.close conn))
      (.close conn))))

(defn- server-loop
  [root server-socket]
  (let [conn (.accept server-socket)]
    (do (.setKeepAlive conn true)
        (future (handle-connection root conn))
        (recur root server-socket))))

(defn start-server
  "Start the server on the specified port."
  [root port]
  (do (println "Starting Server on Port:" port)
      (let [ss (ServerSocket. port)]
        (future (server-loop root ss))
        (swap! server-state (fn [old] {:socket ss :port port})))))

(defn stop-server
  []
  (.close (:socket @server-state)))

(defn browser-eval
  [form]
  (let [return-value (promise)]
    (send-for-eval form
                   (fn [val] (deliver return-value val)))
    (let [ret @return-value]
      ;; TODO: Check for errors.
      {:type :return
       :value ret})))

(defrecord BrowserEvaluator [root port]
  IEvaluator
  (-setup [this]
    (comp/with-core-cljs (start-server root port)))
  (-evaluate [this line js]
    (browser-eval js))
  (-put [this k v]
    nil)
  (-tear-down [this]
    (do (stop-server)
        (reset! server-state {}))))

(defn repl-env [root port]
  (BrowserEvaluator. root port))

(comment
  
  (require '[cljs.repl :as repl])
  (require '[cljs.repl.browser :as browser])
  (def env (browser/repl-env "samples/repl2" 9000))
  (repl/repl env)
  ;; simulate the browser with curl
  ;; curl -v -d "ready" http://127.0.0.1:9000
  ClojureScript:> (+ 1 1)
  ;; curl -v -d "2" http://127.0.0.1:9000

  ;; TODO: You need to figure out when to send a (ns cljs.user) form
  ;; to the browser.

  (let [env {:context :statement :locals {}}]
    (repl/evaluate-form this
                        (assoc env :ns (@comp/namespaces comp/*cljs-ns*))
                        '(ns cljs.user)))
  

  )
