;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl.browser
  (:refer-clojure :exclude [loaded-libs])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cljs.compiler :as comp]
            [cljs.closure :as cljsc]
            [cljs.repl :as repl])
  (:import java.io.BufferedReader
           java.io.BufferedWriter
           java.io.InputStreamReader
           java.io.OutputStreamWriter
           java.net.Socket
           java.net.ServerSocket
           cljs.repl.IJavaScriptEnv))

(defonce server-state (atom {:socket nil
                             :connection nil
                             :promised-conn nil
                             :return-value-fn nil
                             :client-js nil}))

(def loaded-libs (atom #{}))

(defn- connection
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

(defn- set-connection
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

(defn- set-return-value-fn
  "Save the return value function which will be called when the next
  return value is received."
  [f]
  (swap! server-state (fn [old] (assoc old :return-value-fn f))))

(defn- status-line [status]
  (case status
    200 "HTTP/1.1 200 OK"
    404 "HTTP/1.1 404 Not Found"
    "HTTP/1.1 500 Error"))

(defn send-and-close
  "Use the passed connection to send a form to the browser. Send a
  proper HTTP response."
  ([conn status form]
     (send-and-close conn status form "text/html"))
  ([conn status form content-type]
     (let [utf-8-form (.getBytes form "UTF-8")
           content-length (count utf-8-form)
           headers (map #(.getBytes (str % "\r\n"))
                        [(status-line status)
                         "Server: ClojureScript REPL"
                         (str "Content-Type: "
                              content-type
                              "; charset=utf-8")
                         (str "Content-Length: " content-length)
                         ""])]
       (with-open [os (.getOutputStream conn)]
         (do (doseq [header headers]
               (.write os header 0 (count header)))
             (.write os utf-8-form 0 content-length)
             (.flush os)
             (.close conn))))))

(defn send-404 [conn path]
  (send-and-close conn 404
                  (str "<html><body>"
                       "<h2>Page not found</h2>"
                       "No page " path " found on this server."
                       "</body></html>")
                  "text/html"))

(defn send-for-eval
  "Given a form and a return value function, send the form to the
  browser for evaluation. The return value function will be called
  when the return value is received."
  ([form return-value-fn]
     (send-for-eval @(connection) form return-value-fn))
  ([conn form return-value-fn]
     (do (set-return-value-fn return-value-fn)
         (send-and-close conn 200 form "text/javascript"))))

(defn- return-value
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

(defn repl-client-js []
  (slurp @(:client-js @server-state)))

(defn send-repl-client-page
  [opts conn request]
  (send-and-close conn 200
    (str "<html><head><meta charset=\"UTF-8\"></head><body>
          <script type=\"text/javascript\">"
         (repl-client-js)
         "</script>"
         "<script type=\"text/javascript\">
          clojure.browser.repl.client.start(\"http://" (-> request :headers :host) "\");
          </script>"
         "</body></html>")
    "text/html"))

(defn handle-get [opts conn request]
  (let [path (:path request)]
    (if (.startsWith path "/repl")
      (send-repl-client-page opts conn request)
      (send-404 conn (:path request)))))

(declare browser-eval)

(defn handle-connection
  [opts conn]
  (let [rdr (BufferedReader. (InputStreamReader. (.getInputStream conn)))]
    (if-let [request (read-request rdr)]
      (case (:method request)
        :get (handle-get opts conn request)
        :post (if (= (:content request) "ready")
                (do (reset! loaded-libs #{})
                    (send-for-eval conn
                                   "goog.provide('cljs.user');"
                                   identity))
                (do (return-value (:content request))
                    (set-connection conn)))
        (.close conn))
      (.close conn))))

(defn server-loop
  [opts server-socket]
  (let [conn (.accept server-socket)]
    (do (.setKeepAlive conn true)
        (future (handle-connection opts conn))
        (recur opts server-socket))))

(defn start-server
  "Start the server on the specified port."
  [opts]
  (do (println "Starting Server on Port:" (:port opts))
      (let [ss (ServerSocket. (:port opts))]
        (future (server-loop opts ss))
        (swap! server-state (fn [old] (assoc old :socket ss :port (:port opts)))))))

(defn stop-server
  []
  (.close (:socket @server-state)))

(defn browser-eval
  "Given a string of JavaScript, evaluate it in the browser and return a map representing the
   result of the evaluation. The map will contain the keys :type and :value. :type can be
   :success, :exception, or :error. :success means that the JavaScript was evaluated without
   exception and :value will contain the return value of the evaluation. :exception means that
   there was an exception in the browser while evaluating the JavaScript and :value will
   contain the error message. :error means that some other error has occured."
  [form]
  (let [return-value (promise)]
    (send-for-eval form
                   (fn [val] (deliver return-value val)))
    (let [ret @return-value]
      (try (read-string ret)
           (catch Exception e
             {:status :error
              :value (str "Could not read return value: " ret)})))))

(defn- object-query-str
  "Given a list of goog namespaces, create a JavaScript string which, when evaluated,
  will return true if all of the namespaces exist and false if any do not exist."
  [ns]
  (str "if("
       (apply str (interpose " && " (map #(str "goog.getObjectByName('" (name %) "')") ns)))
       "){true}else{false};"))

(defn load-javascript [repl-env ns url]
  (let [missing (remove #(contains? @loaded-libs %) ns)]
    (when (seq missing)
      (let [ret (browser-eval (object-query-str ns))]
        (when-not (and (= (:status ret) :success)
                       (= (:value ret) "true"))
          (browser-eval (slurp url))))
      (swap! loaded-libs (partial apply conj) missing))))

(extend-protocol repl/IJavaScriptEnv
  clojure.lang.IPersistentMap
  (-setup [this]
    (comp/with-core-cljs (start-server this)))
  (-evaluate [_ _ js] (browser-eval js))
  (-load [this ns url] (load-javascript this ns url))
  (-put [_ _ _] nil)
  (-tear-down [_]
    (do (stop-server)
        (reset! server-state {}))))

(defn compile-client-js [opts]
  (cljsc/build '[(ns clojure.browser.repl.client
                   (:require [goog.events :as event]
                             [clojure.browser.repl :as repl]))
                 (defn start [url]
                   (event/listen js/window
                                 "load"
                                 (fn []
                                   (repl/start-evaluator url))))]
               {:optimizations (:optimizations opts)
                :output-dir (:working-dir opts)}))

(defn create-client-js-file [opts file-path]
  (let [file (io/file file-path)]
    (when (not (.exists file))
      (spit file (compile-client-js opts)))
    file))

(defn repl-env [& {:as opts}]
  (let [opts (merge {:port 9000 :optimizations :simple :working-dir ".repl"} opts)]
    (do (swap! server-state
               (fn [old] (assoc old :client-js
                               (future (create-client-js-file
                                        opts
                                        (io/file (:working-dir opts) "client.js"))))))
        opts)))

(comment
  
  (require '[cljs.repl :as repl])
  (require '[cljs.repl.browser :as browser])
  (def env (browser/repl-env))
  (repl/repl env)
  ;; simulate the browser with curl
  ;; curl -v -d "ready" http://127.0.0.1:9000
  ClojureScript:> (+ 1 1)
  ;; curl -v -d "2" http://127.0.0.1:9000

  )
