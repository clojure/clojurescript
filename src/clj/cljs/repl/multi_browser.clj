;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl.multi-browser
  "Evaluation environment which allows connections to multiple
  browsers at the same time."
  (:refer-clojure :exclude [loaded-libs])
  (:use [cljs.repl.browser :only [send-404 parse-headers read-headers read-post
                                  read-get read-request ordering add-in-order run-in-order
                                  constrain-order compile-client-js create-client-js-file]])
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

(defprotocol IConnection
  (-closed? [this]))

(extend-protocol IConnection

  java.net.Socket
  (-closed? [this] (.isClosed this))

  clojure.lang.IPersistentMap
  (-closed? [this] (:closed this))

  )

(defonce
  ^{:doc "Source of unique client ids."}
  next-client-id (atom 0))

(defonce server-state
  (atom {:socket nil
         :connections nil
         :promised-conns nil
         :return-values nil
         :client-js nil
         :leader-cleint-id nil}))

(defn- active-connections
  "Return the list of tuples of the form [client-id connection].

  Optionally takes a map of client-ids to connections."
  ([]
     (active-connections (get @server-state :connections {})))
  ([m]
     (remove (fn [[_ c]] (-closed? c)) m)))

(comment
  ;; TODO: move to tests
  (active-connections {1 {:closed true} 2 {:closed false}})
  )

(defn- safe-read-string
  "Accepts a string and attempts to read the string to Clojure data
  with *read-eval* bound to false."
  [s]
  (binding [*read-eval* false] (read-string s)))

(defn- add-return-value
  "Accepts a client-id and return value and stores it in the
  server-state atom.

  The passed val is serialized Clojure data. This val is read before
  being stored."
  [form-id client-id val]
  (swap! server-state assoc-in [:return-values form-id client-id]
         (try (safe-read-string val)
              (catch Exception e
                {:status :error
                 :value (str "Could not read return value: " val)}))))

(defn- get-return-values [form-id]
  (get (:return-values @server-state) form-id))

(defn- active-leader-client-id
  "Accepts a map of client-ids to connections and returns the smallest
  active client-id."
  [connection-map]
  (let [conns (active-connections connection-map)]
    (if (seq conns)
      (apply min (map first conns))
      nil)))

(defn- set-leader-client-id
  "Updates server-state, setting the value of the :leader-cleint-id."
  []
  (swap! server-state
         (fn [old]
           (assoc old :leader-client-id
                  (active-leader-client-id (get old :connections {}))))))

(def ^{:doc "A map of client-ids to sets of loaded libraries for that client."}
  loaded-libs (atom nil))

(defn- connections
  "Promise to return a list of active connections when at least one
  connection is available."
  []
  (let [p (promise)
        conns (vals (active-connections))]
    (do (if (seq conns)
          (deliver p conns)
          (swap! server-state assoc :promised-conns p))
        p)))

(defn- add-connection
  "Given a client-id and a new available connection, either use it to
  deliver the connections which was promised or store the connection
  for later use."
  [client-id conn]
  (if-let [promised-conns (:promised-conns @server-state)]
    (do (swap! server-state assoc :connections nil :promised-conns nil)
        (deliver promised-conns [conn]))
    (swap! server-state assoc-in [:connections client-id] conn)))

(defn- status-line [status]
  (case status
    200 "HTTP/1.1 200 OK"
    404 "HTTP/1.1 404 Not Found"
    "HTTP/1.1 500 Error"))

;; Note: This version of the send-and-close function may work in
;; cljs.repl.browser.clj.
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
       (try (with-open [os (.getOutputStream conn)]
              (doseq [header headers]
                (.write os header 0 (count header)))
              (.write os utf-8-form 0 content-length)
              (.flush os)
              (.close conn)
              true)
            (catch Exception e
              (.close conn)
              false)))))

(defn- pack-return-values
  "Accepts a map of client-ids to return values. Packs them as a
  proper REPL return value."
  [return-values]
  (let [vals (map second return-values)]
    (if (every? #{:exception} (map :status vals))
      (second (first return-values))
      (let [d (distinct vals)]
        (if (= (count d) 1)
          (first d)
          {:status :success :value (pr-str return-values)})))))

(defn- wait-for-return-values
  "Accepts a conn-count (the number of connections to wait for), a
  timeout (the amount of time to wait) and a callback function. When
  all results are received, pass them to the callback function. If the
  timeout is exceeded, only pass the received results."
  [form-id conn-count timeout return-value-fn]
  #_(println "waiting for " conn-count " results for " form-id)
  (loop [timeout timeout
         return-values (get-return-values form-id)]
    (if (or (= conn-count (count return-values))
            (<= timeout 0))
      (return-value-fn (pack-return-values return-values))
      (do (Thread/sleep 100)
          (recur (- timeout 100) (get-return-values form-id))))))

(def form-ids (atom 0))

(defn send-for-eval
  "Given a form and a return value function, send the form to all
  browsers for evaluation. The return value function will be called
  with all return values once all return values have been received.

  This function optionally takes a seq of connections and a
  timeout (which defaults to about 60 seconds.). If the timeout is
  exceeded then return the list of received results."
  ([form return-value-fn]
     (send-for-eval @(connections) form return-value-fn 10000))
  ([conns form return-value-fn]
     (send-for-eval conns form return-value-fn 10000))
  ([conns form return-value-fn timeout]
     (set-leader-client-id)
     (let [form-id (swap! form-ids inc)
           sent (doall (map #(send-and-close %
                                             200
                                             (pr-str {:id form-id :js form})
                                             "text/javascript")
                            conns))]
       (future (wait-for-return-values form-id
                                       (count (filter true? sent))
                                       timeout
                                       return-value-fn)))))

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

(defmulti handle-post (fn [_ m] (:type m)))

(defn client-init-fn [client-id]
  [(list 'ns 'cljs.user)
   (list 'set! '*print-fn* 'clojure.browser.repl/repl-print)
   (list 'clojure.browser.repl/set-client-id client-id)])

(defmethod handle-post :ready [conn _]
  (let [client-id (swap! next-client-id inc)]
    (swap! loaded-libs assoc client-id #{})
    (send ordering (fn [_] {:expecting nil :fns {}}))
    (send-for-eval [conn] (cljsc/-compile (client-init-fn client-id) {}) identity)))

(defmethod handle-post :print [conn {:keys [content order client-id]}]
  (if (= client-id (:leader-client-id @server-state))
    (do (constrain-order order (fn [] (do (print (safe-read-string content))
                                         (.flush *out*))))
        (send-and-close conn 200 "ignore__"))
    (send-and-close conn 200 "ignore__")))

(defmethod handle-post :result [conn {:keys [content order form-id client-id]}]
  (if (= client-id (:leader-client-id @server-state))
    (constrain-order order (fn [] (do (add-return-value form-id client-id content)
                                     (add-connection client-id conn))))
    ;; If this is the leader which we are printing then we can
    ;; constain the order, otherwise we go ahead and update the server-state.
    (do (add-return-value form-id client-id content)
        (add-connection client-id conn))))

(defn handle-connection
  [opts conn]
  (let [rdr (BufferedReader. (InputStreamReader. (.getInputStream conn)))]
    (if-let [request (read-request rdr)]
      (case (:method request)
        :get (handle-get opts conn request)
        :post (handle-post conn (safe-read-string (:content request)))
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
  (let [ss (ServerSocket. (:port opts))]
    (future (server-loop opts ss))
    (swap! server-state assoc :socket ss :port (:port opts))))

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
  ([form]
     (browser-eval nil form))
  ([conn form]
     (let [return-value (promise)
           args [(when conn [conn]) form #(deliver return-value %)]]
       (apply send-for-eval (remove nil? args))
       @return-value)))

(defn- get-client-connection [client-id]
  (get (:connections @server-state) client-id nil))

(defn- object-query-str
  "Given a list of goog namespaces, create a JavaScript string which, when evaluated,
  will return true if all of the namespaces exist and false if any do not exist."
  [ns]
  (str "if("
       (apply str (interpose " && " (map #(str "goog.getObjectByName('" (name %) "')") ns)))
       "){true}else{false};"))

(defn- load-javascript*
  [ns url client-id]
  (let [missing (remove #(contains? (get @loaded-libs client-id) %) ns)]
    (when (seq missing)
      (when-let [conn (get-client-connection client-id)]
        (let [ret (browser-eval conn (object-query-str ns))
              ret (safe-read-string (:value ret))]
          (when-not (and (= (:status ret) :success)
                         (= (:value ret) "true"))
            (when-let [conn (get-client-connection client-id)]
              (browser-eval conn (slurp url))))))
      (swap! loaded-libs
             (fn [old]
               (let [ll (get old client-id)]
                 (assoc old client-id (apply conj ll missing))))))))

(defn load-javascript
  "Accepts a namespace and url (the JavaScript for this namespace) and
  ensures that the namespace is loaded in the browser. Does this
  for each actively connected browser."
  [ns url]
  (let [connections (active-connections)
        futures (doall (map (fn [[client-id _]]
                              (future (load-javascript* ns url client-id)))
                            connections))]
    (doseq [future futures]
      @future)))

(extend-protocol repl/IJavaScriptEnv
  clojure.lang.IPersistentMap
  (-setup [this]
    (comp/with-core-cljs (start-server this)))
  (-evaluate [_ _ _ js] (browser-eval js))
  (-load [this ns url] (load-javascript ns url))
  (-tear-down [_]
    (do (stop-server)
        (reset! server-state {}))))

(defn repl-env [& {:as opts}]
  (let [opts (merge {:port 9000 :optimizations :simple :working-dir ".repl"} opts)]
    (do (swap! server-state
               (fn [old] (assoc old :client-js
                               (future (create-client-js-file
                                        opts
                                        (io/file (:working-dir opts) "client.js"))))))
        opts)))
