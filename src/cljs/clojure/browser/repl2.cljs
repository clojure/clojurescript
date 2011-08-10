;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.browser.repl2
  (:require [clojure.browser.net   :as net]
            [clojure.browser.event :as event]))

(defn log-obj [obj]
  (.log js/console obj))

;; See my notes inline. Delete them whenever you want.

(def result-state (atom nil))

;; Can't send a new post while in the middle of another connection. I
;; changed this so that instead of sending the result back we store the
;; result in an atom and return. The result will be sent when the connection
;; is ready.

(defn process-block
  "Process a single block of JavaScript received from the server"
  [connection block]
  (log-obj (str "evaluating: " block))
  (let [result (try (js* "eval(~{block})")
                    (catch js/Error e (pr-str e)))]
    (log-obj (str "result: " result))
    (reset! result-state result)))

;; We are long polling so this cannot time out.

(defn transmit-post [connection url data]
  (net/transmit connection url "POST" data nil 0))

;; on-ready is called when the connection is ready to send.

(defn on-ready [connection url]
  (log-obj "connection is ready")
  (let [result @result-state]
    (log-obj (str "sending: " result))
    (transmit-post connection url result)))

(defn start-repl
  "Start the REPL loop"
  [url]
  (let [connection (net/open url)]
    (event/listen connection
                  :success
                  (fn [e]
                    (process-block connection
                                   (.getResponseText e/currentTarget ()))))
    (event/listen connection
                  :ready
                  (fn [e]
                    (on-ready connection url)))
    ;; The server is expecting to see the string "ready" for the
    ;; initial connection.
    (transmit-post connection url "ready")))

;; The client will need to monitor the conenction and re-open it if it
;; has been closed. There are all kinds of error states that we need
;; to deal with.
