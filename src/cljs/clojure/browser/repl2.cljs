;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.browser.repl2
  (:require [clojure.browser.net   :as net]
            [clojure.browser.event :as event]
            [goog.json :as gjson]))

;; Notes
;; =====
;;
;; Using keywords for the service names does not work in Chrome or
;; FireFox.
;;
;; --

(defn log-obj [obj]
  (.log js/console obj))

;; Outer/Parent Peer
;; =================
;;
;; The code in this section will be run in the parent page which
;; exists in the application's domain. This is where code will be
;; evaluated.

(def parent-channel (atom nil))

(defn- ensure-string [val]
  (if (string? val)
    val
    (str val)))

(defn evaluate-javascript
  "Given a block of JavaScript, evaluate it and transmit the result to
  the inner peer of the cross domain channel."
  [block]
  (log-obj (str "evaluating: " block))
  (let [result (pr-str
                (try {:status :success :value (ensure-string (js* "eval(~{block})"))} 
                     (catch js/Error e {:status :exception :value (pr-str e)})))]
    (log-obj (str "result: " result))
    (net/transmit @parent-channel "return-value" result)))

(defn create-cross-domain-channel
  "Create a cross domain channel with an iframe which can communicate
  with the REPL server."
  [url]
  (let [chnl (doto (net/xpc-connection {:peer_uri (str url "/repl")})
               (net/register-service "evaluate-javascript" evaluate-javascript)
               (net/connect document.body
                            (fn [] (log-obj "Parent channel connected."))
                            (fn [iframe] (set! iframe.style.display "none"))))]
    (reset! parent-channel chnl)))

(defn connect
  "Connect to a ClojureScript REPL server located at the passed url."
  [url]
  (goog.events/listen js/window "load" #(create-cross-domain-channel url)))

;; Inner peer
;; =========
;;
;; The code in this section will be run in the child iframe and can
;; communicate with REPL server.

(def state (atom {:connection nil :url nil}))

(def child-channel (atom nil))

(defn transmit-post [connection url data]
  (net/transmit connection url "POST" data nil 0))

(defn start-repl-connection
  "Start the REPL loop"
  [url]
  (let [connection (net/xhr-connection)]
    (reset! state {:connection connection :url url})
    (event/listen connection
                  :success
                  (fn [e]
                    (net/transmit @child-channel
                                  "evaluate-javascript"
                                  (.getResponseText e/currentTarget ()))))
    ;; The server is expecting to see the string "ready" for the
    ;; initial connection.
    (transmit-post connection url "ready")))

(defn return-value [val]
  (log-obj (str "sending: " val))
  (transmit-post (:connection @state) (:url @state) val))

;; I can't get this to work using the clojure.browser.net api.

(defn inner-peer-channel
  "This function will be called from a script in the child iframe."
  [repl-url]
  (let [cfg (gjson/parse (.getParameterValue (goog.Uri. window.location.href) "xpc"))
        chnl (doto (goog.net.xpc.CrossPageChannel. cfg)
               (net/register-service "return-value" return-value)
               (.connect #(log-obj "Child channel connected.")))]
    (do (reset! child-channel chnl)
        (js/setTimeout #(start-repl-connection repl-url) 500))))

