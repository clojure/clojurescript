;;  Copyright (c) Rich Hickey. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;  which can be found in the file epl-v10.html at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.

(ns ^{:doc "Network functionality for browser connected REPL"
      :author "Bobby Calderwood and Alex Redington"}
  clojure.browser.net
  (:require [clojure.browser.event :as event]
            [goog.net.XhrIo :as gxhrio]
            [goog.net.EventType :as gevent-type]
            [goog.net.xpc.CfgFields :as gxpc-config-fields]))

(def *timeout* 10000)

(def event-types
  (into {}
        (map
         (fn [[k v]]
           [(keyword (. k (toLowerCase)))
            v])
         (merge
          (js->clj goog.net.EventType)))))

(defprotocol IConnection
  (connect [this] [this opt1])
  (transmit
    [this opt]
    [this opt opt2]
    [this opt opt2 opt3]
    [this opt opt2 opt3 opt4]
    [this opt opt2 opt3 opt4 opt5])
  (close [this]))

(extend-type goog.net.XhrIo

  IConnection
  (transmit [this uri]
    (transmit this uri "GET"  nil nil *timeout*))
  (transmit [this uri method]
    (transmit this uri method nil nil *timeout*))
  (transmit [this uri method content]
    (transmit this uri method content nil *timeout*))
  (transmit [this uri method content headers]
    (transmit this uri method content headers *timeout*))
  (transmit [this uri method content headers timeout]
    (.setTimeoutInterval this timeout)
    (.send this uri method content headers))
  (close [this] nil)

  event/EventType
  (event-types [this]
    (into {}
          (map
           (fn [[k v]]
             [(keyword (. k (toLowerCase)))
              v])
           (merge
            (js->clj goog.net.EventType))))))

(def xpc-config-fields
  (into {}
        (map
         (fn [[k v]]
           [(keyword (. k (toLowerCase)))
            v])
         (js->clj gxpc-config-fields))))

(defn xhr-connection
  "Returns a connection"
  []
  (goog.net.XhrIo.))

(defprotocol CrossPageChannel
  (register-service [this type fn] [this type fn encode-json?]))

(extend-type goog.net.xpc.CrossPageChannel

  CrossPageChannel
  (register-service [this type fn]
    (register-service this type fn false))
  (register-service [this type fn encode-json?]
    (.registerService this (name type) fn encode-json?))

  IConnection
  (connect [this fn]
    (.connect this fn))
  (transmit [this type payload]
    (.send this (name type) payload)))

(defn cross-page-channel
  [url & config]
  (goog.net.xpc.CrossPageChannel.
   (.strobj (reduce (fn [sum [k v]]
                      (when-let [field (xpc-config-fields k)]
                        (assoc sum field v)))
                    {}
                    (assoc (apply hash-map config)
                      :peer_uri
                      url)))))
