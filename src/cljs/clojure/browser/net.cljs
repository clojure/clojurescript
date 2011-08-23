;;  Copyright (c) Rich Hickey. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;  which can be found in the file epl-v10.html at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.

(ns ^{:doc "Network communication library, wrapping goog.net.
Includes XhrIo, CrossPageChannel, and Websockets."
      :author "Bobby Calderwood and Alex Redington"}
  clojure.browser.net
  (:require [clojure.browser.event :as event]
            [goog.net.XhrIo :as gxhrio]
            [goog.net.EventType :as gevent-type]
            [goog.net.xpc.CfgFields :as gxpc-config-fields]
            [goog.net.xpc.CrossPageChannel :as xpc]
            [goog.json :as gjson]))

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
  (connect
    [this]
    [this opt1]
    [this opt1 opt2]
    [this opt1 opt2 opt3])
  (transmit
    [this opt]
    [this opt opt2]
    [this opt opt2 opt3]
    [this opt opt2 opt3 opt4]
    [this opt opt2 opt3 opt4 opt5])
  (close [this]))

(extend-type goog.net.XhrIo

  IConnection
  (transmit
    ([this uri]
       (transmit this uri "GET"  nil nil *timeout*))
    ([this uri method]
       (transmit this uri method nil nil *timeout*))
    ([this uri method content]
       (transmit this uri method content nil *timeout*))
    ([this uri method content headers]
       (transmit this uri method content headers *timeout*))
    ([this uri method content headers timeout]
       (.setTimeoutInterval this timeout)
       (.send this uri method content headers)))
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
         (js->clj goog.net.xpc.CfgFields))))

(defn xhr-connection
  "Returns a connection"
  []
  (goog.net.XhrIo.))

(defprotocol CrossPageChannel
  (register-service [this type fn] [this type fn encode-json?]))

(extend-type goog.net.xpc.CrossPageChannel

  CrossPageChannel
  (register-service
    ([this type fn]
       (register-service this type fn false))
    ([this type fn encode-json?]
       (.registerService this (name type) fn encode-json?)))

  IConnection
  (connect
    ([this]
       (connect this nil))
    ([this on-connect-fn]
       (.connect this on-connect-fn))
    ([this on-connect-fn config-iframe-fn]
       (connect this on-connect-fn config-iframe-fn (.body js/document)))
    ([this on-connect-fn config-iframe-fn iframe-parent]
       (.createPeerIframe this iframe-parent config-iframe-fn)
       (.connect this on-connect-fn)))

  (transmit [this type payload]
    (.send this (name type) payload)))

(defn xpc-connection
  "When passed with a config hash-map, returns a parent
  CrossPageChannel object. Keys in the config hash map are downcased
  versions of the goog.net.xpc.CfgFields enum keys,
  e.g. goog.net.xpc.CfgFields.PEER_URI becomes :peer_uri in the config
  hash.

  When passed with no args, creates a child CrossPageChannel object,
  and the config is automatically taken from the URL param 'xpc', as
  per the CrossPageChannel API."
  ([]
     (when-let [config (.getParameterValue
                        (goog.Uri. (.href (.location js/window)))
                        "xpc")]
       (goog.net.xpc.CrossPageChannel. (gjson/parse config))))
  ([config]
     (goog.net.xpc.CrossPageChannel.
      (.strobj (reduce (fn [sum [k v]]
                         (when-let [field (get xpc-config-fields k)]
                           (assoc sum field v)))
                       {}
                       config)))))
