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
            [goog.net.EventType :as gevent-type]))

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
  (transmit
    [this uri]
    [this uri method]
    [this uri method content]
    [this uri method content headers]
    [this uri method content headers timeout])
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

(defn open
  "Returns a connection"
  []
  (goog.net.XhrIo.))
