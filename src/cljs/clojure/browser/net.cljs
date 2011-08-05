;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Network functionality for browser connected REPL"
      :author "Bobby Calderwood and Alex Redington"}
  clojure.browser.net
  (:require [clojure.browser.event :as event]
            [goog.net.XhrIo :as gxhrio]))

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
    [this]
    [this method]
    [this method content]
    [this method content headers]
    [this method content headers timeout])
  (close [this]))

(deftype XhrConnection [url xhrio]

  IConnection
  (transmit [this]
    (transmit this "GET"  nil nil *timeout*))
  (transmit [this method]
    (transmit this method nil nil *timeout*))
  (transmit [this method content]
    (transmit this method content nil *timeout*))
  (transmit [this method content headers]
    (transmit this method content headers *timeout*))
  (transmit [this method content headers timeout]
    (.setTimeoutInterval xhrio timeout)
    (.send xhrio url method content headers))
  (close [this] nil)

  event/EventTarget
  (listen
    [this type fn]
    (event/listen this type fn false))
  (listen
    [this type fn capture?]
    (.addEventListener xhrio
                       (get event-types type type)
                       fn
                       capture?)))

(defn open
  "Returns a connection"
  [url]
  (XhrConnection. url (goog.net.XhrIo.)))
