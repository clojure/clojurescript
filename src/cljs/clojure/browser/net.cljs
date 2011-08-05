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

(defprotocol IConnection
  (transmit
    [connection]
    [connection method]
    [connection method content]
    [connection method]
    [connection method timeout])
  (close [connection]))

(deftype XhrConnection [url]
  goog.net.XhrIo

  IConnection
  (transmit [connection]
    (transmit connection "GET"  "" {} *timeout*))
  (transmit [connection method]
    (transmit connection method "" {} *timeout*))
  (transmit [connection method content]
    (transmit connection method content {} *timeout*))
  (transmit [connection method content headers]
    (transmit connection method content headers *timeout*))
  (transmit [connection method content headers timeout]
    (.setTimeoutInterval connection timeout)
    (.send connection (:url connection) method content headers))

  (close [connection] nil))

(defn open
  "Returns a connection"
  [url]
  (XhrConnection. url))
