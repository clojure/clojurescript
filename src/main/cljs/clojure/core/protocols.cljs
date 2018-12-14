;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.core.protocols)

(defprotocol Datafiable
  :extend-via-metadata true
  (datafy [o] "return a representation of o as data (default identity)"))

(extend-protocol Datafiable
  nil
  (datafy [_] nil)

  default
  (datafy [o] o))

(defprotocol Navigable
  :extend-via-metadata true
  (nav [coll k v] "return (possibly transformed) v in the context of coll and k (a key/index or nil),
defaults to returning v."))

(extend-protocol Navigable
  default
  (nav [_ _ x] x))
