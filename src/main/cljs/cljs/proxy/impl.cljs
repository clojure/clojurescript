;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.proxy.impl)

(deftype SimpleCache [^:mutable obj ^:mutable cnt]
  Object
  (set [this k v]
    (when (== cnt 1024)
      (.clear this))
    (unchecked-set obj k v)
    (set! cnt (inc cnt))
    v)
  (get [this k]
    (unchecked-get obj k))
  (clear [this]
    (set! obj #js {})
    (set! cnt 0)))

(deftype MapIterator [^:mutable iter f]
  Object
  (next [_]
    (let [x (.next iter)]
      (if-not ^boolean (. x -done)
        #js {:value (f (. x -value))
             :done  false}
        x))))
