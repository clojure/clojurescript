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
