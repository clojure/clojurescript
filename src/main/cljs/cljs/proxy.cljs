(ns cljs.proxy
  (:refer-global :only [Proxy isNaN])
  (:require [cljs.proxy.impl :refer [SimpleCache]]))

(defn- write-through [f]
  (let [cache (SimpleCache. #js {} 0)]
    (fn [x]
      (let [v (.get cache x)] 
        (if (some? v)
          v
          (.set cache x (f x)))))))

(def ^{:private true}
  desc
  #js {:configurable true
       :enumerable   true})

(defn builder
  "EXPERIMENTAL: Return a JavaScript Proxy ctor fn with the provided key-fn. You
  can proxy ClojureScript map and vectors. Access pattern from JavaScript
  will lazily wrap collection values in Proxy if needed. Note key-fn
  is only used for proxied ClojureScript maps. This function should map
  strings to the appropriate key representation. All maps proxied from the
  same ctor fn will share the same key-fn cache."
  ([]
   (builder keyword))
  ([key-fn]
   (js* "var __ctor")
   (let [cache-key-fn (write-through key-fn)
         vec-handler  #js {:get (fn [^cljs.core/IIndexed target prop receiver]
                                  (if (identical? prop "length")
                                    (-count ^cljs.core/ICounted target)
                                    (let [n (js* "+~{}" prop)]
                                      (when (and (number? n)
                                                 (not (isNaN n)))
                                        (js/__ctor (-nth target n nil))))))

                           :has (fn [^cljs.core/IAssociative target prop]
                                  (if (identical? prop "length")
                                    true
                                    (let [n (js* "+~{}" prop)]
                                      (and (number? n)
                                           (not (isNaN n))
                                           (<= 0 n)
                                           (< n (-count ^cljs.core/ICounted target))))))

                           :getPrototypeOf
                           (fn [target] nil)

                           :ownKeys
                           (fn [target] #js ["length"])

                           :getOwnPropertyDescriptor
                           (fn [target prop] desc)}
         map-handler #js {:get (fn [^cljs.core/ILookup target prop receiver]
                                 (js/__ctor (-lookup target (cache-key-fn prop))))

                          :has (fn [^cljs.core/IAssociative target prop]
                                 (-contains-key? target (cache-key-fn prop)))

                          :getPrototypeOf
                          (fn [target] nil)

                          :ownKeys
                          (fn [target]
                            (when (nil? (.-cljs$cachedOwnKeys target))
                              (set! (. target -cljs$cachedOwnKeys)
                                (into-array (map -name (keys target)))))
                            (.-cljs$cachedOwnKeys target))

                          :getOwnPropertyDescriptor
                          (fn [target prop] desc)}
         __ctor (fn [target]
                  (cond
                    (implements? IMap target) (Proxy. target map-handler)
                    (implements? IVector target) (Proxy. target vec-handler)
                    :else target))] 
     __ctor)))

(comment

  (def c (SimpleCache. #js {} 0))
  (.set c "foo" :foo)
  (.get c "foo")
  (.-cnt c)
  (.clear c)
  (.get c "foo")

  (def kw (write-through keyword))
  (kw "foo")

  (time
    (dotimes [i 1e6]
      (kw "foo")))

  (time
    (dotimes [i 1e6]
      (keyword "foo")))

  (def proxy (builder))

  (def raw-map {:foo 1 :bar 2})
  (def proxied-map (proxy {:foo 1 :bar 2}))

  (require '[goog.object :as gobj])
  (gobj/get proxied-map "foo")
  (gobj/get proxied-map "bar")
  (gobj/getKeys proxied-map)
  (.keys js/Object proxied-map)
  
  (time
    (dotimes [i 1e7]
      (unchecked-get proxied-map "foo")))

  (def k :foo)
  (time
    (dotimes [i 1e7]
      (get raw-map k)))

  (def proxied-vec (proxy [1 2 3 4]))
  (alength proxied-vec)
  (time
    (dotimes [i 1e6]
      (alength proxied-vec)))

  (nth [1 2 3 4] 1)

  (aget proxied-vec 1)

  (time
    (dotimes [i 1e7]
      (aget proxied-vec 1)))

  (def proxied-deep (proxy [{:foo "Hello"}]))
  (-> proxied-deep (aget 0) (unchecked-get "foo"))
  
)
