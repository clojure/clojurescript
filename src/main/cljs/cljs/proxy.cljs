(ns cljs.proxy
  (:refer-global :only [isNaN Proxy Symbol])
  (:require [cljs.proxy.impl :refer [SimpleCache MapIterator]]))

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
  "EXPERIMENTAL: Returns a JavaScript Proxy ctor fn with the provided
  key-fn. Invoking the returned fn on ClojureScript maps and vectors
  will returned proxied values that can be used transparently as
  JavaScript objects and arrays:

    (def proxy (builder))

    (def proxied-map (proxy {:foo 1 :bar 2}))
    (goog.object/get proxied-map \"foo\") ;; => 1

    (def proxied-vec (proxy [1 2 3 4]))
    (aget proxied-vec 1) ;; => 2

  Access patterns from JavaScript on these proxied values will lazily,
  recursively return further proxied values:

    (def nested-proxies (proxy [{:foo 1 :bar 2}]))
    (-> nested-proxies (aget 0) (goog.object/get \"foo\")) ;; => 1

  Note key-fn is only used for proxied ClojureScript maps. This
  function should map strings to the appropriate key
  representation. If unspecified, key-fn defaults to keyword. All maps
  proxied from the same ctor fn will share the same key-fn cache.

  A cache-fn may be suppled to override the default cache. This fn
  should take key-fn and return a memoized version."
  ([]
   (builder keyword))
  ([key-fn]
   (builder keyword write-through))
  ([key-fn cache-fn]
   (js* "var __ctor")
   (let [cache-key-fn (cache-fn key-fn)
         vec-handler  #js {:get (fn [^cljs.core/IIndexed target prop receiver]
                                  (cond
                                    (identical? "length" prop)
                                    (-count ^cljs.core/ICounted target)

                                    (identical? (. Symbol -iterator) prop)
                                    (fn []
                                      (MapIterator.
                                        ((.bind (unchecked-get target prop) target)) js/__ctor))

                                    :else
                                    (let [n (js* "+~{}" prop)]
                                      (when (and (number? n)
                                                 (not (isNaN n)))
                                        (js/__ctor (-nth target n nil))))))

                           :has (fn [^cljs.core/IAssociative target prop]
                                  (cond
                                    (identical? prop "length") true

                                    (identical? (. Symbol -iterator) prop) true

                                    :else
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

(def ^{:doc "Default proxy for maps and vectors."}
  proxy (builder))

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

  (aget ((cljs.proxy/builder) [{}]) 0)
  
)
