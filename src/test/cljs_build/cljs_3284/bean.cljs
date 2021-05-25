(ns cljs-3284.bean)

(deftype ^:private SomeType [a])

(defn some-type
  [a]
  (SomeType. a))
