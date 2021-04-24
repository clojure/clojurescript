(ns cljs-3284.core
  (:require
    cljs-3284.bean))

(defn maybe-bean
  [x]
  (if (object? x)
    (cljs-3284.bean/some-type x)
    x))
