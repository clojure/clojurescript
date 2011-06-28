(ns hello.core
  (:require [hello.foo.bar :as bar]))

(defn greet [n]
  (str "Hello " n))

(defn sum [xs]
  (bar/sum xs))
