(ns hello.core
  (:require [hello.foo.bar :as bar]))

(defn ^{:export greet} greet [n]
  (str "Hello " n))

(defn ^:export sum [xs]
  (bar/sum xs))
