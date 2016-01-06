(ns circular-deps.a
  (:require [circular-deps.b]))

(defn foo [a b]
  (+ a b))
