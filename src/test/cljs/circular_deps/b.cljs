(ns circular-deps.b
  (:require [circular-deps.a]))

(defn bar [c d]
  (* c d))
