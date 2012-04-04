(ns cljs.macro-test.macros
  (:refer-clojure :exclude [==]))

(defmacro == [a b]
  `(+ ~a ~b))