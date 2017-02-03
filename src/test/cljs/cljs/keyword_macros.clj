(ns cljs.keyword-macros)

(defmacro add
  [a b]
  `(+ ~a ~b))
