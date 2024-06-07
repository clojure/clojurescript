(ns single-seg-macros)

(defmacro test-macro [a b]
  `(+ ~a ~b))
