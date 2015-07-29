(ns bootstrap-test.macros)

(defmacro foo [a b]
  `(* ~a ~b))