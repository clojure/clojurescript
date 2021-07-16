(ns cljs-3276.macros)

(defmacro macro-that-requires []
  '(ns test.foo
     (:require cljs-3276.foo)))
