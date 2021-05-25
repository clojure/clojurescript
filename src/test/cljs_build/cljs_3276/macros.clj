(ns cljs-3276.macros)

(defmacro macro-that-requires []
  `(require 'cljs-3276.foo))
