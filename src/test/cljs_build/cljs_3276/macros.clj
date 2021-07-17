(ns cljs-3276.macros)

(defmacro macro-that-requires []
  '(ns* (:require 'cljs-3276.foo)))
