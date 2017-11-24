(ns cljs.spec.test.test-macros
  #?(:cljs (:require [cljs.spec.alpha :as s])))

(defmacro add
  [a b]
  `(+ ~a ~b))

#?(:cljs
   (s/fdef add
     :args (s/cat :a number? :b number?)))
