(ns cljs.top-level
  (:refer-clojure :exclude [test])
  (:require [cljs.test :refer-macros [deftest is]]))

(let [foo 1]
  (defn bar []
    foo))

(let [foo 2]
  (defn baz []
    foo))

(deftest test
  (is (= (bar) 1))
  (is (= (baz) 2)))
