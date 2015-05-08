(ns foo.ns-shadow-test
  (:require [cljs.test :refer-macros [deftest is]]
            baz))

(defn bar [] 1)

(defn quux [foo]
  (+ (foo.ns-shadow-test/bar) foo))

(defn id [x] x)

(defn foo [] (id 42))

(defn baz
  ([] (baz 2))
  ([x] (quux 2)))

(deftest test-shadow
  (is (= (quux 2) 3))
  (is (= (foo) 42))
  (is (= (baz) 3)))
