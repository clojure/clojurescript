(ns cljs.ns-test.foo
  (:require [cljs.test :refer-macros [deftest is]]))

(defn baz [] 123)

(def kw ::foo)
(def qkw '::foo)

(deftest test-namespaced-keywords
  (is (= (str kw) ":cljs.ns-test.foo/foo"))
  (is (= (str qkw) ":cljs.ns-test.foo/foo")))
