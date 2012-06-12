(ns cljs.ns-test
  (:refer-clojure :exclude [+])
  (:require [cljs.ns-test.foo :refer [baz]])
  (:use [cljs.ns-test.bar :only [quux]]))

(def + -)

(defn test-ns []
  (assert (= 4 (clojure.core/+ 2 1 1)))
  (assert (= 0 (cljs.ns-test/+ 2 1 1)))
  (assert (= 0 (+ 2 1 1)))
  (assert (= 123 (baz)))
  (assert (= 123 (quux)))
  :ok)
