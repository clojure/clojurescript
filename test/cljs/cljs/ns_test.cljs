(ns cljs.ns-test
  (:refer-clojure :exclude [+ for])
  (:require-macros [clojure.core :as lang]
                   [cljs.test :refer [deftest is]])
  (:require [cljs.test]
            [cljs.ns-test.foo :refer [baz]]
            [clojure.set :as s])
  (:use [cljs.ns-test.bar :only [quux]]))

(def + -)

(deftest test-ns
  (is (= 4 (clojure.core/+ 2 1 1)))
  (is (= 0 (cljs.ns-test/+ 2 1 1)))
  (is (= 0 (+ 2 1 1)))
  (is (= 123 (baz)))
  (is (= 123 (quux)))

  (is (= (range 5) (lang/for [x (range 5)] x)))
  (is (= #{1 2 3} (s/union #{1} #{2 3}))))
