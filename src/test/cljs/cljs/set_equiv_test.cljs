(ns cljs.set-equiv-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is]]
            [clojure.string :as s]
            [clojure.set :as set]))

(deftest test-set-equality
  (testing "Testing set equality"
    (is (= (sorted-set 3 2 1)    (sorted-set 1 2 3)))
    (is (= (hash-set   3 2 1)    (sorted-set 1 2 3)))
    (is (= (sorted-set :a :b :c) (hash-set :a :b :c)))
    (is (= (hash-set   :a :b :c) (hash-set :a :b :c))))

  (testing "CLJS-2731 uncomparable values"
    (is (not= (sorted-set 3 2 1) (sorted-set :a :b :c)))
    (is (not= (hash-set 3 2 1)   (sorted-set :a :b :c)))
    (is (not= (sorted-set 3 2 1) (hash-set   :a :b :c)))
    (is (not= (hash-set 3 2 1)   (hash-set   :a :b :c)))))
