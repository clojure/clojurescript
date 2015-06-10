(ns clojure.walk-test
  (:require [cljs.test :as test
             :refer-macros [deftest is testing]]
            [clojure.walk :as w]))

(defrecord Rec1 [a])

(defn inc-leaf [x]
  (if (number? x)
    (inc x)
    x))

(deftest test-api
  (testing "Test walking"
    (is (= [2 {1 "1", :two 2}] (w/postwalk inc-leaf [1 {0 "1", :two 1}])))
    (is (= [(Rec1. 2)] (w/postwalk inc-leaf [(Rec1. 1)])))

    (is (= (map->Rec1 {:a 1, ":a" 1})
           (w/prewalk #(if (keyword? %) (str %) %) (Rec1. 1)))
        "Mirror Clojure behavior")))


