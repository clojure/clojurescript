(ns clojure.data-test
  (:require [clojure.data :refer [diff]]))

(defn test-data []
  (assert (= [nil nil nil] (diff nil nil)))
  (assert (= [1 2 nil] (diff 1 2)))
  (assert (= [nil nil [1 2 3]] (diff [1 2 3] '(1 2 3))))
  (assert (= [1 [:a :b] nil] (diff 1 [:a :b])))
  (assert (= [{:a 1} :b nil] (diff {:a 1} :b)))
  (assert (= [:team #{:p1 :p2} nil] (diff :team #{:p1 :p2})))
  (assert (= [{0 :a} [:a] nil] (diff {0 :a} [:a])))
  (assert (= [nil [nil 2] [1]] (diff [1] [1 2])))
  (assert (= [nil nil [1 2]] (diff [1 2] (into-array [1 2]))))
  (assert (= [#{:a} #{:b} #{:c :d}] (diff #{:a :c :d} #{:b :c :d})))
  (assert (= [nil nil {:a 1}] (diff {:a 1} {:a 1})))
  (assert (= [{:a #{2}} {:a #{4}} {:a #{3}}] (diff {:a #{2 3}} {:a #{3 4}})))
  (assert (= [nil nil [1 2]] (diff [1 2] (into-array [1 2]))))
  (assert (= [nil nil [1 2]] (diff (into-array [1 2]) [1 2])))
  (assert (= [{:a {:c [1]}} {:a {:c [0]}} {:a {:c [nil 2] :b 1}}]
             (diff {:a {:b 1 :c [1 2]}} {:a {:b 1 :c [0 2]}})))
  (assert (= [{:a nil} {:a false} {:b nil :c false}]
             (diff {:a nil :b nil :c false} {:a false :b nil :c false}))))
