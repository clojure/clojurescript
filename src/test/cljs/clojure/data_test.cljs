;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns clojure.data-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.data :refer [diff]]))

(deftest test-data
  (is (= [nil nil nil] (diff nil nil)))
  (is (= [1 2 nil] (diff 1 2)))
  (is (= [nil nil [1 2 3]] (diff [1 2 3] '(1 2 3))))
  (is (= [1 [:a :b] nil] (diff 1 [:a :b])))
  (is (= [{:a 1} :b nil] (diff {:a 1} :b)))
  (is (= [:team #{:p1 :p2} nil] (diff :team #{:p1 :p2})))
  (is (= [{0 :a} [:a] nil] (diff {0 :a} [:a])))
  (is (= [nil [nil 2] [1]] (diff [1] [1 2])))
  (is (= [nil nil [1 2]] (diff [1 2] (into-array [1 2]))))
  (is (= [#{:a} #{:b} #{:c :d}] (diff #{:a :c :d} #{:b :c :d})))
  (is (= [nil nil {:a 1}] (diff {:a 1} {:a 1})))
  (is (= [{:a #{2}} {:a #{4}} {:a #{3}}] (diff {:a #{2 3}} {:a #{3 4}})))
  (is (= [nil nil [1 2]] (diff [1 2] (into-array [1 2]))))
  (is (= [nil nil [1 2]] (diff (into-array [1 2]) [1 2])))
  (is (= [{:a {:c [1]}} {:a {:c [0]}} {:a {:c [nil 2] :b 1}}]
             (diff {:a {:b 1 :c [1 2]}} {:a {:b 1 :c [0 2]}})))
  (is (= [{:a nil} {:a false} {:b nil :c false}]
             (diff {:a nil :b nil :c false} {:a false :b nil :c false}))))
