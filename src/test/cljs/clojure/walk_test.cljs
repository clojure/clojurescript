;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

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

(deftest test-preserves-meta
  (testing "Test preserves meta"
    (is (= (-> (w/prewalk identity [1 (with-meta [1 2] {:foo 3})])
             (nth 1) meta)
           {:foo 3}))
    (is (= (-> (w/postwalk identity [1 (with-meta [1 2] {:foo 3})])
             (nth 1) meta)
          {:foo 3}))))
