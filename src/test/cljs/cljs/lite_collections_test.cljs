;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.lite-collections-test
  (:require [cljs.test :refer [deftest testing is]]))

;; NOTE: ** this namespace must be tested with :lite-mode true **

(deftest test-obj-map
  (let [a (. ObjMap -EMPTY)
        b {}]
    (is (identical? a b)))
  (let [a {:foo 1}]
    (is (== 1 (:foo a)))))

(deftest test-simple-map-entry-eq-hash
  (is (= (simple-map-entry 1 2) (simple-map-entry 1 2)))
  (is (= (simple-map-entry 1 2)
        (MapEntry. 1 2 nil)))
  (is (== (hash (simple-map-entry 1 2))
          (hash (MapEntry. 1 2 nil)))))

(deftest test-simple-set-with-set
  (is (= (simple-set []) (set [])))
  (is (= (set []) (simple-set [])))
  (is (= (simple-set [(simple-map-entry 1 2)])
         (set [(MapEntry. 1 2 nil)]))))

(deftest test-hash-map-simple-map-entry
  (let [m (assoc (. HashMap -EMPTY) (simple-map-entry 1 2) true)]
    (is (contains? m (simple-map-entry 1 2)))))

(deftest test-simple-set-simple-map-entry
  (let [a (simple-set [(simple-map-entry 1 2)])]
    (is (contains? a (simple-map-entry 1 2)))))

(deftest test-simple-map-entry-lookups
  (let [me (simple-map-entry :foo "bar")]
    (is (= :foo (get me 0)))
    (is (= "bar" (get me 1)))
    (is (= [0 :foo]
          (vec (find (simple-map-entry :foo "bar") 0))))
    (is (= [:foo "b"]
          (vec (update (simple-map-entry :foo "bar") 1 first))
          (vec (update-in (simple-map-entry :foo "bar") [1] first))))))

(comment

  (require '[cljs.lite-collections-test] :reload)
  (cljs.test/run-tests)

  )
