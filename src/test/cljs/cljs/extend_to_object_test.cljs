(ns cljs.extend-to-object-test
  (:require [cljs.test :refer-macros [deftest is]]))

;;; Note: The tests in this namespace manipulate object (at test
;;; run time) and this namespace should be loaded last by test
;;; runners so as to not affect other tests.

;; r1798 core fn protocol regression
(deftest test-extend-to-object
  (extend-type object
    ISeqable
    (-seq [coll]
      (map #(vector % (aget coll %)) (js-keys coll)))

    ILookup
    (-lookup
      ([coll k]
       (-lookup coll k nil))
      ([coll k not-found]
       (if-let [v (aget coll k)]
         v
         not-found))))
  (is (= (seq (js-obj "foo" 1 "bar" 2)) '(["foo" 1] ["bar" 2])))
  (is (= (get (js-obj "foo" 1) "foo") 1))
  (is (= (get (js-obj "foo" 1) "bar" ::not-found) ::not-found))
  (is (= (reduce (fn [s [k v]] (+ s v)) 0 (js-obj "foo" 1 "bar" 2)) 3)))
