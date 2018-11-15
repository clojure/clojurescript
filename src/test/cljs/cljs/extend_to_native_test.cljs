(ns cljs.extend-to-native-test
  (:require [cljs.test :refer-macros [deftest is]]))

;;; Note: The tests in this namespace manipulate native types (at
;;; test run time) and this namespace should be loaded last by test
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

(deftest test-cljs-2812
  (extend-protocol IPrintWithWriter
    object
    (-pr-writer [obj writer _]
      (write-all writer "#object[custom-print-cljs-2812]"))
    boolean
    (-pr-writer [obj writer _]
      (write-all writer "#boolean[" (str obj) "]"))
    number
    (-pr-writer [obj writer _]
      (write-all writer "#number[" (str obj) "]"))
    string
    (-pr-writer [obj writer _]
      (write-all writer "#string[" obj "]"))
    array
    (-pr-writer [obj writer _]
      (write-all writer "#array[" (count obj) "]"))
    function
    (-pr-writer [obj writer _]
      (write-all writer "#function[custom-print-cljs-2812]")))
  (is (= "#object[custom-print-cljs-2812]" (pr-str #js {})))
  (is (= "#boolean[true]" (pr-str true)))
  (is (= "#number[11]" (pr-str 11)))
  (is (= "#string[hello]" (pr-str "hello")))
  (is (= "#array[3]" (pr-str #js [1 2 3])))
  (is (= "#function[custom-print-cljs-2812]" (pr-str map)))
  ;; Restore basic native types so that test summary output looks correct
  (extend-protocol IPrintWithWriter
    boolean
    (-pr-writer [obj writer _]
      (write-all writer (str obj)))
    number
    (-pr-writer [obj writer _]
      (write-all writer (str obj)))
    string
    (-pr-writer [obj writer _]
      (write-all writer obj))))

(deftest test-cljs-2974
  (extend-protocol IEmptyableCollection
    array
    (-empty [_] #js []))
  (let [empty-array (empty #js [1 2 3])]
    (is (and (array? empty-array)
             (empty? empty-array)))))
