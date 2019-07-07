;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

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
    object
    (-pr-writer [obj writer _]
      (write-all writer (str obj)))
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

(defn test-map-entry [x] (when (map-entry? x) (-key x)))
(defn test-coll [x] (when (coll? x) (-conj x 1)))
(defn test-set [x] (when (set? x) (-disjoin x 1)))
(defn test-associative [x] (when (associative? x) (-assoc x 1 2)))
(defn test-find [x] (when (ifind? x) (-find x 1)))
(defn test-sorted [x] (when (sorted? x) (-sorted-seq x true)))
(defn test-map [x] (when (map? x) (-dissoc x 1)))
(defn test-vector [x] (when (vector? x) (-assoc-n x 1 2)))
(defn test-chunked-seq [x] (when (chunked-seq? x) (-chunked-first x)))
(defn test-ifn [x] (when (ifn? x) (-invoke x)))
(defn test-reversible [x] (when (reversible? x) (-rseq x)))
(defn test-iterable [x] (when (iterable? x) (-iterator x)))
(defn test-cloneable [x] (when (cloneable? x) (-clone x)))
(defn test-counted [x] (when (counted? x) (-count x)))
(defn test-indexed [x] (when (indexed? x) (-nth x 0)))
(defn test-seqable [x] (when (seqable? x) (-seq x)))
(defn test-reduceable [x] (when (reduceable? x) (-reduce x inc)))

(deftest test-extend-to-protocols
  (extend-type string IMapEntry (-key [_] :a))
  (is (nil? (test-map-entry "a")))
  (extend-type string ICollection (-conj [_ _] :b))
  (is (= :b (test-coll "a")))
  (extend-type string ISet (-disjoin [_ _] :c))
  (is (= :c (test-set "a")))
  (extend-type string IAssociative (-assoc [_ _ _] :d))
  (is (= :d (test-associative "a")))
  (extend-type string IFind (-find [_ _] :e))
  (is (= :e (test-find "a")))
  (extend-type string ISorted (-sorted-seq [_ _] :f))
  (is (= :f (test-sorted "a")))
  (extend-type string IMap (-dissoc [_ _] :g))
  (is (= :g (test-map "a")))
  (extend-type string IVector (-assoc-n [_ _ _] :h))
  (is (= :h (test-vector "a")))
  (extend-type string IChunkedSeq (-chunked-first [_] :i))
  (is (nil? (test-chunked-seq "a")))
  (extend-type string IFn (-invoke [_] :j))
  (is (= :j (test-ifn "a")))
  (extend-type string IReversible (-rseq [_] :k))
  (is (= :k (test-reversible "a")))
  (extend-type string IIterable (-iterator [_] :l))
  (is (= :l (test-iterable "a")))
  (extend-type string ICloneable (-clone [_] :m))
  (is (= :m (test-cloneable "a")))
  (extend-type string ICounted (-count [_] :n))
  (is (= :n (test-counted "a")))
  (extend-type string IIndexed (-nth [_] :o))
  (is (= :o (test-indexed "a")))
  (extend-type number ISeqable (-seq [_] :p))
  (is (= :p (test-seqable 1)))
  (extend-type string IReduce (-reduce [_ _] :q))
  (is (= :q (test-reduceable "a"))))
