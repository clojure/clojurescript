;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.primitives-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is]]
            [clojure.string :as s]
            [clojure.set :as set]))

(deftest test-js-primitives
  ;; js primitives
  (let [keys #(vec (js-keys %))]
    (testing "Testing js primitives"
      (is (= [] (keys (js-obj)) (keys (apply js-obj []))))
      (is (= ["x"] (keys (js-obj "x" "y")) (keys (apply js-obj ["x" "y"])))))))

(deftest test-equiv
  (testing "Testing -equiv"
    (is (= 1))
    (is (= 1 1))
    (is (= 1 1 1))
    (is (= 1 1 1 1))
    (is (not (= 1 2)))
    (is (not (= 1 2 1)))
    (is (not (= 1 1 2)))
    (is (not (= 1 1 2 1)))
    (is (not (= 1 1 1 2)))))

(deftest test-arithmetic
  (testing "Testing addition"
    (is (= (+) 0))
    (is (= (apply + []) 0))
    (is (= (+ 1) 1))
    (is (= (apply + [1]) 1))
    (is (= (+ 1 1) 2))
    (is (= (apply + [1 1]) 2))
    (is (= (+ 1 2 3) 6))
    (is (= (apply + [1 2 3]) 6)))

  (testing "Testing subtraction"
    (is (= (- 1) -1))
    (is (= (apply - [1]) -1))
    (is (= (- 1 1) 0))
    (is (= (apply - [1 1]) 0))
    (is (= (- 3 2 1) 0))
    (is (= (apply - [3 2 1]) 0)))

  (testing "Testing multiplication"
    (is (= (*) 1))
    (is (= (apply * []) 1))
    (is (= (* 2) 2))
    (is (= (apply * [2]) 2))
    (is (= (* 2 3) 6))
    (is (= (apply * [2 3]) 6)))

  (testing "Testing division"
    (is (= (/ 2) 0.5))
    (is (= (apply / [2]) 0.5))
    (is (= (/ 6 2) 3))
    (is (= (apply / [6 2]) 3))
    (is (= (/ 6 3 2) 1))
    (is (= (apply / [6 3 2]) 1)))

  (testing "Testing less than"
    (is (= (< 1) true))
    (is (= (apply < [1]) true))
    (is (= (< 1 2) true))
    (is (= (apply < [1 2]) true))
    (is (= (< 1 1) false))
    (is (= (apply < [1 1]) false))
    (is (= (< 2 1) false))
    (is (= (apply < [2 1]) false))
    (is (= (< 1 2 3) true))
    (is (= (apply < [1 2 3]) true))
    (is (= (< 1 1 3) false))
    (is (= (apply < [1 1 3]) false))
    (is (= (< 3 1 1) false))
    (is (= (apply < [3 1 1]) false)))

  (testing "Testing less than or equal to"
    (is (= (<= 1) true))
    (is (= (apply <= [1]) true))
    (is (= (<= 1 1) true))
    (is (= (apply <= [1 1]) true))
    (is (= (<= 1 2) true))
    (is (= (apply <= [1 2]) true))
    (is (= (<= 2 1) false))
    (is (= (apply <= [2 1]) false))
    (is (= (<= 1 2 3) true))
    (is (= (apply <= [1 2 3]) true))
    (is (= (<= 1 1 3) true))
    (is (= (apply <= [1 1 3]) true))
    (is (= (<= 3 1 1) false))
    (is (= (apply <= [3 1 1]) false)))

  (testing "Testing greater than"
    (is (= (> 1) true))
    (is (= (apply > [1]) true))
    (is (= (> 2 1) true))
    (is (= (apply > [2 1]) true))
    (is (= (> 1 1) false))
    (is (= (apply > [1 1]) false))
    (is (= (> 1 2) false))
    (is (= (apply > [1 2]) false))
    (is (= (> 3 2 1) true))
    (is (= (apply > [3 2 1]) true))
    (is (= (> 3 1 1) false))
    (is (= (apply > [3 1 1]) false))
    (is (= (> 1 1 3) false))
    (is (= (apply > [1 1 3]) false)))

  (testing "Testing greater than or equal to"
    (is (= (>= 1) true))
    (is (= (apply >= [1]) true))
    (is (= (>= 2 1) true))
    (is (= (apply >= [2 1]) true))
    (is (= (>= 1 1) true))
    (is (= (apply >= [1 1]) true))
    (is (= (>= 1 2) false))
    (is (= (apply >= [1 2]) false))
    (is (= (>= 3 2 1) true))
    (is (= (apply >= [3 2 1]) true))
    (is (= (>= 3 1 1) true))
    (is (= (apply >= [3 1 1]) true))
    (is (= (>= 3 1 2) false))
    (is (= (apply >= [3 1 2]) false))
    (is (= (>= 1 1 3) false))
    (is (= (apply >= [1 1 3]) false)))

  (testing "Testing dec/inc"
    (is (= (dec 1) 0))
    (is (= (apply dec [1]) 0))
    (is (= (inc 0) 1))
    (is (= (apply inc [0]) 1)))

  (testing "Testing zero? pos? neg? even? odd?"
    (is (= (zero? 0) true))
    (is (= (apply zero? [0]) true))
    (is (= (zero? 1) false))
    (is (= (apply zero? [1]) false))
    (is (= (zero? -11) false))
    (is (= (apply zero? [-11]) false))
    (is (= (pos? 0) false))
    (is (= (apply pos? [0]) false))
    (is (= (pos? 1) true))
    (is (= (apply pos? [1]) true))
    (is (= (pos? -1) false))
    (is (= (apply pos? [-1]) false))
    (is (= (neg? -1) true))
    (is (= (apply neg? [-1]) true))
    (is (neg? -1))
    (is (not (neg? 1)))
    (is (neg? -1.765))
    (is (not (neg? 0)))
    (is (= [true false true false true false true false]
          (map integer?
            [1 1.00001 0x7e7 [] (- 88 1001991881) :foo 0 "0"])))
    (is (= [true false true false true false]
          (map odd? [1 2 3 4 -1 0])))
    (is (= [true false true false true true]
          (map even? [2 3 4 5 -2 0]))))

  (testing "Testing max / min"
    (is (= (max 1) 1))
    (is (= (apply max [1]) 1))
    (is (= (max 1 2) 2))
    (is (= (apply max [1 2]) 2))
    (is (= (max 2 1) 2))
    (is (= (apply max [2 1]) 2))
    (is (= (max 1 2 3) 3))
    (is (= (apply max [1 2 3]) 3))
    (is (= (max 1 3 2) 3))
    (is (= (apply max [1 3 2]) 3))

    (is (= (min 1) 1))
    (is (= (apply min [1]) 1))
    (is (= (min 1 2) 1))
    (is (= (apply min [1 2]) 1))
    (is (= (min 2 1) 1))
    (is (= (apply min [2 1]) 1))
    (is (= (min 1 2 3) 1))
    (is (= (apply min [1 2 3]) 1))
    (is (= (min 2 1 3) 1))
    (is (= (apply min [3 1 3]) 1)))

  (testing "Testing mod"
    (is (= (mod 4 2) 0))
    (is (= (apply mod [4 2]) 0))
    (is (= (mod 3 2) 1))
    (is (= (apply mod [3 2]) 1))
    (is (= (mod -2 5) 3)))

  (testing "Testing numeric equality in collections"
    (is (= [4 3 2 1 0]
          (loop [i 0 j ()]
            (if (< i 5)
              (recur (inc i) (conj j (fn [] i)))
              (map #(%) j)))))
    (is (= [[1 1] [1 2] [1 3] [2 1] [2 2] [2 3]]
          (map #(%) (for [i [1 2] j [1 2 3]] (fn [] [i j]))))))

  (testing "Testing integer? predicate"
    (is (integer? 0))
    (is (integer? 42))
    (is (integer? -42))
    (is (not (integer? "")))
    (is (not (integer? 1e308)))
    (is (not (integer? js/Infinity)))
    (is (not (integer? (- js/Infinity))))
    (is (not (integer? js/NaN))))

  (testing "Testing integer coercions"
    (is (= 42 (int 42.5)))
    (is (integer? (int 42.5)))
    (is (= 42 (long 42.5)))
    (is (integer? (long 42.5)))
    (is (= -1 (int -1.5)))
    (is (= -9 (long -9.8))))

  (testing "Testing numeric equality from collection"
    (is (= 2 (:b {:a 1 :b 2})))
    (is (= 2 ('b '{:a 1 b 2})))
    (is (= 2 ({:a 1 :b 2} :b)))
    (is (= 2 ({1 1 2 2} 2)))
    (is (= 2 (:a {:b 1} 2)))
    (is (= 2 (:a {} 2)))
    (is (= 2 ({:b 1} :a 2)))
    (is (= 2 ({} :a 2)))
    (is (= nil (:a {})))
    (is (= nil (:a "")))
    (is (= 2 (:a "" 2)))
    (is (= 2 (#{1 2 3} 2)))
    (is (= 1 (apply :a '[{:a 1 a 2}])))
    (is (= 1 (apply 'a '[{a 1 :b 2}])))
    (is (= 1 (apply {:a 1} [:a])))
    (is (= 2 (apply {:a 1} [:b 2]))))

  (testing "Testing quot"
    (is (= (quot 4 2) 2))
    (is (= (quot 3 2) 1))
    (is (= (quot 6 4) 1))
    (is (= (quot 0 5) 0))
    (is (= (quot 42 5) 8))
    (is (= (quot 42 -5) -8))
    (is (= (quot -42 -5) 8))
    (is (= (quot 9 3) 3))
    (is (= (quot 9 -3) -3))
    (is (= (quot -9 3) -3))
    (is (= (quot 2 -5) 0))
    (is (= (quot -2 5) 0))
    (is (= (quot 0 3) 0))
    (is (= (quot 0 -3) 0)))

  (testing "Testing mod"
    (is (= (mod 4 2) 0))
    (is (= (mod 3 2) 1))
    (is (= (mod 6 4) 2))
    (is (= (mod 0 5) 0))
    (is (= (mod 4.5 2.0) 0.5))
    (is (= (mod 42 5) 2))
    (is (= (mod 9 3) 0))
    (is (= (mod 9 -3) 0))
    (is (= (mod -9 3) 0))
    (is (= (mod -9 -3) 0))
    (is (= (mod 0 3) 0))
    (is (= (mod 3216478362187432 432143214) 120355456)))

  (testing "Testing rem"
    (is (= (rem 4 2) 0))
    (is (= (rem 0 5) 0))
    (is (= (rem 4.5 2.0) 0.5))
    (is (= (rem 42 5) 2))
    (is (= (rem 2 5) 2))
    (is (= (rem 2 -5) 2))
    (is (= (rem 0 3) 0)))
  )

;; See
;; https://github.com/clojure/tools.reader#differences-from-lispreaderjava
;; about why these tests won't pass. Not clear if we should change the reader
;; or the test
;; (assert (= "baz" (name 'foo/bar/baz)))
;; (assert (= "foo/bar" (namespace 'foo/bar/baz)))
;; (assert (= "baz" (name :foo/bar/baz)))
;; (assert (= "foo/bar" (namespace :foo/bar/baz)))
;; TODO: These next two tests need Clojure 1.5
;; (assert (= "foo" (namespace 'foo//)))
;; (assert (= "/" (name 'foo//)))

(deftest test-symbols-and-keywords
  (testing "Testing name / namespace"
    (is (nil? (namespace '/)))
    (is (= "/" (name '/)))
    (is (= "keyword" (name :keyword))))

  (testing "Testing str on keywords / symbols"
    (is (= ":hello" (str :hello)))
    (is (= "hello" (str 'hello)))
    (is (= "hello:world" (str "hello" :world)))
    (is (= ":helloworld" (str :hello 'world))))

  (testing "Testing symbol ctor is idempotent"
    (is (= 'a (symbol 'a))))

  (testing "Testing constructed division symbol"
    (is (= '/ (symbol "/")))
    (is (= (namespace '/) (namespace (symbol "/"))))
    (is (= (hash '/) (hash (symbol "/")))))

  (testing "Testing keyword ctor"
    (is (= :a (keyword "a")))
    (is (= :a (keyword 'a)))
    (is (= :a/b (keyword 'a 'b)))
    (is (= :a (keyword :a))))

  (testing "Testing name munging CLJS-1432"
    (is (not= :$ :.))
    (is (not= '$ '.))))
