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

(deftest test-bit-operations
  (testing "Testing bit operations"
    (is (= [1 0 0 40 43 49 49])
      [(bit-xor 0 1)
       (bit-xor 1 1)
       (bit-xor 1 0)
       (bit-xor 41 1)
       (bit-xor 42 1)
       (bit-xor 42 1 26)
       (apply bit-xor [42 1 26])])
    (is (= [0 0 1 0 1 1 1]
          [(bit-and 1 0)
           (bit-and 0 0)
           (bit-and 1 1)
           (bit-and 42 1)
           (bit-and 41 1)
           (bit-and 41 1 27)
           (apply bit-and [41 1 27])]))
    (is (= [1 0 1 43 41 59 59]
          [(bit-or 1 0)
           (bit-or 0 0)
           (bit-or 1 1)
           (bit-or 42 1)
           (bit-or 41 1)
           (bit-or 41 1 26)
           (apply bit-or [41 1 26])]))
    (is (= [1 0 0 42 32 32]
          [(bit-and-not 1 0)
           (bit-and-not 0 0)
           (bit-and-not 1 1)
           (bit-and-not 42 1)
           (bit-and-not 41 1 27)
           (apply bit-and-not [41 1 27])]))
    (is (= [0 2 968 16649 0]
          [(bit-clear 1 0)
           (bit-clear 2 0)
           (bit-clear 1000 5)
           (bit-clear 16713 6)
           (bit-clear 1024 10)]))
    (is (= [0 0 992 18761 0]
          [(bit-flip 1 0)
           (bit-flip 2 1)
           (bit-flip 1000 3)
           (bit-flip 16713 11)
           (bit-flip 1024 10)]))
    (is (= [-2 -3 999 -16714 -1025]
          [(bit-not 1)
           (bit-not 2)
           (bit-not -1000)
           (bit-not 16713)
           (bit-not 1024)]))
    (is (= [1 2 1000 18761 1024]
          [(bit-set 1 0)
           (bit-set 2 1)
           (bit-set 1000 3)
           (bit-set 16713 11)
           (bit-set 1024 10)]))
    (is (= [true true true false true]
          [(bit-test 1 0)
           (bit-test 2 1)
           (bit-test 1000 3)
           (bit-test 16713 11)
           (bit-test 1024 10)]))))

(deftest test-apply
  (testing "Testing apply"
    (is (= 0 (apply + nil)))
    (is (= 0 (apply + (list))))
    (is (= 1 (apply + (list 1))))
    (is (= 3 (apply + 1 (list 2))))
    (is (= 7 (apply + 1 2 (list 4))))
    (is (= 15 (apply + 1 2 4 (list 8))))
    (is (= 31 (apply + 1 2 4 8 (list 16))))
    (is (= 63 (apply + 1 2 4 8 16 (list 32))))
    (is (= 127 (apply + 1 2 4 8 16 (list 32 64))))
    (is (= 4950 (apply + (take 100 (iterate inc 0)))))
    (is (= () (apply list [])))
    (is (= [1 2 3] (apply list [1 2 3])))
    (is (= 6 (apply apply [+ [1 2 3]])))
    ;; apply with infinite sequence
    (is (= 3 (apply (fn [& args]
                      (+ (nth args 0)
                        (nth args 1)
                        (nth args 2)))
               (iterate inc 0))))
    (is (= [0 1 2 3 4] (take 5 (apply (fn [& m] m) (iterate inc 0)))))
    (is (= [1 2 3 4 5] (take 5 (apply (fn [x & m] m) (iterate inc 0)))))
    (is (= [2 3 4 5 6] (take 5 (apply (fn [x y & m] m) (iterate inc 0)))))
    (is (= [3 4 5 6 7] (take 5 (apply (fn [x y z & m] m) (iterate inc 0)))))
    (is (= [4 5 6 7 8] (take 5 (apply (fn [x y z a & m] m) (iterate inc 0)))))
    (is (= [5 6 7 8 9] (take 5 (apply (fn [x y z a b & m] m) (iterate inc 0)))))
    ;; apply arity tests
    (let [single-arity-non-variadic (fn [x y z] [z y x])
          multiple-arity-non-variadic (fn ([x] x) ([x y] [y x]) ([x y z] [z y x]))
          single-arity-variadic-fixedargs (fn [x y & more] [more y x])
          single-arity-variadic-nofixedargs (fn [& more] more)
          multiple-arity-variadic (fn ([x] x) ([x y] [y x]) ([x y & more] [more y x]))]
      (testing "arities"
        (is (= [3 2 1] (apply single-arity-non-variadic [1 2 3])))
        (is (= [3 2 1] (apply single-arity-non-variadic 1 [2 3])))
        (is (= [3 2 1] (apply single-arity-non-variadic 1 2 [3])))
        (is (= 42 (apply multiple-arity-non-variadic [42])))
        (is (= [2 1] (apply multiple-arity-non-variadic [1 2])))
        (is (= [2 1] (apply multiple-arity-non-variadic 1 [2])))
        (is (= [3 2 1] (apply multiple-arity-non-variadic [1 2 3])))
        (is (= [3 2 1] (apply multiple-arity-non-variadic 1 [2 3])))
        (is (= [3 2 1] (apply multiple-arity-non-variadic 1 2 [3])))
        (is (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs [1 2 3 4 5])))
        (is (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs 1 [2 3 4 5])))
        (is (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs 1 2 [3 4 5])))
        (is (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs 1 2 3 [4 5])))
        (is (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs 1 2 3 4 [5])))
        (is (= [3 4 5] (take 3 (first (apply single-arity-variadic-fixedargs (iterate inc 1))))))
        (is (= [2 1] (rest (apply single-arity-variadic-fixedargs (iterate inc 1)))))
        (is (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs [1 2 3 4 5])))
        (is (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs 1 [2 3 4 5])))
        (is (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs 1 2 [3 4 5])))
        (is (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs 1 2 3 [4 5])))
        (is (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs 1 2 3 4 [5])))
        (is (= [1 2 3 4 5] (take 5 (apply single-arity-variadic-nofixedargs (iterate inc 1)))))
        (is (= 42 (apply multiple-arity-variadic [42])))
        (is (= [2 1] (apply multiple-arity-variadic [1 2])))
        (is (= [2 1] (apply multiple-arity-variadic 1 [2])))
        (is (= [[3 4 5] 2 1] (apply multiple-arity-variadic [1 2 3 4 5])))
        (is (= [[3 4 5] 2 1] (apply multiple-arity-variadic 1 [2 3 4 5])))
        (is (= [[3 4 5] 2 1] (apply multiple-arity-variadic 1 2 [3 4 5])))
        (is (= [[3 4 5] 2 1] (apply multiple-arity-variadic 1 2 3 [4 5])))
        (is (= [[3 4 5] 2 1] (apply multiple-arity-variadic 1 2 3 4 [5])))
        (is (= [3 4 5] (take 3 (first (apply multiple-arity-variadic (iterate inc 1))))))
        (is (= [2 1] (rest (apply multiple-arity-variadic (iterate inc 1)))))))))

(deftest test-booleans
  (testing "Testing boolean predicates"
    (is (= [true false true false false false true true false false]
          [(true? true)
           (true? false)
           (false? false)
           (false? true)
           (true? js/undefined)
           (false? js/undefined)
           (boolean? true)
           (boolean? false)
           (boolean? nil)
           (boolean? js/undefined)]))))

(deftest test-try-catch
  (let [a (atom nil)]
    (testing "Testing try/catch"
      (is (= 1 (try 1)))
      (is (= 2 (try 1 (throw (js/Error.)) (catch js/Error e 2))))
      (is (= 2 (try 1 (throw (js/Error.)) (catch js/Error e 1 2))))
      (is (= 2 (try 1 (throw (js/Error.)) (catch js/Error e 2) (catch :default e 3))))
      (is (= 3 (try 1 (throw true) (catch js/Error e 2) (catch :default e 3))))
      (is (= 2 (try 1 (throw 2) (catch js/Error e 3) (catch :default e e))))
      (is (= 1 (try 1 (finally (reset! a 42)))))
      (is (= 42 (deref a))))))

(deftest test-list-comprehensions
  (let [v [1 2 3]]
    (testing "Testing list comprehensions"
      (is (= v (for [e v] e)))
      (is (= [[1 1] [2 4] [3 9]] (for [e v :let [m (* e e)]] [e m])))
      (is (= [1 2] (for [e v :while (< e 3)] e)))
      (is (= [3] (for [e v :when (> e 2)] e)))
      (is (= [[1 1] [2 4]] (for [e v :while (< e 3) :let [m (* e e)]] [e m]))))))

(deftest test-partial-and-comp
  (let [a10 (partial + 10)
        a20 (partial + 10 10)
        a21 (partial + 10 10 1)
        a22 (partial + 10 5  4 3)
        a23 (partial + 10 5  4 3 1)]
    (testing "Testing partial"
      (is (= 110 (a10 100)))
      (is (= 120 (a20 100)))
      (is (= 121 (a21 100)))
      (is (= 122 (a22 100)))
      (is (= 123 (a23 100)))))
  (let [n2 (comp first rest)
        n3 (comp first rest rest)
        n4 (comp first rest rest rest)
        n5 (comp first rest rest rest rest)
        n6 (comp first rest rest rest rest rest)]
    (testing "Testing comp"
      (is (= 2 (n2 [1 2 3 4 5 6 7])))
      (is (= 3 (n3 [1 2 3 4 5 6 7])))
      (is (= 4 (n4 [1 2 3 4 5 6 7])))
      (is (= 5 (n5 [1 2 3 4 5 6 7])))
      (is (= 6 (n6 [1 2 3 4 5 6 7]))))))

(deftest test-regexps
  (testing "Testing regexps"
    (let [r1 #"foo", r2 (re-pattern r1)]
      (is (= r1 r2)))
    (is (= (str (re-pattern "f(.)o")) (str (js* "/f(.)o/"))))
    (is (= (re-find (re-pattern "foo") "foo bar foo baz foo zot") "foo"))
    (is (= (re-find (re-pattern "f(.)o") "foo bar foo baz foo zot") ["foo" "o"]))
    (is (= (re-matches (re-pattern "foo") "foo") "foo"))
    (is (= (re-matches (re-pattern "foo") "foo bar foo baz foo zot") nil))
    (is (= (re-matches (re-pattern "foo.*") "foo bar foo baz foo zot") "foo bar foo baz foo zot"))
    (is (= (re-seq (re-pattern "foo") "foo bar foo baz foo zot") (list "foo" "foo" "foo")))
    (is (= (re-seq (re-pattern "f(.)o") "foo bar foo baz foo zot") (list ["foo" "o"] ["foo" "o"] ["foo" "o"])))
    (is (= (re-matches (re-pattern "(?i)foo") "Foo") "Foo"))
    ; new RegExp("").source => "(?:)" on webkit-family envs, "" elsewhere
    (is (#{"#\"\"" "#\"(?:)\""} (pr-str #"")))
    (is (= (re-find (re-pattern "[\u2028]") " \u2028 ") "\u2028")))) ; regression test for CLJS-889
