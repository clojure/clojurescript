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

(deftest test-arrays
  (testing "Testing array operations"
    (let [a (to-array [1 2 3])]
      (testing "basic ops"
        (is (= [10 20 30] (seq (amap a i ret (* 10 (aget a i))))))
        (is (= 6 (areduce a i ret 0 (+ ret (aget a i)))))
        (is (= (seq a) (seq (to-array [1 2 3]))))
        (is (= 42 (aset a 0 42)))
        (is (not= (seq a) (seq (to-array [1 2 3]))))
        (is (not= a (aclone a)))))
    (let [a (array (array 1 2 3) (array 4 5 6))]
      (testing "aget"
        (is (= (aget a 0 1) 2))
        (is (= (apply aget a [0 1]) 2))
        (is (= (aget a 1 1) 5))
        (is (= (apply aget a [1 1]) 5))
        (aset a 0 0 "foo")
        (is (= (aget a 0 0) "foo"))
        (apply aset a [0 0 "bar"])
        (is (= (aget a 0 0) "bar"))))))

(defn- primitive-arrays-equal
  [a b]
  (= (js->clj a) (js->clj b)))

(deftest test-make-array
  (testing "Testing make-array"
    (is (primitive-arrays-equal #js [] (make-array 0)))
    (is (primitive-arrays-equal #js [] (apply make-array [0])))
    (is (primitive-arrays-equal #js [nil] (make-array 1)))
    (is (primitive-arrays-equal #js [nil] (apply make-array [1])))
    (is (primitive-arrays-equal #js [nil nil] (make-array 2)))
    (is (primitive-arrays-equal #js [nil nil] (apply make-array [2])))
    (is (primitive-arrays-equal #js [] (make-array nil 0)))
    (is (primitive-arrays-equal #js [] (apply make-array [nil 0])))
    (is (primitive-arrays-equal #js [nil] (make-array nil 1)))
    (is (primitive-arrays-equal #js [nil] (apply make-array [nil 1])))
    (is (primitive-arrays-equal #js [nil nil] (make-array nil 2)))
    (is (primitive-arrays-equal #js [nil nil] (apply make-array [nil 2])))
    (is (primitive-arrays-equal #js [] (make-array nil 0 0)))
    (is (primitive-arrays-equal #js [] (apply make-array [nil 0 0])))
    (is (primitive-arrays-equal #js [] (make-array nil 0 1)))
    (is (primitive-arrays-equal #js [] (apply make-array [nil 0 1])))
    (is (primitive-arrays-equal #js [#js []] (make-array nil 1 0)))
    (is (primitive-arrays-equal #js [#js []] (apply make-array [nil 1 0])))
    (is (primitive-arrays-equal #js [#js [] #js []] (make-array nil 2 0)))
    (is (primitive-arrays-equal #js [#js [] #js []] (apply make-array [nil 2 0])))
    (is (primitive-arrays-equal #js [#js [nil]] (make-array nil 1 1)))
    (is (primitive-arrays-equal #js [#js [nil]] (apply make-array [nil 1 1])))
    (is (primitive-arrays-equal #js [#js [nil] #js [nil]] (make-array nil 2 1)))
    (is (primitive-arrays-equal #js [#js [nil] #js [nil]] (apply make-array [nil 2 1])))
    (is (primitive-arrays-equal #js [#js [nil nil] #js [nil nil]] (make-array nil 2 2)))
    (is (primitive-arrays-equal #js [#js [nil nil] #js [nil nil]] (apply make-array [nil 2 2])))
    (is (primitive-arrays-equal #js [] (make-array nil 0 0 0)))
    (is (primitive-arrays-equal #js [] (apply make-array [nil 0 0 0])))
    (is (primitive-arrays-equal #js [] (make-array nil 0 1 1)))
    (is (primitive-arrays-equal #js [] (apply make-array [nil 0 1 1])))
    (is (primitive-arrays-equal #js [#js []] (make-array nil 1 0 0)))
    (is (primitive-arrays-equal #js [#js []] (apply make-array [nil 1 0 0])))
    (is (primitive-arrays-equal #js [#js [] #js []] (make-array nil 2 0 0)))
    (is (primitive-arrays-equal #js [#js [] #js []] (apply make-array [nil 2 0 0])))
    (is (primitive-arrays-equal #js [#js [#js []]] (make-array nil 1 1 0)))
    (is (primitive-arrays-equal #js [#js [#js []]] (apply make-array [nil 1 1 0])))
    (is (primitive-arrays-equal #js [#js [#js [nil]]] (make-array nil 1 1 1)))
    (is (primitive-arrays-equal #js [#js [#js [nil]]] (apply make-array [nil 1 1 1])))
    (is (primitive-arrays-equal #js [#js [#js [nil nil] #js [nil nil]] #js [#js [nil nil] #js [nil nil]]]
          (make-array nil 2 2 2)))
    (is (primitive-arrays-equal #js [#js [#js [nil nil] #js [nil nil]] #js [#js [nil nil] #js [nil nil]]]
          (apply make-array [nil 2 2 2])))))

(deftest test-comparable
  (testing "Testing IComparable"
    (is (=  0 (compare false false)))
    (is (= -1 (compare false true)))
    (is (=  1 (compare true  false)))

    (is (= -1 (compare  0  1)))
    (is (= -1 (compare -1  1)))
    (is (=  0 (compare  1  1)))
    (is (=  1 (compare  1  0)))
    (is (=  1 (compare  1 -1)))

    (is (=  0 (compare "cljs" "cljs")))
    (is (=  0 (compare :cljs :cljs)))
    (is (=  0 (compare 'cljs 'cljs)))
    (is (= -1 (compare "a" "b")))
    (is (= -1 (compare :a :b)))
    (is (= -1 (compare 'a 'b)))
    ;; cases involving ns
    (is (= -1 (compare :b/a :c/a)))
    (is (= -1 (compare :c :a/b)))
    (is (=  1 (compare :a/b :c)))
    (is (= -1 (compare 'b/a 'c/a)))
    (is (= -1 (compare 'c 'a/b)))
    (is (=  1 (compare 'a/b 'c)))

    ;; This is different from clj. clj gives -2 next 3 tests
    (is (= -1 (compare "a" "c")))
    (is (= -1 (compare :a :c)))
    (is (= -1 (compare 'a 'c)))

    (is (= -1 (compare [1 2] [1 1 1])))
    (is (= -1 (compare [1 2] [1 2 1])))
    (is (= -1 (compare [1 1] [1 2])))
    (is (=  0 (compare [1 2] [1 2])))
    (is (=  1 (compare [1 2] [1 1])))
    (is (=  1 (compare [1 1 1] [1 2])))
    (is (=  1 (compare [1 1 2] [1 1 1])))
    (is (=  0 (compare [] [])))
    (is (=  0 (compare (vec #js []) [])))
    (is (=  0 (compare (with-meta [] {}) [])))
    (is (=  0 (compare (pop [1]) [])))

    (is (= -1 (compare (subvec [1 2 3] 1) (subvec [1 2 4] 1))))
    (is (=  0 (compare (subvec [1 2 3] 1) (subvec [1 2 3] 1))))
    (is (=  1 (compare (subvec [1 2 4] 1) (subvec [1 2 3] 1))))
    (is (=  0 (compare (subvec [1] 0 0) (subvec [2] 0 0))))

    (is (=  0 (compare (js/Date. 2015 2 8 19 13 00 999)
                (js/Date. 2015 2 8 19 13 00 999))))
    (is (= -1 (compare (js/Date. 2015 2 8 19 12 00 999)
                (js/Date. 2015 2 8 19 13 00 999))))
    (is (=  1 (compare (js/Date. 2015 2 8 19 14 00 999)
                (js/Date. 2015 2 8 19 13 00 999))))
    ))

(deftest test-dot
  (let [s "abc"]
    (testing "Testing dot operations"
      (is (= 3 (.-length s)))
      (is (= 3 (. s -length)))
      (is (= 3 (. (str 138) -length)))
      (is (= 3 (. "abc" -length)))
      (is (= "bc" (.substring s 1)))
      (is (= "bc" (.substring "abc" 1)))
      (is (= "bc" ((memfn substring start) s 1)))
      (is (= "bc" (. s substring 1)))
      (is (= "bc" (. s (substring 1))))
      (is (= "bc" (. s (substring 1 3))))
      (is (= "bc" (.substring s 1 3)))
      (is (= "ABC" (. s (toUpperCase))))
      (is (= "ABC" (. "abc" (toUpperCase))))
      (is (= "ABC" ((memfn toUpperCase) s)))
      (is (= "BC" (. (. s (toUpperCase)) substring 1)))
      (is (= 2 (.-length (. (. s (toUpperCase)) substring 1))))
      )))

(deftest test-type
  (is (= nil         (type nil)))
  (is (= js/Number   (type 0)))
  (is (= js/Number   (type js/NaN)))
  (is (= js/Number   (type js/Infinity)))
  (is (= js/String   (type "")))
  (is (= js/Boolean  (type true)))
  (is (= js/Boolean  (type false)))
  (is (= js/Function (type identity)))
  (is (= js/Function (type (fn [x] x))))
  (is (= js/Object   (type (js-obj))))
  (is (= js/Array    (type (array))))
  (is (= js/Date     (type (js/Date.))))
  (is (= js/Function (type js/Object))))

(deftest test-instance?
  (is (not (instance? js/Object  nil)))
  (is (not (instance? js/Number  0)))
  (is (not (instance? js/Number  js/NaN)))
  (is (not (instance? js/Number  js/Infinity)))
  (is (not (instance? js/String  "")))
  (is (not (instance? js/Boolean true)))
  (is (not (instance? js/Boolean false)))
  (is (instance? js/Number   (js/Number. 0)))
  (is (instance? js/Object   (js/Number. 0)))
  (is (instance? js/String   (js/String. "")))
  (is (instance? js/Object   (js/String. "")))
  (is (instance? js/Boolean  (js/Boolean.)))
  (is (instance? js/Object   (js/Boolean.)))
  (is (instance? js/Function identity))
  (is (instance? js/Object   identity))
  (is (instance? js/Function (fn [x] x)))
  (is (instance? js/Object   (js-obj)))
  (is (instance? js/Array    (array)))
  (is (instance? js/Object   (array)))
  (is (instance? js/Date     (js/Date.)))
  (is (instance? js/Object   (js/Date.)))
  (is (instance? js/Function js/Object)))

(deftest test-case
  (testing "Test case expr"
    (let [x 1]
      (is (= (case x 1 :one) :one)))
    (let [x 1]
      (is (= (case x 2 :two :default) :default)))
    (let [x 1]
      (is (= (try
               (case x 3 :three)
               (catch js/Error e
                 :fail))
            :fail)))
    (let [x 1]
      (is (= (case x
               (1 2 3) :ok
               :fail)
            :ok)))
    (let [x [:a :b]]
      (is (= (case x
               [:a :b] :ok)
            :ok)))
    (let [a 'a]
      (is (= (case a
               nil nil
               & :amp
               :none)
            :none)))
    (let [a '&]
      (is (= (case a
               nil nil
               & :amp
               :none)
            :amp)))
    (let [foo 'a]
      (testing "multiple match"
        (is (= (case foo
                 (a b c) :sym
                 :none)
              :sym))
        (is (= (case foo
                 (b c d) :sym
                 :none)
              :none))))
    ))

(deftest test-inext
  (testing "Testing INext"
    (is (= nil (next nil)))
    (is (= nil (next (seq (array 1)))))
    (is (= '(2 3) (next (seq (array 1 2 3)))))
    (is (= nil (next (reverse (seq (array 1))))))
    (is (= '(2 1) (next (reverse (seq (array 1 2 3))))))
    (is (= nil (next (cons 1 nil))))
    (is (= '(2 3) (next (cons 1 (cons 2 (cons 3 nil))))))
    (is (= nil (next (lazy-seq (cons 1 nil)))))
    (is (= '(2 3) (next (lazy-seq
                          (cons 1
                            (lazy-seq
                              (cons 2
                                (lazy-seq (cons 3 nil)))))))))
    (is (= nil (next (list 1))))
    (is (= '(2 3) (next (list 1 2 3))))
    (is (= nil (next [1])))
    (is (= '(2 3) (next [1 2 3])))
    (is (= nil (next (range 1 2))))
    (is (= '(2 3) (next (range 1 4))))
    ))

;; this fails in v8 - why?
;; (assert (= "symbol\"'string" (pr-str (str 'symbol \" \' "string"))))
(deftest test-misc
  (testing "Testing miscellaneous operations"
    (is (= 9 (reduce + (next (seq (array 1 2 3 4))))))
    (is (not (= "one" "two")))
    (is (= 3 (count "abc")))
    (is (= 4 (count (array 1 2 3 4))))
    (is (= "c" (nth "abc" 2)))
    (is (= "quux" (nth "abc" 3 "quux")))
    (is (= 1 (nth (array 1 2 3 4) 0)))
    (is (= "val" (nth (array 1 2 3 4) 4 "val")))
    (is (= "b" (get "abc" 1)))
    (is (= "harriet" (get "abcd" 4 "harriet")))
    (is (= 4 (get (array 1 2 3 4) 3)))
    (is (= "zot" (get (array 1 2 3 4) 4 "zot")))
    (is (= 10 (reduce + (array 1 2 3 4))))
    (is (= 20 (reduce + 10 (array 1 2 3 4))))
    (is (= "cabd" (let [jumble (fn [a b] (str (apply str (reverse (str a))) b))]
                    (reduce jumble "abcd"))))
    (is (= "cafrogbd" (let [jumble (fn [a b] (str (apply str (reverse (str a))) b))]
                        (reduce jumble "frog" "abcd"))))
    (is (= [3] (nthnext [1 2 3] 2)))
    (assert (not= 1 2))
    (is (not (not= 1 1)))
    (is (not (not-empty [])))
    (is (boolean (not-empty [1 2 3])))
    (is (= "joel" (min-key count "joel" "tom servo" "crooooooooow")))
    (is (= "crooooooooow" (max-key count "joel" "tom servo" "crooooooooow")))
    (is (= (partition-all 4 [1 2 3 4 5 6 7 8 9])
          [[1 2 3 4] [5 6 7 8] [9]]))
    (is (= (partition-all 4 2 [1 2 3 4 5 6 7 8 9])
          [[1 2 3 4] [3 4 5 6] [5 6 7 8] [7 8 9] [9]]))
    (is (= [true true] (take-while true? [true true 2 3 4])))
    (is (= [[true true] [false false false] [true true]]
          (partition-by true? [true true false false false true true])))
    (is (= [0 2 4 6 8 10] (take-nth 2 [0 1 2 3 4 5 6 7 8 9 10])))
    (let [sf (some-fn number? keyword? symbol?)]
      (testing "Testing some-fn"
        (is (sf :foo 1))
        (is (sf :foo))
        (is (sf 'bar 1))
        (is (not (sf [] ())))))
    (let [ep (every-pred number? zero?)]
      (testing "Testing every-pred"
        (is (ep 0 0 0))
        (is (not (ep 1 2 3 0)))))
    (is ((complement number?) :foo))
    (is (= [1 [2 3] [1 2 3]] ((juxt first rest seq) [1 2 3])))
    (is (= 5 (max 1 2 3 4 5)))
    (is (= 5 (max 5 4 3 2 1)))
    (is (= 5.5 (max 1 2 3 4 5 5.5)))
    (is (= 1 (min 5 4 3 2 1)))
    (is (= 1 (min 1 2 3 4 5)))
    (is (= 0.5 (min 5 4 3 0.5 2 1)))
    (let [x (array 1 2 3)]
      (testing "Testing setting property on JS array"
        (set! (.-foo x) :hello)
        (is (= (.-foo x) :hello))))
    ;; last
    (is (= nil (last nil)))
    (is (= 3 (last [1 2 3])))
    ;; dotimes
    (let [s (atom [])]
      (dotimes [n 5]
        (swap! s conj n))
      (is (= [0 1 2 3 4] @s)))
    ;; doseq
    (let [v [1 2 3 4 5]
          s (atom ())]
      (doseq [n v] (swap! s conj n))
      (is (= @s (reverse v))))
    ;; memoize
    (let [f (memoize (fn [] (rand)))]
      (f)
      (is (= (f) (f))))
    ;; range
    (is (= (range 10) (list 0 1 2 3 4 5 6 7 8 9)))
    (is (= (range 10 20) (list 10 11 12 13 14 15 16 17 18 19)))
    (is (= (range 10 20 2) (list 10 12 14 16 18)))
    (is (= (take 20 (range)) (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)))
    ;; group-by
    (let [d (group-by second {:a 1 :b 2 :c 1 :d 4 :e 1 :f 2})]
      (testing "group-by"
        (is (= 3 (count (get d 1))))
        (is (= 2 (count (get d 2))))
        (is (= 1 (count (get d 4))))))
    (is (= {1 2 3 4 5 6} (merge {1 2} {3 4} {5 6})))
    (is (= {1 2 3 4} (merge {1 2} {3 4} nil)))
    ;; frequencies
    (is (= {:a 3 :b 2} (frequencies [:a :b :a :b :a])))
    ;; reductions
    (is (= [1 3 6 10 15] (reductions + [1 2 3 4 5])))
    ;; keep
    (is (= [1 3 5 7 9] (keep #(if (odd? %) %) [1 2 3 4 5 6 7 8 9 10])))
    (is (= [2 4 6 8 10] (keep #(if (even? %) %) [1 2 3 4 5 6 7 8 9 10])))
    ;; keep-indexed
    (is (= [1 3 5 7 9] (keep-indexed #(if (odd? %1) %2) [0 1 2 3 4 5 6 7 8 9 10])))
    (is (= [2 4 5] (keep-indexed #(if (pos? %2) %1) [-9 0 29 -7 45 3 -8])))
    ;; map-indexed
    (is (= [[0 :a] [1 :b] [2 :c]] (map-indexed #(vector % %2) [:a :b :c])))
    ;; merge-with
    (is (= '{"Foo" ("foo" "FOO" "fOo"), "Bar" ("bar" "BAR" "BAr"), "Baz" ["baz"], "Qux" ["qux" "quux"]}
          (merge-with concat
            {"Foo" ["foo" "FOO"]
             "Bar" ["bar" "BAR"]
             "Baz" ["baz"]}
            {"Foo" ["fOo"]
             "Bar" ["BAr"]
             "Qux" ["qux" "quux"]})))
    (is (= {:a 111, :b 102, :c 13}
          (merge-with +
            {:a 1 :b 2 :c 3}
            {:a 10 :c 10}
            {:a 100 :b 100})))
    (is (= {:a 3, :b 102, :c 13}
          (apply merge-with [+
                             {:a 1 :b 100}
                             {:a 1 :b 2 :c 3}
                             {:a 1 :c 10}])))
    (is (= '[a c e] (replace '[a b c d e] [0 2 4])))
    (is (= [:one :zero :two :zero]
          (replace {0 :zero 1 :one 2 :two} '(1 0 2 0))))
    ;; split-at
    (is (= [[1 2] [3 4 5]] (split-at 2 [1 2 3 4 5])))
    ;; split-with
    (is (= [[1 2 3] [4 5]] (split-with (partial >= 3) [1 2 3 4 5])))
    ;; trampoline
    (is (= 10000 (trampoline (fn f [n] (if (>= n 10000) n #(f (inc n)))) 0)))
    ;; vary-meta
    (is (= {:a 1} (meta (vary-meta [] assoc :a 1))))
    (is (= {:a 1 :b 2} (meta (vary-meta (with-meta [] {:b 2}) assoc :a 1))))
    ;; comparator
    (is (= [1 1 2 2 3 5] (seq (.sort (to-array [2 3 1 5 2 1]) (comparator <)))))
    (is (= [5 3 2 2 1 1] (seq (.sort (to-array [2 3 1 5 2 1]) (comparator >)))))
    (is (= (hash 'foo) (hash (symbol "foo"))))
    (is (= (hash 'foo/bar) (hash (symbol "foo" "bar"))))
    (is (= (lazy-cat [1] [2] [3]) '(1 2 3)))
    ;; Make sure take/drop raise an error when given nil as an argument
    (is (try (do (take nil [1 2 3]) false)
             (catch js/Error e true)))
    (is (try (do (drop nil [1 2 3]) false)
             (catch js/Error e true)))
    (is (try (do (take-nth nil [1 2 3]) false)
             (catch js/Error e true)))))

(deftest test-496
  (is (= (char 65) \A))
  (is (= (char \A) \A)))

(deftest test-717
  (testing "Testing CLJS-717, JS literals"
    (is (array? #js [1 2 3]))
    (is (= (alength #js [1 2 3]) 3))
    (is (= (seq #js [1 2 3]) (seq [1 2 3])))
    (is (= (set (js-keys #js {:foo "bar" :baz "woz"})) #{"foo" "baz"}))
    (is (= (aget #js {:foo "bar"} "foo") "bar"))
    (is (= (aget #js {"foo" "bar"} "foo") "bar"))
    (is (array? (aget #js {"foo" #js [1 2 3]} "foo")))
    (is (= (seq (aget #js {"foo" #js [1 2 3]} "foo")) '(1 2 3)))))

(deftest test-1556
  (testing "Testing CLJS-1556, JS object literal code emission, beginning of statement"
    ;; Really testing that this evaluates properly
    (is (= 1 (do #js {:a 1}
                 1)))
    (is (= 1 (aget #js {:a 1} "a")))
    (is (= 1 (.-a #js {:a 1})))))