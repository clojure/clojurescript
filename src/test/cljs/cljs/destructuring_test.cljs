;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.destructuring-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is]]
            [clojure.string :as s]
            [clojure.set :as set]))

(deftest test-destructuring
  (testing "Testing destructuring"
    (is (= [2 1] (let [[a b] [1 2]] [b a])))
    (is (= #{1 2} (let [[a b] [1 2]] #{a b})))
    (is (= [1 2] (let [{a :a b :b} {:a 1 :b 2}] [a b])))
    (is (= [1 2] (let [{:keys [a b]} {:a 1 :b 2}] [a b])))
    (is (= [1 2 [1 2]] (let [[a b :as v] [1 2]] [a b v])))
    (is (= [1 42] (let [{:keys [a b] :or {b 42}} {:a 1}] [a b])))
    (is (= [1 nil] (let [{:keys [a b] :or {c 42}} {:a 1}] [a b])))
    (is (= [2 1] (let [[a b] '(1 2)] [b a])))
    (is (= {1 2} (let [[a b] [1 2]] {a b})))
    (is (= [2 1] (let [[a b] (seq [1 2])] [b a])))
    (testing "namespaced keys"
      (let [{:keys [:a :b]} {:a 1 :b 2}]
        (testing "basic"
          (is (= 1 a))
          (is (= 2 b))))
      (let [{:keys [:a/b :c/d]} {:a/b 1 :c/d 2}]
        (testing "keyword syntax"
          (is (= 1 b))
          (is (= 2 d))))
      (let [{:keys [a/b c/d]} {:a/b 1 :c/d 2}]
        (testing "symbol syntax"
          (is (= 1 b))
          (is (= 2 d))))
      (let [{:syms [a/b c/d]} {'a/b 1 'c/d 2}]
        (testing ":syms"
          (is (= 1 b))
          (is (= 2 d))))
      (let [{:keys [::s/x ::s/y]} {:clojure.string/x 1 :clojure.string/y 2}]
        (testing ":keys"
          (is (= x 1))
          (is (= y 2))))
      )))

(deftest keywords-in-destructuring
  (let [m {:a 1 :b 2}]
    (let [{:keys [:a :b]} m]
      (is (= [1 2] [a b])))
    (let [{:keys [:a :b :c] :or {c 3}} m]
      (is (= [1 2 3] [a b c])))))

(deftest namespaced-keywords-in-destructuring
  (let [m {:a/b 1 :c/d 2}]
    (let [{:keys [:a/b :c/d]} m]
      (is (= [1 2] [b d])))
    (let [{:keys [:a/b :c/d :e/f] :or {f 3}} m]
      (is (= [1 2 3] [b d f])))))

(deftest namespaced-keys-in-destructuring
  (let [m {:a/b 1 :c/d 2}]
    (let [{:keys [a/b c/d]} m]
      (is (= [1 2] [b d])))
    (let [{:keys [a/b c/d e/f] :or {f 3}} m]
      (is (= [1 2 3] [b d f])))))

(deftest namespaced-syms-in-destructuring
  (let [{:syms [a/b c/d e/f] :or {f 3}} {'a/b 1 'c/d 2}]
    (is (= [1 2 3] [b d f]))))

(deftest namespaced-keys-syntax
  (let [{:a/keys [b c d] :or {d 3}} {:a/b 1 :a/c 2}]
    (is (= [1 2 3] [b c d]))))

(deftest namespaced-syms-syntax
  (let [{:a/syms [b c d] :or {d 3}} {'a/b 1 'a/c 2}]
    (is (= [1 2 3] [b c d]))))

(deftest resolve-keyword-ns-alias-in-destructuring
  (let [{:keys [::s/x ::s/y ::s/z] :or {z 3}} {:clojure.string/x 1 :clojure.string/y 2}]
    (is (= [1 2 3] [x y z]))))
