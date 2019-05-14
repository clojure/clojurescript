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

(defprotocol IHasFirst
  (-get-first [this]))

(defprotocol IFindsFirst
  (-find-first [this other]))

(deftype First [xs]
  ISeqable
  (-seq [this] (seq xs))
  IIndexed
  (-nth [this i] (nth xs i))
  (-nth [this i not-found] (nth xs i not-found))
  IFn
  (-invoke [[x]] x)
  (-invoke [this x] this)
  Object
  (toString [[x]] (str x))
  IHasFirst
  (-get-first [[x]] x)
  IFindsFirst
  (-find-first [_ [x]] x))

(deftype DestructuringWithLocals [a]
  IFindsFirst
  (-find-first [_ [x y]]
    [x y a]))

(deftest test-protocol-method-destructuring
  (testing "Testing protocol method destructuring"
    (let [fv (First. [1 2 3])
          fs (First. "asdf")]
      (testing "basic operations"
        (is (= (fv) 1))
        (is (= (fs) \a))
        (is (= (str fs) \a))
        (is (= (-get-first fv) 1))
        (is (= (-get-first fs) \a))
        (is (= (-find-first fv [1]) 1))
        (is (identical? (fv 1) fv))))
    (let [t (DestructuringWithLocals. 1)]
      (testing "with locals"
        (is (= [2 3 1] (-find-first t [2 3])))))))

(defn destructure-1216
  ([kvs] kvs)
  ([k v & args] [k v args]))

(defn foo-1216
  ([a] (foo-1216 a 10))
  ([a b & [c]] [a b c]))

(deftest test-cljs-1216
  (testing "varargs regression"
    (is (= (foo-1216 1) [1 10 nil]))
    (is (= (foo-1216 1 2) [1 2 nil]))
    (is (= (foo-1216 1 2 3) [1 2 3]))
    (is (= [1 2 [3 4]]
          (destructure-1216 1 2 3 4)))
    (is (= [1 2 [3 4]]
          (apply destructure-1216 [1 2 3 4])))
    (is (= (destructure-1216 1 2 3 4)[1 2 [3 4]]
          (apply destructure-1216 [1 2 3 4])))))

(defprotocol CLJS-1600-IFoo
  (foo-fn [_ {:keys [a b] :as x}]))

(defrecord CLJS-1600-Foo []
  CLJS-1600-IFoo
  (foo-fn [_ {:keys [a b] :as args}]
    args))

(deftest test-cljs-1600
  (let [foo (reify
              CLJS-1600-IFoo
              (foo-fn [_ {:keys [a b] :as args}]
                args))]
    (is (= (foo-fn (->CLJS-1600-Foo) {:a 1 :b 2})
          {:a 1 :b 2}))
    (is (= (foo-fn foo {:a 1 :b 2})
          {:a 1 :b 2})))
  ;; test that the destructuring works
  (let [foo (reify
              CLJS-1600-IFoo
              (foo-fn [_ {:keys [a b] :as args}]
                {:a a :b b}))]
    (is (= (foo-fn foo {:a 1 :b 2})
          {:a 1 :b 2})))
  (let [foo (reify
              CLJS-1600-IFoo
              (foo-fn [_ {:keys [a b c] :or {c 3}}]
                {:c c}))]
    (is (= (foo-fn foo {:a 1 :b 2})
          {:c 3}))))

(deftest test-cljs-3076
  (let [f (fn [& [a _]]
            a)]
    (is (nil? (f nil)))
    (is (= 1 (f 1)))
    (is (= 1 (f 1 2))))
  (let []))
