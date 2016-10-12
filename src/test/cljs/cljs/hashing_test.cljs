;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.hashing-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is]]
            [clojure.string :as s]
            [clojure.set :as set]))

(deftest test-hash-null
  (is (zero? (hash (aget (js-obj) "foo")))))

;; hashing bug in many JS runtimes CLJ-118
(deftest test-clj-118
  (let [g #{(conj #{:2} :alt)}
        h #{#{:2 :alt}}]
    (is (= g h)))
  (is (= (hash {:a 1 :b 2})
        (hash {:b 2 :a 1})))
  (is (= (hash (hash-map :a 1 :b 2))
        (hash (hash-map :b 2 :a 1))))
  (is (= (hash {:start 133 :end 134})
        (hash (apply hash-map [:start 133 :end 134]))))
  (is (= (hash :a)
        (hash (keyword "a")))))

(deftest test-962-empty-literal-hashes
  (testing "CLJS-962: empty literals should produce collections with correct hash codes"
    (let [l ()
          v []
          s #{}
          m {}]
      (is (== (hash l) (hash v) (hash-ordered-coll ())))
      (is (== (hash s) (hash m) (hash-unordered-coll #{})))))
  (testing "CLJS-962: EMPTY collections should have correct hash codes"
    (let [l   (.-EMPTY List)
          pv  (.-EMPTY PersistentVector)
          phs (.-EMPTY PersistentHashSet)
          pts (.-EMPTY PersistentTreeSet)
          pam (.-EMPTY PersistentArrayMap)
          phm (.-EMPTY PersistentHashMap)
          ptm (.-EMPTY PersistentTreeMap)]
      (is (== (hash l) (hash pv) (hash-ordered-coll ())))
      (is (apply == (hash-unordered-coll #{}) (map hash [phs pts pam phm ptm]))))))

(deftest test-uuid-compile-and-runtime-hash
  (is (= (hash (.toString #uuid "0d1f9029-40fc-4728-8bdd-9862172d4370"))
         (hash (.toString (UUID. "0d1f9029-40fc-4728-8bdd-9862172d4370" nil))))))

(deftest test-cljs-1524
  (let [x0 []
        x1 (conj x0 1)
        x2 (conj x1 2)
        x3 (remove #{1} x2)
        x4 (remove #{2} x3)
        x5 (conj x4 3)
        x6 (conj x5 4)
        x7 (conj x6 5)]
    (is (not (== (hash x0) (hash x1) (hash x2) (hash x3) (hash x4)
               (hash x5) (hash x6) (hash x7))))))

(deftest test-nil-hashing-cljs-1649
  (is (zero? (hash-string nil)))
  (is (not (zero? (hash-string "null")))))

(deftest test-cljs-1779
  (is (= (hash (keyword 'app "foo"))
         (hash (keyword "app" "foo")))))

(deftest test-mumur-support
  (testing "Testing murmur support"
    ;; int-rotate-left
    (is (== (int-rotate-left (bit-or 0x87654321 0) 4) (bit-or 0x76543218 0)))
    (is (== (int-rotate-left (bit-or 0x87654321 0) 8) (bit-or 0x65432187 0)))
    (is (== (int-rotate-left (bit-or 0x80000000 0) 1) 0x1))
    (is (== (int-rotate-left (bit-or 0x78123456 0) 4) (bit-or 0x81234567 0)))
    (is (== (int-rotate-left (bit-or 0xffffffff 0) 4) (bit-or 0xffffffff 0)))

    ;; imul
    (is (== (imul 3 3) 9))
    (is (== (imul -1 8) -8))
    (is (== (imul -2 -2) 4))
    (is (== (imul 0xffffffff 5) -5))
    (is (== (imul 0xfffffffe 5) -10))
    ))

(deftest test-cljs-1818
  (is (= (hash true) 1231))
  (is (= (hash false) 1237)))
