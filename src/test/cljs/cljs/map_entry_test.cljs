;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.map-entry-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is are]]))

(defn map-entry-interface-tests
  "Tests that a MapEntry implements all the expected interfaces correctly.
  Expects a MapEntry type with key `:key` and a val `:val`." 
  [e]
  (testing "map entry interfaces"
    (testing "IMapEntry"
      (testing "-key"
        (is (= :key (-key e))))
      (testing "-val"
        (is (= :val (-val e)))))

    (testing "IEquiv"
      (testing "-equiv"
        (are [x y] (-equiv x y)
          e [:key :val]
          e '(:key :val))))

    (testing "ILookup"
      (testing "-lookup 2-arity"
        (are [x y] (= x y)
          :key (-lookup e 0)
          :val (-lookup e 1)
          nil  (-lookup e 2)
          nil  (-lookup e -1)))
      (testing "-lookup 3-arity"
        (are [x y] (= x y)
          :key (-lookup e 0 :not-found)
          :val (-lookup e 1 :not-found)
          :not-found (-lookup e 2 :not-found)
          :not-found (-lookup e -1 :not-found))))

    (testing "IStack"
      (testing "-peek"
        (is (= :val (-peek e))))
      (testing "-pop"
        (is (vector? (-pop e)))
        (is (= [:key] (-pop e)))))

    (testing "ICollection"
      (testing "-conj"
        (is (vector? (-conj e :thing)))
        (is (= [:key :val :thing] (-conj e :thing)))))

    (testing "IEmptyableCollection"
      (testing "-empty"
        (is (= [] (empty e)))))

    (testing "ISequential"
      (is (satisfies? ISequential e)))

    (testing "ISeqable"
      (testing "-seq"
        (is (= (list :key :val) (-seq e)))))

    (testing "ICounted"
      (testing "-count"
        (is (= 2 (-count e)))))

    (testing "IIndexed"
      (testing "-nth 2-arity"
        (are [x y] (= x y)
          :key (-nth e 0)
          :val (-nth e 1))
        (is (thrown? js/Error (-nth e 2)))
        (is (thrown? js/Error (-nth e -1))))
      (testing "-nth 3-arity"
        (are [x y] (= x y)
          :key (-nth e 0 :not-found)
          :val (-nth e 1 :not-found)
          :not-found (-nth e 2 :not-found)
          :not-found (-nth e -1 :not-found))))

    (testing "IAssociative"
      (testing "-assoc"
        (are [x y] (= x y)
          [:new :val] (-assoc e 0 :new)
          [:key :new] (-assoc e 1 :new)
          [:key :val :new] (-assoc e 2 :new)))
      (testing "-contains-key?"
        (are [x y] (= x y)
          true (-contains-key? e 0)
          true (-contains-key? e 1)
          false (-contains-key? e 2)
          false (-contains-key? e -1))))

    (testing "IVector"
      (testing "-assoc-n"
        (are [x y] (= x y)
          [:new :val] (-assoc-n e 0 :new)
          [:key :new] (-assoc-n e 1 :new)
          [:key :val :new] (-assoc-n e 2 :new))))

    (testing "IReduce"
      (testing "-reduce 2-arity"
        (is (= [:key :val] (-reduce e (fn [r e] [r e])))))
      (testing "-reduce 3-arity"
        (is (= [:key :val] (-reduce e conj [])))))

    (testing "IFind"
      (testing "-find"
        (are [x y] (= x y)
          [0 :key] (-find e 0)
          [1 :val] (-find e 1)
          nil (-find e 2)
          nil (-find e -1))))

    (testing "IFn"
      (testing "-invoke 2-arity"
        (are [x y] (= x y)
          :key (e 0)
          :val (e 1))
        (is (thrown? js/Error (e 2)))
        (is (thrown? js/Error (e -1))))
      (testing "-invoke 3-arity"
        (are [x y] (= x y)
          :key (e 0 :not-found)
          :val (e 1 :not-found)
          :not-found (e 2 :not-found)
          :not-found (e -1 :not-found))))

    (testing "IComparable"
      (testing "-compare"
        (is (zero? (-compare e [:key :val])))))))

(deftest all-map-entry-tests
  (testing "BlackNode"
    (map-entry-interface-tests (BlackNode. :key :val nil nil nil)))
  (testing "RedNode"
    (map-entry-interface-tests (RedNode. :key :val nil nil nil)))
  (testing "Vector"
    (map-entry-interface-tests [:key :val]))
  (testing "MapEntry"
    (map-entry-interface-tests (MapEntry. :key :val nil))))
