;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.new-new-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is]]
            [clojure.string :as s]
            [clojure.set :as set]))

(defprotocol IProtocolWithDocStrings
  (-method1 [this] "some doc")
  (-method2 [this] ""))

(defprotocol IBar (-bar [this x]))

(defn baz [f]
  (reify
    IBar
    (-bar [_ x]
      (f x))))

(deftest test-405
  (is (= 2 (-bar (baz inc) 1))))

(defprotocol IWoz
  (-woz [this]))

(def noz [])

(deftest test-414
  (testing "Testing CLJS-414, specify"
    (is (= (specify noz IWoz (-woz [_] :boz)) noz))
    (is (not (identical? (specify noz IWoz (-woz [_] :boz)) noz)))
    (is (= (-woz (specify noz IWoz (-woz [this] this))) noz))
    (is (= (-woz (specify noz IWoz (-woz [_] :boz))) :boz))))

(defrecord Person [firstname lastname])
(defrecord A [])
(defrecord C [a b c])
(defrecord A' [x])
(defrecord B' [x])
(defrecord FooComparable [x]
  IComparable
  (-compare [_ o] (compare x (.-x o))))

(deftest test-records
  (let [fred (Person. "Fred" "Mertz")
        fred-too (Person. "Fred" "Mertz")
        ethel (with-meta (assoc (Person. "Ethel" "Mertz") :husband :fred)
                {:married true})
        ethel-too (with-meta (assoc (Person. "Ethel" "Mertz")  :husband :fred)
                    {:married true})
        letters (C. "a" "b" "c")
        more-letters (assoc letters :d "d" :e "e" :f "f")]
    (testing "Testing records"
      (is (record? fred))
      (is (not (record? {})))
      (is (= (:firstname fred) "Fred"))
      (is (= fred fred-too))
      (is (false? (= fred nil)))
      (is (false? (= nil fred)))
      (is (= (meta ethel) {:married true}))
      (is (= ethel ethel-too))
      (is (= (map->Person {:firstname "Fred" :lastname "Mertz"}) fred))
      (is (= (->Person "Fred" "Mertz") fred))
      (is (= (count fred) 2))
      (is (= (count ethel) 3))
      (is (= (conj fred {:wife :ethel :friend :ricky})
            (map->Person {:firstname "Fred" :lastname "Mertz" :wife :ethel :friend :ricky})))
      (is (= (conj fred {:lastname "Flintstone"})
            (map->Person {:firstname "Fred" :lastname "Flintstone"})))
      (is (= (assoc fred :lastname "Flintstone")
            (map->Person {:firstname "Fred" :lastname "Flintstone"})))
      (is (= (assoc fred :wife :ethel)
            (map->Person {:firstname "Fred" :lastname "Mertz" :wife :ethel})))
      (is (= (dissoc ethel :husband)
            (map->Person {:firstname "Ethel" :lastname "Mertz"})))
      (is (= (reduce-kv assoc {:age 30} fred)
            {:age 30 :firstname "Fred" :lastname "Mertz"}))
      (is (= {:foo 'bar} (meta (with-meta (A.) {:foo 'bar}))))
      (is (= 'bar (:foo (assoc (A.) :foo 'bar))))
      (is (= (set (keys letters)) #{:a :b :c}))
      (is (= (set (keys more-letters)) #{:a :b :c :d :e :f}))
      (is (= (set (keys (dissoc more-letters :d))) #{:a :b :c :e :f}))
      (is (= (set (keys (dissoc more-letters :d :e))) #{:a :b :c :f}))
      (is (= (set (keys (dissoc more-letters :d :e :f))) #{:a :b :c}))
      (is (not= (A'. nil) (B'. nil)))
      (is (satisfies? IComparable (->FooComparable 1))))))

(deftype FnLike []
  IFn
  (-invoke [_] :a)
  (-invoke [_ a] :b)
  (-invoke [_ a b] :c))

(deftype FnLikeB [a]
  IFn
  (-invoke [_] a))

(deftest test-ifn
  (testing "Testing IFn implementations"
    (is (= :a ((FnLike.))))
    (is (= :b ((FnLike.) 1)))
    (is (= :c ((FnLike.) 1 2)))
    (is (= [:b :b :b] (map (FnLike.) [0 0 0])))
    (is (= 1 ((FnLikeB. 1))))
    ))

(deftype TypeBasis [a b])

(defrecord RecordBasis [c d e])

(deftest test-get-basis
  (is (= (.getBasis TypeBasis) '[a b]))
  (is (= (.getBasis RecordBasis) '[c d e])))

(deftype PositionalFactoryTest [x])

(deftest test-515
  (is (== 1 (.-x (->PositionalFactoryTest 1)))))
