;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.iterator-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is are]]))

(defn seq-iter-match
  [coll]
  (let [i (-iterator coll)]
    (loop [s (seq coll)
           n 0]
      (if (seq s)
        (do
          (when-not (.hasNext i)
            (throw
              (js/Error.
                (str  "Iterator exhausted before seq at(" n ")" ))))
          (let [iv (.next i)
                sv (first s)]
            (when-not (= iv sv)
              (throw
                (js/Error.
                  (str "Iterator value " iv " and seq value " sv " did not match at ( "  n ")")))))
          (recur (rest s) (inc n)))
        (if (.hasNext i)
          (throw
            (js/Error.
              (str  "Seq exhausted before iterator at (" n ")")))
          true)))))

(defrecord TestIterRec [a b])

(deftest coll-iter-seq-match
  (testing "Direct iterators match sequences"
    (let [test-map (apply hash-map (range 200))
          test-set (apply hash-set (range 200))
          test-queue (into cljs.core.PersistentQueue.EMPTY (vec (range 100)))
          test-record (into (TestIterRec. 1 2) {:c 3 :d 4})]
      (is (= true (seq-iter-match test-map)))
      (is (= true (seq-iter-match test-set)))
      (is (= true (seq-iter-match test-queue)))
      (is (= true (seq-iter-match test-record))))))
