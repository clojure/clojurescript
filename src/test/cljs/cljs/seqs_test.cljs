;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.seqs-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is]]
            [clojure.string :as s]
            [clojure.set :as set]))

(deftest test-sequential-equality
  (testing "Testing ISequential equality"
    (is (= (list 3 2 1) [3 2 1]))
    (is (= [3 2 1] (seq (array 3 2 1))))))

(deftest test-seq-operations
  (testing "Testing basic seq operations"
    (is (= () (rest nil)))
    (is (= nil (seq (array))))
    (is (= nil (seq "")))
    (is (= nil (seq [])))
    (is (= nil (seq {})))
    (is (= () (rest ())))
    (is (= () (rest [1])))
    (is (= () (rest (array 1))))))

(deftest test-empy-and-seq
  (testing "Testing empty & seq"
    (is (nil? (empty nil)))
    (let [e-lazy-seq (empty (with-meta (lazy-seq (cons :a nil)) {:b :c}))]
      (testing "lazy seq"
        (is (seq? e-lazy-seq))
        (is (empty? e-lazy-seq))
        (is (= {:b :c} (meta e-lazy-seq)))))
    (let [e-list (empty '^{:b :c} (1 2 3))]
      (testing "list"
        (is (seq? e-list))
        (is (empty? e-list))
        (is (= {:b :c} (meta e-list)))))
    (let [e-elist (empty '^{:b :c} ())]
      (testing "empty list with metadata"
        (is (seq? e-elist))
        (is (empty? e-elist))
        (is (= :c (get (meta e-elist) :b)))))
    (let [e-cons (empty (with-meta (cons :a nil) {:b :c}))]
      (testing "cons"
        (is (seq? e-cons))
        (is (empty? e-cons))
        (is (= {:b :c} (meta e-cons)))))
    (let [e-vec (empty ^{:b :c} [:a :d :g])]
      (testing "vector"
        (is (vector? e-vec))
        (is (empty? e-vec))
        (is (= {:b :c} (meta e-vec)))))
    (let [e-omap (empty ^{:b :c} {:a :d :g :h})]
      (testing "map"
        (is (map? e-omap))
        (is (empty? e-omap))
        (is (= {:b :c} (meta e-omap)))))
    (let [e-hmap (empty ^{:b :c} {[1 2] :d :g :h})]
      (testing "map with complex keys"
        (is (map? e-hmap))
        (is (empty? e-hmap))
        (is (= {:b :c} (meta e-hmap)))))
    (let [smap (with-meta (sorted-map-by (comp - compare) 2 :a 1 :b 5 :c) {:b :c})
          e-smap (empty smap)]
      (testing "sorted-map-by"
        (is (map? e-smap))
        (is (empty? e-smap))
        (is (= {:b :c} (meta e-smap)))
        (is (identical? (-comparator smap) (-comparator e-smap)))
        (is (= [[5 :c] [2 :a] [1 :b]] (seq (assoc e-smap 2 :a 1 :b 5 :c))))))
    (let [sset (with-meta (sorted-set-by (comp - compare) 5 1 2) {:b :c})
          e-sset (empty sset)]
      (testing "sorted-set-by"
        (is (set? e-sset))
        (is (empty? e-sset))
        (is (= {:b :c} (meta e-sset)))
        (is (identical? (-comparator sset) (-comparator e-sset)))
        (is (= [5 2 1] (seq (conj e-sset 5 1 2))))))
    (let [e-queue (empty (with-meta (.-EMPTY PersistentQueue) {:b :c}))]
      (testing "queue"
        (is (identical? (type e-queue) PersistentQueue))
        (is (empty? e-queue))
        (is (= {:b :c} (meta e-queue)))))))

(deftest test-distinct
  (testing "Testing distinct? & distinct"
    (is (distinct? 1 2 3))
    (is (not (distinct? 1 2 3 1)))
    (is (= (distinct ()) ()))
    (is (= (distinct '(1)) '(1)))
    (is (= (distinct '(1 2 3 1 1 1)) '(1 2 3)))
    (is (= (distinct [1 1 1 2]) '(1 2)))
    (is (= (distinct [1 2 1 2]) '(1 2)))
    (is (= (distinct "a") ["a"]))
    (is (= (distinct "abcabab") ["a" "b" "c"]))
    (is (= (distinct ["abc" "abc"]) ["abc"]))
    (is (= (distinct [nil nil]) [nil]))
    (is (= (distinct [0.0 0.0]) [0.0]))
    (is (= (distinct ['sym 'sym]) '[sym]))
    (is (= (distinct [:kw :kw]) [:kw]))
    (is (= (distinct [42 42]) [42]))
    (is (= (distinct [[] []]) [[]]))
    (is (= (distinct ['(1 2) '(1 2)]) '[(1 2)]))
    (is (= (distinct [() ()]) [()]))
    (is (= (distinct [[1 2] [1 2]]) [[1 2]]))
    (is (= (distinct [{:a 1 :b 2} {:a 1 :b 2}]) [{:a 1 :b 2}]))
    (is (= (distinct [{} {}]) [{}]))
    (is (= (distinct [#{1 2} #{1 2}]) [#{1 2}]))
    (is (= (distinct [#{} #{}]) [#{}]))))

(deftest test-rearrange-sequential
  (testing "Test rearranging sequential collections"
    (is (= [1 2 3 4 5] (sort [5 3 1 4 2])))
    (is (= [1 2 3 4 5] (sort < [5 3 1 4 2])))
    (is (= [5 4 3 2 1] (sort > [5 3 1 4 2])))
    (is (= ["a" [ 1 2] "foo"] (sort-by count ["foo" "a" [1 2]])))
    (is (= ["foo" [1 2] "a"] (sort-by count > ["foo" "a" [1 2]])))
    (let [coll [1 2 3 4 5 6 7 8 9 10]
          ;; while it is technically possible for this test to fail with a false negative,
          ;; it's _extraordinarily_ unlikely.
          shuffles (filter #(not= coll %) (take 100 (iterate shuffle coll)))]
      (is (not (empty? shuffles))))
    ))

(deftest test-ISequential-indexOf
  (testing "Testing JS .indexOf in ISequential types"
    ;; PersistentVector
    (is (= (.indexOf [] 2) -1))
    (is (= (.indexOf [] 2 3) -1))
    (is (= (.indexOf [1 2 3 4 5] 2) 1))
    (is (= (.indexOf [1 2 3 4 5] 6) -1))
    (is (= (.indexOf [1 2 3 4 5] -1) -1))
    (is (= (.indexOf [1 2 "x" 4 5 "a"] "a") 5))
    (is (= (.indexOf [1 2 3 4 5] 1 2) -1))
    (is (= (.indexOf [1 2 3 4 5] 2 2) -1))
    (is (= (.indexOf [1 2 3 1 5] 1 2) 3))
    (is (= (.indexOf [1 2 3 4 5] 2) 1))
    (is (= (.indexOf '(1 2 3 4 5) 2) 1))
    (is (= (.indexOf (list 1 2 3) 3) 2))
    (is (= (.indexOf (lazy-seq [1 2 3 4 5]) 3)) 2)
    (is (= (.indexOf (sequence (map inc) '(0 1 2 3 4)) 5) 4))))

(deftest test-ISequential-lastIndexOf
  (testing "Testing JS .lastIndexOf in ISequential types"
    ;; PersistentVector
    (is (= (.lastIndexOf [] 2) -1))
    (is (= (.lastIndexOf [] 2 3) -1))
    (is (= (.lastIndexOf [1 2 3 4 5] 2) 1))
    (is (= (.lastIndexOf [1 2 3 1 5] 1) 3))
    (is (= (.lastIndexOf [1 2 3 1 5] 1 3) 3))
    (is (= (.lastIndexOf [1 2 3 1 5] 1 2) 0))
    (is (= (.lastIndexOf [1 2 3 1] 1 0) 0))
    (is (= (.lastIndexOf [1 2 3 4 5] 3 100) 2))
    (is (= (.lastIndexOf [1 1 1 1 1] 1) 4))
    (is (= (.lastIndexOf [1 1 1 1 1] 1 6) 4))
    (is (= (.lastIndexOf [1 2 1 1 1] 2) 1))
    (is (= (.lastIndexOf [1 2 3 4 5] 3 -100) -1))
    (is (= (.lastIndexOf [1 2 3 4 5] 3 -2) 2))
    (is (= (.lastIndexOf '(1 2 1 4 5) 1) 2))
    (is (= (.lastIndexOf (list 1 2 3 1 5) 1) 3))
    (is (= (.lastIndexOf (lazy-seq [1 2 1 4 5]) 1)) 2)
    (is (= (.lastIndexOf (sequence (map inc) '(0 1 0 3 4)) 1) 2))))

(deftest test-chunked
  (let [r (range 64)
        v (into [] r)]
    (testing "Testing Chunked Seqs"
      (is (= (hash (seq v)) (hash (seq v))))
      (is (= 6 (reduce + (array-chunk (array 1 2 3)))))
      (is (instance? ChunkedSeq (seq v)))
      (is (= r (seq v)))
      (is (= (map inc r) (map inc v)))
      (is (= (filter even? r) (filter even? v)))
      (is (= (filter odd? r) (filter odd? v)))
      (is (= (concat r r r) (concat v v v)))
      (is (satisfies? IReduce (seq v)))
      (is (== 2010 (reduce + (nnext (nnext (seq v))))))
      (is (== 2020 (reduce + 10 (nnext (nnext (seq v)))))))))

(deftest test-778
  (testing "Testing CLJS-778, -rest, -next RSeq"
    (is (= (-rest (rseq [0])) ()))
    (is (nil? (-next (rseq [0]))))
    (is (= (set (rseq [0])) #{0}))))

(deftest test-indexed-seqs
  (testing "Testing IndexedSeq"
    (testing "Sequence equality"
      (is (= (list 0 1 2 3 4 5) (seq (array 0 1 2 3 4 5)))))
    (testing "nth lookup within bounds"
      (is (= 0 (nth (seq (array 0 1 2 3 4 5)) 0)))
      (is (= 0 (nth (seq (array 0 1 2 3 4 5)) 0 :not-found)))
      (is (= 5 (nth (seq (array 0 1 2 3 4 5)) 5)))
      (is (= 5 (nth (seq (array 0 1 2 3 4 5)) 5 :not-found))))
    (testing "nth lookup out of bounds"
      (is (thrown? js/Error (nth (seq (array 0 1 2 3 4 5)) 6)))
      (is (= :not-found (nth (seq (array 0 1 2 3 4 5)) 6 :not-found)))
      (is (thrown? js/Error (nth (seq (array 0 1 2 3 4 5)) -1)))
      (is (= :not-found (nth (seq (array 0 1 2 3 4 5)) -1 :not-found))))))

(deftest test-cljs-2131
  (testing "calling empty on a ChunkedSeq returns an empty list"
    (let [small-vec [1 2 3]
          big-vec   (into [] (range 1000))]
      (is (identical? (empty (seq small-vec)) ()))
      (is (identical? (empty (seq big-vec))   ())))))

(defrecord Foo [a b])

(deftest test-cljs-2482
  (testing "seq on defrecord returns map entries"
    (is (every? map-entry? (seq (->Foo 1 2))))))

(deftest test-cljs-2911
  (testing "partition-by works correclty with infinite seqs"
    (is (= (first (second (partition-by zero? (range)))) 1))))
