;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.collections-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is are]]
            [clojure.string :as s]
            [clojure.set :as set]))

(deftest test-map-operations
  (testing "Test basic map collection operations"
    (is (= {:a :b} (get {[1 2 3] {:a :b}, 4 5} [1 2 3])))
    (is (not (= {:a :b :c nil} {:a :b :d nil})))
    (is (= {:a :b} (dissoc {:a :b :c :d} :c)))
    (is (= (hash-map :foo 5)
          (assoc (cljs.core.ObjMap. nil (array) (js-obj)) :foo 5))))
  (testing "Testing assoc dissoc"
    (is (= {1 2 3 4} (assoc {} 1 2 3 4)))
    (is (= {1 2} (assoc {} 1 2)))
    (is (= [42 2] (assoc [1 2] 0 42)))
    (is (= {} (dissoc {1 2 3 4} 1 3)))
    (is (= {1 2} (dissoc {1 2 3 4} 3)))
    (is (nil? (dissoc nil :foo))))
  (testing "Testing find"
    (is (= (find {} :a) nil))
    (is (= (find {:a 1} :a) [:a 1]))
    (is (= (find {:a 1} :b) nil))
    (is (= (find {:a 1 :b 2} :a) [:a 1]))
    (is (= (find {:a 1 :b 2} :b) [:b 2]))
    (is (= (find {:a 1 :b 2} :c) nil))
    (is (= (find {} nil) nil))
    (is (= (find {:a 1} nil) nil))
    (is (= (find {:a 1 :b 2} nil) nil))
    (is (= (find [1 2 3] 0) [0 1]))
    (is (= (find [1 2 3] -1) nil))
    (is (= (find [1 2 3] js/NaN) nil))
    (is (= (find [1 2 3] :a) nil))
    (is (= (find [1 2 3] 10) nil)))
  )

(deftest test-vectors
  (testing "Testing vectors"
    (is (= :a (nth [:a :b :c :d] 0)))
    (is (= :a (nth [:a :b :c :d] 0.1)))
    (let [pv (vec (range 97))]
      (testing "basic ops"
        (is (= (nth pv 96) 96))
        (is (= (nth pv 97 nil) nil))
        (is (= (pv 96) 96))
        (is (nil? (rseq [])))
        (is (= (reverse pv) (rseq pv)))))
    (let [pv (vec (range 33))]
      (testing "pop"
        (is (= pv (-> pv pop pop (conj 31) (conj 32))))))
    (let [stack1 (pop (vec (range 97)))
          stack2 (pop stack1)]
      (testing "stack operations"
        (is (= 95 (peek stack1)))
        (is (= 94 (peek stack2)))))
    (let [v1 (vec (range 10))
          v2 (vec (range 5))
          s (subvec v1 2 8)]
      (testing "subvec"
        (is (= s (-> v1 (subvec 2) (subvec 0 6)) (->> v1 (drop 2) (take 6))))
        (is (= 6 (count s)))
        (is (= [2 3 4 5 6] (pop s)))
        (is (= 7 (peek s)))
        (is (= [2 3 4 5 6 7 1]
              (assoc s 6 1)
              (conj s 1)))
        (is (= 27 (reduce + s)))
        (is (= s (vec s))) ; pour into plain vector
        ;; go outside ranges
        (is (= :fail (try (subvec v2 0 6) (catch js/Error e :fail))))
        (is (= :fail (try (subvec v2 6 10) (catch js/Error e :fail))))
        (is (= :fail (try (subvec v2 6 10) (catch js/Error e :fail))))
        (is (= :fail (try (subvec v2 3 6) (catch js/Error e :fail))))
        ;; no layered subvecs
        (is (identical? v1 (.-v (subvec s 1 4))))
        (let [m {:x 1}]
          (is (= m (meta (with-meta s m)))))
        ;; CLJS-997
        (is (= (reduce-kv + 123 (vec (range 10 50)))
              (reduce-kv + 123 (subvec (vec (range 100)) 10 50)))))
      ;; CLJS-513
      (let [sentinel (js-obj)
            s (subvec [0 1 2 3] 1 2)]
        (testing "bounds checking"
          (is (identical? sentinel (try (s -1) (catch js/Error _ sentinel))))
          (is (identical? sentinel (try (s 1) (catch js/Error _ sentinel))))))
      ;; CLJS-765
      (let [sv1 (subvec [0 1 2 3] 1 2)
            sv2 (subvec [0 1 2 3] 1 1)]
        (testing "rseq equality"
          (is (= (rseq sv1) '(1)))
          (is (nil? (rseq sv2)))))
      (let [sv1 (subvec [0 1 2 3] 0 2)
            sv2 (subvec [0 1 2 3] 1 3)]
        (testing "IFind"
          (is (= (find sv1 0) [0 0]))
          (is (= (find sv1 1) [1 1]))
          (is (= (find sv1 2) nil))
          (is (= (find sv1 -1) nil))
          (is (= (find sv2 0) [0 1]))
          (is (= (find sv2 1) [1 2]))
          (is (= (find sv2 2) nil))
          (is (= (find sv2 -1) nil))))
      )
    ))

(deftest test-sets
  (testing "Testing persistent sets"
    (is (set []))
    (is (= #{} (set [])))
    (is (= #{} (hash-set)))
    (is (identical? cljs.core.PersistentHashSet (type (hash-set))))

    (is (= #{"foo"} (set ["foo"])))
    (is (= #{"foo"} (hash-set "foo")))
    (is (= #{1 2 3} #{1 3 2}))
    (is (= #{#{1 2 3} [4 5 6] {7 8} 9 10}
          #{10 9 [4 5 6] {7 8} #{1 2 3}}))
    (is (not (= #{nil [] {} 0 #{}} #{})))
    (is (= (count #{nil [] {} 0 #{}}) 5))
    (is (= (conj #{1} 1) #{1}))
    (is (= (conj #{1} 2) #{2 1}))
    (is (= #{} (-empty #{1 2 3 4})))
    (is (= (reduce + #{1 2 3 4 5}) 15))
    (is (= 4 (get #{1 2 3 4} 4)))
    (is (contains? #{1 2 3 4} 4))
    (is (contains? #{[] nil 0 {} #{}} {}))
    (is (contains? #{[1 2 3]} [1 2 3]))
    (is (not (contains? (-disjoin #{1 2 3} 3) 3)))
    (is (= #{1 2 3} (disj #{1 2 3})))
    (is (= #{1 2} (disj #{1 2 3} 3)))
    (is (= #{1} (disj #{1 2 3} 2 3)))
    (is (nil? (disj nil :foo)))))

(deftest test-range
  (testing "Testing Range"
    ;; Range
    (is (= (range 0 10 3) (list 0 3 6 9)))
    (is (= (count (range 0 10 3)) 4))
    (is (= (range 0 -10 -3) (list 0 -3 -6 -9)))
    (is (= (count (range 0 -10 -3)) 4))
    (is (= (range -10 10 3) (list -10 -7 -4 -1 2 5 8)))
    (is (= (count (range -10 10 3)) 7))
    (is (= (range 0 1 1) (list 0)))
    (is (= (range 0 -3 -1) (list 0 -1 -2)))
    (is (= (range 3 0 -1) (list 3 2 1)))
    (is (= (range 0 10 -1) (list)))
    (is (= (take 3 (range 0 1 0)) (list 0 0 0)))
    (is (= (range 10 0 1) (list)))
    (is (= (range 0 0 0) (list)))
    (is (= (count (range 0 10 -1)) 0))
    (is (= (count (take 3 (range 0 2 0))) 3))
    (is (= (count (range 10 0 1)) 0))
    (is (= (count (range 0 0 0)) 0))
    (is (= (take 3 (range 1 0 0)) (list 1 1 1)))
    (is (= (take 3 (range 3 1 0)) (list 3 3 3)))
    ))

(deftest test-cycle
  (testing "Testing Cycle"

    (is (= "(1 2 3 1 2 ...)" (binding [*print-length* 5] (pr-str (cycle [1 2 3])))))

    (are [x y] (= x y)
      (cycle []) ()
      (cycle nil) ()

      (take 3 (cycle [1])) '(1 1 1)
      (take 5 (cycle [1 2 3])) '(1 2 3 1 2)

      (take 3 (cycle [nil])) '(nil nil nil)

      (transduce (take 5) + (cycle [1])) 5
      (transduce (take 5) + 2 (cycle [1])) 7
      (transduce (take 5) + (cycle [3 7])) 23
      (transduce (take 5) + 2 (cycle [3 7])) 25

      (take 2 (cycle (map #(/ 42 %) '(2 1 0)))) '(21 42)
      (first (cycle [1 2 3])) 1
      (first (rest (cycle [1 2 3]))) 2
      (first (next (cycle [1 2 3]))) 2
      (first (conj (cycle [1 2 3]) :hi)) :hi
      (empty (cycle [1 2 3])) ()
      (first (next (cycle (map #(/ 42 %) '(2 1 0))))) 42
      (into [] (take 2) (cycle (map #(/ 42 %) '(2 1 0)))) '(21 42)

      (first (seq (cycle [1 2 3]))) 1)

    ; indexOf fns work on the finite cycle
    (is (= -1 (.indexOf (cycle []) 19)))
    (is (= -1 (.indexOf (cycle []) 19 2)))
    (is (= -1 (.lastIndexOf (cycle []) 19)))
    (is (= -1 (.lastIndexOf (cycle []) 19 2)))

    (is (= {:a 1} (meta (with-meta (cycle [1 2 3]) {:a 1}))))
    (is (= {:a 1} (meta (empty (with-meta (cycle [1 2 3]) {:a 1})))))
    (is (= (take 7 (with-meta (cycle [1 2 3]) {:a 1})) (take 7 (cycle [1 2 3]))))

    (is (realized? (cycle [1 2 3])))

    (are [x y] (= (transduce (take x) conj (cycle [1 2 3])) y)
      0 []
      1 [1]
      2 [1 2]
      3 [1 2 3]
      4 [1 2 3 1]
      5 [1 2 3 1 2]
      6 [1 2 3 1 2 3]
      7 [1 2 3 1 2 3 1])

    (are [x y] (= (transduce (take x) conj [:x] (cycle [1 2 3])) y)
      0 [:x]
      1 [:x 1]
      2 [:x 1 2]
      3 [:x 1 2 3]
      4 [:x 1 2 3 1]
      5 [:x 1 2 3 1 2]
      6 [:x 1 2 3 1 2 3]
      7 [:x 1 2 3 1 2 3 1])))

(deftest test-repeat
  (testing "Testing Repeat"
    (is (= (repeat 5 :x) (repeat 5 :x)))
    (is (= (repeat 5 :x) '(:x :x :x :x :x)))
    (is (= (hash (repeat 5 :x)) (hash '(:x :x :x :x :x))))
    (is (= (assoc (array-map (repeat 1 :x) :y) '(:x) :z) {'(:x) :z}))
    (is (= (assoc (hash-map (repeat 1 :x) :y) '(:x) :z) {'(:x) :z}))

    (is (= "(7 7 7 ...)" (binding [*print-length* 3] (pr-str (repeat 7)))))
    (is (= "(7 7 7)" (pr-str (repeat 3 7))))

    (are [x y] (= x y)
      (take 0 (repeat 7)) ()
      (take 1 (repeat 7)) '(7)
      (take 2 (repeat 7)) '(7 7)
      (take 5 (repeat 7)) '(7 7 7 7 7))

    ; limited sequence
    (are [x y] (= x y)
      (repeat 0 7) ()
      (repeat 1 7) '(7)
      (repeat 2 7) '(7 7)
      (repeat 5 7) '(7 7 7 7 7)

      (repeat -1 7) ()
      (repeat -3 7) ())

    ;; counts
    (are [x y] (= (count x) y)
      (repeat 0 7) 0
      (repeat 1 7) 1
      (repeat 2 7) 2
      (repeat 5 7) 5

      (repeat -1 7) 0
      (repeat -3 7) 0)

    ; test different data types
    (are [x] (= (repeat 3 x) (list x x x))
      nil
      false true
      0 42
      0.0 3.14
      0M 1M
      \c
      "" "abc"
      'sym
      :kw
      () '(1 2)
      [] [1 2]
      {} {:a 1 :b 2}
      #{} #{1 2})

    ; indexOf / lastIndexOf work on finite repeats
    (is (= -1 (.indexOf (repeat 7 5) 19)))
    (is (= -1 (.indexOf (repeat 7 5) 19 2)))
    (is (= -1 (.lastIndexOf (repeat 7 5) 19)))
    (is (= -1 (.lastIndexOf (repeat 7 5) 19 2)))
    (is (= 0 (.indexOf (repeat 7 5) 5)))
    (is (= 6 (.lastIndexOf (repeat 7 5) 5)))
    (is (= 3 (.indexOf (repeat 7 5) 5 3)))
    (is (= 3 (.lastIndexOf (repeat 7 5) 5 3)))

    (is (= {:a 1} (meta (with-meta (repeat 5 7) {:a 1}))))
    (is (= {:a 1} (meta (empty (with-meta (repeat 5 7) {:a 1})))))
    (is (= (with-meta (repeat 5 7) {:a 1}) (repeat 5 7)))

    (is (not (realized? (repeat 5 7))))

    (is (= [1 1] (into [] (drop 98) (repeat 100 1))))

    (is (= () (empty (repeat 100 1))))
    (is (= () (empty (repeat 7))))

    (are [x y] (= (transduce (take x) conj (repeat 1)) y)
      0 []
      1 [1]
      2 [1 1]
      3 [1 1 1])

    (are [x y] (= (transduce (take x) conj [:x] (repeat 1)) y)
      0 [:x]
      1 [:x 1]
      2 [:x 1 1]
      3 [:x 1 1 1])

    (are [x y] (= (transduce (take x) conj (repeat 2 1)) y)
      0 []
      1 [1]
      2 [1 1]
      3 [1 1])

    (are [x y] (= (transduce (take x) conj [:x] (repeat 2 1)) y)
      0 [:x]
      1 [:x 1]
      2 [:x 1 1]
      3 [:x 1 1])))

(deftest test-iterate
  (testing "Testing Iterate"
    (are [x y] (= x y)
      (take 0 (iterate inc 0)) ()
      (take 1 (iterate inc 0)) '(0)
      (take 2 (iterate inc 0)) '(0 1)
      (take 5 (iterate inc 0)) '(0 1 2 3 4))

    (is (= "(0 1 2 ...)" (binding [*print-length* 3] (pr-str (iterate inc 0)))))

    (is (not (realized? (rest (iterate inc 0)))))

    (is (= {:a 1} (meta (with-meta (iterate inc 0) {:a 1}))))
    (is (= {:a 1} (meta (empty (with-meta (iterate inc 0) {:a 1})))))
    (is (= (take 20 (with-meta (iterate inc 0) {:a 1})) (take 20 (iterate inc 0))))

    (is (= [:first 0 1] (take 3 (conj (iterate inc 0) :first))))

    (is (= () (empty (iterate inc 0))))

    (let [v (iterate inc 0)]
      (is (identical? v (seq v))))

    (is (= 0 (first (iterate inc 0))))
    (is (= 1 (first (rest (iterate inc 0)))))
    (is (= 1 (first (next (iterate inc 0)))))

    ;; test other fns
    (is (= '(:foo 42 :foo 42) (take 4 (iterate #(if (= % :foo) 42 :foo) :foo))))
    (is (= '(1 false true true) (take 4 (iterate boolean? 1))))
    (is (= '(256 128 64 32 16 8 4 2 1 0) (take 10 (iterate #(quot % 2) 256))))

    ;; reduce via transduce
    (is (= (transduce (take 5) + (iterate #(* 2 %) 2)) 62))
    (is (= (transduce (take 5) + 1 (iterate #(* 2 %) 2)) 63))

    (are [x y] (= (transduce (take x) conj (iterate inc 0)) y)
      0 []
      1 [0]
      2 [0 1]
      3 [0 1 2])

    (are [x y] (= (transduce (take x) conj [:x] (iterate inc 0)) y)
      0 [:x]
      1 [:x 0]
      2 [:x 0 1]
      3 [:x 0 1 2])))

(deftest test-split-at
  (is (vector? (split-at 2 [])))
  (is (vector? (split-at 2 [1 2 3])))

  (are [x y] (= x y)
    (split-at 2 []) [() ()]
    (split-at 2 [1 2 3 4 5]) [(list 1 2) (list 3 4 5)]

    (split-at 5 [1 2 3]) [(list 1 2 3) ()]
    (split-at 0 [1 2 3]) [() (list 1 2 3)]
    (split-at -1 [1 2 3]) [() (list 1 2 3)]
    (split-at -5 [1 2 3]) [() (list 1 2 3)] ))

(deftest test-rseq
  (testing "Testing RSeq"
    (is (= '(3 2 1) (reverse (seq (array 1 2 3)))))
    (is (= '(3 2 1) (reverse [1 2 3])))
    (is (= '(4 3 2 1) (cons 4 (reverse [1 2 3]))))
    (is (= 6 (reduce + (reverse [1 2 3]))))
    (is (= '(4 3 2) (map inc (reverse [1 2 3]))))
    (is (= '(4 2) (filter even? (reverse [1 2 3 4]))))
    ))

(deftest test-sorted-map
  (testing "Testing sorted maps"
    ;; PersistentTreeMap
    (let [m1 (sorted-map)
          c2 (comp - compare)
          m2 (sorted-map-by c2)]
      (testing "basic ops 1"
        (is (identical? cljs.core.PersistentTreeMap (type m1)))
        (is (identical? cljs.core.PersistentTreeMap (type m2)))
        (is (identical? compare (.-comp m1)))
        (is (zero? (count m1)))
        (is (zero? (count m2)))
        (is (nil? (rseq m1)))
        (let [m1 (assoc m1 :foo 1 :bar 2 :quux 3)
              m2 (assoc m2 :foo 1 :bar 2 :quux 3)]
          (testing "basic ops 2"
            (is (= (count m1) 3))
            (is (= (count m2) 3))
            (is (= (seq m1) (list [:bar 2] [:foo 1] [:quux 3])))
            (is (= (seq m2) (list [:quux 3] [:foo 1] [:bar 2])))
            (is (= (seq m1) (rseq m2)))
            (is (= (seq m2) (rseq m1)))
            (is (= (conj m1 [:wibble 4]) {:foo 1 :bar 2 :quux 3 :wibble 4}))
            (is (= (count (conj m1 [:wibble 4])) 4))
            (is (= (conj m2 [:wibble 4]) {:foo 1 :bar 2 :quux 3 :wibble 4}))
            (is (= (count (conj m2 [:wibble 4])) 4))
            (is (= (map key (assoc m1 nil 4)) (list nil :bar :foo :quux)))
            (is (= (map key (assoc m2 nil 4)) (list :quux :foo :bar nil)))))))
    (let [m (->> [[0 10] [20 30] [10 20] [50 60] [30 40] [40 50]]
              (mapcat (partial apply range))
              (mapcat #(list % %))
              (apply sorted-map))
          s1 (map #(vector % %) (range 60))
          s2 (map #(vector % %) (range 59 -1 -1))]
      (testing "edge cases 1"
        (is (= (count m) 60))
        (is (= (seq m) s1))
        (is (= (rseq m) s2))))
    (let [m (sorted-map :foo 1 :bar 2 :quux 3)]
      (testing "edge cases 2"
        (is (= (dissoc m :foo) (hash-map :bar 2 :quux 3)))
        (is (= (count (dissoc m :foo)) 2))
        (is (= (hash m) (hash (hash-map :foo 1 :bar 2 :quux 3))))
        (is (= (subseq m < :foo)  (list [:bar 2])))
        (is (= (subseq m <= :foo) (list [:bar 2] [:foo 1])))
        (is (= (subseq m > :foo)  (list [:quux 3])))
        (is (= (subseq m >= :foo) (list [:foo 1] [:quux 3])))
        (is (= (map #(reduce (fn [_ x] x) %) m) (list 2 1 3)))
        (is (= (map #(reduce (fn [x _] x) 7 %) m) (list 7 7 7)))))
    ))

(deftest test-sorted-sets
  (let [s1 (sorted-set)
        c2 (comp - compare)
        s2 (sorted-set-by c2)
        c3 #(compare (quot %1 2) (quot %2 2))
        s3 (sorted-set-by c3)
        s4 (sorted-set-by <)]
    (testing "Testing sorted set"
      (is (identical? cljs.core.PersistentTreeSet (type s1)))
      (is (identical? cljs.core.PersistentTreeSet (type s2)))
      (is (identical? compare (-comparator s1)))
      (is (zero? (count s1)))
      (is (zero? (count s2)))
      (is (nil? (rseq s1)))
      (let [s1 (conj s1 1 2 3)
            s2 (conj s2 1 2 3)
            s3 (conj s3 1 2 3 7 8 9)
            s4 (conj s4 1 2 3)]
        (testing "basic ops"
          (is (= (hash s1) (hash s2)))
          (is (= (hash s1) (hash #{1 2 3})))
          (is (= (seq s1)  (list 1 2 3)))
          (is (= (rseq s1) (list 3 2 1)))
          (is (= (seq s2)  (list 3 2 1)))
          (is (= (rseq s2) (list 1 2 3)))
          (is (= (count s1) 3))
          (is (= (count s2) 3))
          (is (= (count s3) 4))
          (is (= (get s3 0) 1))
          (is (= (subseq s3 > 5) (list 7 8)))
          (is (= (subseq s3 > 6) (list 8)))
          (is (= (subseq s3 >= 6) (list 7 8)))
          (is (= (subseq s3 >= 12) nil))
          (is (= (subseq s3 < 0) (list)))
          (is (= (subseq s3 < 5) (list 1 2)))
          (is (= (subseq s3 < 6) (list 1 2)))
          (is (= (subseq s3 <= 6) (list 1 2 7)))
          (is (= (subseq s3 >= 2 <= 6) (list 2 7)))
          (is (= (subseq s4 >= 2 < 3) (list 2)))
          (let [s1 (disj s1 2)
                s2 (disj s2 2)]
            (testing "edge cases"
              (is (= (seq s1)  (list 1 3)))
              (is (= (rseq s1) (list 3 1)))
              (is (= (seq s2)  (list 3 1)))
              (is (= (rseq s2) (list 1 3)))
              (is (= (count s1) 2))
              (is (= (count s2) 2)))))))))

(deftest test-lazy-seq-realized?
  (testing "Testing LazySeq IPending"
    (let [xs (lazy-seq
               (cons 1
                 (lazy-seq
                   (cons 2
                     (lazy-seq (cons 3 nil))))))]
      (is (not (realized? xs)))
      (is (not (realized? (rest xs))))
      (is (realized? xs))
      (is (not (realized? (nthrest xs 2))))
      (is (realized? (rest xs))))))

(deftest test-784
  (testing "Testing CLJS-784, conj on maps"
    (doseq [m [(array-map) (hash-map) (sorted-map)]]
      (is (= :ok
            (try
              (conj m "foo")
              (catch js/Error _
                :ok))))
      (is (= {:foo 1} (conj m [:foo 1])))
      (is (= {:foo 1} (conj m {:foo 1})))
      (is (= {:foo 1} (conj m (list [:foo 1])))))
    (doseq [mt [array-map hash-map sorted-map]]
      (is (= {:foo 1 :bar 2 :baz 3}
            (conj (mt :foo 1)
              ((fn make-seq [from-seq]
                 ;; this tests specifically for user defined seq's, that implement the bare minimum, i.e. no INext
                 (when (seq from-seq)
                   (reify
                     ISeqable
                     (-seq [this] this)
                     ISeq
                     (-first [this] (first from-seq))
                     (-rest [this] (make-seq (rest from-seq))))))
                [[:bar 2] [:baz 3]]))))))
  )

(deftest test-849
  (let [xs [44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24]]
    (testing "Testing CLJS-849, transient contains?"
      (is (loop [m (transient (zipmap xs (repeat 1)))
                 xs xs]
            (if-let [x (first xs)]
              (if (contains? m x)
                (recur (dissoc! m x) (next xs))
                false)
              true))))))

(deftest test-large-array-map
  (let [m (array-map 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13 14 14 15 15)]
    (testing "Testing large array maps"
      (is (instance? cljs.core/PersistentArrayMap m))
      (is (= (seq m) [[0 0] [1 1] [2 2] [3 3] [4 4] [5 5] [6 6] [7 7] [8 8] [9 9] [10 10] [11 11] [12 12] [13 13] [14 14] [15 15]])))))

(def test-map
  {:a 1
   :b 2
   #inst "2013-12-19T05:00:00.000-00:00" 3
   :d 4
   :e 5
   #inst "2013-12-06T05:00:00.000-00:00" 6
   :g 7
   :h 8
   :i 9
   :j 10})

(deftest test-716
  (testing "Testing CLJS-716, date as keys in maps"
    (is (= (test-map #inst "2013-12-19T05:00:00.000-00:00") 3))
    (is (= (test-map #inst "2013-12-06T05:00:00.000-00:00") 6))))

(deftest test-transient-edge-case-1
  (let [v1 (vec (range 15 48))
        v2 (vec (range 40 57))
        v1 (persistent! (assoc! (conj! (pop! (transient v1)) :foo) 0 :quux))
        v2 (persistent! (assoc! (conj! (transient v2) :bar) 0 :quux))
        v  (into v1 v2)]
    (is (= v (vec (concat [:quux] (range 16 47) [:foo]
                    [:quux] (range 41 57) [:bar]))))))

(deftest test-transient-edge-case-2
  (is (loop [v  (transient [])
             xs (range 100)]
        (if-let [x (first xs)]
          (recur
            (condp #(%1 (mod %2 3)) x
              #{0 2} (conj! v x)
              #{1}   (assoc! v (count v) x))
            (next xs))
          (= (vec (range 100)) (persistent! v))))))

(deftest test-phm
  ;; PersistentHashMap & TransientHashMap
  (loop [m1 cljs.core.PersistentHashMap.EMPTY
         m2 (transient cljs.core.PersistentHashMap.EMPTY)
         i 0]
    (if (< i 100)
      (recur (assoc m1 i i) (assoc! m2 i i) (inc i))
      (let [m2 (persistent! m2)]
        (is (= (count m1) 100))
        (is (= (count m2) 100))
        (is (= m1 m2))
        (loop [i 0]
          (if (< i 100)
            (do (is (= (m1 i) i))
                (is (= (m2 i) i))
                (is (= (get m1 i) i))
                (is (= (get m2 i) i))
                (is (contains? m1 i))
                (is (contains? m2 i))
                (recur (inc i)))))
        (is (= (map vector (range 100) (range 100)) (sort-by first (seq m1))))
        (is (= (map vector (range 100) (range 100)) (sort-by first (seq m2))))
        (is (not (contains? (dissoc m1 3) 3))))))
  (let [tm (-> (->> (interleave (range 10) (range 10))
                 (apply assoc cljs.core.PersistentHashMap.EMPTY))
             (dissoc 3 5 7)
             transient)]
    (doseq [k [0 1 2 4 6 8 9]]
      (is (= k (get tm k))))
    (let [m (persistent! tm)]
      (is (= 2 (try (dissoc! tm 1) 1 (catch js/Error e 2))))
      (is (= 2 (try (assoc! tm 10 10) 1 (catch js/Error e 2))))
      (is (= 2 (try (persistent! tm) 1 (catch js/Error e 2))))
      (is (= 2 (try (count tm) 1 (catch js/Error e 2))))
      (is (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))
  (let [m (-> (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentHashMap.EMPTY))
            (dissoc 3 5 7))]
    (testing "Testing PHM dissoc"
      (is (= (count m) 7))
      (is (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))
  (let [m (-> (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentHashMap.EMPTY))
            (conj [:foo 1]))]
    (testing "Testing PHM conj"
      (is (= (count m) 11))
      (is (= m {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :foo 1}))))
  (let [m (-> (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentHashMap.EMPTY)
                transient)
            (conj! [:foo 1])
            persistent!)]
    (testing "Testing PHM conj!"
      (is (= (count m) 11))
      (is (= m {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :foo 1}))))
  (let [tm (->> (interleave (range 10) (range 10))
             (apply assoc cljs.core.PersistentHashMap.EMPTY)
             transient)]
    (testing "Testing transient PHM"
      (is (loop [tm tm ks [3 5 7]]
            (if-let [k (first ks)]
              (recur (dissoc! tm k) (next ks))
              (let [m (persistent! tm)]
                (and (= (count m) 7)
                  (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))))))
  )

(deftype FixedHash [h v]
  IHash
  (-hash [this] h)
  IEquiv
  (-equiv [this other]
    (and (instance? FixedHash other) (= v (.-v other)))))

(def fixed-hash-foo (FixedHash. 0 :foo))
(def fixed-hash-bar (FixedHash. 0 :bar))

(deftest test-phm-fixed-hash
  (let [m (assoc cljs.core.PersistentHashMap.EMPTY
            fixed-hash-foo 1
            fixed-hash-bar 2)]
    (is (= (get m fixed-hash-foo) 1))
    (is (= (get m fixed-hash-bar) 2))
    (is (= (count m) 2))
    (let [m (dissoc m fixed-hash-foo)]
      (is (= (get m fixed-hash-bar) 2))
      (is (not (contains? m fixed-hash-foo)))
      (is (= (count m) 1))))

  (let [m (into cljs.core.PersistentHashMap.EMPTY ; make sure we're testing
            (zipmap (range 100) (range 100))) ; the correct map type
        m (assoc m fixed-hash-foo 1 fixed-hash-bar 2)]
    (is (= (count m) 102))
    (is (= (get m fixed-hash-foo) 1))
    (is (= (get m fixed-hash-bar) 2))
    (let [m (dissoc m 3 5 7 fixed-hash-foo)]
      (is (= (get m fixed-hash-bar) 2))
      (is (not (contains? m fixed-hash-foo)))
      (is (= (count m) 98))))

  (let [m (into cljs.core.PersistentHashMap.EMPTY ; make sure we're testing
            (zipmap (range 100) (range 100))) ; the correct map type
        m (transient m)
        m (assoc! m fixed-hash-foo 1)
        m (assoc! m fixed-hash-bar 2)
        m (persistent! m)]
    (is (= (count m) 102))
    (is (= (get m fixed-hash-foo) 1))
    (is (= (get m fixed-hash-bar) 2))
    (let [m (dissoc m 3 5 7 fixed-hash-foo)]
      (is (= (get m fixed-hash-bar) 2))
      (is (not (contains? m fixed-hash-foo)))
      (is (= (count m) 98)))))

(def array-map-conversion-threshold
  cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD)

(deftest test-pam
  (let [m (-> (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentArrayMap.EMPTY))
            (dissoc 3 5 7))]
    (is (= (count m) 7))
    (is (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9})))
  (let [m (-> (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentArrayMap.EMPTY))
            (conj [:foo 1]))]
    (is (= (count m) 11))
    (is (= m {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :foo 1})))
  (let [m (-> (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentArrayMap.EMPTY)
                transient)
            (conj! [:foo 1])
            persistent!)]
    (is (= (count m) 11))
    (is (= m {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :foo 1})))
  (let [tm (->> (interleave (range 10) (range 10))
             (apply assoc cljs.core.PersistentArrayMap.EMPTY)
             transient)]
    (loop [tm tm ks [3 5 7]]
      (if-let [k (first ks)]
        (recur (dissoc! tm k) (next ks))
        (let [m (persistent! tm)]
          (is (= (count m) 7))
          (is (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))))
  (let [tm (-> (->> (interleave (range 10) (range 10))
                 (apply assoc cljs.core.PersistentArrayMap.EMPTY))
             (dissoc 3 5 7)
             transient)]
    (doseq [k [0 1 2 4 6 8 9]]
      (is (= k (get tm k))))
    (let [m (persistent! tm)]
      (is (= 2 (try (dissoc! tm 1) 1 (catch js/Error e 2))))
      (is (= 2 (try (assoc! tm 10 10) 1 (catch js/Error e 2))))
      (is (= 2 (try (persistent! tm) 1 (catch js/Error e 2))))
      (is (= 2 (try (count tm) 1 (catch js/Error e 2))))
      (is (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))
  (let [m (apply assoc cljs.core.PersistentArrayMap.EMPTY
            (interleave (range (* 2 array-map-conversion-threshold))
              (range (* 2 array-map-conversion-threshold))))]
    (is (= (count m) (* 2 array-map-conversion-threshold)))
    (is (= (m array-map-conversion-threshold) array-map-conversion-threshold))
    (is (= m (into cljs.core.PersistentHashMap.EMPTY
               (map #(vector % %)
                 (range (* 2 array-map-conversion-threshold)))))))
  )

(deftest test-literal-maps
  (loop [m1 {} m2 {} i 0]
    (if (< i 100)
      (recur (assoc m1 i i) (assoc m2 (str "foo" i) i) (inc i))
      (do (is (= m1 (into cljs.core.PersistentHashMap.EMPTY
                      (map vector (range 100) (range 100)))))
          (is (= m2 (into cljs.core.PersistentHashMap.EMPTY
                      (map vector
                        (map (partial str "foo") (range 100))
                        (range 100)))))
          (is (= (count m1) 100))
          (is (= (count m2) 100)))))
  )

(deftest test-461
  ;; CLJS-461: automatic map conversions
  (loop [i 0 m (with-meta {} {:foo :bar}) result []]
    (if (<= i (+ cljs.core.ObjMap.HASHMAP_THRESHOLD 2))
      (recur (inc i) (assoc m (str i) i) (conj result (meta m)))
      (let [n (inc (+ cljs.core.ObjMap.HASHMAP_THRESHOLD 2))
            expected (repeat n {:foo :bar})]
        (is (= result expected)))))
  (loop [i 0 m (with-meta {-1 :quux} {:foo :bar}) result []]
    (if (<= i (+ cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD 2))
      (recur (inc i) (assoc m i i) (conj result (meta m)))
      (let [n (inc (+ cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD 2))
            expected (repeat n {:foo :bar})]
        (is (= result expected))))))

(deftest test-transient-hash-set
  ;; TransientHashSet
  (loop [s (transient #{})
         i 0]
    (if (< i 100)
      (recur (conj! s i) (inc i))
      (loop [s s i 0]
        (if (< i 100)
          (if (zero? (mod i 3))
            (recur (disj! s i) (inc i))
            (recur s (inc i)))
          (let [s (persistent! s)]
            (is (= s (loop [s #{} xs (remove #(zero? (mod % 3)) (range 100))]
                       (if-let [x (first xs)]
                         (recur (conj s x) (next xs))
                         s))))
            (is (= s (set (remove #(zero? (mod % 3)) (range 100))))))))))
  )

(deftest test-cljs-1189
  (testing "array-map should always return array maps"
    (let [am (apply array-map (range 100))]
      (is (== (count am) 50))
      (is (instance? PersistentArrayMap am)))))

(deftest test-cljs-1199
  (testing "array-map should skip dropped elements of IndexedSeq"
    (is (= {:a 1} (apply array-map (drop 1 [0 :a 1]))))))

(deftest test-cljs-1212
  (is (= (set {:a 0 :b 0 :c 0 :d 0 :e 0 :f 0 :g 0 :h 0 :i 0})
        #{[:a 0] [:b 0] [:c 0] [:d 0] [:e 0] [:f 0] [:g 0] [:h 0] [:i 0]})))

(deftest test-count-hash-set
  (is
    (== (count (hash-set [1 4] [2 4] [3 4] [0 3] [1 3] [2 3] [3 3]
                 [0 2] [1 2] [2 2] [3 2] [4 2] [0 1] [1 1]
                 [2 1] [3 1] [1 0] [2 0] [3 0]))
      (count (list [1 4] [2 4] [3 4] [0 3] [1 3] [2 3] [3 3]
               [0 2] [1 2] [2 2] [3 2] [4 2] [0 1] [1 1]
               [2 1] [3 1] [1 0] [2 0] [3 0])))))

(deftest test-734
  (testing "Testing CLJS-734, transient operations"
    (is (= (-> (transient []) (conj! 1 2) persistent!) [1 2]))
    (is (= (-> (transient #{1 2 3}) (disj! 1 2) persistent!) #{3}))
    (is (= (-> (transient {}) (assoc! :a 1 :b 2) persistent!) {:a 1 :b 2}))
    (is (= (-> (transient {:a 1 :b 2 :c 3}) (dissoc! :a :b) persistent!) {:c 3}))))

(deftest test-vec
  (let [v (vec #js [1 2 3 4])]
    (is (= (count v) 4))
    (is (= (first v) 1))
    (is (= (last v) 4))
    (is (= v [1 2 3 4]))))

(deftest test-phm-from-array
  (let [m (.fromArray PersistentHashMap #js [1 2 3 4] true)]
    (is (= (count m) 2))
    (is (contains? m 1))
    (is (contains? m 3))
    (is (= (get m 1) 2))
    (is (= (get m 3) 4))
    (is (= m {1 2 3 4}))))

(deftest test-cljs-1809
  (is (= (into) []))
  (is (= (into [1 2]) [1 2])))

(deftest test-cljs-1951
  (is (= () (interleave)))
  (is (= '(1 2 3) (interleave [1 2 3]))))

(deftest test-cljs-1497
  (testing "PersistentArrayMap"
    (let [metadata {:a 1}
          k [1 2 3]
          v 1
          map (array-map (with-meta k metadata) v)
          [k' v'] (find map k)]
      (is (= k k'))
      (is (= v v'))
      (is (= metadata (meta k')))))
  (testing "PersistentHashMap"
    (let [metadata {:a 1}
          k [1 2 3]
          v 1
          map (hash-map (with-meta k metadata) v)
          [k' v'] (find map k)]
      (is (= k k'))
      (is (= v v'))
      (is (= metadata (meta k'))))
    (let [map (hash-map nil :foo)]
      (is (= (find map nil) [nil :foo])))
    (let [metadata {:a 1}
          k [1 2 3]
          v 1
          map (hash-map (with-meta k metadata) v nil 2)
          [k' v'] (find map k)]
      (is (= k k'))
      (is (= v v'))
      (is (= metadata (meta k')))))
  (testing "PersistentTreeMap"
    (let [metadata {:a 1}
          k [1 2 3]
          v 1
          map (sorted-map (with-meta k metadata) v)
          [k' v'] (find map k)]
      (is (= k k'))
      (is (= v v'))
      (is (= metadata (meta k'))))
    (let [map (sorted-map nil :foo)]
      (is (= (find map nil) [nil :foo])))))

(deftype CustomVectorThing [v]
  ;; Subvec expects its argument to implement IVector.
  ;; Note, that this method is never actually called.
  IVector
  (-assoc-n [coll i val] nil)

  IIndexed
  (-nth [coll i] (nth v i))
  (-nth [coll i not-found] (nth v i not-found))

  ICounted
  (-count [coll] (count v)))

(deftest test-cljs-2128
  (testing "Subvec iteration"
    (testing "Subvec over PersistentVector uses RangedIterator"
      (is (instance? RangedIterator (-iterator (subvec [0 1 2 3] 1 3)))))
    (testing "Subvec over other vectors uses naive SeqIter"
      (is (instance? SeqIter (-iterator (subvec (->CustomVectorThing [0 1 2 3]) 1 3))))))
  (testing "Subvec reduce"
    (testing "Subvec over PersistentVector reduces as expected"
      (is (= [1 2] (reduce conj [] (subvec [0 1 2 3] 1 3)))))
    (testing "Subvec over other vectors reduces as expected"
      (is (= [1 2] (reduce conj [] (subvec (->CustomVectorThing [0 1 2 3]) 1 3)))))))

(deftest test-cljs-2145
  (testing "PersistentHashMap -find implementation"
    (is (= (find (hash-map) :a) nil))
    (is (= (find (hash-map :a 1) :a) [:a 1]))
    (is (= (find (hash-map :a false) :a) [:a false]))
    (is (= (find (zipmap (range 1000) (repeat :foo)) 999) [999 :foo]))
    (is (= (find (zipmap (range 1000) (repeat :foo)) 1000) nil))))