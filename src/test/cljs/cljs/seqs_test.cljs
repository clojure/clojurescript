;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.seqs-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing are is]]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer-macros [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]
            [clojure.test.check.random :as random]
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
        (is (= {:b :c} (meta e-queue)))))
    (testing "non-emptyable"
      (is (nil? (empty 1)))
      (is (nil? (empty "abc")))
      (is (nil? (empty #js [1 2 3]))))))

(deftest test-empty?
  (are [x] (empty? x)
       nil
       ()
       (lazy-seq nil)    ; => ()
       []
       {}
       #{}
       ""
       (into-array [])
       (transient [])
       (transient #{})
       (transient {}))

  (are [x] (not (empty? x))
       '(1 2)
       (lazy-seq [1 2])
       [1 2]
       {:a 1 :b 2}
       #{1 2}
       "abc"
       (into-array [1 2])
       (transient [1])
       (transient #{1})
       (transient {1 2})))

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

(deftest test-cljs-3230
  (testing "sequence ops on ES6 collections"
    (let [s (js/Set.)]
      (is (= () (rest s)))
      (is (nil? (next s)))
      (is (empty? s)))))

(deftest test-js-iterable?
  (testing "test that js-iterable? works on ES6 collections and normal values"
    (is (true? (js-iterable? (js/Set.))))
    (is (false? (js-iterable? 1)))
    (is (false? (js-iterable? nil)))))

(deftest test-iteration-opts
  (let [genstep (fn [steps]
                  (fn [k] (swap! steps inc) (inc k)))
        test (fn [expect & iteropts]
               (is (= expect
                     (let [nsteps (atom 0)
                           iter (apply iteration (genstep nsteps) iteropts)
                           ret (doall (seq iter))]
                       {:ret ret :steps @nsteps})
                     (let [nsteps (atom 0)
                           iter (apply iteration (genstep nsteps) iteropts)
                           ret (into [] iter)]
                       {:ret ret :steps @nsteps}))))]
    (test {:ret [1 2 3 4]
           :steps 5}
      :initk 0 :somef #(< % 5))
    (test {:ret [1 2 3 4 5]
           :steps 5}
      :initk 0 :kf (fn [ret] (when (< ret 5) ret)))
    (test {:ret ["1"]
           :steps 2}
      :initk 0 :somef #(< % 2) :vf str))

  ;; kf does not stop on false
  (let [iter #(iteration (fn [k]
                           (if (boolean? k)
                             [10 :boolean]
                             [k k]))
                :vf second
                :kf (fn [[k v]]
                      (cond
                        (= k 3) false
                        (< k 14) (inc k)))
                :initk 0)]
    (is (= [0 1 2 3 :boolean 11 12 13 14]
          (into [] (iter))
          (seq (iter))))))

(deftest test-iteration
  ;; equivalence to es6-iterator-seq
  (let [arr #js [1 nil 3 true false 4 6 nil 7]]
    (is (= (let [iter (es6-iterator arr)]
             (vec (iteration (fn [_] (.next iter))
                    :somef #(not (.-done %))
                    :vf #(.-value %))))
          (let [iter (es6-iterator arr)]
            (vec (es6-iterator-seq iter))))))

  ;; paginated API
  (let [items 12 pgsize 5
        src (vec (repeatedly items #(random-uuid)))
        api (fn [tok]
              (let [tok (or tok 0)]
                (when (< tok items)
                  {:tok (+ tok pgsize)
                   :ret (subvec src tok (min (+ tok pgsize) items))})))]
    (is (= src
          (mapcat identity (iteration api :kf :tok :vf :ret))
          (into [] cat (iteration api :kf :tok :vf :ret)))))

  (let [src [:a :b :c :d :e]
        api (fn [k]
              (let [k (or k 0)]
                (if (< k (count src))
                  {:item (nth src k)
                   :k (inc k)})))]
    (is (= [:a :b :c]
          (vec (iteration api
                 :somef (comp #{:a :b :c} :item)
                 :kf :k
                 :vf :item))
          (vec (iteration api
                 :kf #(some-> % :k #{0 1 2})
                 :vf :item))))))

(defn- make-rng [seed]
  (atom (random/make-random seed)))

(defn- next-long [rng]
  (let [[r1 r2] (random/split @rng)]
    (reset! rng r2)
    (long (random/rand-long r1))))

(deftest test-take
  (are [x y] (= x y)
    (take 1 [1 2 3 4 5]) '(1)
    (take 3 [1 2 3 4 5]) '(1 2 3)
    (take 5 [1 2 3 4 5]) '(1 2 3 4 5)
    (take 9 [1 2 3 4 5]) '(1 2 3 4 5)

    (take 0 [1 2 3 4 5]) ()
    (take -1 [1 2 3 4 5]) ()
    (take -2 [1 2 3 4 5]) ()

    (take 0.25 [1 2 3 4 5]) '(1)))


(deftest test-drop
  (are [x y] (= x y)
    (drop 1 [1 2 3 4 5]) '(2 3 4 5)
    (drop 3 [1 2 3 4 5]) '(4 5)
    (drop 5 [1 2 3 4 5]) ()
    (drop 9 [1 2 3 4 5]) ()

    (drop 0 [1 2 3 4 5]) '(1 2 3 4 5)
    (drop -1 [1 2 3 4 5]) '(1 2 3 4 5)
    (drop -2 [1 2 3 4 5]) '(1 2 3 4 5)

    (drop 0.25 [1 2 3 4 5]) '(2 3 4 5) )

  (are [coll] (= (drop 4 coll) (drop -2 (drop 4 coll)))
    [0 1 2 3 4 5]
    (seq [0 1 2 3 4 5])
    (range 6)
    (repeat 6 :x)))

(deftest test-nthrest
  (are [x y] (= x y)
    (nthrest [1 2 3 4 5] 1) '(2 3 4 5)
    (nthrest [1 2 3 4 5] 3) '(4 5)
    (nthrest [1 2 3 4 5] 5) ()
    (nthrest [1 2 3 4 5] 9) ()

    (nthrest [1 2 3 4 5] 0) '(1 2 3 4 5)
    (nthrest [1 2 3 4 5] -1) '(1 2 3 4 5)
    (nthrest [1 2 3 4 5] -2) '(1 2 3 4 5)

    (nthrest [1 2 3 4 5] 0.25) '(2 3 4 5)
    (nthrest [1 2 3 4 5] 1.2) '(3 4 5))

  ;; (nthrest coll 0) should return coll
  (are [coll] (let [r (nthrest coll 0)] (and (= coll r) (= (type coll) (type r))))
    [1 2 3]
    (seq [1 2 3])
    (range 10)
    (repeat 10 :x)
    (seq "abc")))

(deftest test-nthnext
  (are [x y] (= x y)
    (nthnext [1 2 3 4 5] 1) '(2 3 4 5)
    (nthnext [1 2 3 4 5] 3) '(4 5)
    (nthnext [1 2 3 4 5] 5) nil
    (nthnext [1 2 3 4 5] 9) nil

    (nthnext [1 2 3 4 5] 0) '(1 2 3 4 5)
    (nthnext [1 2 3 4 5] -1) '(1 2 3 4 5)
    (nthnext [1 2 3 4 5] -2) '(1 2 3 4 5)

    (nthnext [1 2 3 4 5] 0.25) '(2 3 4 5)
    (nthnext [1 2 3 4 5] 1.2) '(3 4 5) ))

(deftest test-partitionv-all
  (is (= (partitionv-all 4 [1 2 3 4 5 6 7 8 9])
        [[1 2 3 4] [5 6 7 8] [9]]))
  (is (= (partitionv-all 4 2 [1 2 3 4 5 6 7 8 9])
        [[1 2 3 4] [3 4 5 6] [5 6 7 8] [7 8 9] [9]])))

(deftest test-partition
  (are [x y] (= x y)
    (partition 2 [1 2 3]) '((1 2))
    (partition 2 [1 2 3 4]) '((1 2) (3 4))
    (partition 2 []) ()

    (partition 2 3 [1 2 3 4 5 6 7]) '((1 2) (4 5))
    (partition 2 3 [1 2 3 4 5 6 7 8]) '((1 2) (4 5) (7 8))
    (partition 2 3 []) ()

    (partition 1 []) ()
    (partition 1 [1 2 3]) '((1) (2) (3))

    (partition 5 [1 2 3]) ()

    (partition 4 4 [0 0 0] (range 10)) '((0 1 2 3) (4 5 6 7) (8 9 0 0))

    (partition -1 [1 2 3]) ()
    (partition -2 [1 2 3]) ())

  ;; reduce
  (is (= [1 2 4 8 16] (map #(reduce * (repeat % 2)) (range 5))))
  (is (= [3 6 12 24 48] (map #(reduce * 3 (repeat % 2)) (range 5))))

  ;; equality and hashing
  (is (= (repeat 5 :x) (repeat 5 :x)))
  (is (= (repeat 5 :x) '(:x :x :x :x :x)))
  (is (= (hash (repeat 5 :x)) (hash '(:x :x :x :x :x))))
  (is (= (assoc (array-map (repeat 1 :x) :y) '(:x) :z) {'(:x) :z}))
  (is (= (assoc (hash-map (repeat 1 :x) :y) '(:x) :z) {'(:x) :z})))

(deftest test-partitionv
  (are [x y] (= x y)
    (partitionv 2 [1 2 3]) '((1 2))
    (partitionv 2 [1 2 3 4]) '((1 2) (3 4))
    (partitionv 2 []) ()

    (partitionv 2 3 [1 2 3 4 5 6 7]) '((1 2) (4 5))
    (partitionv 2 3 [1 2 3 4 5 6 7 8]) '((1 2) (4 5) (7 8))
    (partitionv 2 3 []) ()

    (partitionv 1 []) ()
    (partitionv 1 [1 2 3]) '((1) (2) (3))

    (partitionv 5 [1 2 3]) ()

    (partitionv -1 [1 2 3]) ()
    (partitionv -2 [1 2 3]) ()))

(deftest test-reduce-on-coll-seqs
  ;; reduce on seq of coll, both with and without an init
  (are [coll expected expected-init]
    (and
      (= expected-init (reduce conj [:init] (seq coll)))
      (= expected (reduce conj (seq coll))))
    ;; (seq [ ... ])
    []      []    [:init]
    [1]     1     [:init 1]
    [[1] 2] [1 2] [:init [1] 2]

    ;; (seq { ... })
    {}        []          [:init]
    {1 1}     [1 1]       [:init [1 1]]
    {1 1 2 2} [1 1 [2 2]] [:init [1 1] [2 2]]

    ;; (seq (hash-map ... ))
    (hash-map)         []          [:init]
    (hash-map 1 1)     [1 1]       [:init [1 1]]
    (hash-map 1 1 2 2) [1 1 [2 2]] [:init [1 1] [2 2]]

    ;; (seq (sorted-map ... ))
    (sorted-map)         []          [:init]
    (sorted-map 1 1)     [1 1]       [:init [1 1]]
    (sorted-map 1 1 2 2) [1 1 [2 2]] [:init [1 1] [2 2]])

  (are [coll expected expected-init]
    (and
      (= expected-init (reduce + 100 (seq coll)))
      (= expected (reduce + (seq coll))))

    ;; (seq (range ...))
    (range 0)   0 100
    (range 1 2) 1 101
    (range 1 3) 3 103))

(defspec iteration-seq-equals-reduce 1000
  (prop/for-all [initk gen/small-integer
                 seed gen/small-integer]
    (let [src (fn []
                (let [rng (make-rng seed)]
                  (iteration #(unchecked-add % (next-long rng))
                    :somef (complement #(zero? (mod % 1000)))
                    :vf str
                    :initk initk)))]
      (= (into [] (src))
        (into [] (seq (src)))))))

(deftest cljs-3419-seq-js-iterable
  (let [js-set (js/Set. #js [1 2 3 4])
        js-map (js/Map. #js [#js [1 2] #js [3 4]])]
    (is (seqable? js-set))
    (is (seqable? js-map))))
