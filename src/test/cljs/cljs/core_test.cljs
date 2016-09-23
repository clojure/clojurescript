;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is]]
            [clojure.string :as s]
            [clojure.set :as set]))

(deftest test-hash-null
  (is (zero? (hash (aget (js-obj) "foo")))))

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
    (is (= (find [1 2 3] 0) [0 1])))
)

(deftest test-metadata
  (testing "Testing metadata"
    (is (= {"x" "y"} (meta ^{"x" "y"} [])))
    ))

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
          (is (nil? (rseq sv2))))))
    ))

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

(deftest test-fn-with-metadata
  (let [f (fn [x] (* x 2))
        m {:foo "bar"}
        mf (with-meta f m)]
    (testing "Testing functions with metadata"
      (is (nil? (meta f)))
      (is (fn? mf))
      (is (= 4 (mf 2)))
      (is (= 4 (apply mf [2])))
      (is (= (meta mf) m)))))

(deftest test-atoms-and-volatile
  (let [a (atom 0)]
    (testing "Testing basic atom operations"
      (is (= 0 (deref a)))
      (is (= 1 (swap! a inc)))
      (is (= false (compare-and-set! a 0 42)))
      (is (= true (compare-and-set! a 1 7)))
      (is (nil? (meta a)))
      (is (nil? (get-validator a)))))
  (let [a (atom 0)]
    (testing "Testing swap!"
      (is (= 1 (swap! a + 1)))
      (is (= 4 (swap! a + 1 2)))
      (is (= 10 (swap! a + 1 2 3)))
      (is (= 20 (swap! a + 1 2 3 4)))))
  (let [a (atom [1] :validator coll? :meta {:a 1})]
    (testing "Testing atom validators"
      (is (= coll? (get-validator a)))
      (is (thrown? js/Error (reset! a 1)))
      (is (= {:a 1} (meta a)))
      (alter-meta! a assoc :b 2)
      (is (= {:a 1 :b 2} (meta a)))))
  (let [v (volatile! 1)]
    (testing "Testing volatile"
      (is (volatile? v))
      (is (not (volatile? (atom 1))))
      (is (= 2 (vreset! v 2)))
      (is (= 3 (vswap! v inc)))
      (is (= 3 @v)))))

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

(deftest test-contains?
  (testing "Testing contains?"
    (is (contains? {:a 1 :b 2} :a))
    (is (not (contains? {:a 1 :b 2} :z)))
    (is (contains? [5 6 7] 1))
    (is (contains? [5 6 7] 2))
    (is (not (contains? [5 6 7] 3)))
    (is (contains? (to-array [5 6 7]) 1))
    (is (contains? (to-array [5 6 7]) 2))
    (is (not (contains? (to-array [5 6 7]) 3)))
    (is (not (contains? nil 42)))
    (is (contains? "f" 0))
    (is (not (contains? "f" 55)))))

(deftest test-run!
  (testing "Testing run!"
    (let [a (atom 0)]
      (run! (fn [n]
              (swap! a + n))
        (range 5))
      (is (= 10 @a)))
    (is (nil? (run! identity [1])))
    (is (nil? (run! reduced (range))))))

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

(deftest test-in-operations
  (testing "Testing update-in"
    (is (= {:foo {:bar {:baz 1}}}
          (update-in {:foo {:bar {:baz 0}}} [:foo :bar :baz] inc)))
    (is (= {:foo 1 :bar 2 :baz 10}
          (update-in {:foo 1 :bar 2 :baz 3} [:baz] + 7)))
    (is (= [{:foo 1, :bar 2} {:foo 1, :bar 3}]
          (update-in [{:foo 1 :bar 2}, {:foo 1 :bar 2}] [1 :bar] inc)))
    (is (= [{:foo {:bar 2}} {:foo {:bar 3}}]
          (update-in [{:foo {:bar 2}}, {:foo {:bar 2}}] [1 :foo :bar] inc))))
  (testing "Testing assoc-in"
    (is (= {:foo {:bar {:baz 100}}}
          (assoc-in {:foo {:bar {:baz 0}}} [:foo :bar :baz] 100)))
    (is (= {:foo 1 :bar 2 :baz 100}
          (assoc-in {:foo 1 :bar 2 :baz 3} [:baz] 100)))
    (is (= [{:foo [{:bar 2} {:baz 3}]} {:foo [{:bar 2} {:baz 100}]}]
          (assoc-in [{:foo [{:bar 2} {:baz 3}]}, {:foo [{:bar 2} {:baz 3}]}]
            [1 :foo 1 :baz] 100)))
    (is (= [{:foo 1, :bar 2} {:foo 1, :bar 100}]
          (assoc-in [{:foo 1 :bar 2}, {:foo 1 :bar 2}] [1 :bar] 100))))
  (testing "Testing get-in"
    (is (= 1 (get-in {:foo 1 :bar 2} [:foo])))
    (is (= 2 (get-in {:foo {:bar 2}} [:foo :bar])))
    (is (= 1 (get-in [{:foo 1}, {:foo 2}] [0 :foo])))
    (is (= 4 (get-in [{:foo 1 :bar [{:baz 1}, {:buzz 2}]}, {:foo 3 :bar [{:baz 3}, {:buzz 4}]}]
               [1 :bar 1 :buzz]))))
  )

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

(deftest test-js-clj-conversions
  (testing "Testing JS / CLJS data conversions"
    (testing "js->clj"
      (is (= {"a" 1, "b" 2} (js->clj (js* "{\"a\":1,\"b\":2}"))))
      (is (= {"a" nil} (js->clj (js* "{\"a\":null}"))))
      (is (= {} (js->clj (js* "{}"))))
      (is (= {"a" true, "b" false} (js->clj (js* "{\"a\":true,\"b\":false}"))))
      (is (= {:a 1, :b 2} (js->clj (js* "{\"a\":1,\"b\":2}") :keywordize-keys true)))
      (is (= [[{:a 1, :b 2} {:a 1, :b 2}]]
                (js->clj (js* "[[{\"a\":1,\"b\":2}, {\"a\":1,\"b\":2}]]") :keywordize-keys true)))
      (is (= [[{:a 1, :b 2} {:a 1, :b 2}]]
                (js->clj [[{:a 1, :b 2} {:a 1, :b 2}]])))
      (is (= (js->clj nil) nil)))
    (testing "clj->js"
      (is (= (clj->js 'a) "a"))
      (is (= (clj->js :a) "a"))
      (is (= (clj->js "a") "a"))
      (is (= (clj->js 1) 1))
      (is (= (clj->js nil) (js* "null")))
      (is (= (clj->js true) (js* "true")))
      (is (goog/isArray (clj->js [])))
      (is (goog/isArray (clj->js #{})))
      (is (goog/isArray (clj->js '())))
      (is (goog/isObject (clj->js {})))
      (is (= (aget (clj->js {:a 1}) "a") 1))
      (is (= (-> (clj->js {:a {:b {{:k :ey} :d}}})
                   (aget "a")
                   (aget "b")
                   (aget "{:k :ey}"))
                "d")))))

(deftest test-delay
  (let [a (atom 0)
        d (delay (swap! a inc))]
    (testing "Testing delay"
      (is (false? (realized? d)))
      (is (zero? @a)) ;; delay hasn't triggered yet
      (is (= 1 @d)) ;; trigger it
      (is (= 1 @a)) ;; make sure side effect has happened
      (is (true? (realized? d)))
      (is (= 1 @d)) ;; body doesn't happen again
      (is (= 1 @a)) ;; atom hasn't changed either
      (is (= (force d) @d))
      (is (= 1 (force 1))))) ;; you can safely force non-delays
  )

(deftest test-hierarchy
  (testing "Testing hierarchy operations"
    (derive ::rect ::shape)
    (derive ::square ::rect)
    (is (= #{:cljs.core-test/shape} (parents ::rect)))
    (is (= #{:cljs.core-test/rect :cljs.core-test/shape} (ancestors ::square)))
    (is (= #{:cljs.core-test/rect :cljs.core-test/square} (descendants ::shape)))
    (is (true? (isa? 42 42)))
    (is (true? (isa? ::square ::shape)))
    (derive cljs.core.ObjMap ::collection)
    (derive cljs.core.PersistentHashSet ::collection)
    (is (true? (isa? cljs.core.ObjMap ::collection)))
    (is (true? (isa? cljs.core.PersistentHashSet ::collection)))
    (is (false? (isa? cljs.core.IndexedSeq ::collection)))
    ;; ?? (isa? String Object)
    (is (true? (isa? [::square ::rect] [::shape ::shape])))
    ;; ?? (ancestors java.util.ArrayList)
    ;; ?? isa? based dispatch tests
    ))

(defmulti bar (fn [x y] [x y]))
(defmethod bar [::rect ::shape] [x y] :rect-shape)
(defmethod bar [::shape ::rect] [x y] :shape-rect)

;;(bar ::rect ::rect)
;; -> java.lang.IllegalArgumentException:
;;  Multiple methods match dispatch value:
;;  [:cljs.core-test/rect :cljs.core-test/rect] -> [:cljs.core-test/rect :cljs.core-test/shape]
;;  and [:cljs.core-test/shape :cljs.core-test/rect],
;;  and neither is preferred

(deftest test-multimethods-1
  (testing "Testing basic multimethod usage"
    (is (zero? (count (prefers bar))))
    (prefer-method bar [::rect ::shape] [::shape ::rect])
    (is (= 1 (count (prefers bar))))
    (is (= :rect-shape (bar ::rect ::rect)))
    (is (= :rect-shape (apply (-get-method bar [::rect ::shape]) [::rect ::shape])))
    ))

(defmulti nested-dispatch (fn [m] (-> m :a :b)))
(defmethod nested-dispatch :c [m] :nested-a)
(defmethod nested-dispatch :default [m] :nested-default)

(defmulti nested-dispatch2 ffirst)
(defmethod nested-dispatch2 :a [m] :nested-a)
(defmethod nested-dispatch2 :default [m] :nested-default)

(defmulti foo1 (fn [& args] (first args)))
(defmethod foo1 :a [& args] :a-return)
(defmethod foo1 :default [& args] :default-return)

(defmulti area :Shape)
(defn rect [wd ht] {:Shape :Rect :wd wd :ht ht})
(defn circle [radius] {:Shape :Circle :radius radius})
(defmethod area :Rect [r]
    (* (:wd r) (:ht r)))
(defmethod area :Circle [c]
    (*  Math/PI (* (:radius c) (:radius c))))
(defmethod area :default [x] :oops)
(defmulti foo2 (fn []))
(defmethod foo2 :default [] :foo)

(defmulti apply-multi-test (fn ([_] 0) ([_ _] 0) ([_ _ _] 0)))
(defmethod apply-multi-test 0
  ([x] :one)
  ([x y] :two)
  ([x y & r] [:three r]))

;; CLJS-469, helpful exception message on bad dispatch
(defmulti no-dispatch-value :test)

;; custom hierarchy
(def my-map-hierarchy
  (atom (-> (make-hierarchy)
          (derive (type (obj-map)) ::map)
          (derive (type (array-map)) ::map)
          (derive (type (hash-map)) ::map)
          (derive (type (sorted-map)) ::map))))

(defmulti my-map? type :hierarchy my-map-hierarchy)
(defmethod my-map? ::map [_] true)
(defmethod my-map? :default [_] false)

(defmulti foo2' identity)
(defmethod foo2' 0 [x] x)

(deftest test-multimethods-2
  (let [r (rect 4 13)
        c (circle 12)]
    (testing "Testing multimethod edge cases"
      (is (= :nested-a (nested-dispatch {:a {:b :c}})))
      (is (= :nested-a (nested-dispatch2 [[:a :b]])))
      (is (= :a-return (foo1 :a)))
      (is (= :default-return (foo1 1)))
      (is (= 52 (area r)))
      (is (= :oops (area {})))
      ;; CLJS-863
      (is (= :foo (foo2)))
      ;; remove method tests
      (is (= 2 (count (methods bar))))
      (remove-method bar [::rect ::shape])
      (is (= 1 (count (methods bar))))
      (remove-all-methods bar)
      (is (zero? (count (methods bar))))
      (is (= [:three '(2)] (apply apply-multi-test [0 1 2])))
      (is (try
            (no-dispatch-value {:test :test})
            (catch js/Error e
              (not= -1 (.indexOf (.-message e) "cljs.core-test/no-dispatch-value")))))
      (doseq [m [(obj-map) (array-map) (hash-map) (sorted-map)]]
        (is (my-map? m)))
      (doseq [not-m [[] 1 "asdf" :foo]]
        (is (not (my-map? not-m))))
      ;; multimethod hashing
      (is (= foo2' (ffirst {foo2' 1})))
)))

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

(deftest test-transducers
  (testing "Testing transducers"
    (is (= (sequence (map inc) (array 1 2 3)) '(2 3 4)))
    (is (= (apply str (sequence (map #(.toUpperCase %)) "foo")) "FOO"))
    (is (== (hash [1 2 3]) (hash (sequence (map inc) (range 3)))))
    (is (= [1 2 3] (sequence (map inc) (range 3))))
    (is (= (sequence (map inc) (range 3)) [1 2 3]))
    (is (= (sequence (remove even?) (range 10)) '(1 3 5 7 9)))
    (is (= (sequence (take 5) (range 10))
          '(0 1 2 3 4)))
    (is (= (sequence (take-while #(< % 5)) (range 10))
          '(0 1 2 3 4)))
    (is (= (sequence (drop 5) (range 10))
          '(5 6 7 8 9)))
    (is (= (sequence (drop-while #(< % 5)) (range 10))
          '(5 6 7 8 9)))
    (is (= (sequence (take-nth 2) (range 10))
          '(0 2 4 6 8)))
    (is (= (sequence (replace {:foo :bar}) '(:foo 1 :foo 2))
          '(:bar 1 :bar 2)))
    (let [ret (into [] (map inc) (range 3))]
      (is (and (vector? ret) (= ret '(1 2 3)))))
    (let [ret (into [] (filter even?) (range 10))]
      (is (and (vector? ret) (= ret '(0 2 4 6 8)))))
    (is (= (map inc (sequence (map inc) (range 3)))
          '(2 3 4)))
    (is (= (sequence (dedupe) [1 1 2 2 3 3])
          '(1 2 3)))
    (is (= (mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])
          (range 10)))
    (is (= (sequence (mapcat reverse) [[3 2 1 0] [6 5 4] [9 8 7]])
          (range 10)))
    (is (= (seq (eduction (map inc) [1 2 3])) '(2 3 4)))
    (is (= (seq (eduction (map inc) (map inc) [1 2 3])) '(3 4 5)))
    (is (= (sequence (partition-by #{:split}) [1 2 3 :split 4 5 6])
          '([1 2 3] [:split] [4 5 6])))
    (is (= (sequence (partition-all 3) '(1 2 3 4 5))
          '([1 2 3] [4 5])))
    (is (= (sequence (keep identity) [1 nil 2 nil 3])
          '(1 2 3)))
    (is (= (keep-indexed identity [:foo nil :bar nil :baz])
          (sequence (keep-indexed identity) [:foo nil :bar nil :baz])))
    (let [xform (comp (map inc)
                  (filter even?)
                  (dedupe)
                  (mapcat range)
                  (partition-all 3)
                  (partition-by #(< (apply + %) 7))
                  (mapcat flatten)
                  (random-sample 1.0)
                  (take-nth 1)
                  (keep #(when (odd? %) (* % %)))
                  (keep-indexed #(when (even? %1) (* %1 %2)))
                  (replace {2 "two" 6 "six" 18 "eighteen"})
                  (take 11)
                  (take-while #(not= 300 %))
                  (drop 1)
                  (drop-while string?)
                  (remove string?))
          data (vec (interleave (range 18) (range 20)))]
      (is (= (sequence xform data) '(36 200 10))))
    (let [xf (map #(+ %1 %2))]
      (is (= (sequence xf [0 0] [1 2]) [1 2])))
    (is (= (-> (sequence (map inc) [1 2 3])
             (with-meta {:a 1})
             meta) {:a 1}))))

(deftest test-obj-equiv
  (testing "Object equiv method"
    (is (.equiv :foo :foo))
    (is (.equiv 'foo 'foo))
    (is (.equiv {:foo 1 :bar 2} {:foo 1 :bar 2}))
    (is (.equiv [1 2 3] [1 2 3]))
    (is (.equiv '(1 2 3) '(1 2 3)))
    (is (.equiv (map inc [1 2 3]) (map inc [1 2 3])))
    (is (.equiv #{:cat :dog :bird} #{:cat :dog :bird}))
    ))

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

(deftest test-es6-interfaces
  (testing "ES6 collection interfaces"
    (let [iter (es6-iterator [1 2 3])]
      (testing "basic iterations"
        (is (= (.-value (.next iter)) 1))
        (is (= (.-value (.next iter)) 2))
        (is (= (.-value (.next iter)) 3))
        (is (.-done (.next iter)))))
    (is (.has {:foo "bar"} :foo))
    (is (= (.get {:foo "bar"} :foo) "bar"))
    (is (= (.get {:foo "bar"} :bar :default) :default))
    (let [iter (.keys {:foo "bar" :baz "woz"})]
      (testing "map key iteration"
        (is (#{:foo :baz} (.-value (.next iter))))
        (is (#{:foo :baz} (.-value (.next iter))))
        (is (.-done (.next iter)))))
    (let [eiter (.entries {:foo "bar" :baz "woz"})]
      (testing "map entry iteration"
        (let [entries #{(seq #js [:foo "bar"]) (seq #js [:baz "woz"])}]
          (is (entries (seq (.-value (.next eiter)))))
          (is (entries (seq (.-value (.next eiter))))))
        (is (.-done (.next eiter)))))
    (let [iter (.values {:foo "bar" :baz "woz"})]
      (testing "map value iteration"
        (is (#{"bar" "woz"} (.-value (.next iter))))
        (is (#{"bar" "woz"} (.-value (.next iter))))
        (is (.-done (.next iter)))))
    (is (.has #{:cat :bird :dog} :bird))
    (let [iter (.keys #{:cat :bird :dog})]
      (testing "set key iteration"
        (is (#{:cat :bird :dog} (.-value (.next iter))))
        (is (#{:cat :bird :dog} (.-value (.next iter))))
        (is (#{:cat :bird :dog} (.-value (.next iter))))
        (is (.-done (.next iter)))))
    (let [iter (.entries #{:cat :bird :dog})]
      (testing "set entry iteration"
        (is (#{[:cat :cat] [:bird :bird] [:dog :dog]} (seq (.-value (.next iter)))))
        (is (#{[:cat :cat] [:bird :bird] [:dog :dog]} (seq (.-value (.next iter)))))
        (is (#{[:cat :cat] [:bird :bird] [:dog :dog]} (seq (.-value (.next iter)))))
        (is (.-done (.next iter)))))
    (let [iter (.values #{:cat :bird :dog})]
      (testing "set value iteration"
        (is (#{:cat :bird :dog} (.-value (.next iter))))
        (is (#{:cat :bird :dog} (.-value (.next iter))))
        (is (#{:cat :bird :dog} (.-value (.next iter))))
        (is (.-done (.next iter)))))
))

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

(deftest test-reader-literals
  (testing "Testing reader literals"
    (is (= #queue [1] (into cljs.core.PersistentQueue.EMPTY [1])))
    (is (not= #queue [1 2] (into cljs.core.PersistentQueue.EMPTY [1])))
    (is (= #inst "2010-11-12T18:14:15.666-00:00"
          #inst "2010-11-12T13:14:15.666-05:00"))
    (is (= #uuid "550e8400-e29b-41d4-a716-446655440000"
          #uuid "550e8400-e29b-41d4-a716-446655440000"))
    (is (= 42
          (get {#uuid "550e8400-e29b-41d4-a716-446655440000" 42}
            #uuid "550e8400-e29b-41d4-a716-446655440000")))
    ))

(deftest test-uuid
  (testing "Testing UUID"
    (is (= (cljs.core/uuid "550e8400-e29b-41d4-a716-446655440000")
              (cljs.core/uuid "550e8400-e29b-41d4-a716-446655440000")))
    (is (not (identical? (cljs.core/uuid "550e8400-e29b-41d4-a716-446655440000")
                   (cljs.core/uuid "550e8400-e29b-41d4-a716-446655440000"))))
    (is (= 42 (get {(cljs.core/uuid "550e8400-e29b-41d4-a716-446655440000") 42}
                    (cljs.core/uuid "550e8400-e29b-41d4-a716-446655440000")
                    :not-at-all-found)))
    (is (= :not-at-all-found
              (get {(cljs.core/uuid "550e8400-e29b-41d4-a716-446655440000") 42}
                (cljs.core/uuid "666e8400-e29b-41d4-a716-446655440000")
                :not-at-all-found)))
    (is (= -1 (compare (cljs.core/uuid "550e8400-e29b-41d4-a716-446655440000")
                       (cljs.core/uuid "666e8400-e29b-41d4-a716-446655440000"))))
    (is (=  1 (compare (cljs.core/uuid "550e8400-e29b-41d4-a716-446655440000")
                       (cljs.core/uuid "550e8400-a29b-41d4-a716-446655440000"))))
    (is (=  0 (compare (cljs.core/uuid "550e8400-e29b-41d4-a716-446655440000")
                       (cljs.core/uuid "550e8400-e29b-41d4-a716-446655440000")))))
  (testing "UUID hashing"
    (let [id   "550e8400-e29b-41d4-a716-446655440000"
          uuid (cljs.core/uuid id)
          expected (hash id)]
      (is (= expected (hash uuid)))
      (is (= expected (.-__hash uuid))))))

;; =============================================================================
;; Tickets

(deftest test-383
  (testing "Testing CLJS-383"
    (let [f1 (fn f1 ([] 0) ([a] 1) ([a b] 2) ([a b c & more] 3))
          f2 (fn f2 ([x] :foo) ([x y & more] (apply f1 y more)))]
      (is (= 1 (f2 1 2))))
    (let [f (fn ([]) ([a & more] more))]
      (is (nil? (f :foo))))
    (is (nil? (array-seq (array 1) 1))))  )

(deftest test-513
  (testing "Testing CLJS-513"
    (let [sentinel (js-obj)]
      (is (identical? sentinel (try ([] 0) (catch js/Error _ sentinel)))))))

(defprotocol IFoo (foo [this]))

(deftest test-reify-meta
  (is (= (meta (with-meta (reify IFoo (foo [this] :foo)) {:foo :bar}))
            {:foo :bar})))

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

(let [x "original"]
  (defn original-closure-stmt [] x))

(deftest test-401-411
  (let [x "overwritten"]
    (is (= "original" (original-closure-stmt))))
  (is (= "original" (let [x "original"
                               oce (fn [] x)
                               x "overwritten"]
                           (oce)))))

(deftest test-letfn-shadowing
  (letfn [(x [] "original")
          (y [] (x))]
    (let [x (fn [] "overwritten")]
      (is (= "original" (y))))))

(deftest test-459
  (is (= (reduce-kv conj [] (sorted-map :foo 1 :bar 2))
        [:bar 2 :foo 1])))

(deftest test-kv-reduce
  (letfn [(kvr-test [data expect]
            (and
              (= :reduced
                (reduce-kv
                  (fn [_ _ _] (reduced :reduced))
                  [] data))
              (= (sort expect)
                (sort
                  (reduce-kv
                    (fn [r k v] (-> r (conj [k v])))
                    [] data)))))]
    (testing "Testing IKVReduce"
      (doseq [[data expect] [[(obj-map :k0 :v0 :k1 :v1) [[:k0 :v0] [:k1 :v1]]]
                             [(hash-map :k0 :v0 :k1 :v1) [[:k0 :v0] [:k1 :v1]]]
                             [(array-map :k0 :v0 :k1 :v1) [[:k0 :v0] [:k1 :v1]]]
                             [[:v0 :v1] [[0 :v0] [1 :v1]]]]]
        (is (kvr-test data expect)))
      (is (= {:init :val} (reduce-kv assoc {:init :val} nil))))))

(deftest test-data-conveying-exceptions
  (is (= {:foo 1}
             (try (throw (ex-info "asdf" {:foo 1}))
                  (catch ExceptionInfo e
                    (ex-data e)))))
  (is (instance? js/Error (ex-info "asdf" {:foo 1})))
  (is (= (pr-str (ex-info "abc" {:x 1})) "#error {:message \"abc\", :data {:x 1}}"))
  (is (= (pr-str (ex-info "abc" {:x 1} "def")) "#error {:message \"abc\", :data {:x 1}, :cause \"def\"}"))
  (is (= (.toString (ex-info "abc" {:x 1} "def")) "#error {:message \"abc\", :data {:x 1}, :cause \"def\"}"))
  (is (= (str (ex-info "abc" {:x 1} "def")) "#error {:message \"abc\", :data {:x 1}, :cause \"def\"}"))
  (is (not (instance? cljs.core.ExceptionInfo (js/Error.)))))

(deftest test-435
  (is (= (assoc {} 154618822656 1 261993005056 1)
            {154618822656 1 261993005056 1})))

(deftest test-458
  (is (= (get-in {:a {:b 1}} [:a :b :c] :nothing-there)
        :nothing-there)))

(deftest test-464
  (is (nil? (get-in {:foo {:bar 2}} [:foo :bar :baz]))))

(deftest test-symbol-meta
  (is (= (meta (with-meta 'foo {:tag 'int})) {:tag 'int}))
  (is (= (meta (quote ^{:bar true} foo)) {:bar true}))
  (is (= (meta (quote ^:bar foo)) {:bar true}))
  (is (= (meta (first '[^:bar x])) {:bar true})))

(deftest test-467
  (is (= (reduce-kv + 0 (apply hash-map (range 1000)))
        (reduce + (range 1000)))))

(deftest test-477
  (is (= [js/undefined 1 2] ((fn [& more] more) js/undefined 1 2)))
  (is (= [js/undefined 4 5] ((fn [a b & more] more) 1 2 js/undefined 4 5))))

(deftest test-493
  (is (nil? (get 42 :anything)))
  (is (= (get 42 :anything :not-found) :not-found))
  (is (nil? (first (map get [42] [:anything]))))
  (is (= (first (map get [42] [:anything] [:not-found])) :not-found)))

(deftest test-481
  (let [fs (atom [])]
    (doseq [x (range 4)
            :let [y (inc x)
                  f (fn [] y)]]
      (swap! fs conj f))
    (is (= (map #(%) @fs) '(1 2 3 4)))))

(def exists?-test-val 'foo)

(deftest test-495
  (testing "Testing CLJS-495, exists?"
    (is (false? (exists? js/jQuery)))
    (is (exists? exists?-test-val))))

(deftest test-496
  (is (= (char 65) \A))
  (is (= (char \A) \A)))

(deftype PositionalFactoryTest [x])

(deftest test-515
  (is (== 1 (.-x (->PositionalFactoryTest 1)))))

(deftest test-518
  (is (nil? (:test "test"))))

;; r1798 core fn protocol regression
(extend-type object
  ISeqable
  (-seq [coll]
    (map #(vector % (aget coll %)) (js-keys coll)))

  ILookup
  (-lookup
    ([coll k]
     (-lookup coll k nil))
    ([coll k not-found]
     (if-let [v (aget coll k)]
       v
       not-found))))

(deftest test-extend-to-object
  (is (= (seq (js-obj "foo" 1 "bar" 2)) '(["foo" 1] ["bar" 2])))
  (is (= (get (js-obj "foo" 1) "foo") 1))
  (is (= (get (js-obj "foo" 1) "bar" ::not-found) ::not-found))
  (is (= (reduce (fn [s [k v]] (+ s v)) 0 (js-obj "foo" 1 "bar" 2)) 3)))

(deftest test-541
  (letfn [(f! [x] (print \f) x)
          (g! [x] (print \g) x)]
    (is (= "ffgfg"
          (with-out-str
            (instance? Symbol (f! 'foo))
            (max (f! 5) (g! 10))
            (min (f! 5) (g! 10)))))))

(deftest test-582
  (is (= #{1 2} (set [1 2 2])))
  (is (= #{1 2} (hash-set 1 2 2)))
  (is (= #{1 2} (apply hash-set [1 2 2]))))

(deftest test-585
  (is (= (last (map identity (into [] (range 32)))) 31))
  (is (= (into #{} (range 32))
            (set (map identity (into [] (range 32)))))))

(def foo580)
(def foo580 {:a (fn []) :b (fn [] (foo580 :a))})

(deftest test-580
  (is (nil? (((:b foo580))))))

(deftest test-587
  (is (== (first (filter #(== % 9999) (range))) 9999)))

(deftest test-604
  (is (= () (concat nil [])))
  (is (= () (concat [] []))))

(deftest test-600
  (is (= "foobar" (apply str (concat "foo" "bar")))))

(deftest test-608
  (is (= '("") (re-seq #"\s*" ""))))

(deftype KeywordTest []
  ILookup
  (-lookup [o k] :nothing)
  (-lookup [o k not-found] not-found))

(deftest tset-638
  (is (= (:a (KeywordTest.)) :nothing)))

(deftest test-648
  (let [a (reify IHash (-hash [_] 42))
        b (reify IHash (-hash [_] 42))
        s (set (range 128))]
    (testing "Testing CLJS-648 (CLJ-1285)"
      (is (= (-> (conj s a b) transient (disj! a) persistent! (conj a))
             (-> (conj s a b) transient (disj! a) persistent! (conj a)))))))

(deftest test-660
  (testing "Testing CLJS-660, namespace handling"
    (is (= (-> 'a.b keyword ((juxt namespace name))) [nil "a.b"]))
    (is (= (-> 'a.b/c keyword ((juxt namespace name))) ["a.b" "c"]))
    (is (= (-> "a.b" keyword ((juxt namespace name))) [nil "a.b"]))
    (is (= (-> "a.b/c" keyword ((juxt namespace name))) ["a.b" "c"]))))

(deftest test-663
  (testing "Testing CLJS-663, invalid keywords"
    (is (= (keyword 123) nil))
    (is (= (keyword (js/Date.)) nil))))

(deftest test-647
  (let [keys #(vec (js-keys %))
        z "x"]
    (testing "Testing CLJS-647, js-keys"
      (assert (= ["x"]
                (keys (js-obj "x" "y"))
                (keys (js-obj (identity "x") "y"))
                (keys (js-obj z "y")))))))


(def some-x 1)
(def some-y 1)

(deftest test-583
  (is (= (count #{some-x some-y}) 1)))

(deftest test-584
  (is (= (count {some-x :foo some-y :bar}) 1)))

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

(deftest test-725
  (testing "Testing CLJS-725, drop"
    (is (= (apply vector (drop-while (partial = 1) [1 2 3])) [2 3]))
    (is (= (apply list (drop-while (partial = 1) [1 2 3])) '(2 3)))
    (is (= (set (drop 1 #js [1 2 3])) #{2 3}))))

(deftest test-724
  (is (nil? (first (rest (rest (rest (range 3))))))))

(deftest test-730
  (testing "Testing CLJS-730, object? predicate"
    (is (true? (object? #js {})))
    (is (false? (object? nil)))))

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

(deftest test-767
  (testing "Testing CLJS-767, invalid assoc"
    (doseq [n [nil "-1" "" "0" "1" false true (js-obj)]]
      (is (= :fail (try (assoc [1 2] n 4)
                        (catch js/Error e :fail))))
      (is (= :fail (try (assoc (subvec [1 2 3] 2) n 4)
                     (catch js/Error e :fail))))
      (is (= :fail (try (assoc (range 1 3) n 4)
                     (catch js/Error e :fail)))))))

(deftest test-768
  (testing "Testing CLJS-768, invalid assoc!"
    (doseq [n [nil "-1" "" "0" "1" false true (js-obj)]]
      (is (= :fail (try (assoc! (transient [1 2]) n 4)
                        (catch js/Error e :fail)))))))

(defn cljs-739 [arr names]
  (let [name (first names)]
    (if name
      (recur (conj arr (fn [] (println name)))
        (rest names))
      arr)))

(deftest test-739
  (testing "Testing CLJS-739, with-out-str"
    (binding [*print-newline* true]
      (is (= (with-out-str (doseq [fn (cljs-739 [] [:a :b :c :d])] (fn)))
          ":a\n:b\n:c\n:d\n")))))

(deftest test-728
  (testing "Testing CLJS-728, lookup with default"
    (doseq [n [nil "-1" "" "0" "1" false true (js-obj)]]
      (is (nil? (get [1 2] n)))
      (is (= :fail (try (nth [1 2] n) (catch js/Error e :fail))))
      (is (= 4 (get [1 2] n 4)))
      (is (= :fail (try (nth [1 2] n 4) (catch js/Error e :fail))))

      (is (nil? (get (subvec [1 2] 1) n)))
      (is (= :fail (try (nth (subvec [1 2] 1) n) (catch js/Error e :fail))))
      (is (= 4 (get (subvec [1 2] 1) n 4)))
      (is (= :fail (try (nth (subvec [1 2] 1) n 4) (catch js/Error e :fail))))

      (is (nil? (get (transient [1 2]) n)))
      (is (= :fail (try (nth (transient [1 2]) n) (catch js/Error e :fail))))
      (is (= 4 (get (transient [1 2]) n 4)))
      (is (= :fail (try (nth (transient [1 2]) n 4) (catch js/Error e :fail))))

      (is (nil? (get (range 1 3) n)))
      (is (= :fail (try (nth (range 1 3) n) (catch js/Error e :fail))))
      (is (= 4 (get (range 1 3) n 4)))
      (is (= :fail (try (nth (range 1 3) n 4) (catch js/Error e :fail))))))
  )

(deftest test-778
  (testing "Testing CLJS-778, -rest, -next RSeq"
    (is (= (-rest (rseq [0])) ()))
    (is (nil? (-next (rseq [0]))))
    (is (= (set (rseq [0])) #{0}))))

(def cljs-780 (atom {:foo (with-meta [] {:bar '(1 2 3)})}))

(deftest test-780
  (let [_ (swap! cljs-780 update-in [:foo] vary-meta update-in [:bar] vec)
        x (-> @cljs-780 :foo meta :bar)]
    (testing "Testing CLJS-780, update-in + vary-meta"
      (is (vector? x))
      (is (= x [1 2 3])))) )

(deftest test-782
  (testing "Testing CLJS-782, UUID toString"
    (is (= (.toString #uuid "550e8400-e29b-41d4-a716-446655440000")
          "550e8400-e29b-41d4-a716-446655440000"))))

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

(deftest test-case-keyword
  (is (= (let [x "a"] (case x :a 1 "a")) "a")))

(deftest test-801
  (testing "Testing CLJS-801, str"
    (is (= "0atrue:key/wordsymb/olfalse[1 2 3 4]1234.56789"
          (str 0 "a" true nil :key/word 'symb/ol false [1 2 3 4] 1234.5678 0x09)))))

(defn case-recur [value]
  (case value
    :a (recur :b)
    :b 0))

(deftest test-812
  (testing "Testing CLJS-812, case with recur"
    (is (= (case-recur :a) 0))))

(deftest test-816
  (testing "Testing CLJS-816, rename-keys"
    (is (= (set/rename-keys {:a "one" :b "two"} {:a :z}) {:z "one" :b "two"}))
    (is (= (set/rename-keys {:a "one" :b "two"} {:a :z :c :y}) {:z "one" :b "two"}))
    (is (= (set/rename-keys {:a "one" :b "two" :c "three"} {:a :b :b :a})
          {:a "two" :b "one" :c "three"}))) )

(deftest test-881
  (testing "Testing CLJS-881, duplicate keys in array maps"
    (is (= [:foo] (keys (apply array-map [:foo 1 :foo 2]))))))

(deftest test-810
  (let [not-strings [true false nil 1 (fn [])]]
    (testing "Testing CLJS-810, exception on bad input to regex fns"
      (is (every? #(= :failed (try (re-find #"." %)
                                   (catch js/TypeError _ :failed))) not-strings))
      (is (every? #(= :failed (try (re-matches #"." %)
                                   (catch js/TypeError _ :failed))) not-strings))
      (is (every? #(= :failed (try (re-find #"nomatch" %)
                                   (catch js/TypeError _ :failed))) not-strings))
      (is (every? #(= :failed (try (re-matches #"nomatch" %)
                                   (catch js/TypeError _ :failed))) not-strings)))))

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

(deftest test-853
  (testing "Testing CLJS-853, function metadata"
    (is (= {:foo true} (meta ^:foo (fn []))))))

(deftest test-807 
  (testing "Testing CLJS-807, big int, float, big dec literals"
    (is (= -1 -1N))
    (is (= 9.007199254740996E15 9007199254740995N))
    (is (= 1.5 1.5M))
    (is (= 4.9E-324 5E-324M))))

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

(deftest test-921-var-meta-name
  (testing "testing CLJS-921, :name var metadata should be unqualified"
    (is (= (-> (var first) meta :name) 'first))))

(deftype MyWatchable []
  IWatchable
  (-notify-watches [this oldval newval])
  (-add-watch [this key f])
  (-remove-watch [this key]))

(deftest test-920-watch-ops-return-ref
  (testing "tesing CLJS-920, add-watch/return-watch should return reference"
    (let [w (MyWatchable.)]
      (is (identical? (add-watch w :foo (fn [])) w))
      (is (identical? (remove-watch w :foo) w)))))

(deftype MyCustomAtom [^:mutable state]
  IDeref
  (-deref [_] state)
  IReset
  (-reset! [_ newval]
    (set! state newval)))

(deftest test-919-generic-cas
  (testing "testing CLJS-919, CAS should on custom atom types"
    (let [a0 (MyCustomAtom. 10)
          a1 (MyCustomAtom. 0)]
      (compare-and-set! a0 0 20)
      (compare-and-set! a1 0 20)
      (is (== @a0 10))
      (is (== @a1 20)))))

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

(deftest test-map-new-transducers
  (testing "Test distinct, interpose, map-indexed transducers"
    (is (= [1 2 3]
           (transduce (distinct) conj [] [1 1 2 2 3 3])))
    (is (= [1 :foo 2 :foo 3]
           (transduce (interpose :foo) conj [] [1 2 3])))
    (is (= [[0 1] [1 2] [2 3]]
           (transduce (map-indexed (fn [i x] [i x])) conj [] [1 2 3])))))

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

(defn foo-var [f]
  (fn [x]
    (f x)))

(defn foo-set [x]
  (first x))

(deftest test-cljs-982-var-deref
  (let [f (foo-var #'foo-set)]
    (is (= (f [1 2 3]) 1))
    (set! foo-set (fn [x] :oops))
    (is (= (f [1 2 3]) :oops))))

(deftest test-cljs-993
  (is (nil? (binding [*print-level* 4])))
  (is (= (binding [*print-level* 4] *print-level*) 4))
  (is (nil? (try
              (binding [*print-level* 4]
                (throw (js/Error.)))
              (catch js/Error e
                *print-level*)))))

(defn meta-test-fn
  "A docstring"
  {:foo :bar, :baz 12345, :whatever "String Metadata"}
  [a b]
  (+ a b))

(deftest test-cljs-1046
  (let [m (meta #'meta-test-fn)]
    (is (= "A docstring" (:doc m)))
    (is (= :bar (:foo m)))
    (is (= 12345 (:baz m)))
    (is (= "String Metadata" (:whatever m)))))

(defmulti cljs-1144 identity :default ::default)

(deftest test-cljs-1144
  (is (not= map (dispatch-fn cljs-1144)))
  (is (= identity (dispatch-fn cljs-1144)))
  (is (= ::default (default-dispatch-val cljs-1144))))

(defn foo-1187 [] (print "foo!"))

(defn bar-1187 [] (print "bar!"))

(defn print-foo-1187 [fb]
  (apply
    (case fb
      :foo #'foo-1187
      :bar #'bar-1187) []))

(deftest test-var?
  (is (var? #'inc))
  (is (not (var? 1))))

(deftest test-cljs-1187
  (testing "Internal var nodes analyzed in expression context"
    (is (= (with-out-str (print-foo-1187 :foo))
           "foo!"))))

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

(deftest test-var-arglists
  (is (= (-> #'first meta :arglists) '([coll])))
  (is (= (-> #'hash-map meta :arglists) '([& keyvals])))
  (is (= (-> #'map meta :arglists)
        '([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls]))))

(deftest tagged-literals
  (let [tl (tagged-literal 'x "y")]
    (is (tagged-literal? tl))
    (is (not (tagged-literal? {:tag 'x :form "y"})))
    (is (= (:tag tl) 'x))
    (is (= (:form tl) "y"))
    (is (= tl (tagged-literal 'x "y")))
    (is (not= tl (tagged-literal 'z "y")))
    (is (not= tl (tagged-literal 'x "z")))
    (is (= (hash tl) (hash (tagged-literal 'x "y"))))
    (is (= "#foo [1]" (str (tagged-literal 'foo [1]))))))

(defn- incme []
  (let [incme (fn [a queue & args] (inc a))]
    (incme 1 [1] :color "#fff")))

(deftest test-cljs-1225
  (is (= (incme) 2)))

(defn my-conj
  [acc x]
  (conj acc x))

(deftest test-cljs-1209
  (is (= (reduce my-conj [] (eduction (map identity) [1 2 3]))
         [1 2 3])))

(deftest test-get-with-float
  (is (= (get #js [\h \i] 1.7) \i))
  (is (= (get "hi" 1.7) \i)))

(defn foo-1284
  ([] nil)
  ([x0 & xs] [x0 xs]))

(deftest test-cljs-1284
  (let [xs (IndexedSeq. #js [] 0 nil)
        ys (IndexedSeq. #js [1] 3 nil)]
    (is (nil? (first xs)))
    (is (nil? (seq xs)))
    (is (= (rest xs) ()))
    (is (= (pr-str xs) "()"))
    (is (= (foo-1284 0) [0 ()]))
    (is (= (pr-str (foo-1284 0)) "[0 ()]"))
    (is (zero? (count ys)))
    (is (= (transduce (map inc) conj [] ys) []))))

(deftest test-symbol-from-string
  (let [x (symbol "js/Array")]
    (is (= x 'js/Array))
    (is (= (hash x) (hash 'js/Array)))
    (is (= (namespace x) "js"))
    (is (= (name x) "Array"))))

(deftest test-1276
  (is (= #'first #'first))
  (is (not= #'first #'last)))

(deftest test-1248
  (let [v (vary-meta #'first assoc :foo 'bar)]
    (is (= (-> v meta :foo) 'bar))))

(deftest test-1210
  (is (= ((fn []
            (let [{:keys [arguments]} {}
                  arguments (or arguments [])]
              arguments)))
         [])))

(deftest test-munge-demunge
  (is (= 'cljs.core/first?
         (demunge (munge 'cljs.core/first?)))))

(deftest test-munge
  (is (= "a_b" (munge "a-b")))
  (is (= "a_SLASH_b" (munge "a/b")))
  (is (= "_DOT__DOT_" (munge "..")))
  (is (= "abstract$" (munge "abstract")))
  (is (= 'abc (munge 'abc)))
  (is (= "toString" (munge "toString"))))

(deftest test-uuid-compile-and-runtime-hash
  (is (= (hash (.toString #uuid "0d1f9029-40fc-4728-8bdd-9862172d4370"))
         (hash (.toString (UUID. "0d1f9029-40fc-4728-8bdd-9862172d4370" nil))))))

(defprotocol IFooBar
  (a-method [t]))

(deftest test-cljs-1451
  (is (= "foobar" (a-method (reify
                              IFooBar
                              (cljs.core-test/a-method [_] "foobar"))))))

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

(deftest test-cljs-1569
  (is (= (meta (with-meta (seq [1 2 3]) {:a 1})) {:a 1})))

(deftest test-cljs-1420
  (is (= :2-arity
         (get-in
           (reify
             ILookup
             (-lookup [o k] :2-arity)
             (-lookup [o k not-found] :3-arity))
           [:foo]))))

(deftest test-cljs-1594
  (is  (not (js/isNaN (hash Infinity))))
  (is  (not (js/isNaN (hash -Infinity))))
  (is  (not (js/isNaN (hash NaN))))
  (is  (=  (hash-set Infinity -Infinity 0 1 2 3 4 5 6 7 8)
          (set  (keys  (zipmap  [Infinity -Infinity 0 1 2 3 4 5 6 7 8]  (repeat nil)))))))

(deftest test-cljs-1590
  (is (= [""] (s/split "" #"\n")))
  (is (= [] (s/split "\n\n\n" #"\n")))
  (is (= [""] (s/split-lines "")))
  (is (= [] (s/split-lines "\n\n\n"))))

(deftest test-reductions-obeys-reduced
  (is (= [0 :x]
        (reductions (constantly (reduced :x))
          (range))))
  (is (= [:x]
        (reductions (fn [acc x] x)
          (reduced :x)
          (range))))
  (is (= [2 6 12 12]
        (reductions (fn [acc x]
                      (if (= x :stop)
                        (reduced acc)
                        (+ acc x)))
          [2 4 6 :stop 8 10]))))

(deftest test-nil-hashing-cljs-1649
  (is (zero? (hash-string nil)))
  (is (not (zero? (hash-string "null")))))

(deftest test-cljs-1721
  (is (= 1          (get-in {:a (array 1 2 3 4)} [:a 0] :not-found)))
  (is (= :not-found (get-in {:a (array 1 2 3 4)} [:a 4] :not-found)))
  (is (= "d"        (get-in {:a "data"} [:a 0] :not-found)))
  (is (= :not-found (get-in {:a "data"} [:a 4] :not-found))))

(deftest test-cljs-1739
  (is (= (-> {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9}
           rest rest rest rest rest rest rest rest rest)
         ())))

(deftest test-cljs-1744
  (doseq [i (range 1 64)]
    (let [m (zipmap (range i) (range i))]
      (is (= () (last (take (inc i) (iterate rest m))))))))

(def cljs.core-test/foo-1274 42)

(deftest test-cljs-1274
  (is (= foo-1274 42))
  (is (= cljs.core-test/foo-1274 42)))

(deftest test-cljs-1779
  (is (= (hash (keyword 'app "foo"))
         (hash (keyword "app" "foo")))))

(defrecord CLJS1780 [a b c])

(deftest test-cljs-1780
  (let [record (->CLJS1780 1 2 3)]
    (is (= (into #{} (sequence (map identity)
                               record))
           #{[:a 1] [:b 2] [:c 3]}))
    (is (= (into #{} (sequence (map identity)
                               (assoc record :d 4 :e 5)) )
           #{[:a 1] [:b 2] [:c 3] [:d 4] [:e 5]}))))

(deftest test-cljs-1775
  (is (nil? (get "foo" nil)))
  (is (= 42 (get {nil 42} nil) 42))
  (is (= (get #js [\h \i] 1.7 :not-found) \i))
  (is (= (get "hi" 1.7 :not-found) \i)))

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

(deftest test-cljs-1748
  (is (thrown? js/Error (nth (array 0 1 2)  3)))
  (is (thrown? js/Error (nth (array 0 1 2) -1)))
  (is (= (nth (array 0 1 2)  3 :not-found) :not-found))
  (is (= (nth (array 0 1 2) -1 :not-found) :not-found))

  (is (thrown? js/Error (nth "012"  3)))
  (is (thrown? js/Error (nth "012" -1)))
  (is (= (nth "012"  3 :not-found) :not-found))
  (is (= (nth "012" -1 :not-found) :not-found)))

(comment
  ;; ObjMap
  ;; (let [ks (map (partial str "foo") (range 500))
  ;;       m  (apply obj-map (interleave ks (range 500)))]
  ;;   (assert (instance? cljs.core.ObjMap m))
  ;;   (assert (= 500 (count m)))
  ;;   (assert (= 123 (m "foo123"))))

  ;; vars

  ;; (defn var-test
  ;;   "A docstring"
  ;;   [a b]
  ;;   (+ a b))

  ;; (let [var-meta (meta #'var-test)]
  ;;   (assert (= (:doc var-meta) "A docstring"))
  ;;   (assert (= (:arglists var-meta) '([a b]))))

  )
