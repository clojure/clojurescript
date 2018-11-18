;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is are]]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer-macros [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]
            [clojure.string :as s]
            [clojure.set :as set]
            [goog.object :as gobject]))

(deftest test-metadata
  (testing "Testing metadata"
    (is (= {"x" "y"} (meta ^{"x" "y"} [])))
    ))

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
      (is (thrown? js/Error (set-validator! a number?)))
      (is (some? (get-validator a)))
      (set-validator! a nil)
      (is (nil? (get-validator a)))
      (let [e1 (ex-info "" {})]
        (try
          (set-validator! a (fn [_] (throw e1)))
          (catch :default e2
            (is (identical? e1 e2)))))
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
      (is (= (js->clj nil) nil))
      (let [map-entry (->MapEntry #js {:foo 1} #js [1 2] nil)]
        (is (= (->MapEntry {"foo" 1} [1 2] nil) (js->clj map-entry)))))
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
      (is (= (gobject/get (clj->js {:a 1}) "a") 1))
      (is (= (-> (clj->js {:a {:b {{:k :ey} :d}}})
                   (gobject/get "a")
                   (gobject/get "b")
                   (gobject/get "{:k :ey}"))
                "d")))
    (is (= (-> (clj->js {:foo/bar "a"})
               (gobject/get "bar"))
           "a"))
    (is (= (-> (clj->js {:foo/bar "a"} :keyword-fn namespace)
               (gobject/get "foo"))
           "a"))))

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

(def three-levels-h (-> (make-hierarchy)
                        (derive :parent :gparent)
                        (derive :child :parent)))

(defmulti multi-with-h (fn [v] v) :hierarchy #'three-levels-h)
(defmethod multi-with-h :gparent [_] :gparent)
(defmethod multi-with-h :parent [_] :parent)

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
      (is (= :parent (multi-with-h :child)))
)))

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
             meta) {:a 1}))
    (let [xf (fn [rf]
               (fn
                 ([] (rf))
                 ([result] (rf result :foo))
                 ([result input] (rf result input))))]
      (is (= (sequence xf [1 2 3]) [1 2 3 :foo]))))
  (testing "CLJS-2258"
    (is (= ["1"] (sequence (map str) (eduction [1]))))))

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

(def constantly-nil (constantly nil))

(deftest some->test
  (is (nil? (some-> nil)))
  (is (= 0 (some-> 0)))
  (is (= -1 (some-> 1 (- 2))))
  (is (nil? (some-> 1 constantly-nil (- 2)))))

(deftest some->>test
  (is (nil? (some->> nil)))
  (is (= 0 (some->> 0)))
  (is (= 1 (some->> 1 (- 2))))
  (is (nil? (some->> 1 constantly-nil (- 2)))))

(deftest cond->test
  (is (= 0 (cond-> 0)))
  (is (= -1 (cond-> 0 true inc true (- 2))))
  (is (= 0 (cond-> 0 false inc)))
  (is (= -1 (cond-> 1 true (- 2) false inc))))

(deftest cond->>test
  (is (= 0 (cond->> 0)))
  (is (= 1 (cond->> 0 true inc true (- 2))))
  (is (= 0 (cond->> 0 false inc)))
  (is (= 1 (cond->> 1 true (- 2) false inc))))

(deftest as->test
  (is (= 0 (as-> 0 x)))
  (is (= 1 (as-> 0 x (inc x))))
  (is (= 2 (as-> [0 1] x
             (map inc x)
             (reverse x)
             (first x)))))

(deftest threading-loop-recur
  (is (nil? (loop []
              (as-> 0 x
                (when-not (zero? x)
                  (recur))))))
  (is (nil? (loop [x nil] (some-> x recur))))
  (is (nil? (loop [x nil] (some->> x recur))))
  (is (= 0 (loop [x 0] (cond-> x false recur))))
  (is (= 0 (loop [x 0] (cond->> x false recur)))))

(defspec boolean-test 10
  (prop/for-all [b gen/boolean]
    (boolean? b)))

(deftest aget-test
  (is (= 11 (aget #js [10 11 12] 1)))
  (is (= 11 (apply aget [#js [10 11 12] 1])))
  (is (= 3 (aget #js [1 2 #js [3 4]] 2 0)))
  (is (= 3 (apply aget [#js [1 2 #js [3 4]] 2 0]))))

(deftest aset-test
  (let [array #js [10 11 12]]
    (is (= 13 (aset array 1 13)))
    (is (= 13 (aget array 1))))
  (let [array #js [10 11 12]]
    (is (= 13 (apply aset [array 1 13])))
    (is (= 13 (aget array 1))))
  (let [array #js [1 2 #js [3 4]]]
    (is (= 13 (aset array 2 0 13)))
    (is (= 13 (aget array 2 0))))
  (let [array #js [1 2 #js [3 4]]]
    (is (= 13 (apply aset [array 2 0 13])))
    (is (= 13 (aget array 2 0)))))

(deftest unchecked-get-test
  (is (= 1 (unchecked-get #js {:a 1} "a")))
  (is (nil? (unchecked-get #js {:a 1} "b")))
  (is (nil? (unchecked-get #js {:a 1} nil))))

(deftest js-invoke-test
  (let [o (doto (js-obj) (gobject/set "my sum" (fn [a b] (+ a b))))]
    (is (= 5 (js-invoke o "my sum" 2 3)))))

(deftest memfn-test
  (let [substr (memfn substr start length)]
    (is (= "cde" (substr "abcdefg" 2 3))))
  (let [trim (memfn trim)]
    (is (= ["abc" "def"] (map trim ["   abc   " "  def   "])))))

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


(defprotocol Slashy (/ [_]))

(extend-type string
       Slashy
       (/ [_] "result"))

(deftest test-protocol-with-slash
  (is (=  "result" (/ ""))))

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

(deftest test-2067
  (is (= 0 (reduce-kv
             (fn [x k _]
               (when (zero? k)
                 (reduced k)))
             nil (zipmap (range 17) (repeat 0))))))

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

(deftest test-2764
  (testing "Testing CLJS-2764, exists? on multi-segment symbols"
    (is (false? (exists? this.ns.does.not.exist)))
    (is (true? (exists? cljs.core.first)))
    (is (true? (exists? cljs.core/first)))
    (is (true? (exists? (:foo {:foo 1}))))
    (is (false? (exists? (:foo {}))))))

(deftest test-518
  (is (nil? (:test "test"))))

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

(deftest print-ns-maps
  (testing "Testing CLJS-1786, *print-namespace-maps*"
    (is (= "#:user{:a 1}" (binding [*print-namespace-maps* true] (pr-str {:user/a 1}))))
    (is (= "{:user/a 1}" (binding [*print-namespace-maps* false] (pr-str {:user/a 1}))))))

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
                                   (catch js/TypeError _ :failed))) not-strings))
      (is (every? #(= :failed (try (re-seq #"." %)
                                   (catch js/TypeError _ :failed))) not-strings))
      (is (every? #(= :failed (try (re-seq #"nomatch" %)
                                   (catch js/TypeError _ :failed))) not-strings)))))

(deftest test-853
  (testing "Testing CLJS-853, function metadata"
    (is (= {:foo true} (meta ^:foo (fn []))))))

(deftest test-807
  (testing "Testing CLJS-807, big int, float, big dec literals"
    (is (= -1 -1N))
    (is (= 9.007199254740996E15 9007199254740995N))
    (is (= 1.5 1.5M))
    (is (= 4.9E-324 5E-324M))))

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

(deftest test-map-new-transducers
  (testing "Test distinct, interpose, map-indexed transducers"
    (is (= [1 2 3]
           (transduce (distinct) conj [] [1 1 2 2 3 3])))
    (is (= [1 :foo 2 :foo 3]
           (transduce (interpose :foo) conj [] [1 2 3])))
    (is (= [[0 1] [1 2] [2 3]]
           (transduce (map-indexed (fn [i x] [i x])) conj [] [1 2 3])))))

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

(defprotocol IFooBar
  (a-method [t]))

(deftest test-cljs-1451
  (is (= "foobar" (a-method (reify
                              IFooBar
                              (cljs.core-test/a-method [_] "foobar"))))))

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
  (is  (not (js/isNaN (hash js/Infinity))))
  (is  (not (js/isNaN (hash js/-Infinity))))
  (is  (not (js/isNaN (hash js/NaN))))
  (is  (=  (hash-set js/Infinity js/-Infinity 0 1 2 3 4 5 6 7 8)
          (set  (keys  (zipmap  [js/Infinity js/-Infinity 0 1 2 3 4 5 6 7 8]  (repeat nil)))))))

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

(deftest test-cljs-1748
  (is (thrown? js/Error (nth (array 0 1 2)  3)))
  (is (thrown? js/Error (nth (array 0 1 2) -1)))
  (is (= (nth (array 0 1 2)  3 :not-found) :not-found))
  (is (= (nth (array 0 1 2) -1 :not-found) :not-found))

  (is (thrown? js/Error (nth "012"  3)))
  (is (thrown? js/Error (nth "012" -1)))
  (is (= (nth "012"  3 :not-found) :not-found))
  (is (= (nth "012" -1 :not-found) :not-found)))

(let [foo-1536 2]
  (def foo-1536 foo-1536))

(let [foo-1536-2 1]
  (defn foo-1536-2 []
    foo-1536-2))

(deftest test-cljs-1536
  (is (= foo-1536 2))
  (is (= (foo-1536-2) 1))
  ;; these two lines generate a `:redef-in-file` warning, which is caused by `cljs.test/is`
  (is (= ((let [z 1] (defn z [] z))) 1))
  (is (= (let [w 1] ((defn w [] w))) 1)))

(deftest test-cljs-1837
  (testing "halt-when transducer"
    (is (= (transduce (halt-when #{1}) conj [] [5 4 1 2 3])
           1))
    (is (= (transduce (halt-when #{1} (fn [ret input] input)) conj [] [5 4 1 2 3])
           1))
    (is (= (transduce (halt-when #{1} (fn [ret input] ret)) conj [] [5 4 1 2 3])
           [5 4]))
    (is (= (transduce (halt-when #{1} (fn [ret input] (conj ret input))) conj [] [5 4 1 2 3])
           [5 4 1]))
    (is (= (into [] (halt-when #{1} (fn [ret in] (conj! ret in)))  [2 3 1]))
        [2 3 1])))

(deftest test-cljs-1839
  (let [x #js {:foo (fn [])}
        foo (.-foo x)]
    (is (instance? foo (new foo)))
    (is (instance? foo (foo.)))
    (is (instance? foo (new (.-foo x))))))

(deftest test-cljs-1845
  (let [sv (subvec [0 1 2 3 4 5 7 8 9] 2 6)]
    (is (= [2 3 4 5] sv))
    (is (= [2 3 0 5] (assoc sv 2 0)))
    (is (= [2 3 4 0] (assoc sv 3 0)))
    (is (= [2 3 4 5 0] (assoc sv 4 0)))
    (is (thrown? js/Error (assoc sv 5 0)))
    (is (thrown? js/Error (assoc sv -1 0)))))

(deftest test-cljs-1829
  (is (= (get "0123" -1 :not-found) :not-found))
  (is (= (get #js [0 1 2 3] -1 :not-found) :not-found))
  (is (= (get "0123" nil :not-found) :not-found))
  (is (= (get #js [0 1 2 3] nil :not-found) :not-found)))

(deftest test-cljs-2028
  (let [x (sequence (filter pos?) [1 2 -1])]
    (is (not (realized? x)))
    (is (= x [1 2]))
    (is (realized? x))))

(deftest test-1518
  (testing "Test evaluate expression once - keyword tests"
    (let [m {:a :b
             :b :c}
          x (atom :a)]
      (case (swap! x m) :a 0 :default)
      (is (= :b @x)))))

(deftest test-cljs-2021
  (let [check-if-throws #(try (%) (catch js/Error e :fail))]
    (is (= :fail (check-if-throws #(subvec nil 0 0))))
    (is (= :fail (check-if-throws #(subvec {:foo :bar} 0 1))))
    (is (= :fail (check-if-throws #(subvec '(:foo) 0 1))))
    (is (= :fail (check-if-throws #(subvec #{:foo} 0 1))))))

(deftest test-cljs-2075
  (testing "PersistentTreeMap kv-reduce should honor reduced"
    (let [sm (sorted-map 1 1, 2 2, 3 3, 4 4, 5 5, 6 6, 7 7)]
      (is (= [1 2 3 4] (reduce-kv (fn [m k v] (if (= 5 k) (reduced m) (conj m k))) [] sm))))))

(defrecord CLJS2079 [a b])

(deftest test-cljs-2079
  (testing "Records and maps should not be equal"
    (let [am (array-map :a 1 :b 2)
          hm (hash-map :a 1 :b 2)
          sm (sorted-map :a 1 :b 2)
          r (->CLJS2079 1 2)]
      (is (= am hm sm))

      (is (not= r am))
      (is (not= am r))

      (is (not= r hm))
      (is (not= hm r))

      (is (not= r sm))
      (is (not= sm r)))))

(deftype MapWithNoIKVReduce [backing-map]
  IMap
  (-dissoc [_ _] nil)

  ISeqable
  (-seq [_] (seq backing-map)))

(deftest test-cljs-2083
  (testing "maps which do not implement IKVReduce can be compared"
    (is (true?  (equiv-map (MapWithNoIKVReduce. {:a 1 :b 2 :c 3}) {:a 1 :b 2 :c 3})))
    (is (false? (equiv-map (MapWithNoIKVReduce. {:a 1 :b 2 :c 3}) {:a 1 :b 2 :c 4})))))

(deftest test-cljs-1685
  (testing "nil start or end param throws error"
    (is (= :fail (try (subvec nil nil)
                      (catch js/Error e :fail))))
    (is (= :fail (try (subvec nil 1 nil)
                      (catch js/Error e :fail))))))

(def ^:const cljs-2104 "cljs-2104")

(deftest test-const-emission
  (testing "const exprs emission context, not definition context (CLJS-2104)"
    (is (= cljs-2104 "cljs-2104"))
    (is (= (if-some [x true]
             cljs-2104
             "unreachable")
          "cljs-2104"))))

(deftest test-cljs-2113
  (is (thrown? js/Error (nth (range 2) -2)))
  (is (thrown? js/Error (nth (range 2 1 0) -2)))
  (is (= ::not-found (nth (range 2) -2 ::not-found)))
  (is (= ::not-found (nth (range 2 1 0) -2 ::not-found))))

(deftest test-cljs-2109
  (testing "Syntax quoted dotted symbol without namespace should resolve to itself"
    (is (= 'clojure.core `clojure.core))))

(deftype Partial [f args]
  IFn
  (-invoke [_ & a]
    (apply (apply partial f args) a)))

(deftest test-cljs-2133
  (testing "Invalid variadic IFn implementation should work"
    (let [p (Partial. + [1])]
      (p 2))))

(deftest test-resolve
  (testing "Resolve should return valid var"
    (is (= 1 ((resolve 'first) [1 2 3])))))

(deftest test-cljs-1998
  (testing "printing an Object with a null constructor"
    (is (= "#object[Object]" (pr-str (.create js/Object nil))))))

(deftest test-cljs-2184
  (testing "ns-publics"
    (is (contains? (ns-publics 'clojure.string) 'join))
    (is (not (contains? (ns-publics 'clojure.string) 'replace-all)))
    (is (= (find (ns-publics 'clojure.string) 'join)
          ['join #'clojure.string/join])))
  (testing "ns-imports"
    (is (contains? (ns-imports 'clojure.string) 'StringBuffer))
    (is (= (find (ns-imports 'clojure.string) 'StringBuffer)
          ['StringBuffer goog.string.StringBuffer]))))

(deftest test-cljs-2190
  (binding [*print-namespace-maps* true]
    (testing "printing a javascript map with a slash on keyword"
      (is (= "#js {\"foo/bar\" 33}" (pr-str (doto (js-obj) (gobject/set "foo/bar" 33)))))
      (is (= "#js {\"foo/bar\" #:var{:quux 66}}" (pr-str (doto (js-obj) (gobject/set "foo/bar" {:var/quux 66}))))))))

(def ^:const true-2267 true)
(def ^:const false-2267 false)
(def ^:const nil-2267 nil)
(def ^:const empty-string-2267 "")
(def ^:const non-empty-string-2267 "x")
(def ^:const zero-2267 0)
(def ^:const non-zero-2267 1)

(deftest test-cljs-2267
  (is (= :then (if true-2267 :then :else)))
  (is (= :else (if false-2267 :then :else)))
  (is (= :else (if nil-2267 :then :else)))
  (is (= :then (if empty-string-2267 :then :else)))
  (is (= :then (if non-empty-string-2267 :then :else)))
  (is (= :then (if zero-2267 :then :else)))
  (is (= :then (if non-zero-2267 :then :else))))

(deftest test-cljs-2278
  (is (= "#js {:alpha 1, \"beta gamma\" 2, \"delta/epsilon\" 3}" (pr-str #js {"alpha" 1 "beta gamma" 2 "delta/epsilon" 3})))
  (is (= "#js {\":abc\" 1}" (pr-str #js {":abc" 1})))
  (is (= "#js {\"0abc\" 1}" (pr-str #js {"0abc" 1})))
  (is (= "#js {:abc-def 1}" (pr-str #js {"abc-def" 1})))
  (is (= "#js {:x*+?!-' 1}" (pr-str #js {"x*+?!-'" 1}))))

(deftest test-cljs-2282
  (is (= "#js {:_abc 1}" (pr-str #js {"_abc" 1})))
  (is (= "#js {:*compiler* 1}" (pr-str #js {"*compiler*" 1}))))

(deftest test-cljs-2403
  (are [f k coll expected] (= expected (apply f k coll))
    min-key :x [{:x 1000} {:x 1001} {:x 1002} {:x 1000 :second true}] {:x 1000 :second true}
    max-key :x [{:x 1000} {:x 999} {:x 998} {:x 1000 :second true}] {:x 1000 :second true}))

(deftest swap-vals-returns-old-value
  (let [a (atom 0)]
    (is (= [0 1] (swap-vals! a inc)))
    (is (= [1 2] (swap-vals! a inc)))
    (is (= 2 @a))))

(deftest deref-swap-arities
  (let [a (atom 0)]
    (is (= [0 1] (swap-vals! a + 1)))
    (is (= [1 3] (swap-vals! a + 1 1)))
    (is (= [3 6] (swap-vals! a + 1 1 1)))
    (is (= [6 10] (swap-vals! a + 1 1 1 1)))
    (is (= 10 @a))))

(deftest deref-reset-returns-old-value
  (let [a (atom 0)]
    (is (= [0 :b] (reset-vals! a :b)))
    (is (= [:b 45M] (reset-vals! a 45M)))
    (is (= 45M @a))))

(deftest reset-on-deref-reset-equality
  (let [a (atom :usual-value)]
    (is (= :usual-value (reset! a (first (reset-vals! a :almost-never-seen-value)))))))

(deftest test-cljs-2374
  (is (= "##NaN" (pr-str js/NaN)))
  (is (= "##Inf" (pr-str js/Infinity)))
  (is (= "##-Inf" (pr-str js/-Infinity))))

(deftest test-cljs-2449
  (is (= 1 (let [catch identity] (catch 1))))
  (is (= 1 (let [finally identity] (finally 1)))))

(deftype Foo2407 [x y])
(defrecord Bar2407 [x y])

(deftest test-cljs-2407
  (is (= "Positional factory function for cljs.core-test/Foo2407." (:doc (meta #'->Foo2407))))
  (is (= "Positional factory function for cljs.core-test/Bar2407." (:doc (meta #'->Bar2407))))
  (is (= "Factory function for cljs.core-test/Bar2407, taking a map of keywords to field values." (:doc (meta #'map->Bar2407)))))

(deftest test-cljs-2283
  (is (nil? (doseq []))))

(deftest test-cljs-2453
  (is (= (re-seq #"[Bc]?" "aBcD") '("" "B" "c" "" "")))
  (is (= (re-seq #"[BcD]?$" "aBcD") '("D" "")))
  (is (= (map first (re-seq #"(\d+)" "ClojureScript 1.9.222")) '("1" "9" "222")))
  (is (= (re-seq #"\d+" "a1b2c3d") '("1" "2" "3")))
  (is (= (re-seq #"\d?" "a1b2c3d") '("" "1" "" "2" "" "3" "" "")))
  (is (= (re-seq #"\d*" "a1b2c3d") '("" "1" "" "2" "" "3" "" "")))
  (is (= (re-seq #"\d+" "a1b22c333d") '("1" "22" "333")))
  (is (= (re-seq #"\d?" "a1b22c333d") '("" "1" "" "2" "2" "" "3" "3" "3" "" "")))
  (is (= (re-seq #"\d*" "a1b22c333d") '("" "1" "" "22" "" "333" "" "")))
  (is (= (re-seq #"\w+" "once upon a time") '("once" "upon" "a" "time")))
  (is (nil? (re-seq #"\w+" ""))))

(deftest test-cljs-2001
  (is (map-entry? (MapEntry. :key :val 0)))
  (is (not (map-entry? [:key :val]))))

(deftype Foo2455 []
  ISequential)

(deftest test-cljs-2455
  (is (= :x (nth (eduction [:x]) 0)))
  (is (thrown-with-msg? js/Error #"Index out of bounds" (nth (eduction [:x]) 1)))
  (is (= :x (nth (eduction [:x]) 0 :not-found)))
  (is (= :not-found (nth (eduction [:x]) 1 :not-found)))
  ;; Calling nth on a type satisfying ISequential should attempt coercion
  (is (thrown-with-msg? js/Error #".* is not ISeqable" (nth (->Foo2455) 0))))

(deftest test-cljs-2457
  (is (thrown-with-msg? js/Error #".* is not ISeqable" (seq #js {:a 1 :b 2}))))

(deftest test-cljs-2549
  (let [tap (fn [_])]
    (add-tap tap)
    (is (set? @tapset))
    (is (contains? @tapset tap))
    (remove-tap tap)))

(deftest test-cljs-2552
  (is (boolean? (tap> nil))))

;; Delete a bogus property from the beta? fn
;; Without the fix this js-delete form code-gens to code that deletes the alpha? fn:
;;   delete (cljs.core_test.alpha_2585_QMARK_) && (cljs.core_test.beta_2585_QMARK_)["bogus-property"]
(defn ^boolean alpha-2585? [] true)
(defn ^boolean beta-2585? [] true)
(js-delete (and alpha-2585? beta-2585?) "bogus-property")

(deftest test-cljs-2585
  (is (= true ((or int? string?) 1)))
  ;; Make sure we didn't delete the alpha? fn
  (is (some? alpha-2585?)))

(defn fn-2741* ([x]) ([x y]))
(def fn-2741 fn-2741*)

(deftest test-cljs-2741
  (is (thrown-with-msg? js/Error #".*Invalid arity: 0" ((fn ([x]) ([x y])))))
  (is (thrown-with-msg? js/Error #".*Invalid arity: 3" ((fn ([x]) ([x y])) 1 2 3)))
  (is (thrown-with-msg? js/Error #".*Invalid arity: 0" (fn-2741)))
  (is (thrown-with-msg? js/Error #".*Invalid arity: 3" (fn-2741 1 2 3)))
  (is (thrown-with-msg? js/Error #".*Invalid arity: 0" ({})))
  (is (thrown-with-msg? js/Error #".*Invalid arity: 3" ({} 1 2 3))))

(deftest test-cljs-2799
  (is (thrown? js/Error (nth (repeat :x) -1)))
  (is (= ::not-found (nth (repeat :x) -1 ::not-found))))

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

(deftest uri-predicate
  (testing "Testing uri?"
    (is (not (uri? "http://clojurescript.org")))
    (is (not (uri? 42)))
    (is (not (uri? [])))
    (is (not (uri? {})))
    (is (uri? (goog.Uri. "")))
    (is (uri? (goog.Uri. "http://clojurescript.org")))
    (is (uri? (goog.Uri. "some string")))))

(defrecord CLJS-2787 [])

(deftest test-cljs-2787
  (let [x (map->CLJS-2787 {1 2})
        y (map->CLJS-2787 x)]
    (is (= x y))))

(deftest test-cljs-2807
  (testing "Quoted sets should work"
    (is (macroexpand '(fn [x] #{(into [] x)})))))

(deftest var-desugar-test
  (testing "dotted variable in return position"
    (= cljs.core.PersistentQueue.EMPTY
       ((fn [] cljs.core.PersistentQueue.EMPTY)))
    (= 1
       (let [a #js {:b 1}]
         ((fn [] a.b))))))

(deftest test-cljs-2832
  (is (true? ((comp not empty?) "foo")))
  (is (false? ((comp not empty?) "")))
  (is (thrown? js/Error ((not empty?) "foo")))
  (is (thrown? js/Error ((not empty?) ""))))

(deftest test-cljs-2864
  (is (= "" (str)))
  (is (= "a" (str "a")))
  (is (= "1" (str 1)))
  (is (= "xyzzy" (str "x" "y" "z" "z" "y")))
  (is (= "a1b2c3" (str "a" 1 "b" 2 "c" 3))))

(deftest test-cljs-2934
  (let [x (delay 1)]
    (is (= "#object[cljs.core.Delay {:status :pending, :val nil}]" (pr-str x)))
    (force x)
    (is (= "#object[cljs.core.Delay {:status :ready, :val 1}]" (pr-str x)))))

(deftest test-cljs-2943
  (let [m1 {:a 2, :b 3, :c 5}
        m2 {:a 7, :b 11, :d 13, :e 17}
        m3 {:a 19, :d 23, :f 29}
        m4 {:a 28, :b 14, :c 5, :d 36, :e 17, :f 29}
        sorted (fn [m] (into (sorted-map) m))]
    (is (= m4 (merge-with + m1 m2 m3)))
    (is (= m4 (merge-with + (sorted m1) m2 m3)))
    (is (= m4 (merge-with + (sorted m1) (sorted m2) m3)))
    (is (= m4 (merge-with + m1 (sorted m2) m3)))
    (is (= m4 (merge-with + m1 (sorted m2) (sorted m3))))))
