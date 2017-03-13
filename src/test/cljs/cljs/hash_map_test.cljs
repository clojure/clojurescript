;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.hash-map-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is]]))

(defn iter->set
  "Return a set of elements in iterator"
  [iter]
  (loop [entries #{} iter iter]
    (if (.hasNext iter)
      (recur (conj entries (.next iter)) iter)
      entries)))

(deftest test-cljs-1765
  (is (true? (.hasNext (-iterator (hash-map nil 1)))))
  (is (true? (.hasNext (-iterator (hash-map :a 1 :b 2 :c 3 :d 4 nil 5)))))
  (is (= #{[nil 1]}
         (-> (hash-map nil 1)
             (-iterator)
             (iter->set))))
  (is (= #{[:a 1] [:b 2] [:c 3] [:d 4] [nil 5]}
         (-> (hash-map :a 1 :b 2 :c 3 :d 4 nil 5)
             (-iterator)
             (iter->set)))))

(deftest test-cljs-1817
  (let [cljscore-hash hash]
    (with-redefs [hash (fn [x]
                         (if (or (#{:a :b} x) 0)
                           cljscore-hash))]
      (let [x (hash-map :a :a :b -1)]
        (is (= (assoc x :b :b) {:a :a :b :b}))))))

(deftest test-array-map-with-duplicate-keys
  (testing "Testing duplicate keys in array maps"
    ;; runtime
    (is (= [:foo] (keys (apply array-map [:foo 1 :foo 2]))))
    (let [sym-a (with-meta 'foo :first)
          sym-b (with-meta 'foo :second)]
      (is (= {sym-a 2} (apply array-map [sym-a 1 sym-b 2]))))
    ;; compile-time
    (is (= {:foo 2} (array-map :foo 1 :foo 2)))
    (let [sym-a (with-meta 'foo :first)
          sym-b (with-meta 'foo :second)]
      (is (= {sym-a 2} (array-map sym-a 1 sym-b 2))))))

(defrecord T [index a b])

(deftest test-cljs-1976
  ;; We must detect hash collisions when two values have different hashes but
  ;; still have the same 32-bit hash. Hash producers may be lazy and not
  ;; truncate their hash to 32-bits.
  (let [bad-record-1 (->T :eavt 17592186192276 nil)
        ;; (hash bad-record-1) is 1454955434
        bad-record-2 (->T :avet 10 :fhir.ElementDefinition/minValueDateTime$cr)
        ;; (hash bad-record-2) is -2840011862
        ;; But (bit-or (hash bad-record-2) 0) is 1454955434. Collision!
        ;; These dates have the same property
        bad-date-1   #inst "2017-03-13T22:21:08.666-00:00"
        bad-date-2   #inst "2015-11-02T19:53:15.706-00:00"]
    (testing "Transient assoc of hash-colliding keys with different hash values"
      (is (= :ok (try
                   (hash-map bad-record-1 nil bad-record-2 nil)
                   :ok
                   (catch :default _ :error))))
      (is (= :ok (try
                   (hash-map bad-date-1 nil bad-date-2 nil)
                   :ok
                   (catch :default _ :error)))))

    (testing "Non-transient assoc of hash-colliding keys with different hash values"
      (is (= :ok (try
                   (assoc (hash-map bad-record-1 nil) bad-record-2 nil)
                   :ok
                   (catch :default _ :error))))

      (is (= :ok (try
                   (assoc (hash-map bad-date-1 nil) bad-date-2 nil)
                   :ok
                   (catch :default _ :error)))))))
