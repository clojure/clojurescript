;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.metadata-test
  (:require
   #?(:cljs [cljs.test    :refer-macros [deftest testing is]]
      :clj  [clojure.test :refer        [deftest testing is]])))

(defn seq-interface-tests
  "Tests that all seqs handle metadata correctly."
  [s]
  (when (seq s)
    (testing "seqs can have metadata"
      (is #?(:clj  (instance? clojure.lang.IObj s)
             :cljs (satisfies? IMeta s)))
      (let [m {:meta :data}]
        (is (= m (meta (with-meta s m)))))))

  (when (seq s)
    (let [s (with-meta s {:meta :data})]
      (testing "next should have nil metadata"
        (is (nil? (meta (next s)))))
      (testing "rest should have nil metadata"
        (is (nil? (meta (rest s)))))
      (testing "empty should have nil metadata"
        (is (nil? (meta (empty s))))))))


(defn coll-interface-tests
  "Tests that all collections handle metadata correctly"
  [coll]
  (testing "collections can have metadata"
    (is #?(:clj  (instance? clojure.lang.IObj coll)
           :cljs (satisfies? IMeta coll)))
    (let [m {:meta :data}]
      (is (= coll (with-meta coll m)))
      (is (= m (meta (with-meta coll m))))))

  (testing "conj keeps metadata"
    (let [m {:meta :data}
          coll (with-meta coll m)
          thing (if (map? coll) [:k :v] :x)]
      (is (= m (meta (conj coll thing))))))

  (testing "empty keeps metadata"
    (let [m {:meta :data}
          coll (with-meta coll m)]
      (is (= m (meta (empty coll))))))

  (testing "seq has no metadata"
    (let [m {:meta :data}
          coll (with-meta coll m)]
      (is (nil? (meta (seq coll)))))))


(defn disj-interface-tests
  "Tests that collections supporting disj handle metadata correctly"
  [coll]
  (testing "disj keeps metadata"
    (let [m {:meta :data}
          coll (with-meta (conj coll :k) m)]
      (is (= m (meta (disj coll :k)))))))


(deftest metadata-tests
  (testing "Collection"
    (testing "PersistentVector"
      (testing "Empty"
        (coll-interface-tests []))
      (testing "Medium"
        (coll-interface-tests [0 1 2 3]))
      (testing "Large"
        (coll-interface-tests (vec (range 100)))))

    (testing "PersistentHashSet"
      (testing "Empty"
        (coll-interface-tests (hash-set))
        (disj-interface-tests (hash-set)))
      (testing "Medium"
        (coll-interface-tests (hash-set 0 1 2 3 4 5))
        (disj-interface-tests (hash-set 0 1 2 3 4 5)))
      (testing "Large"
        (coll-interface-tests (apply hash-set (range 100)))
        (disj-interface-tests (apply hash-set (range 100)))))

    (testing "PersistentHashMap"
      (testing "Empty"
        (coll-interface-tests (hash-map)))
      (testing "Medium"
        (coll-interface-tests (hash-map 0 1 2 3 4 5)))
      (testing "Large"
        (coll-interface-tests (apply hash-map (range 100)))))

    (testing "PersistentArrayMap"
      (testing "Empty"
        (coll-interface-tests (array-map)))
      (testing "Medium"
        (coll-interface-tests (array-map 0 1 2 3 4 5)))))


  (testing "Seq over collections"
    (testing "PersistentVector"
      (testing "Empty"
        (seq-interface-tests (seq []))
        (seq-interface-tests (rseq [])))
      (testing "Medium"
        (seq-interface-tests (seq [0 1 2 3]))
        (seq-interface-tests (rseq [0 1 2 3])))
      (testing "Large"
        (seq-interface-tests (seq (vec (range 100))))
        (seq-interface-tests (rseq (vec (range 100))))))

    (testing "PersistentHashSet"
      (testing "Empty"
        (seq-interface-tests (seq (hash-set))))
      (testing "Medium"
        (seq-interface-tests (seq (hash-set 0 1 2 3 4 5))))
      (testing "Large"
        (seq-interface-tests (seq (apply hash-set (range 100))))))

    (testing "PersistentHashMap"
      (testing "Empty"
        (seq-interface-tests (seq (hash-map))))
      (testing "Medium"
        (seq-interface-tests (seq (hash-map 0 1 2 3 4 5))))
      (testing "Large"
        (seq-interface-tests (seq (apply hash-map (range 100))))))

    (testing "PersistentArrayMap"
      (testing "Empty"
        (seq-interface-tests (seq (array-map))))
      (testing "Medium"
        (seq-interface-tests (seq (array-map 0 1 2 3 4 5))))))

  (testing "generators"
    (testing "cycle"
      (seq-interface-tests (cycle [1 2 3])))
    (testing "range"
      (seq-interface-tests (range 10)))
    (testing "repeat"
      (seq-interface-tests (repeat 10 :x)))
    (testing "iterate"
      (seq-interface-tests (iterate inc 0)))))
