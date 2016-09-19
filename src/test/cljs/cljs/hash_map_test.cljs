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
