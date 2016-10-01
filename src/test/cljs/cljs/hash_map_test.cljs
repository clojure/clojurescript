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
