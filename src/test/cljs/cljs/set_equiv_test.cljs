;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.set-equiv-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is]]
            [clojure.string :as s]
            [clojure.set :as set]))

(deftest test-set-equality
  (testing "Testing set equality"
    (is (= (sorted-set 3 2 1)    (sorted-set 1 2 3)))
    (is (= (hash-set   3 2 1)    (sorted-set 1 2 3)))
    (is (= (sorted-set :a :b :c) (hash-set :a :b :c)))
    (is (= (hash-set   :a :b :c) (hash-set :a :b :c))))

  (testing "CLJS-2731 uncomparable values"
    (is (not= (sorted-set 3 2 1) (sorted-set :a :b :c)))
    (is (not= (hash-set 3 2 1)   (sorted-set :a :b :c)))
    (is (not= (sorted-set 3 2 1) (hash-set   :a :b :c)))
    (is (not= (hash-set 3 2 1)   (hash-set   :a :b :c)))))
