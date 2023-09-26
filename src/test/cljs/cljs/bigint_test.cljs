;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.bigint-test
  (:require [cljs.test :refer-macros [deftest is testing]]))

(deftest test-bigint
  (testing "BigInt Basics"
    (is (bigint? 1N))
    (is (bigint? 9007199254740992))
    (is (bigint? -9007199254740992)))
  (testing "BigInt & Number equality"
    (is (= 1 1N))
    (is (= 1N 1))
    (is (= 1 1N 1))
    (is (== 1 1N))
    (is (== 1N 1))
    (is (== 1 1N 1)))
  (testing "BigInt Hashing"
    (is (= (hash 1N) (hash 1)))
    (is (= (hash 9007199254740992) (hash 9007199254740992)))
    (is (= (hash -9007199254740992) (hash -9007199254740992))))
  (testing "BigInt as HashMap keys"
    (let [m {1N 2}]
      (is (= 2 (get m 1N)))
      (is (= 2 (get m 1)))))
  (testing "Interop"
    (is (= (js/BigInt 1) 1N))
    (is (= 1N (js/BigInt 1)))
    (is (= (js/BigInt 1) 1N))
    (is (= (js/BigInt 1) 1))
    (is (= 1 (js/BigInt 1))))
  (testing "Interaction with core"
    (is (= (range 1 5) (range 1 5N)))))

(comment

  (cljs.test/run-tests)

  )
