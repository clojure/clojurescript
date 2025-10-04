;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.lite-collections-test
  (:require [cljs.test :refer [deftest testing is]]))

;; NOTE: ** this namespace must be tested with :lite-mode true **

(deftest test-obj-map
  (let [a (. ObjMap -EMPTY)
        b {}]
    (is (identical? a b)))
  (let [a {:foo 1}]
    (is (== 1 (:foo a)))))

(deftest test-simple-set-with-set
  (is (= (simple-set []) (set [])))
  (is (= (set []) (simple-set [])))
  (is (= (simple-set [(MapEntry. 1 2 nil)])
         (set [(MapEntry. 1 2 nil)]))))

(comment

  (require '[cljs.lite-collections-test] :reload)
  (cljs.test/run-tests)

  )
