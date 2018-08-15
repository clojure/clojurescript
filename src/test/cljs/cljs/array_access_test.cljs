;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.array-access-test
  (:require-macros [cljs.array-access-test :refer [suppress-errs]])
  (:require [cljs.test :as test :refer [deftest is]]
            [cljs.array-access.alpha :as alpha]))

(deftest cljs-2861-test
  ;; With cljs-2718, a typo led to (set! *unchecked-arrays* true) as
  ;; not being treated as a no-op generating intrisic, which we can
  ;; detect here when this test is run in JVM ClojureScript.
  (is (false? *unchecked-arrays*)))

(deftest unchecked-arrays-file-scope-test
  (is (not (alpha/unchecked-arrays?))))

(deftest aget-test
  (is (thrown? js/Error (aget nil 1)))
  (is (nil? (aget #js {} 1)))
  (is (nil? (aget #js [] 0)))
  (is (nil? (aget #js [1] -1)))
  (is (nil? (aget #js [1] 1)))
  (is (== 1 (aget #js [1] "0")))
  (is (nil? (aget [1] 0)))
  (is (== 1 (aget #js [1] 0)))
  (is (== 1 (aget #js {:foo 1} "foo")))
  (is (nil? (aget #js [#js {}] 0 0)))
  (is (nil? (aget #js [#js []] 0 0)))
  (is (nil? (aget #js [#js [1]] 0 -1)))
  (is (nil? (aget #js [#js [1]] 0 1)))
  (is (== 1 (aget #js [#js [1]] 0 "0")))
  (is (== 1 (aget #js [#js [1]] 0 0))))

(deftest aset-test
  (is (thrown? js/Error (aset nil 1 "x")))
  (is (= "x" (aset #js {} 1 "x")))
  (is (= "x" (aset #js [] 0 "x")))
  (is (= "x" (aset #js [1] -1 "x")))
  (is (= "x" (aset #js [1] 1 "x")))
  (is (= "x" (aset #js [1] "0" "x")))
  (is (= "x" (aset [1] 0 "x")))
  (is (= "x" (aset #js [1] 0 "x")))
  (let [v #js [1]]
    (aset v 0 "x")
    (is (= "x" (aget v 0))))
  (let [v #js {:foo 1}]
    (aset v "foo" "x")
    (is (= "x" (aget v "foo"))))
  (is (= "x" (aset #js [#js {}] 0 0 "x")))
  (is (= "x" (aset #js [#js []] 0 0 "x")))
  (is (= "x" (aset #js [#js [1]] 0 -1 "x")))
  (is (= "x" (aset #js [#js [1]] 0 1 "x")))
  (is (= "x" (aset #js [#js [1]] 0 "0" "x")))
  (is (= "x" (aset #js [#js [1]] 0 0 "x")))
  (let [v #js [#js [1]]]
    (aset v 0 0 "x")
    (is (= "x" (aget v 0 0)))))

(deftest unchecked-aget-test
  (is (thrown? js/Error (unchecked-get nil 1)))
  (is (nil? (unchecked-get #js {} 1)))
  (is (nil? (unchecked-get #js [] 0)))
  (is (nil? (unchecked-get #js [1] -1)))
  (is (nil? (unchecked-get #js [1] 1)))
  (is (== 1 (unchecked-get #js [1] "0")))
  (is (nil? (unchecked-get [1] 0)))
  (is (== 1 (unchecked-get #js [1] 0)))
  (is (== 1 (unchecked-get #js {:foo 1} "foo"))))

(deftest unchecked-set-test
  (is (thrown? js/Error (unchecked-set nil 1 "x")))
  (is (= "x" (unchecked-set #js {} 1 "x")))
  (is (= "x" (unchecked-set #js [] 0 "x")))
  (is (= "x" (unchecked-set #js [1] -1 "x")))
  (is (= "x" (unchecked-set #js [1] 1 "x")))
  (is (= "x" (unchecked-set #js [1] "0" "x")))
  (is (= "x" (unchecked-set [1] 0 "x")))
  (is (= "x" (unchecked-set #js [1] 0 "x")))
  (let [v #js [1]]
    (unchecked-set v 0 "x")
    (is (= "x" (aget v 0))))
  (let [v #js {:foo 1}]
    (unchecked-set v "foo" "x")
    (is (= "x" (aget v "foo")))))

;; to suppress compile time warnings
(defn checked-aget-alias [& args]
  (apply checked-aget args))

(defn checked-aset-alias [& args]
  (apply checked-aset args))

(deftest checked-aget-test
  (suppress-errs
    (is (thrown? js/Error (checked-aget-alias nil 1)))
    (is (nil? (checked-aget-alias #js {} 1)))
    (is (nil? (checked-aget-alias #js [] 0)))
    (is (nil? (checked-aget-alias #js [1] -1)))
    (is (nil? (checked-aget-alias #js [1] 1)))
    (is (== 1 (checked-aget-alias #js [1] "0")))
    (is (nil? (checked-aget-alias [1] 0)))
    (is (== 1 (checked-aget-alias #js [1] 0)))
    (is (== 1 (checked-aget-alias #js {:foo 1} "foo")))
    (is (nil? (checked-aget-alias #js [#js {}] 0 0)))
    (is (nil? (checked-aget-alias #js [#js []] 0 0)))
    (is (nil? (checked-aget-alias #js [#js [1]] 0 -1)))
    (is (nil? (checked-aget-alias #js [#js [1]] 0 1)))
    (is (== 1 (checked-aget-alias #js [#js [1]] 0 "0")))
    (is (== 1 (checked-aget-alias #js [#js [1]] 0 0)))))

(deftest checked-aset-test
  (suppress-errs
    (is (thrown? js/Error (checked-aset-alias nil 1 "x")))
    (is (= "x" (checked-aset-alias #js {} 1 "x")))
    (is (= "x" (checked-aset-alias #js [] 0 "x")))
    (is (= "x" (checked-aset-alias #js [1] -1 "x")))
    (is (= "x" (checked-aset-alias #js [1] 1 "x")))
    (is (= "x" (checked-aset-alias #js [1] "0" "x")))
    (is (= "x" (checked-aset-alias [1] 0 "x")))
    (is (= "x" (checked-aset-alias #js [1] 0 "x")))
    (let [v #js [1]]
      (checked-aset-alias v 0 "x")
      (is (= "x" (aget v 0))))
    (let [v #js {:foo 1}]
      (checked-aset-alias v "foo" "x")
      (is (= "x" (aget v "foo"))))
    (is (= "x" (checked-aset-alias #js [#js {}] 0 0 "x")))
    (is (= "x" (checked-aset-alias #js [#js []] 0 0 "x")))
    (is (= "x" (checked-aset-alias #js [#js [1]] 0 -1 "x")))
    (is (= "x" (checked-aset-alias #js [#js [1]] 0 1 "x")))
    (is (= "x" (checked-aset-alias #js [#js [1]] 0 "0" "x")))
    (is (= "x" (checked-aset-alias #js [#js [1]] 0 0 "x")))
    (let [v #js [#js [1]]]
      (checked-aset-alias v 0 0 "x")
      (is (= "x" (aget v 0 0))))))

;; to suppress compile time warnings
(defn checked-aget'-alias [& args]
  (apply checked-aget' args))

(defn checked-aset'-alias [& args]
  (apply checked-aset' args))

(deftest checked-aget'-test
  (is (thrown? js/Error (checked-aget'-alias nil 1)))
  (is (thrown? js/Error (checked-aget'-alias #js {} 1)))
  (is (thrown? js/Error (checked-aget'-alias #js [] 0)))
  (is (thrown? js/Error (checked-aget'-alias #js [1] -1)))
  (is (thrown? js/Error (checked-aget'-alias #js [1] 1)))
  (is (thrown? js/Error (checked-aget'-alias #js [1] "0")))
  (is (thrown? js/Error (checked-aget'-alias [1] 0)))
  (is (== 1 (checked-aget'-alias #js [1] 0)))
  (is (thrown? js/Error (checked-aget'-alias #js [#js {}] 0 0)))
  (is (thrown? js/Error (checked-aget'-alias #js [#js []] 0 0)))
  (is (thrown? js/Error (checked-aget'-alias #js [#js [1]] 0 -1)))
  (is (thrown? js/Error (checked-aget'-alias #js [#js [1]] 0 1)))
  (is (thrown? js/Error (checked-aget'-alias #js [#js [1]] 0 "0")))
  (is (== 1 (checked-aget'-alias #js [#js [1]] 0 0))))

(deftest checked-aset'-test
  (is (thrown? js/Error (checked-aset'-alias nil 1 "x")))
  (is (thrown? js/Error (checked-aset'-alias #js {} 1 "x")))
  (is (thrown? js/Error (checked-aset'-alias #js [] 0 "x")))
  (is (thrown? js/Error (checked-aset'-alias #js [1] -1 "x")))
  (is (thrown? js/Error (checked-aset'-alias #js [1] 1 "x")))
  (is (thrown? js/Error (checked-aset'-alias #js [1] "0" "x")))
  (is (thrown? js/Error (checked-aset'-alias [1] 0 "x")))
  (is (= "x" (checked-aset'-alias #js [1] 0 "x")))
  (let [v #js [1]]
    (checked-aset'-alias v 0 "x")
    (is (= "x" (aget v 0))))
  (is (thrown? js/Error (checked-aset'-alias #js [#js {}] 0 0 "x")))
  (is (thrown? js/Error (checked-aset'-alias #js [#js []] 0 0 "x")))
  (is (thrown? js/Error (checked-aset'-alias #js [#js [1]] 0 -1 "x")))
  (is (thrown? js/Error (checked-aset'-alias #js [#js [1]] 0 1 "x")))
  (is (thrown? js/Error (checked-aset'-alias #js [#js [1]] 0 "0" "x")))
  (is (= "x" (checked-aset'-alias #js [#js [1]] 0 0 "x")))
  (let [v #js [#js [1]]]
    (checked-aset'-alias v 0 0 "x")
    (is (= "x" (aget v 0 0)))))
