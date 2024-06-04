;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.macro-test
  (:refer-clojure :exclude [==])
  (:require [cljs.test :as test :refer-macros [deftest is]])
  (:use-macros [cljs.macro-test.macros :only [== sm-cljs-3027]])
  (:require-macros [cljs.macro-test.cljs2852]
                   [single-seg-macros]))

(deftest test-macros
  (is (= (== 1 1) 2)))

(deftest macroexpansion
  (is (= 1 (macroexpand-1 '1)))
  (is (= '(if true (do 1)) (macroexpand-1 '(when true 1))))
  (is (= 1 (macroexpand '1)))
  (is (= '(if true (do 1)) (macroexpand '(when true 1)))))

(deftest test-cljs-2283
  (is (= ":a" (first (js-keys (js-obj :a 1))))))

(deftest test-cljs-2852
  (is (= '([x])) (cljs.macro-test.cljs2852/beta))
  (is (= '([x] [x y])) (cljs.macro-test.cljs2852/delta))
  (is (= '([x] [x & xs])) (cljs.macro-test.cljs2852/zeta)))

(deftest test-cljs-3027
  (is (= {"a" "b"} (sm-cljs-3027))))

(deftest test-cljs-3413
  (is (= 5 (single-seg-macros/test-macro 2 3))))
