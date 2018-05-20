;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.ns-test
  (:refer-clojure :exclude [+ for] :rename {mapv core-mapv})
  (:require-macros [clojure.core :as lang :refer [when when-let] :rename {when always
                                                                          when-let always-let}]
                   [cljs.test :refer [deftest is]])
  (:require [goog :as goog-alias]
            [cljs.test]
            [cljs.ns-test.foo :refer [baz]]
            [clojure.set :as s :refer [intersection] :rename {intersection itsc}]
            [cljs.analyzer :as ana])
  (:use [cljs.ns-test.bar :only [quux]]))

(def + -)

(deftest test-ns
  (is (= 4 (clojure.core/+ 2 1 1)))
  (is (= 0 (cljs.ns-test/+ 2 1 1)))
  (is (= 0 (+ 2 1 1)))
  (is (= 123 (baz)))
  (is (= 123 (quux)))

  (is (= (range 5) (lang/for [x (range 5)] x)))
  (is (= #{1 2 3} (s/union #{1} #{2 3}))))

(deftest test-cljs-1508
  (is (= (itsc #{1 2 3} #{2}) #{2}))
  (is (= #'itsc #'clojure.set/intersection))
  (is (= itsc clojure.set/intersection))
  (is (= (always true 42) 42))
  (is (= (core-mapv inc [1 2]) [2 3]))
  (is (= (always-let [foo 42] foo) 42)))

(deftest test-cljs-1677
  (is (.isNumber js/goog 3))
  (is (goog/isNumber 3))
  (is (goog-alias/isNumber 3)))
