(ns cljs.macro-test
  (:refer-clojure :exclude [==])
  (:require [cljs.test :refer-macros [deftest is]])
  (:use-macros [cljs.macro-test.macros :only [==]]))

(deftest test-macros
  (is (= (== 1 1) 2)))
