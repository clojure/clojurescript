(ns cljs.npm-deps-test
  (:require [cljs.test :refer [deftest is]]
            ["lodash/array" :as array]))

(deftest test-module-processing
  (is (= (array/nth #js [1 2 3] 1) 2)))
