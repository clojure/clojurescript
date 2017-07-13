(ns cljs.npm-deps-test
  (:require [cljs.test :refer [deftest is]]
            ["lodash/array" :as array :refer [slice] :rename {slice slc}]
            [calculator :refer [add] :rename {add plus}]))

(deftest test-module-processing
  (is (= (array/nth #js [1 2 3] 1) 2))
  ;; rename works
  (is (= (array-seq (slc #js [1 2 3] 1)) [2 3])))

(deftest test-global-exports
  (is (= (plus 1 2) 3)))
