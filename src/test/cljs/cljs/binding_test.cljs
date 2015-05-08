(ns cljs.binding-test
  (:require [cljs.test :refer-macros [deftest is]]
            [cljs.binding-test-other-ns :as o]))

(deftest test-binding
  (is (binding [o/*foo* 2]
        (= o/*foo* 2)))
  (is (= o/*foo* 1)))

(deftest test-with-redefs
  (is (with-redefs [o/bar 2]
        (= o/bar 2)))
  (is (= o/bar 10)))
