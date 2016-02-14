(ns cljs.util-tests
  (:require [cljs.util :as util])
  (:use clojure.test))

(deftest test-levenshtein-distance
  (testing "levenshtein-distance"
    (is (= 0 (util/levenshtein-distance "abc" "abc")))
    (is (= 1 (util/levenshtein-distance "abc" "abcd")))
    (is (= 1 (util/levenshtein-distance "abcd" "abc")))
    (is (= 3 (util/levenshtein-distance "kitten" "sitting")))))

(deftest test-suggestion
  (testing "suggestion"
    (is (= ":optimization" (util/suggestion 3 ":optimization" [":optimization" ":static-fns"])))))

(deftest test-unknown-opts
  (testing "unknown-opts"
    (is (= [[:bogus nil]
            [:optimisations :optimizations]]
          (sort (util/unknown-opts #{:optimisations :bogus} #{:optimizations :static-fns}))))))
