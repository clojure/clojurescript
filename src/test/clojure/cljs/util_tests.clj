;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.util-tests
  (:require [cljs.util :as util]
            [clojure.java.io :as io])
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

(deftest test-relative-name
  (let [initial (System/getProperty "user.dir")]
    (System/setProperty "user.dir" "/Users/user/clojurescript")
    (is (= (util/relative-name (io/file "/Users/user/clojurescript/out/index.js")) "out/index.js"))
    (is (= (util/relative-name (io/as-url (io/file "/Users/user/clojurescript/out/index.js"))) "out/index.js"))
    (System/setProperty "user.dir" initial)))
