;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.hash-set-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is]]))

(deftest test-hash-set-with-duplicate-keys
  (testing "Testing duplicate keys in array maps"
    ;; runtime
    (is (= [:foo] (keys (apply array-map [:foo :foo]))))
    (let [sym-a (with-meta 'foo :first)
          sym-b (with-meta 'foo :second)]
      (is (= #{sym-a} (apply hash-set [sym-a sym-b]))))
    ;; compile-time
    (is (= {:foo 2} (hash-set :foo :foo)))
    (let [sym-a (with-meta 'foo :first)
          sym-b (with-meta 'foo :second)]
      (is (= #{sym-a} (hash-set sym-a sym-b))))))
