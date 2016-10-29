;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.tagged-literals-test
  (:require [cljs.test :refer-macros [deftest is]]
            [cljs.reader :as reader]))

(deftest test-identity-custom-literal
  (is (= #cljs/tag [1 2 3] [1 2 3])))

(deftest test-runtime-reader
  (is (object? (reader/read-string "#js {}")))
  (is (= {} (reader/read-string "#cljs/tag {}")))
  (is (= (reader/read-string "#cljs/inc 0") 1))
  (is (= (reader/read-string "#cljs/union #{1}") #{1})))
