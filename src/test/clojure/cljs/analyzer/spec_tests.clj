;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.spec-tests
  (:require [cljs.analyzer-tests :refer [analyze ns-env]]
            [cljs.analyzer.specs :as a]
            [clojure.test :as test :refer [deftest is]]
            [clojure.spec.alpha :as s]))

(deftest test-if
  (is (s/valid? ::a/node (analyze ns-env '(if true true))))
  (is (s/valid? ::a/node (analyze ns-env '(if true true false)))))

(comment

  (test/run-tests)

  )
