;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns clojure.datafy-test
  (:require [cljs.test :as test
             :refer-macros [deftest is testing]]
            [clojure.datafy :as d]))

(deftest datafy-test
  (testing "Datafy works when datafied value is arbitrary JS objects"
    (let [datafied #js {}
          x (with-meta [1 2 3] {:clojure.datafy/datafy (fn [_] datafied)})]
      (is (= datafied (d/datafy x)))))
  (testing "Datafy adds ::obj metadata when return value != original value and supports metadata"
    (let [datafied [2 3 4]
          original [1 2 3]
          x (with-meta original {:clojure.datafy/datafy (fn [_] datafied)})]
      (is (= datafied (d/datafy x)))
      (is (= {:clojure.datafy/obj original} (meta (d/datafy x)))))))
