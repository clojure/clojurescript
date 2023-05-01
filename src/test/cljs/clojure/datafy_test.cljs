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
          x (with-meta [1 2 3] {`clojure.core.protocols/datafy (fn [_] datafied)})]
      (is (= datafied (d/datafy x)))))
  (testing "Datafy adds ::obj metadata when return value != original value and supports metadata"
    (let [datafied [2 3 4]
          original [1 2 3]
          x (with-meta original {`clojure.core.protocols/datafy (fn [_] datafied)})]
      (is (= datafied (d/datafy x)))
      (is (= {:clojure.datafy/obj original} (meta (d/datafy x)))))))

(deftest datafy-js-errors-test
  (let [x (js/Error. "foo")]
    (is (= (Throwable->map x) (d/datafy x))))
  ;; Ensure we can datafy objects that extend js/Error
  (let [x (js/RangeError. "x must be between 1 and 5")]
    (is (= (Throwable->map x) (d/datafy x)))))

(deftest datafy-ex-info-test
  (let [x (ex-info "foo" {:a 1} (ex-info "bar" {:b 2}))]
    (is (= (Throwable->map x) (d/datafy x)))))
