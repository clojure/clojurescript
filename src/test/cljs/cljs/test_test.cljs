;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.test-test
  (:require [cljs.test :refer-macros [deftest testing is] :as ct]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn- nan?
  [x]
  (and (number? x)
       (js/isNaN x)))

(deftest js-line-and-column-test
  (is (= [2 3] (ct/js-line-and-column "foo:bar:2:3")))
  (is (= [2 3] (ct/js-line-and-column "foo:2:3")))
  (is (= [2 3] (ct/js-line-and-column "2:3")))
  (let [[line column] (ct/js-line-and-column "foo:bogus:3")]
    (is (nan? line))
    (is (== 3 column)))
  (let [[line column] (ct/js-line-and-column "foo:2:bogus")]
    (is (== 2 line))
    (is (nan? column)))
  (let [[line column] (ct/js-line-and-column "foo:bogus:bogus")]
    (is (nan? line))
    (is (nan? column)))
  (let [[line column] (ct/js-line-and-column "foo:3")]
    (is (nan? line))
    (is (== 3 column)))
  (let [[line column] (ct/js-line-and-column "foo")]
    (is (nan? line))
    (is (nan? column))))

(deftest test-js-filename
  (is (= "core-advanced-test.js" (ct/js-filename (str "nW@" (ct/cljs-output-dir) "/core-advanced-test.js:1191:77")))))
