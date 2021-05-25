;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.transpile-tests
  (:require [cljs.closure :as closure :refer [closure-transpile]]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is run-tests]]))

(deftest test-transpile-lang-in-lang-out
  (let [source (closure-transpile
                 (io/resource "goog/async/throttle.js")
                 {:language-in :es6 :language-out :es6})]
    (is (nil? (re-find #"jscomp" source))))
  (let [source (closure-transpile
                 (io/resource "goog/async/throttle.js")
                 {:language-in :es6 :language-out :es5})]
    (is (some? (re-find #"jscomp" source)))))
