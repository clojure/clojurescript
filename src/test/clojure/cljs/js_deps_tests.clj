;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.js-deps-tests
  (:require [cljs.js-deps :as js-deps]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is run-tests]]))

(deftest test-parse-js-ns-returns-require-types
  (let [ns-info (js-deps/parse-js-ns
                  (line-seq (io/reader (io/resource "goog/events/eventhandler.js"))))]
    (is (true? (contains? ns-info :require-types)))))

(deftest test-js-dependency-index-has-require-types
  (let [deps (js-deps/build-index (js-deps/goog-dependencies*))
        ns-info (get deps "goog.events.EventHandler")]
    (is (true? (contains? ns-info :require-types)))))
