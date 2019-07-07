;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.externs-parsing-test
  (:require [cljs.closure :as closure]
            [cljs.externs :as externs]
            [clojure.java.io :as io]
            [clojure.test :as test :refer [deftest is]]))

(deftest cljs-3121
  (let [externs (externs/parse-externs
                  (closure/js-source-file "goog/string/string.js"
                    (io/input-stream (io/resource "goog/string/string.js"))))]
    (is (every?
          (fn [xs]
            (= (count (distinct xs))
               (count xs)))
          externs))))
