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
