(ns cljs.keyword-test
  (:require-macros [clojure.core :as cc]
                   [cljs.test :refer [deftest is]])
  (:require [cljs.keyword-other :as other]
            [cljs.test]))

(deftest test-keyword
  (is (= ::bar :cljs.keyword-test/bar))
  (is (= ::other/foo :cljs.keyword-other/foo))
  (is (= ::cc/foo :clojure.core/foo)))
