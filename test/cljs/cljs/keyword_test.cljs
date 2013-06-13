(ns cljs.keyword-test
  (:require [cljs.keyword-other :as other]))

(defn test-keyword []
  (assert (= ::bar :cljs.keyword-test/bar))
  (assert (= ::other/foo :cljs.keyword-other/foo)))
