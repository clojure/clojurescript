(ns cljs.keyword-test
  (:require [cljs.keyword-other :as other])
  (:require-macros [clojure.core :as cc]))

(defn test-keyword []
  (assert (= ::bar :cljs.keyword-test/bar))
  (assert (= ::other/foo :cljs.keyword-other/foo))
  (assert (= ::cc/foo :clojure.core/foo)))
