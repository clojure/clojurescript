(ns cljs.keyword-test
  (:require [cljs.keyword-other :as other]))

(defn test-keyword []  
  (assert (= ::other/foo :cljs.keyword-other/foo)))
