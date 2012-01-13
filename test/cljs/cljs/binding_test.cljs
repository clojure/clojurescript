(ns cljs.binding-test
  (:require [cljs.binding-test-other-ns :as o]))

(defn test-binding []
  (binding [o/*foo* 2]
    (assert (= o/*foo* 2)))
  (assert (= o/*foo* 1)))