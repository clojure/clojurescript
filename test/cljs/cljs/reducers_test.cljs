(ns cljs.reducers-test
  (:require
   [clojure.core.reducers :as r]))

(defn test-builtin-impls []
  (assert (= 0 (r/fold + nil))))

(defn test-all []
  (test-builtin-impls))
