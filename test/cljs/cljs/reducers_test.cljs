(ns cljs.reducers-test
  (:require
   [clojure.core.reducers :as r]))

(defn test-builtin-impls []
  (assert (= 0 (r/fold + nil)))
  (assert (= [1 2 3 4] (seq (r/reduce r/append! (r/cat) [1 2 3 4]))))
  (assert (= 10 (r/reduce + (array 1 2 3 4))))
  (assert (= 11 (r/reduce + 1 (array 1 2 3 4))))
  (assert (= 10 (r/reduce + (list 1 2 3 4))))
  (assert (= 11 (r/reduce + 1 (list 1 2 3 4)))))

(defn test-all []
  (test-builtin-impls))
