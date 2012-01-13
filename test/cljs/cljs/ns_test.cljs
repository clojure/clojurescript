(ns cljs.ns-test
  (:refer-clojure :exclude [+]))

(def + -)

(defn test-ns []
  (assert (= 4 (clojure.core/+ 2 1 1)))
  (assert (= 0 (cljs.ns-test/+ 2 1 1)))
  (assert (= 0 (+ 2 1 1)))
  :ok)
