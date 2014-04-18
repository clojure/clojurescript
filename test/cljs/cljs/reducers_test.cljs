(ns cljs.reducers-test
  (:require
   [clojure.core.reducers :as r]))

(defn test-builtin-impls []
  (assert (= 0 (r/fold + nil)))
  (assert (= [1 2 3 4] (seq (r/reduce r/append! (r/cat) [1 2 3 4]))))
  (assert (= 10 (r/reduce + (array 1 2 3 4))))
  (assert (= 11 (r/reduce + 1 (array 1 2 3 4))))
  (assert (= 10 (r/reduce + (list 1 2 3 4))))
  (assert (= 11 (r/reduce + 1 (list 1 2 3 4))))
  (assert (= (r/fold + + [1 2 3])
             (r/fold + [1 2 3])
             (r/reduce + [1 2 3])
             6))
  (assert (= (r/fold + + (vec (range 2048)))
             (r/reduce + (vec (range 2048)))))
  (letfn [(f [[ks vs] k v]
              [(conj ks k) (conj vs v)])
          (g ([] [#{} #{}])
             ([[ks1 vs1] [ks2 vs2]]
              [(into ks1 ks2) (into vs1 vs2)]))]
    (assert (= (r/reduce f (g) {:a 1 :b 2 :c 3})
               (r/fold g f {:a 1 :b 2 :c 3})
               [#{:a :b :c} #{1 2 3}]))
    (let [m (into {} (for [x (range 2048)] [x (- x)]))]
      (assert (= (r/reduce f (g) m) (r/fold g f m)))))
  ;; CLJS-792
  (assert (= (into [] (r/map identity {})) [])))

(defn test-all []
  (test-builtin-impls))
