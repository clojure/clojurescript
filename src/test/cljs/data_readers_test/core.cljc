(ns data-readers-test.core)

(def custom-identity identity)

(assert (= 1 #test/custom-identity 1))
