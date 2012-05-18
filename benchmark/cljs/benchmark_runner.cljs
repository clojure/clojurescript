(ns cljs.benchmark-runner
  (:refer-clojure :exclude [println]))

(def println print)

(set! *print-fn* js/print)

(println ";;; satisfies?")
(simple-benchmark [coll (list 1 2 3)] (satisfies? ISeq coll) 10000000)
(simple-benchmark [coll [1 2 3]] (satisfies? ISeq coll) 10000000)
(println)

(println ";;; list ops")
(simple-benchmark [coll (list 1 2 3)] (first coll) 1000000)
(simple-benchmark [coll (list 1 2 3)] (-first coll) 1000000)
(simple-benchmark [coll (list 1 2 3)] (rest coll) 1000000)
(simple-benchmark [coll (list 1 2 3)] (next coll) 1000000)
(println)

(println ";;; vector ops")
(simple-benchmark [coll [1 2 3]] (first coll) 1000000)
(simple-benchmark [coll (seq [1 2 3])] (first coll) 1000000)
(simple-benchmark [coll [1 2 3]] (conj coll 4) 1000000)
(simple-benchmark [coll [1 2 3]] (-conj coll 4) 1000000)
(simple-benchmark [coll [1 2 3]] (nth coll 1) 1000000)
(simple-benchmark [coll [1 2 3]] (-nth coll 1) 1000000)
(simple-benchmark [coll [1 2 3]] (rest coll) 1000000)
(simple-benchmark [coll [1 2 3]] (next coll) 1000000)
(println)

(println ";;; seq ops")
(simple-benchmark [coll (range 500000)] (reduce + coll) 1)
(println)

(println "\n")
