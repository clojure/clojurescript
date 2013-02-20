(ns cljs.benchmark-runner
  (:refer-clojure :exclude [println])
  (:require [cljs.reader :as reader]))

(def println print)

(set! *print-fn* js/print)

(simple-benchmark [x 1] (identity x) 1000000)

(println ";; symbol construction")
(simple-benchmark [] (symbol 'foo) 1000000)
(println)

(println ";; array-reduce & ci-reduce")
(def arr (let [arr (array)]
           (dotimes [i 1000000]
             (.push arr i))
           arr))
(defn sum [a b] (+ a b))
(simple-benchmark [coll (seq arr)] (ci-reduce coll + 0) 1)
(simple-benchmark [coll (seq arr)] (ci-reduce coll sum 0) 1)
(simple-benchmark [coll arr] (array-reduce coll + 0) 1)
(simple-benchmark [coll arr] (array-reduce coll sum 0) 1)

(println ";;; instance?")
;; WARNING: will get compiled away under advanced
(simple-benchmark [coll []] (instance? PersistentVector coll) 1000000)
(println ";;; satisfies?")
(simple-benchmark [coll (list 1 2 3)] (satisfies? ISeq coll) 1000000)
(simple-benchmark [coll [1 2 3]] (satisfies? ISeq coll) 1000000)
(println)

(println ";;; list ops")
(simple-benchmark [coll (list 1 2 3)] (first coll) 1000000)
(simple-benchmark [coll (list 1 2 3)] (-first coll) 1000000)
(simple-benchmark [coll (list 1 2 3)] (rest coll) 1000000)
(simple-benchmark [coll (list 1 2 3)] (-rest coll) 1000000)
(simple-benchmark [] (list) 1000000)
(simple-benchmark [] (list 1 2 3) 1000000)
(println)

(println ";;; vector ops")
(simple-benchmark [] [] 1000000)
(simple-benchmark [] [1 2 3] 1000000)
(simple-benchmark [coll [1 2 3]] (transient coll) 100000)
(simple-benchmark [coll [1 2 3]] (nth coll 0) 1000000)
(simple-benchmark [coll [1 2 3]] (-nth coll 0) 1000000)
(simple-benchmark [coll [1 2 3]] (conj coll 4) 1000000)
(simple-benchmark [coll [1 2 3]] (-conj coll 4) 1000000)
(simple-benchmark [coll [1 2 3]] (seq coll) 1000000)
(simple-benchmark [coll [1 2 3]] (-seq coll) 1000000)
(simple-benchmark [coll (seq [1 2 3])] (first coll) 1000000)
(simple-benchmark [coll (seq [1 2 3])] (-first coll) 1000000)
(simple-benchmark [coll (seq [1 2 3])] (rest coll) 1000000)
(simple-benchmark [coll (seq [1 2 3])] (-rest coll) 1000000)
(simple-benchmark [coll (seq [1 2 3])] (next coll) 1000000)
(println)

(println ";;; large vector ops")
(simple-benchmark [] (reduce conj [] (range 40000)) 10)
(simple-benchmark [coll (reduce conj [] (range (+ 32768 32)))] (conj coll :foo) 100000)
(simple-benchmark [coll (reduce conj [] (range 40000))] (assoc coll 123 :foo) 100000)
(simple-benchmark [coll (reduce conj [] (range (+ 32768 33)))] (pop coll) 100000)
(println)

(println ";;; chunked seqs")
(let [v (seq (into [] (range 64)))]
  (simple-benchmark [] (-first v) 1000000)
  (simple-benchmark [] (-next v) 1000000)
  (simple-benchmark [] (-rest v) 1000000))
(println)

(println ";;; transients")
(print "transient vector, conj! 1000000 items")
(time
 (let [v (transient [])]
   (loop [i 0 v v]
     (if (> i 1000000)
       (persistent! v)
       (recur (inc i) (conj! v i))))))

(println ";;; reduce lazy-seqs, vectors, ranges")
(simple-benchmark [coll (take 100000 (iterate inc 0))] (reduce + 0 coll) 1)
(simple-benchmark [coll (range 1000000)] (reduce + 0 coll) 1)
(simple-benchmark [coll (into [] (range 1000000))] (reduce + 0 coll) 1)
(println)

(println ";; apply")
(simple-benchmark [coll (into [] (range 1000000))] (apply + coll) 1)
(simple-benchmark [] (list 1 2 3 4 5) 1000000)
(simple-benchmark [xs (array-seq (array 1 2 3 4 5))] (apply list xs) 1000000)
(simple-benchmark [xs (list 1 2 3 4 5)] (apply list xs) 1000000)
(simple-benchmark [xs [1 2 3 4 5]] (apply list xs) 1000000)
(println)

(println ";; update-in")
(simple-benchmark [coll {:foo 1} ks [:foo]] (update-in coll ks inc) 1000000)
(simple-benchmark [coll (array-map :foo 1) ks [:foo]] (update-in coll ks inc) 1000000)
(println)

(println ";;; obj-map")
(simple-benchmark [coll (obj-map)] (assoc coll :foo :bar) 1000000)
(simple-benchmark [coll (obj-map :foo :bar)] (-lookup coll :foo) 1000000)
(simple-benchmark [coll (obj-map :foo :bar)] (assoc coll :baz :woz) 1000000)
(simple-benchmark [coll (obj-map :foo :bar :baz :woz)] (-lookup coll :baz) 1000000)
(simple-benchmark [coll (obj-map :foo :bar :baz :woz :lol :rofl)] (-lookup coll :lol) 1000000)
(println)

(println ";;; array-map")
(simple-benchmark [] {[1] true [2] true [3] true} 1000000)
(simple-benchmark [coll (array-map)] (assoc coll :foo :bar) 1000000)
(simple-benchmark [coll (array-map :foo :bar)] (-lookup coll :foo) 1000000)
(simple-benchmark [coll (array-map :foo :bar)] (assoc coll :baz :woz) 1000000)
(simple-benchmark [coll (array-map :foo :bar :baz :woz)] (-lookup coll :baz) 1000000)
(simple-benchmark [coll (array-map :foo :bar :baz :woz :lol :rofl)] (-lookup coll :lol) 1000000)
(println)
(def data-atom (atom {:x 0}))

(println ";;; map / record ops")
(simple-benchmark [coll {:foo 1 :bar 2}] (get coll :foo) 1000000)
(simple-benchmark [coll {'foo 1 'bar 2}] (get coll 'foo) 1000000)
(simple-benchmark [coll {:foo 1 :bar 2}] (-lookup coll :foo nil) 1000000)
(simple-benchmark [coll {'foo 1 'bar 2}] (-lookup coll 'foo nil) 1000000)
(simple-benchmark [coll {:foo 1 :bar 2}] (:foo coll) 1000000)
(simple-benchmark [coll {'foo 1 'bar 2}] ('foo coll) 1000000)
(defrecord Foo [bar baz])
(simple-benchmark [coll (Foo. 1 2)] (:bar coll) 1000000)
(simple-benchmark [coll {:foo 1 :bar 2}] (assoc coll :baz 3) 100000)
(simple-benchmark [coll {'foo 1 'bar 2}] (assoc coll 'baz 3) 100000)
(simple-benchmark [coll {:foo 1 :bar 2}] (assoc coll :foo 2) 100000)
(simple-benchmark [coll {'foo 1 'bar 2}] (assoc coll 'foo 2) 100000)
(simple-benchmark [coll {:foo 1 :bar 2}]
                  (loop [i 0 m coll]
                    (if (< i 100000)
                      (recur (inc i) (assoc m :foo 2))
                      m))
                  1)
(simple-benchmark [coll {'foo 1 'bar 2}]
                  (loop [i 0 m coll]
                    (if (< i 100000)
                      (recur (inc i) (assoc m 'foo 2))
                      m))
                  1)
(println ";;; persistent hash maps")
(def pmap (into cljs.core.PersistentHashMap/EMPTY
            [[:a 0] [:b 1] [:c 2] [:d 3] [:e 4] [:f 5] [:g 6] [:h 7]
             [:i 8] [:j 9] [:k 10] [:l 11] [:m 12] [:n 13] [:o 14] [:p 15]
             [:q 16] [:r 17] [:s 18] [:t 19] [:u 20] [:v 21] [:w 22] [:x 23]
             [:y 24] [:z 25] [:a0 26] [:b0 27] [:c0 28] [:d0 29] [:e0 30] [:f0 31]]))
(simple-benchmark [key :f0] (hash key) 1000000)
(simple-benchmark [key :unsynchronized-mutable] (hash key false) 1000000)
(simple-benchmark [key :unsynchronized-mutable] (hash key) 1000000)
(def hash-coll-test
  (loop [i 0 r []]
    (if (< i 1000)
      (recur (inc i) (conj r (str "foo" i)))
      r)))
(simple-benchmark [coll hash-coll-test] (hash-coll coll) 100)
(simple-benchmark [coll pmap] (:f0 coll) 1000000)
(simple-benchmark [coll pmap] (get coll :f0) 1000000)
(simple-benchmark [coll pmap] (-lookup coll :f0 nil) 1000000)
(simple-benchmark [coll pmap] (assoc coll :g0 32) 1000000)
(simple-benchmark [coll pmap]
                  (loop [i 0 m coll]
                    (if (< i 1000000)
                      (recur (inc i) (assoc m :a 1))
                      m))
                  1)
(simple-benchmark [coll cljs.core.PersistentHashMap/EMPTY] (assoc coll :f0 1) 1000000)
(println)

(println ";;; set ops")
(simple-benchmark [] #{} 1000000)
(simple-benchmark [] #{1 2 3} 1000000)
(simple-benchmark [v [1 2 3]] (set v) 1000000)
(simple-benchmark [] (hash-set 1 2 3) 1000000)
(simple-benchmark [coll #{1 2 3}] (conj coll 4) 1000000)
(println)

(println ";;; seq ops")
(simple-benchmark [coll (range 500000)] (reduce + coll) 1)
(println)

(println ";;; reader")
(simple-benchmark [s "{:foo [1 2 3]}"] (reader/read-string s) 1000)
(println)

(println ";;; range")
(simple-benchmark [r (range 1000000)] (last r) 1)
(println)

(defn ints-seq
  ([n] (ints-seq 0 n))
  ([i n]
     (when (< i n)
       (lazy-seq
        (cons i (ints-seq (inc i) n))))))
(def r (ints-seq 1000000))
(println ";;; lazy-seq")
(println ";;; first run")
(simple-benchmark [r r] (last r) 1)
(println ";;; second run")
(simple-benchmark [r r] (last r) 1)
(println)

(println "\n")
