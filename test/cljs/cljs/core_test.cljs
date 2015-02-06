(ns cljs.core-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is]]
            [clojure.string :as s]
            [clojure.set :as set]))

(deftest test-js-primitives
  ;; js primitives
  (let [keys #(vec (js-keys %))]
    (testing "Testing js primitives"
      (is (= [] (keys (js-obj)) (keys (apply js-obj []))))
      (is (= ["x"] (keys (js-obj "x" "y")) (keys (apply js-obj ["x" "y"])))))))

(deftest test-equiv
  (testing "Testing -equiv"
    (is (= 1))
    (is (= 1 1))
    (is (= 1 1 1))
    (is (= 1 1 1 1))
    (is (not (= 1 2)))
    (is (not (= 1 2 1)))
    (is (not (= 1 1 2)))
    (is (not (= 1 1 2 1)))
    (is (not (= 1 1 1 2)))))

(deftest test-arithmetic
  (testing "Testing addition"
    (is (= (+) 0))
    (is (= (apply + []) 0))
    (is (= (+ 1) 1))
    (is (= (apply + [1]) 1))
    (is (= (+ 1 1) 2))
    (is (= (apply + [1 1]) 2))
    (is (= (+ 1 2 3) 6))
    (is (= (apply + [1 2 3]) 6)))

  (testing "Testing subtraction"
    (is (= (- 1) -1))
    (is (= (apply - [1]) -1))
    (is (= (- 1 1) 0))
    (is (= (apply - [1 1]) 0))
    (is (= (- 3 2 1) 0))
    (is (= (apply - [3 2 1]) 0)))

  (testing "Testing multiplication"
    (is (= (*) 1))
    (is (= (apply * []) 1))
    (is (= (* 2) 2))
    (is (= (apply * [2]) 2))
    (is (= (* 2 3) 6))
    (is (= (apply * [2 3]) 6)))

  (testing "Testing division"
    (is (= (/ 2) 0.5))
    (is (= (apply / [2]) 0.5))
    (is (= (/ 6 2) 3))
    (is (= (apply / [6 2]) 3))
    (is (= (/ 6 3 2) 1))
    (is (= (apply / [6 3 2]) 1)))
  
  (testing "Testing less than"
    (is (= (< 1) true))
    (is (= (apply < [1]) true))
    (is (= (< 1 2) true))
    (is (= (apply < [1 2]) true))
    (is (= (< 1 1) false))
    (is (= (apply < [1 1]) false))
    (is (= (< 2 1) false))
    (is (= (apply < [2 1]) false))
    (is (= (< 1 2 3) true))
    (is (= (apply < [1 2 3]) true))
    (is (= (< 1 1 3) false))
    (is (= (apply < [1 1 3]) false))
    (is (= (< 3 1 1) false))
    (is (= (apply < [3 1 1]) false)))
  
  (testing "Testing less than or equal to"
    (is (= (<= 1) true))
    (is (= (apply <= [1]) true))
    (is (= (<= 1 1) true))
    (is (= (apply <= [1 1]) true))
    (is (= (<= 1 2) true))
    (is (= (apply <= [1 2]) true))
    (is (= (<= 2 1) false))
    (is (= (apply <= [2 1]) false))
    (is (= (<= 1 2 3) true))
    (is (= (apply <= [1 2 3]) true))
    (is (= (<= 1 1 3) true))
    (is (= (apply <= [1 1 3]) true))
    (is (= (<= 3 1 1) false))
    (is (= (apply <= [3 1 1]) false)))
  
  (testing "Testing greater than"
    (is (= (> 1) true))
    (is (= (apply > [1]) true))
    (is (= (> 2 1) true))
    (is (= (apply > [2 1]) true))
    (is (= (> 1 1) false))
    (is (= (apply > [1 1]) false))
    (is (= (> 1 2) false))
    (is (= (apply > [1 2]) false))
    (is (= (> 3 2 1) true))
    (is (= (apply > [3 2 1]) true))
    (is (= (> 3 1 1) false))
    (is (= (apply > [3 1 1]) false))
    (is (= (> 1 1 3) false))
    (is (= (apply > [1 1 3]) false)))
  
  (testing "Testing greater than or equal to"
    (is (= (>= 1) true))
    (is (= (apply >= [1]) true))
    (is (= (>= 2 1) true))
    (is (= (apply >= [2 1]) true))
    (is (= (>= 1 1) true))
    (is (= (apply >= [1 1]) true))
    (is (= (>= 1 2) false))
    (is (= (apply >= [1 2]) false))
    (is (= (>= 3 2 1) true))
    (is (= (apply >= [3 2 1]) true))
    (is (= (>= 3 1 1) true))
    (is (= (apply >= [3 1 1]) true))
    (is (= (>= 3 1 2) false))
    (is (= (apply >= [3 1 2]) false))
    (is (= (>= 1 1 3) false))
    (is (= (apply >= [1 1 3]) false)))
  
  (testing "Testing dec/inc"
    (is (= (dec 1) 0))
    (is (= (apply dec [1]) 0))
    (is (= (inc 0) 1))
    (is (= (apply inc [0]) 1)))

  (testing "Testing zero? pos? neg? even? odd?"
    (is (= (zero? 0) true))
    (is (= (apply zero? [0]) true))
    (is (= (zero? 1) false))
    (is (= (apply zero? [1]) false))
    (is (= (zero? -11) false))
    (is (= (apply zero? [-11]) false))
    (is (= (pos? 0) false))
    (is (= (apply pos? [0]) false))
    (is (= (pos? 1) true))
    (is (= (apply pos? [1]) true))
    (is (= (pos? -1) false))
    (is (= (apply pos? [-1]) false))
    (is (= (neg? -1) true))
    (is (= (apply neg? [-1]) true))
    (is (neg? -1))
    (is (not (neg? 1)))
    (is (neg? -1.765))
    (is (not (neg? 0)))
    (is (= [true false true false true false true false]
          (map integer?
            [1 1.00001 0x7e7 [] (- 88 1001991881) :foo 0 "0"])))
    (is (= [true false true false true false]
          (map odd? [1 2 3 4 -1 0])))
    (is (= [true false true false true true]
          (map even? [2 3 4 5 -2 0]))))
  
  (testing "Testing max / min"
    (is (= (max 1) 1))
    (is (= (apply max [1]) 1))
    (is (= (max 1 2) 2))
    (is (= (apply max [1 2]) 2))
    (is (= (max 2 1) 2))
    (is (= (apply max [2 1]) 2))
    (is (= (max 1 2 3) 3))
    (is (= (apply max [1 2 3]) 3))
    (is (= (max 1 3 2) 3))
    (is (= (apply max [1 3 2]) 3))

    (is (= (min 1) 1))
    (is (= (apply min [1]) 1))
    (is (= (min 1 2) 1))
    (is (= (apply min [1 2]) 1))
    (is (= (min 2 1) 1))
    (is (= (apply min [2 1]) 1))
    (is (= (min 1 2 3) 1))
    (is (= (apply min [1 2 3]) 1))
    (is (= (min 2 1 3) 1))
    (is (= (apply min [3 1 3]) 1)))

  (testing "Testing mod"
    (is (= (mod 4 2) 0))
    (is (= (apply mod [4 2]) 0))
    (is (= (mod 3 2) 1))
    (is (= (apply mod [3 2]) 1))
    (is (= (mod -2 5) 3)))

  (testing "Testing numeric equality in collections"
    (is (= [4 3 2 1 0]
          (loop [i 0 j ()]
            (if (< i 5)
              (recur (inc i) (conj j (fn [] i)))
              (map #(%) j)))))
    (is (= [[1 1] [1 2] [1 3] [2 1] [2 2] [2 3]]
          (map #(%) (for [i [1 2] j [1 2 3]] (fn [] [i j]))))))

  (testing "Testing integer? predicate"
    (is (integer? 0))
    (is (integer? 42))
    (is (integer? -42))
    (is (not (integer? "")))
    (is (not (integer? 1e308)))
    (is (not (integer? js/Infinity)))
    (is (not (integer? (- js/Infinity))))
    (is (not (integer? js/NaN))))

  (testing "Testing integer coercions"
    (is (= 42 (int 42.5)))
    (is (integer? (int 42.5)))
    (is (= 42 (long 42.5)))
    (is (integer? (long 42.5)))
    (is (= -1 (int -1.5)))
    (is (= -9 (long -9.8))))

  (testing "Testing numeric equality from collection"
    (is (= 2 (:b {:a 1 :b 2})))
    (is (= 2 ('b '{:a 1 b 2})))
    (is (= 2 ({:a 1 :b 2} :b)))
    (is (= 2 ({1 1 2 2} 2)))
    (is (= 2 (:a {:b 1} 2)))
    (is (= 2 (:a {} 2)))
    (is (= 2 ({:b 1} :a 2)))
    (is (= 2 ({} :a 2)))
    (is (= nil (:a {})))
    (is (= nil (:a "")))
    (is (= 2 (:a "" 2)))
    (is (= 2 (#{1 2 3} 2)))
    (is (= 1 (apply :a '[{:a 1 a 2}])))
    (is (= 1 (apply 'a '[{a 1 :b 2}])))
    (is (= 1 (apply {:a 1} [:a])))
    (is (= 2 (apply {:a 1} [:b 2]))))

  (testing "Testing quot"
    (is (= (quot 4 2) 2))
    (is (= (quot 3 2) 1))
    (is (= (quot 6 4) 1))
    (is (= (quot 0 5) 0))
    (is (= (quot 42 5) 8))
    (is (= (quot 42 -5) -8))
    (is (= (quot -42 -5) 8))
    (is (= (quot 9 3) 3))
    (is (= (quot 9 -3) -3))
    (is (= (quot -9 3) -3))
    (is (= (quot 2 -5) 0))
    (is (= (quot -2 5) 0))
    (is (= (quot 0 3) 0))
    (is (= (quot 0 -3) 0)))

  (testing "Testing mod"
    (is (= (mod 4 2) 0))
    (is (= (mod 3 2) 1))
    (is (= (mod 6 4) 2))
    (is (= (mod 0 5) 0))
    (is (= (mod 4.5 2.0) 0.5))
    (is (= (mod 42 5) 2))
    (is (= (mod 9 3) 0))
    (is (= (mod 9 -3) 0))
    (is (= (mod -9 3) 0))
    (is (= (mod -9 -3) 0))
    (is (= (mod 0 3) 0))
    (is (= (mod 3216478362187432 432143214) 120355456)))
  
  (testing "Testing rem"
    (is (= (rem 4 2) 0))
    (is (= (rem 0 5) 0))
    (is (= (rem 4.5 2.0) 0.5))
    (is (= (rem 42 5) 2))
    (is (= (rem 2 5) 2))
    (is (= (rem 2 -5) 2))
    (is (= (rem 0 3) 0)))
)

(deftest test-hash-null
  (is (zero? (hash (aget (js-obj) "foo")))))

;; See
;; https://github.com/clojure/tools.reader#differences-from-lispreaderjava
;; about why these tests won't pass. Not clear if we should change the reader
;; or the test
;; (assert (= "baz" (name 'foo/bar/baz)))
;; (assert (= "foo/bar" (namespace 'foo/bar/baz)))
;; (assert (= "baz" (name :foo/bar/baz)))
;; (assert (= "foo/bar" (namespace :foo/bar/baz)))
;; TODO: These next two tests need Clojure 1.5
;; (assert (= "foo" (namespace 'foo//)))
;; (assert (= "/" (name 'foo//)))

(deftest test-symbols-and-keywords
  (testing "Testing name / namespace"
    (is (nil? (namespace '/)))
    (is (= "/" (name '/)))
    (is (= "keyword" (name :keyword))))

  (testing "Testing str on keywords / symbols"
    (is (= ":hello" (str :hello)))
    (is (= "hello" (str 'hello)))
    (is (= "hello:world" (str "hello" :world)))
    (is (= ":helloworld" (str :hello 'world))))

  (testing "Testing symbol ctor is idempotent"
    (is (= 'a (symbol 'a))))

  (testing "Testing keyword ctor"
    (is (= :a (keyword "a")))
    (is (= :a (keyword 'a)))
    (is (= :a/b (keyword 'a 'b)))
    (is (= :a (keyword :a)))))

(deftest test-map-operations
  (testing "Test basic map collection operations"
    (is (= {:a :b} (get {[1 2 3] {:a :b}, 4 5} [1 2 3])))
    (is (not (= {:a :b :c nil} {:a :b :d nil})))
    (is (= {:a :b} (dissoc {:a :b :c :d} :c)))
    (is (= (hash-map :foo 5)
          (assoc (cljs.core.ObjMap. nil (array) (js-obj)) :foo 5))))
  (testing "Testing assoc dissoc"
    (is (= {1 2 3 4} (assoc {} 1 2 3 4)))
    (is (= {1 2} (assoc {} 1 2)))
    (is (= [42 2] (assoc [1 2] 0 42)))
    (is (= {} (dissoc {1 2 3 4} 1 3)))
    (is (= {1 2} (dissoc {1 2 3 4} 3)))
    (is (nil? (dissoc nil :foo))))
  (testing "Testing find"
    (is (= (find {} :a) nil))
    (is (= (find {:a 1} :a) [:a 1]))
    (is (= (find {:a 1} :b) nil))
    (is (= (find {:a 1 :b 2} :a) [:a 1]))
    (is (= (find {:a 1 :b 2} :b) [:b 2]))
    (is (= (find {:a 1 :b 2} :c) nil))
    (is (= (find {} nil) nil))
    (is (= (find {:a 1} nil) nil))
    (is (= (find {:a 1 :b 2} nil) nil))
    (is (= (find [1 2 3] 0) [0 1])))
)

(deftest test-metadata
  (testing "Testing metadata"
    (is (= {"x" "y"} (meta ^{"x" "y"} [])))
    ))

(deftest test-pr-str
  (testing "Testing printing"
    (is (= "\"asdf\" \"asdf\"" (pr-str "asdf" "asdf")))
    (is (= "[1 true {:a 2, :b #\"x\\\"y\"} #js [3 4]]"
              (pr-str [1 true {:a 2 :b #"x\"y"} (array 3 4)])))
    (is (= "\"asdf\"\n" (prn-str "asdf")))
    (is (= "[1 true {:a 2, :b 42} #js [3 4]]\n"
              (prn-str [1 true {:a 2 :b 42} (array 3 4)])))
    (is (= "asdf" (print-str "asdf")))
    (is (= "asdf\n" (println-str "asdf")))
    (is (= "" (pr-str)))
    (is (= "\n" (prn-str)))
    (is (= "12" (with-out-str (print 1) (print 2))))
    (is (= "12" (with-out-str (*print-fn* 1) (*print-fn* 2))))))

(deftest test-bit-operations
  (testing "Testing bit operations"
    (is (= [1 0 0 40 43 49 49])
        [(bit-xor 0 1)
         (bit-xor 1 1)
         (bit-xor 1 0)
         (bit-xor 41 1)
         (bit-xor 42 1)
         (bit-xor 42 1 26)
         (apply bit-xor [42 1 26])])
    (is (= [0 0 1 0 1 1 1]
          [(bit-and 1 0)
           (bit-and 0 0)
           (bit-and 1 1)
           (bit-and 42 1)
           (bit-and 41 1)
           (bit-and 41 1 27)
           (apply bit-and [41 1 27])]))
    (is (= [1 0 1 43 41 59 59]
          [(bit-or 1 0)
           (bit-or 0 0)
           (bit-or 1 1)
           (bit-or 42 1)
           (bit-or 41 1)
           (bit-or 41 1 26)
           (apply bit-or [41 1 26])]))
    (is (= [1 0 0 42 32 32]
          [(bit-and-not 1 0)
           (bit-and-not 0 0)
           (bit-and-not 1 1)
           (bit-and-not 42 1)
           (bit-and-not 41 1 27)
           (apply bit-and-not [41 1 27])]))
    (is (= [0 2 968 16649 0]
          [(bit-clear 1 0)
           (bit-clear 2 0)
           (bit-clear 1000 5)
           (bit-clear 16713 6)
           (bit-clear 1024 10)]))
    (is (= [0 0 992 18761 0]
          [(bit-flip 1 0)
           (bit-flip 2 1)
           (bit-flip 1000 3)
           (bit-flip 16713 11)
           (bit-flip 1024 10)]))
    (is (= [-2 -3 999 -16714 -1025]
          [(bit-not 1)
           (bit-not 2)
           (bit-not -1000)
           (bit-not 16713)
           (bit-not 1024)]))
    (is (= [1 2 1000 18761 1024]
          [(bit-set 1 0)
           (bit-set 2 1)
           (bit-set 1000 3)
           (bit-set 16713 11)
           (bit-set 1024 10)]))
    (is (= [true true true false true]
          [(bit-test 1 0)
           (bit-test 2 1)
           (bit-test 1000 3)
           (bit-test 16713 11)
           (bit-test 1024 10)]))))

(deftest test-vectors
  (testing "Testing vectors"
    (is (= :a (nth [:a :b :c :d] 0)))
    (is (= :a (nth [:a :b :c :d] 0.1)))
    (let [pv (vec (range 97))]
      (testing "basic ops"
        (is (= (nth pv 96) 96))
        (is (= (nth pv 97 nil) nil))
        (is (= (pv 96) 96))
        (is (nil? (rseq [])))
        (is (= (reverse pv) (rseq pv)))))
    (let [pv (vec (range 33))]
      (testing "pop"
        (is (= pv (-> pv pop pop (conj 31) (conj 32))))))
    (let [stack1 (pop (vec (range 97)))
          stack2 (pop stack1)]
      (testing "stack operations"
        (is (= 95 (peek stack1)))
        (is (= 94 (peek stack2)))))
    (let [v1 (vec (range 10))
          v2 (vec (range 5))
          s (subvec v1 2 8)]
      (testing "subvec"
        (is (= s (-> v1 (subvec 2) (subvec 0 6)) (->> v1 (drop 2) (take 6))))
        (is (= 6 (count s)))
        (is (= [2 3 4 5 6] (pop s)))
        (is (= 7 (peek s)))
        (is (= [2 3 4 5 6 7 1]
                  (assoc s 6 1)
                  (conj s 1)))
        (is (= 27 (reduce + s)))
        (is (= s (vec s))) ; pour into plain vector
        ;; go outside ranges
        (is (= :fail (try (subvec v2 0 6) (catch js/Error e :fail))))
        (is (= :fail (try (subvec v2 6 10) (catch js/Error e :fail))))
        (is (= :fail (try (subvec v2 6 10) (catch js/Error e :fail))))
        (is (= :fail (try (subvec v2 3 6) (catch js/Error e :fail))))
        ;; no layered subvecs
        (is (identical? v1 (.-v (subvec s 1 4))))
        (let [m {:x 1}]
          (is (= m (meta (with-meta s m))))))
      ;; CLJS-513
      (let [sentinel (js-obj)
            s (subvec [0 1 2 3] 1 2)]
        (testing "bounds checking"
          (is (identical? sentinel (try (s -1) (catch js/Error _ sentinel))))
          (is (identical? sentinel (try (s 1) (catch js/Error _ sentinel))))))  
      ;; CLJS-765
      (let [sv1 (subvec [0 1 2 3] 1 2)
            sv2 (subvec [0 1 2 3] 1 1)]
        (testing "rseq equality"
          (is (= (rseq sv1) '(1)))
          (is (nil? (rseq sv2))))))
    ))

(deftest test-sequential-equality
  (testing "Testing ISequential equality"
    (is (= (list 3 2 1) [3 2 1]))
    (is (= [3 2 1] (seq (array 3 2 1))))))

(deftest test-seq-operations
  (testing "Testing basic seq operations"
    (is (= () (rest nil)))
    (is (= nil (seq (array))))
    (is (= nil (seq "")))
    (is (= nil (seq [])))
    (is (= nil (seq {})))
    (is (= () (rest ())))
    (is (= () (rest [1])))
    (is (= () (rest (array 1))))))

(deftest test-apply
  (testing "Testing apply"
    (is (= 0 (apply + nil)))
    (is (= 0 (apply + (list))))
    (is (= 1 (apply + (list 1))))
    (is (= 3 (apply + 1 (list 2))))
    (is (= 7 (apply + 1 2 (list 4))))
    (is (= 15 (apply + 1 2 4 (list 8))))
    (is (= 31 (apply + 1 2 4 8 (list 16))))
    (is (= 63 (apply + 1 2 4 8 16 (list 32))))
    (is (= 127 (apply + 1 2 4 8 16 (list 32 64))))
    (is (= 4950 (apply + (take 100 (iterate inc 0)))))
    (is (= () (apply list [])))
    (is (= [1 2 3] (apply list [1 2 3])))
    (is (= 6 (apply apply [+ [1 2 3]])))
    ;; apply with infinite sequence
    (is (= 3 (apply (fn [& args]
                          (+ (nth args 0)
                            (nth args 1)
                            (nth args 2)))
                   (iterate inc 0))))
    (is (= [0 1 2 3 4] (take 5 (apply (fn [& m] m) (iterate inc 0)))))
    (is (= [1 2 3 4 5] (take 5 (apply (fn [x & m] m) (iterate inc 0)))))
    (is (= [2 3 4 5 6] (take 5 (apply (fn [x y & m] m) (iterate inc 0)))))
    (is (= [3 4 5 6 7] (take 5 (apply (fn [x y z & m] m) (iterate inc 0)))))
    (is (= [4 5 6 7 8] (take 5 (apply (fn [x y z a & m] m) (iterate inc 0)))))
    (is (= [5 6 7 8 9] (take 5 (apply (fn [x y z a b & m] m) (iterate inc 0)))))
    ;; apply arity tests
    (let [single-arity-non-variadic (fn [x y z] [z y x])
          multiple-arity-non-variadic (fn ([x] x) ([x y] [y x]) ([x y z] [z y x]))
          single-arity-variadic-fixedargs (fn [x y & more] [more y x])
          single-arity-variadic-nofixedargs (fn [& more] more)
          multiple-arity-variadic (fn ([x] x) ([x y] [y x]) ([x y & more] [more y x]))]
      (testing "arities"
        (is (= [3 2 1] (apply single-arity-non-variadic [1 2 3])))
        (is (= [3 2 1] (apply single-arity-non-variadic 1 [2 3])))
        (is (= [3 2 1] (apply single-arity-non-variadic 1 2 [3])))
        (is (= 42 (apply multiple-arity-non-variadic [42])))
        (is (= [2 1] (apply multiple-arity-non-variadic [1 2])))
        (is (= [2 1] (apply multiple-arity-non-variadic 1 [2])))
        (is (= [3 2 1] (apply multiple-arity-non-variadic [1 2 3])))
        (is (= [3 2 1] (apply multiple-arity-non-variadic 1 [2 3])))
        (is (= [3 2 1] (apply multiple-arity-non-variadic 1 2 [3])))
        (is (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs [1 2 3 4 5])))
        (is (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs 1 [2 3 4 5])))
        (is (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs 1 2 [3 4 5])))
        (is (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs 1 2 3 [4 5])))
        (is (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs 1 2 3 4 [5])))
        (is (= [3 4 5] (take 3 (first (apply single-arity-variadic-fixedargs (iterate inc 1))))))
        (is (= [2 1] (rest (apply single-arity-variadic-fixedargs (iterate inc 1)))))
        (is (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs [1 2 3 4 5])))
        (is (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs 1 [2 3 4 5])))
        (is (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs 1 2 [3 4 5])))
        (is (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs 1 2 3 [4 5])))
        (is (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs 1 2 3 4 [5])))
        (is (= [1 2 3 4 5] (take 5 (apply single-arity-variadic-nofixedargs (iterate inc 1)))))
        (is (= 42 (apply multiple-arity-variadic [42])))
        (is (= [2 1] (apply multiple-arity-variadic [1 2])))
        (is (= [2 1] (apply multiple-arity-variadic 1 [2])))
        (is (= [[3 4 5] 2 1] (apply multiple-arity-variadic [1 2 3 4 5])))
        (is (= [[3 4 5] 2 1] (apply multiple-arity-variadic 1 [2 3 4 5])))
        (is (= [[3 4 5] 2 1] (apply multiple-arity-variadic 1 2 [3 4 5])))
        (is (= [[3 4 5] 2 1] (apply multiple-arity-variadic 1 2 3 [4 5])))
        (is (= [[3 4 5] 2 1] (apply multiple-arity-variadic 1 2 3 4 [5])))
        (is (= [3 4 5] (take 3 (first (apply multiple-arity-variadic (iterate inc 1))))))
        (is (= [2 1] (rest (apply multiple-arity-variadic (iterate inc 1)))))))))

;; this fails in v8 - why?
;; (assert (= "symbol\"'string" (pr-str (str 'symbol \" \' "string"))))
(deftest test-misc
  (testing "Testing miscellaneous operations"
    (is (= 9 (reduce + (next (seq (array 1 2 3 4))))))
    (is (not (= "one" "two")))
    (is (= 3 (count "abc")))
    (is (= 4 (count (array 1 2 3 4))))
    (is (= "c" (nth "abc" 2)))
    (is (= "quux" (nth "abc" 3 "quux")))
    (is (= 1 (nth (array 1 2 3 4) 0)))
    (is (= "val" (nth (array 1 2 3 4) 4 "val")))
    (is (= "b" (get "abc" 1)))
    (is (= "harriet" (get "abcd" 4 "harriet")))
    (is (= 4 (get (array 1 2 3 4) 3)))
    (is (= "zot" (get (array 1 2 3 4) 4 "zot")))
    (is (= 10 (reduce + (array 1 2 3 4))))
    (is (= 20 (reduce + 10 (array 1 2 3 4))))
    (is (= "cabd" (let [jumble (fn [a b] (str (apply str (reverse (str a))) b))]
                    (reduce jumble "abcd"))))
    (is (= "cafrogbd" (let [jumble (fn [a b] (str (apply str (reverse (str a))) b))]
                        (reduce jumble "frog" "abcd"))))
    (is (= [3] (nthnext [1 2 3] 2)))
    (assert (not= 1 2))
    (is (not (not= 1 1)))
    (is (not (not-empty [])))
    (is (boolean (not-empty [1 2 3])))
    (is (= "joel" (min-key count "joel" "tom servo" "crooooooooow")))
    (is (= "crooooooooow" (max-key count "joel" "tom servo" "crooooooooow")))
    (is (= (partition-all 4 [1 2 3 4 5 6 7 8 9])
          [[1 2 3 4] [5 6 7 8] [9]]))
    (is (= (partition-all 4 2 [1 2 3 4 5 6 7 8 9])
          [[1 2 3 4] [3 4 5 6] [5 6 7 8] [7 8 9] [9]]))
    (is (= [true true] (take-while true? [true true 2 3 4])))
    (is (= [[true true] [false false false] [true true]]
          (partition-by true? [true true false false false true true])))
    (is (= [0 2 4 6 8 10] (take-nth 2 [0 1 2 3 4 5 6 7 8 9 10])))
    (let [sf (some-fn number? keyword? symbol?)]
      (testing "Testing some-fn"
        (is (sf :foo 1))
        (is (sf :foo))
        (is (sf 'bar 1))
        (is (not (sf [] ())))))
    (let [ep (every-pred number? zero?)]
      (testing "Testing every-pred"
        (is (ep 0 0 0))
        (is (not (ep 1 2 3 0)))))
    (is ((complement number?) :foo))
    (is (= [1 [2 3] [1 2 3]] ((juxt first rest seq) [1 2 3])))
    (is (= 5 (max 1 2 3 4 5)))
    (is (= 5 (max 5 4 3 2 1)))
    (is (= 5.5 (max 1 2 3 4 5 5.5)))
    (is (= 1 (min 5 4 3 2 1)))
    (is (= 1 (min 1 2 3 4 5)))
    (is (= 0.5 (min 5 4 3 0.5 2 1)))
    (let [x (array 1 2 3)]
      (testing "Testing setting property on JS array"
        (set! (.-foo x) :hello)
        (is (= (.-foo x) :hello))))
    ;; last
    (is (= nil (last nil)))
    (is (= 3 (last [1 2 3])))
    ;; dotimes
    (let [s (atom [])]
      (dotimes [n 5]
        (swap! s conj n))
      (is (= [0 1 2 3 4] @s)))
    ;; doseq
    (let [v [1 2 3 4 5]
          s (atom ())]
      (doseq [n v] (swap! s conj n))
      (is (= @s (reverse v))))
    ;; memoize
    (let [f (memoize (fn [] (rand)))]
      (f)
      (is (= (f) (f))))
    ;; range
    (is (= (range 10) (list 0 1 2 3 4 5 6 7 8 9)))
    (is (= (range 10 20) (list 10 11 12 13 14 15 16 17 18 19)))
    (is (= (range 10 20 2) (list 10 12 14 16 18)))
    (is (= (take 20 (range)) (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)))
    ;; group-by
    (let [d (group-by second {:a 1 :b 2 :c 1 :d 4 :e 1 :f 2})]
      (testing "group-by"
        (is (= 3 (count (get d 1))))
        (is (= 2 (count (get d 2))))
        (is (= 1 (count (get d 4))))))
    (is (= {1 2 3 4 5 6} (merge {1 2} {3 4} {5 6})))
    (is (= {1 2 3 4} (merge {1 2} {3 4} nil)))
    ;; frequencies
    (is (= {:a 3 :b 2} (frequencies [:a :b :a :b :a])))
    ;; reductions
    (is (= [1 3 6 10 15] (reductions + [1 2 3 4 5])))
    ;; keep
    (is (= [1 3 5 7 9] (keep #(if (odd? %) %) [1 2 3 4 5 6 7 8 9 10])))
    (is (= [2 4 6 8 10] (keep #(if (even? %) %) [1 2 3 4 5 6 7 8 9 10])))
    ;; keep-indexed
    (is (= [1 3 5 7 9] (keep-indexed #(if (odd? %1) %2) [0 1 2 3 4 5 6 7 8 9 10])))
    (is (= [2 4 5] (keep-indexed #(if (pos? %2) %1) [-9 0 29 -7 45 3 -8])))
    ;; map-indexed
    (is (= [[0 :a] [1 :b] [2 :c]] (map-indexed #(vector % %2) [:a :b :c])))
    ;; merge-with
    (is (= '{"Foo" ("foo" "FOO" "fOo"), "Bar" ("bar" "BAR" "BAr"), "Baz" ["baz"], "Qux" ["qux" "quux"]}
              (merge-with concat
                {"Foo" ["foo" "FOO"]
                 "Bar" ["bar" "BAR"]
                 "Baz" ["baz"]}
                {"Foo" ["fOo"]
                 "Bar" ["BAr"]
                 "Qux" ["qux" "quux"]})))
    (is (= {:a 111, :b 102, :c 13}
              (merge-with +
                {:a 1 :b 2 :c 3}
                {:a 10 :c 10}
                {:a 100 :b 100})))
    (is (= {:a 3, :b 102, :c 13}
              (apply merge-with [+
                                 {:a 1 :b 100}
                                 {:a 1 :b 2 :c 3}
                                 {:a 1 :c 10}])))
    (is (= '[a c e] (replace '[a b c d e] [0 2 4])))
    (is (= [:one :zero :two :zero]
              (replace {0 :zero 1 :one 2 :two} '(1 0 2 0))))
    ;; split-at
    (is (= [[1 2] [3 4 5]] (split-at 2 [1 2 3 4 5])))
    ;; split-with
    (is (= [[1 2 3] [4 5]] (split-with (partial >= 3) [1 2 3 4 5])))
    ;; trampoline
    (is (= 10000 (trampoline (fn f [n] (if (>= n 10000) n #(f (inc n)))) 0)))
    ;; vary-meta
    (is (= {:a 1} (meta (vary-meta [] assoc :a 1))))
    (is (= {:a 1 :b 2} (meta (vary-meta (with-meta [] {:b 2}) assoc :a 1))))
    ;; comparator
    (is (= [1 1 2 2 3 5] (seq (.sort (to-array [2 3 1 5 2 1]) (comparator <)))))
    (is (= [5 3 2 2 1 1] (seq (.sort (to-array [2 3 1 5 2 1]) (comparator >)))))
    (is (= (hash 'foo) (hash (symbol "foo"))))
    (is (= (hash 'foo/bar) (hash (symbol "foo" "bar"))))
    (is (= (lazy-cat [1] [2] [3]) '(1 2 3)))
    ))

(deftest test-booleans
  (testing "Testing boolean predicates"
    (is (= [true false true false false false]
          [(true? true)
           (true? false)
           (false? false)
           (false? true)
           (true? js/undefined)
           (false? js/undefined)]))))

(deftest test-fn-with-metadata
  (let [f (fn [x] (* x 2))
        m {:foo "bar"}
        mf (with-meta f m)]
    (testing "Testing functions with metadata"
      (is (nil? (meta f)))
      (is (fn? mf))
      (is (= 4 (mf 2)))
      (is (= 4 (apply mf [2])))
      (is (= (meta mf) m)))))

(deftest test-atoms-and-volatile
  (let [a (atom 0)]
    (testing "Testing basic atom operations"
      (is (= 0 (deref a)))
      (is (= 1 (swap! a inc)))
      (is (= false (compare-and-set! a 0 42)))
      (is (= true (compare-and-set! a 1 7)))
      (is (nil? (meta a)))
      (is (nil? (get-validator a)))))
  (let [a (atom 0)]
    (testing "Testing swap!"
      (is (= 1 (swap! a + 1)))
      (is (= 4 (swap! a + 1 2)))
      (is (= 10 (swap! a + 1 2 3)))
      (is (= 20 (swap! a + 1 2 3 4)))))
  (let [a (atom [1] :validator coll? :meta {:a 1})]
    (testing "Testing atom validators"
      (is (= coll? (get-validator a)))
      (is (= {:a 1} (meta a)))
      (alter-meta! a assoc :b 2)
      (is (= {:a 1 :b 2} (meta a)))))
  (let [v (volatile! 1)]
    (testing "Testing volatile"
      (is (volatile? v))
      (is (not (volatile? (atom 1))))
      (is (= 2 (vreset! v 2)))
      (is (= 3 (vswap! v inc)))
      (is (= 3 @v)))))

(deftest test-empy-and-seq
  (testing "Testing empty & seq"
    (is (nil? (empty nil)))
    (let [e-lazy-seq (empty (with-meta (lazy-seq (cons :a nil)) {:b :c}))]
      (testing "lazy seq"
        (is (seq? e-lazy-seq))
        (is (empty? e-lazy-seq))
        (is (= {:b :c} (meta e-lazy-seq)))))
    (let [e-list (empty '^{:b :c} (1 2 3))]
      (testing "list"
        (is (seq? e-list))
        (is (empty? e-list))
        (is (= {:b :c} (meta e-list)))))
    (let [e-elist (empty '^{:b :c} ())]
      (testing "empty list with metadata"
        (is (seq? e-elist))
        (is (empty? e-elist))
        (is (= :c (get (meta e-elist) :b)))))
    (let [e-cons (empty (with-meta (cons :a nil) {:b :c}))]
      (testing "cons"
        (is (seq? e-cons))
        (is (empty? e-cons))
        (is (= {:b :c} (meta e-cons)))))
    (let [e-vec (empty ^{:b :c} [:a :d :g])]
      (testing "vector"
        (is (vector? e-vec))
        (is (empty? e-vec))
        (is (= {:b :c} (meta e-vec)))))
    (let [e-omap (empty ^{:b :c} {:a :d :g :h})]
      (testing "map"
        (is (map? e-omap))
        (is (empty? e-omap))
        (is (= {:b :c} (meta e-omap)))))
    (let [e-hmap (empty ^{:b :c} {[1 2] :d :g :h})]
      (testing "map with complex keys"
        (is (map? e-hmap))
        (is (empty? e-hmap))
        (is (= {:b :c} (meta e-hmap)))))
    (let [smap (with-meta (sorted-map-by (comp - compare) 2 :a 1 :b 5 :c) {:b :c})
          e-smap (empty smap)]
      (testing "sorted-map-by"
        (is (map? e-smap))
        (is (empty? e-smap))
        (is (= {:b :c} (meta e-smap)))
        (is (identical? (-comparator smap) (-comparator e-smap)))
        (is (= [[5 :c] [2 :a] [1 :b]] (seq (assoc e-smap 2 :a 1 :b 5 :c))))))
    (let [sset (with-meta (sorted-set-by (comp - compare) 5 1 2) {:b :c})
          e-sset (empty sset)]
      (testing "sorted-set-by"
        (is (set? e-sset))
        (is (empty? e-sset))
        (is (= {:b :c} (meta e-sset)))
        (is (identical? (-comparator sset) (-comparator e-sset)))
        (is (= [5 2 1] (seq (conj e-sset 5 1 2))))))
    (let [e-queue (empty (with-meta (.-EMPTY PersistentQueue) {:b :c}))]
      (testing "queue"
        (is (identical? (type e-queue) PersistentQueue))
        (is (empty? e-queue))
        (is (= {:b :c} (meta e-queue)))))))

(deftest test-try-catch
  (let [a (atom nil)]
    (testing "Testing try/catch"
      (is (= 1 (try 1)))
      (is (= 2 (try 1 (throw (js/Error.)) (catch js/Error e 2))))
      (is (= 2 (try 1 (throw (js/Error.)) (catch js/Error e 1 2))))
      (is (= 2 (try 1 (throw (js/Error.)) (catch js/Error e 2) (catch :default e 3))))
      (is (= 3 (try 1 (throw true) (catch js/Error e 2) (catch :default e 3))))
      (is (= 2 (try 1 (throw 2) (catch js/Error e 3) (catch :default e e))))
      (is (= 1 (try 1 (finally (reset! a 42)))))
      (is (= 42 (deref a))))))

(deftest test-list-comprehensions
  (let [v [1 2 3]]
    (testing "Testing list comprehensions"
      (is (= v (for [e v] e)))
      (is (= [[1 1] [2 4] [3 9]] (for [e v :let [m (* e e)]] [e m])))
      (is (= [1 2] (for [e v :while (< e 3)] e)))
      (is (= [3] (for [e v :when (> e 2)] e)))
      (is (= [[1 1] [2 4]] (for [e v :while (< e 3) :let [m (* e e)]] [e m]))))))

(deftest test-partial-and-comp
  (let [a10 (partial + 10)
        a20 (partial + 10 10)
        a21 (partial + 10 10 1)
        a22 (partial + 10 5  4 3)
        a23 (partial + 10 5  4 3 1)]
    (testing "Testing partial"
      (is (= 110 (a10 100)))
      (is (= 120 (a20 100)))
      (is (= 121 (a21 100)))
      (is (= 122 (a22 100)))
      (is (= 123 (a23 100)))))
  (let [n2 (comp first rest)
        n3 (comp first rest rest)
        n4 (comp first rest rest rest)
        n5 (comp first rest rest rest rest)
        n6 (comp first rest rest rest rest rest)]
    (testing "Testing comp"
      (is (= 2 (n2 [1 2 3 4 5 6 7])))
      (is (= 3 (n3 [1 2 3 4 5 6 7])))
      (is (= 4 (n4 [1 2 3 4 5 6 7])))
      (is (= 5 (n5 [1 2 3 4 5 6 7])))
      (is (= 6 (n6 [1 2 3 4 5 6 7]))))))

(deftest test-sets
  (testing "Testing persistent sets"
    (is (set []))
    (is (= #{} (set [])))
    (is (= #{} (hash-set)))
    (is (identical? cljs.core.PersistentHashSet (type (hash-set))))

    (is (= #{"foo"} (set ["foo"])))
    (is (= #{"foo"} (hash-set "foo")))
    (is (= #{1 2 3} #{1 3 2}))
    (is (= #{#{1 2 3} [4 5 6] {7 8} 9 10}
              #{10 9 [4 5 6] {7 8} #{1 2 3}}))
    (is (not (= #{nil [] {} 0 #{}} #{})))
    (is (= (count #{nil [] {} 0 #{}}) 5))
    (is (= (conj #{1} 1) #{1}))
    (is (= (conj #{1} 2) #{2 1}))
    (is (= #{} (-empty #{1 2 3 4})))
    (is (= (reduce + #{1 2 3 4 5}) 15))
    (is (= 4 (get #{1 2 3 4} 4)))
    (is (contains? #{1 2 3 4} 4))
    (is (contains? #{[] nil 0 {} #{}} {}))
    (is (contains? #{[1 2 3]} [1 2 3]))
    (is (not (contains? (-disjoin #{1 2 3} 3) 3)))
    (is (= #{1 2 3} (disj #{1 2 3})))
    (is (= #{1 2} (disj #{1 2 3} 3)))
    (is (= #{1} (disj #{1 2 3} 2 3)))
    (is (nil? (disj nil :foo)))))

(deftest test-contains?
  (testing "Testing contains?"
    (is (contains? {:a 1 :b 2} :a))
    (is (not (contains? {:a 1 :b 2} :z)))
    (is (contains? [5 6 7] 1))
    (is (contains? [5 6 7] 2))
    (is (not (contains? [5 6 7] 3)))
    (is (contains? (to-array [5 6 7]) 1))
    (is (contains? (to-array [5 6 7]) 2))
    (is (not (contains? (to-array [5 6 7]) 3)))
    (is (not (contains? nil 42)))
    (is (contains? "f" 0))
    (is (not (contains? "f" 55)))))

(deftest test-distinct
  (testing "Testing distinct? & distinct"
    (is (distinct? 1 2 3))
    (is (not (distinct? 1 2 3 1)))
    (is (= (distinct ()) ()))
    (is (= (distinct '(1)) '(1)))
    (is (= (distinct '(1 2 3 1 1 1)) '(1 2 3)))
    (is (= (distinct [1 1 1 2]) '(1 2)))
    (is (= (distinct [1 2 1 2]) '(1 2)))
    (is (= (distinct "a") ["a"]))
    (is (= (distinct "abcabab") ["a" "b" "c"]))
    (is (= (distinct ["abc" "abc"]) ["abc"]))
    (is (= (distinct [nil nil]) [nil]))
    (is (= (distinct [0.0 0.0]) [0.0]))
    (is (= (distinct ['sym 'sym]) '[sym]))
    (is (= (distinct [:kw :kw]) [:kw]))
    (is (= (distinct [42 42]) [42]))
    (is (= (distinct [[] []]) [[]]))
    (is (= (distinct ['(1 2) '(1 2)]) '[(1 2)]))
    (is (= (distinct [() ()]) [()]))
    (is (= (distinct [[1 2] [1 2]]) [[1 2]]))
    (is (= (distinct [{:a 1 :b 2} {:a 1 :b 2}]) [{:a 1 :b 2}]))
    (is (= (distinct [{} {}]) [{}]))
    (is (= (distinct [#{1 2} #{1 2}]) [#{1 2}]))
    (is (= (distinct [#{} #{}]) [#{}]))))

(deftest test-regexps
  (testing "Testing regexps"
    (let [r1 #"foo", r2 (re-pattern r1)]
      (is (= r1 r2)))
    (is (= (str (re-pattern "f(.)o")) (str (js* "/f(.)o/"))))
    (is (= (re-find (re-pattern "foo") "foo bar foo baz foo zot") "foo"))
    (is (= (re-find (re-pattern "f(.)o") "foo bar foo baz foo zot") ["foo" "o"]))
    (is (= (re-matches (re-pattern "foo") "foo") "foo"))
    (is (= (re-matches (re-pattern "foo") "foo bar foo baz foo zot") nil))
    (is (= (re-matches (re-pattern "foo.*") "foo bar foo baz foo zot") "foo bar foo baz foo zot"))
    (is (= (re-seq (re-pattern "foo") "foo bar foo baz foo zot") (list "foo" "foo" "foo")))
    (is (= (re-seq (re-pattern "f(.)o") "foo bar foo baz foo zot") (list ["foo" "o"] ["foo" "o"] ["foo" "o"])))
    (is (= (re-matches (re-pattern "(?i)foo") "Foo") "Foo"))
                                        ; new RegExp("").source => "(?:)" on webkit-family envs, "" elsewhere
    (is (#{"#\"\"" "#\"(?:)\""} (pr-str #"")))))

(deftest test-destructuring
  (testing "Testing destructuring"
    (is (= [2 1] (let [[a b] [1 2]] [b a])))
    (is (= #{1 2} (let [[a b] [1 2]] #{a b})))
    (is (= [1 2] (let [{a :a b :b} {:a 1 :b 2}] [a b])))
    (is (= [1 2] (let [{:keys [a b]} {:a 1 :b 2}] [a b])))
    (is (= [1 2 [1 2]] (let [[a b :as v] [1 2]] [a b v])))
    (is (= [1 42] (let [{:keys [a b] :or {b 42}} {:a 1}] [a b])))
    (is (= [1 nil] (let [{:keys [a b] :or {c 42}} {:a 1}] [a b])))
    (is (= [2 1] (let [[a b] '(1 2)] [b a])))
    (is (= {1 2} (let [[a b] [1 2]] {a b})))
    (is (= [2 1] (let [[a b] (seq [1 2])] [b a])))
    (testing "namespaced keys"
      (let [{:keys [:a :b]} {:a 1 :b 2}]
        (testing "basic"
          (is (= 1 a))
          (is (= 2 b))))
      (let [{:keys [:a/b :c/d]} {:a/b 1 :c/d 2}]
        (testing "keyword syntax"
          (is (= 1 b))
          (is (= 2 d))))
      (let [{:keys [a/b c/d]} {:a/b 1 :c/d 2}]
        (testing "symbol syntax"
          (is (= 1 b))
          (is (= 2 d))))
      (let [{:syms [a/b c/d]} {'a/b 1 'c/d 2}]
        (testing ":syms"
          (is (= 1 b))
          (is (= 2 d))))
      (let [{:keys [::s/x ::s/y]} {:clojure.string/x 1 :clojure.string/y 2}]
        (testing ":keys"
          (is (= x 1))
          (is (= y 2))))
      )))

(deftest test-in-operations
  (testing "Testing update-in"
    (is (= {:foo {:bar {:baz 1}}}
          (update-in {:foo {:bar {:baz 0}}} [:foo :bar :baz] inc)))
    (is (= {:foo 1 :bar 2 :baz 10}
          (update-in {:foo 1 :bar 2 :baz 3} [:baz] + 7)))
    (is (= [{:foo 1, :bar 2} {:foo 1, :bar 3}]
          (update-in [{:foo 1 :bar 2}, {:foo 1 :bar 2}] [1 :bar] inc)))
    (is (= [{:foo {:bar 2}} {:foo {:bar 3}}]
          (update-in [{:foo {:bar 2}}, {:foo {:bar 2}}] [1 :foo :bar] inc))))
  (testing "Testing assoc-in"
    (is (= {:foo {:bar {:baz 100}}}
          (assoc-in {:foo {:bar {:baz 0}}} [:foo :bar :baz] 100)))
    (is (= {:foo 1 :bar 2 :baz 100}
          (assoc-in {:foo 1 :bar 2 :baz 3} [:baz] 100)))
    (is (= [{:foo [{:bar 2} {:baz 3}]} {:foo [{:bar 2} {:baz 100}]}]
          (assoc-in [{:foo [{:bar 2} {:baz 3}]}, {:foo [{:bar 2} {:baz 3}]}]
            [1 :foo 1 :baz] 100)))
    (is (= [{:foo 1, :bar 2} {:foo 1, :bar 100}]
          (assoc-in [{:foo 1 :bar 2}, {:foo 1 :bar 2}] [1 :bar] 100))))
  (testing "Testing get-in"
    (is (= 1 (get-in {:foo 1 :bar 2} [:foo])))
    (is (= 2 (get-in {:foo {:bar 2}} [:foo :bar])))
    (is (= 1 (get-in [{:foo 1}, {:foo 2}] [0 :foo])))
    (is (= 4 (get-in [{:foo 1 :bar [{:baz 1}, {:buzz 2}]}, {:foo 3 :bar [{:baz 3}, {:buzz 4}]}]
               [1 :bar 1 :buzz]))))
  )

(deftest test-arrays
  (testing "Testing array operations"
    (let [a (to-array [1 2 3])]
      (testing "basic ops"
        (is (= [10 20 30] (seq (amap a i ret (* 10 (aget a i))))))
        (is (= 6 (areduce a i ret 0 (+ ret (aget a i)))))
        (is (= (seq a) (seq (to-array [1 2 3]))))
        (is (= 42 (aset a 0 42)))
        (is (not= (seq a) (seq (to-array [1 2 3]))))
        (is (not= a (aclone a)))))
    (let [a (array (array 1 2 3) (array 4 5 6))]
      (testing "aget"
        (is (= (aget a 0 1) 2))
        (is (= (apply aget a [0 1]) 2))
        (is (= (aget a 1 1) 5))
        (is (= (apply aget a [1 1]) 5))
        (aset a 0 0 "foo")
        (is (= (aget a 0 0) "foo"))
        (apply aset a [0 0 "bar"])
        (is (= (aget a 0 0) "bar"))))))

(deftest test-rearrange-sequential
  (testing "Test rearranging sequential collections"
    (is (= [1 2 3 4 5] (sort [5 3 1 4 2])))
    (is (= [1 2 3 4 5] (sort < [5 3 1 4 2])))
    (is (= [5 4 3 2 1] (sort > [5 3 1 4 2])))
    (is (= ["a" [ 1 2] "foo"] (sort-by count ["foo" "a" [1 2]])))
    (is (= ["foo" [1 2] "a"] (sort-by count > ["foo" "a" [1 2]])))
    (let [coll [1 2 3 4 5 6 7 8 9 10]
          ;; while it is technically possible for this test to fail with a false negative,
          ;; it's _extraordinarily_ unlikely.
          shuffles (filter #(not= coll %) (take 100 (iterate shuffle coll)))]
      (is (not (empty? shuffles))))
    ))

(deftest test-js-clj-conversions
  (testing "Testing JS / CLJS data conversions"
    (testing "js->clj"
      (is (= {"a" 1, "b" 2} (js->clj (js* "{\"a\":1,\"b\":2}"))))
      (is (= {"a" nil} (js->clj (js* "{\"a\":null}"))))
      (is (= {} (js->clj (js* "{}"))))
      (is (= {"a" true, "b" false} (js->clj (js* "{\"a\":true,\"b\":false}"))))
      (is (= {:a 1, :b 2} (js->clj (js* "{\"a\":1,\"b\":2}") :keywordize-keys true)))
      (is (= [[{:a 1, :b 2} {:a 1, :b 2}]]
                (js->clj (js* "[[{\"a\":1,\"b\":2}, {\"a\":1,\"b\":2}]]") :keywordize-keys true)))
      (is (= [[{:a 1, :b 2} {:a 1, :b 2}]]
                (js->clj [[{:a 1, :b 2} {:a 1, :b 2}]])))
      (is (= (js->clj nil) nil)))
    (testing "clj->js"
      (is (= (clj->js 'a) "a"))
      (is (= (clj->js :a) "a"))
      (is (= (clj->js "a") "a"))
      (is (= (clj->js 1) 1))
      (is (= (clj->js nil) (js* "null")))
      (is (= (clj->js true) (js* "true")))
      (is (goog/isArray (clj->js [])))
      (is (goog/isArray (clj->js #{})))
      (is (goog/isArray (clj->js '())))
      (is (goog/isObject (clj->js {})))
      (is (= (aget (clj->js {:a 1}) "a") 1))
      (is (= (-> (clj->js {:a {:b {{:k :ey} :d}}})
                   (aget "a")
                   (aget "b")
                   (aget "{:k :ey}"))
                "d")))))

(deftest test-delay
  (let [a (atom 0)
        d (delay (swap! a inc))]
    (testing "Testing delay"
      (is (false? (realized? d)))
      (is (zero? @a)) ;; delay hasn't triggered yet
      (is (= 1 @d)) ;; trigger it
      (is (= 1 @a)) ;; make sure side effect has happened
      (is (true? (realized? d)))
      (is (= 1 @d)) ;; body doesn't happen again
      (is (= 1 @a)) ;; atom hasn't changed either
      (is (= (force d) @d))
      (is (= 1 (force 1))))) ;; you can safely force non-delays
  )

(deftest test-hierarchy
  (testing "Testing hierarchy operations"
    (derive ::rect ::shape)
    (derive ::square ::rect)
    (is (= #{:cljs.core-test/shape} (parents ::rect)))
    (is (= #{:cljs.core-test/rect :cljs.core-test/shape} (ancestors ::square)))
    (is (= #{:cljs.core-test/rect :cljs.core-test/square} (descendants ::shape)))
    (is (true? (isa? 42 42)))
    (is (true? (isa? ::square ::shape)))
    (derive cljs.core.ObjMap ::collection)
    (derive cljs.core.PersistentHashSet ::collection)
    (is (true? (isa? cljs.core.ObjMap ::collection)))
    (is (true? (isa? cljs.core.PersistentHashSet ::collection)))
    (is (false? (isa? cljs.core.IndexedSeq ::collection)))
    ;; ?? (isa? String Object)
    (is (true? (isa? [::square ::rect] [::shape ::shape])))
    ;; ?? (ancestors java.util.ArrayList)
    ;; ?? isa? based dispatch tests
    ))

(defmulti bar (fn [x y] [x y]))
(defmethod bar [::rect ::shape] [x y] :rect-shape)
(defmethod bar [::shape ::rect] [x y] :shape-rect)

;;(bar ::rect ::rect)
;; -> java.lang.IllegalArgumentException:
;;  Multiple methods match dispatch value:
;;  [:cljs.core-test/rect :cljs.core-test/rect] -> [:cljs.core-test/rect :cljs.core-test/shape]
;;  and [:cljs.core-test/shape :cljs.core-test/rect],
;;  and neither is preferred

(deftest test-multimethods-1
  (testing "Testing basic multimethod usage"
    (is (zero? (count (prefers bar))))
    (prefer-method bar [::rect ::shape] [::shape ::rect])
    (is (= 1 (count (prefers bar))))
    (is (= :rect-shape (bar ::rect ::rect)))
    (is (= :rect-shape (apply (-get-method bar [::rect ::shape]) [::rect ::shape])))
    ))

(defmulti nested-dispatch (fn [m] (-> m :a :b)))
(defmethod nested-dispatch :c [m] :nested-a)
(defmethod nested-dispatch :default [m] :nested-default)

(defmulti nested-dispatch2 ffirst)
(defmethod nested-dispatch2 :a [m] :nested-a)
(defmethod nested-dispatch2 :default [m] :nested-default)

(defmulti foo1 (fn [& args] (first args)))
(defmethod foo1 :a [& args] :a-return)
(defmethod foo1 :default [& args] :default-return)

(defmulti area :Shape)
(defn rect [wd ht] {:Shape :Rect :wd wd :ht ht})
(defn circle [radius] {:Shape :Circle :radius radius})
(defmethod area :Rect [r]
    (* (:wd r) (:ht r)))
(defmethod area :Circle [c]
    (*  Math/PI (* (:radius c) (:radius c))))
(defmethod area :default [x] :oops)
(defmulti foo2 (fn []))
(defmethod foo2 :default [] :foo)

(defmulti apply-multi-test (fn ([_] 0) ([_ _] 0) ([_ _ _] 0)))
(defmethod apply-multi-test 0
  ([x] :one)
  ([x y] :two)
  ([x y & r] [:three r]))

;; CLJS-469, helpful exception message on bad dispatch
(defmulti no-dispatch-value :test)

;; custom hierarchy
(def my-map-hierarchy
  (atom (-> (make-hierarchy)
          (derive (type (obj-map)) ::map)
          (derive (type (array-map)) ::map)
          (derive (type (hash-map)) ::map)
          (derive (type (sorted-map)) ::map))))

(defmulti my-map? type :hierarchy my-map-hierarchy)
(defmethod my-map? ::map [_] true)
(defmethod my-map? :default [_] false)

(defmulti foo2' identity)
(defmethod foo2' 0 [x] x)

(deftest test-multimethods-2
  (let [r (rect 4 13)
        c (circle 12)]
    (testing "Testing multimethod edge cases"
      (is (= :nested-a (nested-dispatch {:a {:b :c}})))
      (is (= :nested-a (nested-dispatch2 [[:a :b]])))
      (is (= :a-return (foo1 :a)))
      (is (= :default-return (foo1 1)))
      (is (= 52 (area r)))
      (is (= :oops (area {})))
      ;; CLJS-863
      (is (= :foo (foo2)))
      ;; remove method tests
      (is (= 2 (count (methods bar))))
      (remove-method bar [::rect ::shape])
      (is (= 1 (count (methods bar))))
      (remove-all-methods bar)
      (is (zero? (count (methods bar))))
      (is (= [:three '(2)] (apply apply-multi-test [0 1 2])))
      (is (try
            (no-dispatch-value {:test :test})
            (catch js/Error e
              (not= -1 (.indexOf (.-message e) "cljs.core-test/no-dispatch-value")))))
      (doseq [m [(obj-map) (array-map) (hash-map) (sorted-map)]]
        (is (my-map? m)))
      (doseq [not-m [[] 1 "asdf" :foo]]
        (is (not (my-map? not-m))))
      ;; multimethod hashing
      (is (= foo2' (ffirst {foo2' 1})))
)))

(deftest test-range
  (testing "Testing Range"
    ;; Range
    (is (= (range 0 10 3) (list 0 3 6 9)))
    (is (= (count (range 0 10 3)) 4))
    (is (= (range 0 -10 -3) (list 0 -3 -6 -9)))
    (is (= (count (range 0 -10 -3)) 4))
    (is (= (range -10 10 3) (list -10 -7 -4 -1 2 5 8)))
    (is (= (count (range -10 10 3)) 7))
    (is (= (range 0 1 1) (list 0)))
    (is (= (range 0 -3 -1) (list 0 -1 -2)))
    (is (= (range 3 0 -1) (list 3 2 1)))
    (is (= (range 0 10 -1) (list)))
    (is (= (range 0 1 0) (list)))
    (is (= (range 10 0 1) (list)))
    (is (= (range 0 0 0) (list)))
    (is (= (count (range 0 10 -1)) 0))
    (is (= (count (range 0 1 0)) 0))
    (is (= (count (range 10 0 1)) 0))
    (is (= (count (range 0 0 0)) 0))
    (is (= (take 3 (range 1 0 0)) (list 1 1 1)))
    (is (= (take 3 (range 3 1 0)) (list 3 3 3)))
    ))

(deftest test-rseq
  (testing "Testing RSeq"
    (is (= '(3 2 1) (reverse (seq (array 1 2 3)))))
    (is (= '(3 2 1) (reverse [1 2 3])))
    (is (= '(4 3 2 1) (cons 4 (reverse [1 2 3]))))
    (is (= 6 (reduce + (reverse [1 2 3]))))
    (is (= '(4 3 2) (map inc (reverse [1 2 3]))))
    (is (= '(4 2) (filter even? (reverse [1 2 3 4]))))
    ))

(deftest test-sorted-map
  (testing "Testing sorted maps"
    ;; PersistentTreeMap
    (let [m1 (sorted-map)
          c2 (comp - compare)
          m2 (sorted-map-by c2)]
      (testing "basic ops 1"
        (is (identical? cljs.core.PersistentTreeMap (type m1)))
        (is (identical? cljs.core.PersistentTreeMap (type m2)))
        (is (identical? compare (.-comp m1)))
        (is (zero? (count m1)))
        (is (zero? (count m2)))
        (is (nil? (rseq m1)))
        (let [m1 (assoc m1 :foo 1 :bar 2 :quux 3)
              m2 (assoc m2 :foo 1 :bar 2 :quux 3)]
          (testing "basic ops 2"
            (is (= (count m1) 3))
            (is (= (count m2) 3))
            (is (= (seq m1) (list [:bar 2] [:foo 1] [:quux 3])))
            (is (= (seq m2) (list [:quux 3] [:foo 1] [:bar 2])))
            (is (= (seq m1) (rseq m2)))
            (is (= (seq m2) (rseq m1)))
            (is (= (conj m1 [:wibble 4]) {:foo 1 :bar 2 :quux 3 :wibble 4}))
            (is (= (count (conj m1 [:wibble 4])) 4))
            (is (= (conj m2 [:wibble 4]) {:foo 1 :bar 2 :quux 3 :wibble 4}))
            (is (= (count (conj m2 [:wibble 4])) 4))
            (is (= (map key (assoc m1 nil 4)) (list nil :bar :foo :quux)))
            (is (= (map key (assoc m2 nil 4)) (list :quux :foo :bar nil)))))))
    (let [m (->> [[0 10] [20 30] [10 20] [50 60] [30 40] [40 50]]
              (mapcat (partial apply range))
              (mapcat #(list % %))
              (apply sorted-map))
          s1 (map #(vector % %) (range 60))
          s2 (map #(vector % %) (range 59 -1 -1))]
      (testing "edge cases 1"
        (is (= (count m) 60))
        (is (= (seq m) s1))
        (is (= (rseq m) s2))))
    (let [m (sorted-map :foo 1 :bar 2 :quux 3)]
      (testing "edge cases 2"
        (is (= (dissoc m :foo) (hash-map :bar 2 :quux 3)))
        (is (= (count (dissoc m :foo)) 2))
        (is (= (hash m) (hash (hash-map :foo 1 :bar 2 :quux 3))))
        (is (= (subseq m < :foo)  (list [:bar 2])))
        (is (= (subseq m <= :foo) (list [:bar 2] [:foo 1])))
        (is (= (subseq m > :foo)  (list [:quux 3])))
        (is (= (subseq m >= :foo) (list [:foo 1] [:quux 3])))
        (is (= (map #(reduce (fn [_ x] x) %) m) (list 2 1 3)))
        (is (= (map #(reduce (fn [x _] x) 7 %) m) (list 7 7 7)))))
    ))

(deftest test-sorted-sets
  (let [s1 (sorted-set)
        c2 (comp - compare)
        s2 (sorted-set-by c2)
        c3 #(compare (quot %1 2) (quot %2 2))
        s3 (sorted-set-by c3)
        s4 (sorted-set-by <)]
    (testing "Testing sorted set"
      (is (identical? cljs.core.PersistentTreeSet (type s1)))
      (is (identical? cljs.core.PersistentTreeSet (type s2)))
      (is (identical? compare (-comparator s1)))
      (is (zero? (count s1)))
      (is (zero? (count s2)))
      (is (nil? (rseq s1)))
      (let [s1 (conj s1 1 2 3)
            s2 (conj s2 1 2 3)
            s3 (conj s3 1 2 3 7 8 9)
            s4 (conj s4 1 2 3)]
        (testing "basic ops"
          (is (= (hash s1) (hash s2)))
          (is (= (hash s1) (hash #{1 2 3})))
          (is (= (seq s1)  (list 1 2 3)))
          (is (= (rseq s1) (list 3 2 1)))
          (is (= (seq s2)  (list 3 2 1)))
          (is (= (rseq s2) (list 1 2 3)))
          (is (= (count s1) 3))
          (is (= (count s2) 3))
          (is (= (count s3) 4))
          (is (= (get s3 0) 1))
          (is (= (subseq s3 > 5) (list 7 8)))
          (is (= (subseq s3 > 6) (list 8)))
          (is (= (subseq s3 >= 6) (list 7 8)))
          (is (= (subseq s3 >= 12) nil))
          (is (= (subseq s3 < 0) (list)))
          (is (= (subseq s3 < 5) (list 1 2)))
          (is (= (subseq s3 < 6) (list 1 2)))
          (is (= (subseq s3 <= 6) (list 1 2 7)))
          (is (= (subseq s3 >= 2 <= 6) (list 2 7)))
          (is (= (subseq s4 >= 2 < 3) (list 2)))
          (let [s1 (disj s1 2)
                s2 (disj s2 2)]
            (testing "edge cases"
              (is (= (seq s1)  (list 1 3)))
              (is (= (rseq s1) (list 3 1)))
              (is (= (seq s2)  (list 3 1)))
              (is (= (rseq s2) (list 1 3)))
              (is (= (count s1) 2))
              (is (= (count s2) 2)))))))))

(deftest test-transducers
  (testing "Testing transducers"
    (is (= (sequence (map inc) (array 1 2 3)) '(2 3 4)))
    (is (= (apply str (sequence (map #(.toUpperCase %)) "foo")) "FOO"))
    (is (== (hash [1 2 3]) (hash (sequence (map inc) (range 3)))))
    (is (= [1 2 3] (sequence (map inc) (range 3))))
    (is (= (sequence (map inc) (range 3)) [1 2 3]))
    (is (= (sequence (remove even?) (range 10)) '(1 3 5 7 9)))
    (is (= (sequence (take 5) (range 10))
          '(0 1 2 3 4)))
    (is (= (sequence (take-while #(< % 5)) (range 10))
          '(0 1 2 3 4)))
    (is (= (sequence (drop 5) (range 10))
          '(5 6 7 8 9)))
    (is (= (sequence (drop-while #(< % 5)) (range 10))
          '(5 6 7 8 9)))
    (is (= (sequence (take-nth 2) (range 10))
          '(0 2 4 6 8)))
    (is (= (sequence (replace {:foo :bar}) '(:foo 1 :foo 2))
          '(:bar 1 :bar 2)))
    (let [ret (into [] (map inc) (range 3))]
      (is (and (vector? ret) (= ret '(1 2 3)))))
    (let [ret (into [] (filter even?) (range 10))]
      (is (and (vector? ret) (= ret '(0 2 4 6 8)))))
    (is (= (map inc (sequence (map inc) (range 3)))
          '(2 3 4)))
    (is (= (sequence (dedupe) [1 1 2 2 3 3])
          '(1 2 3)))
    (is (= (mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])
          (range 10)))
    (is (= (sequence (mapcat reverse) [[3 2 1 0] [6 5 4] [9 8 7]])
          (range 10)))
    (is (= (seq (eduction (map inc) [1 2 3])) '(2 3 4)))
    (is (= (sequence (partition-by #{:split}) [1 2 3 :split 4 5 6])
          '([1 2 3] [:split] [4 5 6])))
    (is (= (sequence (partition-all 3) '(1 2 3 4 5))
          '([1 2 3] [4 5])))
    (is (= (sequence (keep identity) [1 nil 2 nil 3])
          '(1 2 3)))
    (is (= (keep-indexed identity [:foo nil :bar nil :baz])
          (sequence (keep-indexed identity) [:foo nil :bar nil :baz])))
    (let [xform (comp (map inc)
                  (filter even?)
                  (dedupe)
                  (mapcat range)
                  (partition-all 3)
                  (partition-by #(< (apply + %) 7))
                  (mapcat flatten)
                  (random-sample 1.0)
                  (take-nth 1)
                  (keep #(when (odd? %) (* % %)))
                  (keep-indexed #(when (even? %1) (* %1 %2)))
                  (replace {2 "two" 6 "six" 18 "eighteen"})
                  (take 11)
                  (take-while #(not= 300 %))
                  (drop 1)
                  (drop-while string?)
                  (remove string?))
          data (vec (interleave (range 18) (range 20)))]
      (is (= (sequence xform data) '(36 200 10))))
    (let [xf (map #(+ %1 %2))]
      (is (= (sequence xf [0 0] [1 2]) [1 2])))
    ))

(deftest test-obj-equiv
  (testing "Object equiv method"
    (is (.equiv :foo :foo))
    (is (.equiv 'foo 'foo))
    (is (.equiv {:foo 1 :bar 2} {:foo 1 :bar 2}))
    (is (.equiv [1 2 3] [1 2 3]))
    (is (.equiv '(1 2 3) '(1 2 3)))
    (is (.equiv (map inc [1 2 3]) (map inc [1 2 3])))
    (is (.equiv #{:cat :dog :bird} #{:cat :dog :bird}))
    ))

(deftest test-es6-interfaces
  (testing "ES6 collection interfaces"
    (let [iter (es6-iterator [1 2 3])]
      (testing "basic iterations"
        (is (= (.-value (.next iter)) 1))
        (is (= (.-value (.next iter)) 2))
        (is (= (.-value (.next iter)) 3))
        (is (.-done (.next iter)))))
    (is (.has {:foo "bar"} :foo))
    (is (= (.get {:foo "bar"} :foo) "bar"))
    (let [iter (.keys {:foo "bar" :baz "woz"})]
      (testing "map key iteration"
        (is (#{:foo :baz} (.-value (.next iter))))
        (is (#{:foo :baz} (.-value (.next iter))))
        (is (.-done (.next iter)))))
    (let [eiter (.entries {:foo "bar" :baz "woz"})]
      (testing "map entry iteration"
        (is (= (seq (.-value (.next eiter))) (seq #js [:foo "bar"])))
        (is (= (seq (.-value (.next eiter))) (seq #js [:baz "woz"])))
        (is (.-done (.next eiter)))))
    (let [iter (.values {:foo "bar" :baz "woz"})]
      (testing "map value iteration"
        (is (#{"bar" "woz"} (.-value (.next iter))))
        (is (#{"bar" "woz"} (.-value (.next iter))))
        (is (.-done (.next iter)))))
    (is (.has #{:cat :bird :dog} :bird))
    (let [iter (.keys #{:cat :bird :dog})]
      (testing "set key iteration"
        (is (#{:cat :bird :dog} (.-value (.next iter))))
        (is (#{:cat :bird :dog} (.-value (.next iter))))
        (is (#{:cat :bird :dog} (.-value (.next iter))))
        (is (.-done (.next iter)))))
    (let [iter (.entries #{:cat :bird :dog})]
      (testing "set entry iteration"
        (is (#{[:cat :cat] [:bird :bird] [:dog :dog]} (seq (.-value (.next iter)))))
        (is (#{[:cat :cat] [:bird :bird] [:dog :dog]} (seq (.-value (.next iter)))))
        (is (#{[:cat :cat] [:bird :bird] [:dog :dog]} (seq (.-value (.next iter)))))
        (is (.-done (.next iter)))))
    (let [iter (.values #{:cat :bird :dog})]
      (testing "set value iteration"
        (is (#{:cat :bird :dog} (.-value (.next iter))))
        (is (#{:cat :bird :dog} (.-value (.next iter))))
        (is (#{:cat :bird :dog} (.-value (.next iter))))
        (is (.-done (.next iter)))))
))

(deftest test-mumur-support
  (testing "Testing murmur support"
    ;; int-rotate-left
    (is (== (int-rotate-left (bit-or 0x87654321 0) 4) (bit-or 0x76543218 0)))
    (is (== (int-rotate-left (bit-or 0x87654321 0) 8) (bit-or 0x65432187 0)))
    (is (== (int-rotate-left (bit-or 0x80000000 0) 1) 0x1))
    (is (== (int-rotate-left (bit-or 0x78123456 0) 4) (bit-or 0x81234567 0)))
    (is (== (int-rotate-left (bit-or 0xffffffff 0) 4) (bit-or 0xffffffff 0)))

    ;; imul
    (is (== (imul 3 3) 9))
    (is (== (imul -1 8) -8))
    (is (== (imul -2 -2) 4))
    (is (== (imul 0xffffffff 5) -5))
    (is (== (imul 0xfffffffe 5) -10))
    ))

(deftest test-print-knobs
  (testing "Testing printing knobs"
    (is (= (binding [*print-length* 0] (str [1 2 3 4 5 6 7 8 9 0]))
              "[...]"))
    (is (= (binding [*print-length* 1] (str [1 2 3 4 5 6 7 8 9 0]))
              "[1 ...]"))
    (is (= (binding [*print-length* 2] (str [1 2 3 4 5 6 7 8 9 0]))
              "[1 2 ...]"))
    (is (= (binding [*print-length* 10] (str [1 2 3 4 5 6 7 8 9 0]))
              "[1 2 3 4 5 6 7 8 9 0]"))
    ;; CLJS-804
    (is (= (binding [*print-length* 10] (str {:foo "bar"}))
              "{:foo \"bar\"}"))
    (is (= (binding [*print-length* 0] (str {:foo "bar" :baz "woz"}))
          "{...}"))
    (is (= (binding [*print-length* 1] (str {:foo "bar" :baz "woz"}))
              "{:foo \"bar\", ...}"))
    (is (= (binding [*print-length* 10] (str {:foo "bar" :baz "woz"}))
              "{:foo \"bar\", :baz \"woz\"}")))
  )

(defrecord PrintMe [a b])

(deftest test-pr-str
  (testing "Testing pr-str"
    (is (= (pr-str 1) "1"))
    (is (= (pr-str -1) "-1"))
    (is (= (pr-str -1.5) "-1.5"))
    (is (= (pr-str [3 4]) "[3 4]"))
    (is (= (pr-str "foo") "\"foo\""))
    (is (= (pr-str :hello) ":hello"))
    (is (= (pr-str 'goodbye) "goodbye"))
    ;;(is (= (pr-str #{1 2 3}) "#{1 2 3}"))
    (is (= (pr-str '(7 8 9)) "(7 8 9)"))
    (is (= (pr-str '(deref foo)) "(deref foo)"))
    (is (= (pr-str '(quote bar)) "(quote bar)"))
    (is (= (pr-str 'foo/bar) "foo/bar"))
    (is (= (pr-str \a) "\"a\""))
    (is (= (pr-str :foo/bar) ":foo/bar"))
    (is (= (pr-str nil) "nil"))
    (is (= (pr-str true) "true"))
    (is (= (pr-str false) "false"))
    (is (= (pr-str "string") "\"string\""))
    (is (= (pr-str ["üñîçó∂£" :ทดสอบ/你好 'こんにちは]) "[\"üñîçó∂£\" :ทดสอบ/你好 こんにちは]"))
    (is (= (pr-str "escape chars \t \r \n \\ \" \b \f") "\"escape chars \\t \\r \\n \\\\ \\\" \\b \\f\""))
    (is (= (pr-str (PrintMe. 1 2)) "#cljs.core-test.PrintMe{:a 1, :b 2}"))
    (is (= (pr-str (js/Date. "2010-11-12T13:14:15.666-05:00"))
          "#inst \"2010-11-12T18:14:15.666-00:00\""))
    (doseq [month (range 1 13)
            day   (range 1 29)
            hour  (range 1 23)]
      (let [pad (fn [n]
                  (if (< n 10)
                    (str "0" n)
                    n))
            inst (str "2010-" (pad month) "-" (pad day) "T" (pad hour) ":14:15.666-00:00")]
        (is (= (pr-str (js/Date. inst)) (str "#inst \"" inst "\"")))))
    (let [uuid-str "550e8400-e29b-41d4-a716-446655440000"
          uuid (UUID. uuid-str)]
      (is (= (pr-str uuid) (str "#uuid \"" uuid-str "\""))))
    ;; pr-str PersistentQueueSeq - CLJS-800
    (is (= (pr-str (rest (conj cljs.core.PersistentQueue.EMPTY 1 2 3))) "(2 3)"))
    ))

(deftest test-inext
  (testing "Testing INext"
    (is (= nil (next nil)))
    (is (= nil (next (seq (array 1)))))
    (is (= '(2 3) (next (seq (array 1 2 3)))))
    (is (= nil (next (reverse (seq (array 1))))))
    (is (= '(2 1) (next (reverse (seq (array 1 2 3))))))
    (is (= nil (next (cons 1 nil))))
    (is (= '(2 3) (next (cons 1 (cons 2 (cons 3 nil))))))
    (is (= nil (next (lazy-seq (cons 1 nil)))))
    (is (= '(2 3) (next (lazy-seq
                              (cons 1
                                (lazy-seq
                                  (cons 2
                                    (lazy-seq (cons 3 nil)))))))))
    (is (= nil (next (list 1))))
    (is (= '(2 3) (next (list 1 2 3))))
    (is (= nil (next [1])))
    (is (= '(2 3) (next [1 2 3])))
    (is (= nil (next (range 1 2))))
    (is (= '(2 3) (next (range 1 4))))
    ))

(deftest test-chunked
  (let [r (range 64)
        v (into [] r)]
    (testing "Testing Chunked Seqs"
      (is (= (hash (seq v)) (hash (seq v))))
      (is (= 6 (reduce + (array-chunk (array 1 2 3)))))
      (is (instance? ChunkedSeq (seq v)))
      (is (= r (seq v)))
      (is (= (map inc r) (map inc v)))
      (is (= (filter even? r) (filter even? v)))
      (is (= (filter odd? r) (filter odd? v)))
      (is (= (concat r r r) (concat v v v)))
      (is (satisfies? IReduce (seq v)))
      (is (== 2010 (reduce + (nnext (nnext (seq v))))))
      (is (== 2020 (reduce + 10 (nnext (nnext (seq v)))))))))

(deftest test-reader-literals
  (testing "Testing reader literals"
    (is (= #queue [1] (into cljs.core.PersistentQueue.EMPTY [1])))
    (is (not= #queue [1 2] (into cljs.core.PersistentQueue.EMPTY [1])))
    (is (= #inst "2010-11-12T18:14:15.666-00:00"
          #inst "2010-11-12T13:14:15.666-05:00"))
    (is (= #uuid "550e8400-e29b-41d4-a716-446655440000"
          #uuid "550e8400-e29b-41d4-a716-446655440000"))
    (is (= 42
          (get {#uuid "550e8400-e29b-41d4-a716-446655440000" 42}
            #uuid "550e8400-e29b-41d4-a716-446655440000")))
    ))

(deftest test-uuid
  (testing "Testing UUID"
    (is (= (UUID. "550e8400-e29b-41d4-a716-446655440000")
              (UUID. "550e8400-e29b-41d4-a716-446655440000")))
    (is (not (identical? (UUID. "550e8400-e29b-41d4-a716-446655440000")
                   (UUID. "550e8400-e29b-41d4-a716-446655440000"))))
    (is (= 42 (get {(UUID. "550e8400-e29b-41d4-a716-446655440000") 42}
                    (UUID. "550e8400-e29b-41d4-a716-446655440000")
                    :not-at-all-found)))
    (is (= :not-at-all-found
              (get {(UUID. "550e8400-e29b-41d4-a716-446655440000") 42}
                (UUID. "666e8400-e29b-41d4-a716-446655440000")
                :not-at-all-found)))
    ))

(deftest test-comparable
  (testing "Testing IComparable"
    (is (=  0 (compare false false)))
    (is (= -1 (compare false true)))
    (is (=  1 (compare true  false)))

    (is (= -1 (compare  0  1)))
    (is (= -1 (compare -1  1)))
    (is (=  0 (compare  1  1)))
    (is (=  1 (compare  1  0)))
    (is (=  1 (compare  1 -1)))

    (is (=  0 (compare "cljs" "cljs")))
    (is (=  0 (compare :cljs :cljs)))
    (is (=  0 (compare 'cljs 'cljs)))
    (is (= -1 (compare "a" "b")))
    (is (= -1 (compare :a :b)))
    (is (= -1 (compare 'a 'b)))
    ;; cases involving ns
    (is (= -1 (compare :b/a :c/a)))
    (is (= -1 (compare :c :a/b)))
    (is (=  1 (compare :a/b :c)))
    (is (= -1 (compare 'b/a 'c/a)))
    (is (= -1 (compare 'c 'a/b)))
    (is (=  1 (compare 'a/b 'c)))

    ;; This is different from clj. clj gives -2 next 3 tests
    (is (= -1 (compare "a" "c")))
    (is (= -1 (compare :a :c)))
    (is (= -1 (compare 'a 'c)))

    (is (= -1 (compare [1 2] [1 1 1])))
    (is (= -1 (compare [1 2] [1 2 1])))
    (is (= -1 (compare [1 1] [1 2])))
    (is (=  0 (compare [1 2] [1 2])))
    (is (=  1 (compare [1 2] [1 1])))
    (is (=  1 (compare [1 1 1] [1 2])))
    (is (=  1 (compare [1 1 2] [1 1 1])))

    (is (= -1 (compare (subvec [1 2 3] 1) (subvec [1 2 4] 1))))
    (is (=  0 (compare (subvec [1 2 3] 1) (subvec [1 2 3] 1))))
    (is (=  1 (compare (subvec [1 2 4] 1) (subvec [1 2 3] 1)))))
  )

(deftest test-dot
  (let [s "abc"]
   (testing "Testing dot operations"
     (is (= 3 (.-length s)))
     (is (= 3 (. s -length)))
     (is (= 3 (. (str 138) -length)))
     (is (= 3 (. "abc" -length)))
     (is (= "bc" (.substring s 1)))
     (is (= "bc" (.substring "abc" 1)))
     (is (= "bc" ((memfn substring start) s 1)))
     (is (= "bc" (. s substring 1)))
     (is (= "bc" (. s (substring 1))))
     (is (= "bc" (. s (substring 1 3))))
     (is (= "bc" (.substring s 1 3)))
     (is (= "ABC" (. s (toUpperCase))))
     (is (= "ABC" (. "abc" (toUpperCase))))
     (is (= "ABC" ((memfn toUpperCase) s)))
     (is (= "BC" (. (. s (toUpperCase)) substring 1)))
     (is (= 2 (.-length (. (. s (toUpperCase)) substring 1))))
     )))

(defrecord Person [firstname lastname])
(defrecord A [])
(defrecord C [a b c])
(defrecord A' [x])
(defrecord B' [x])

(deftest test-records
  (let [fred (Person. "Fred" "Mertz")
        fred-too (Person. "Fred" "Mertz")
        ethel (with-meta (assoc (Person. "Ethel" "Mertz") :husband :fred)
                {:married true})
        ethel-too (with-meta (assoc (Person. "Ethel" "Mertz")  :husband :fred)
                    {:married true})
        letters (C. "a" "b" "c")
        more-letters (assoc letters :d "d" :e "e" :f "f")]
    (testing "Testing records"
      (is (= (:firstname fred) "Fred"))
      (is (= fred fred-too))
      (is (false? (= fred nil)))
      (is (false? (= nil fred)))
      (is (= (meta ethel) {:married true}))
      (is (= ethel ethel-too))
      (is (= (map->Person {:firstname "Fred" :lastname "Mertz"}) fred))
      (is (= (->Person "Fred" "Mertz") fred))
      (is (= (count fred) 2))
      (is (= (count ethel) 3))
      (is (= (conj fred {:wife :ethel :friend :ricky})
                (map->Person {:firstname "Fred" :lastname "Mertz" :wife :ethel :friend :ricky})))
      (is (= (conj fred {:lastname "Flintstone"})
                (map->Person {:firstname "Fred" :lastname "Flintstone"})))
      (is (= (assoc fred :lastname "Flintstone")
                (map->Person {:firstname "Fred" :lastname "Flintstone"})))
      (is (= (assoc fred :wife :ethel)
                (map->Person {:firstname "Fred" :lastname "Mertz" :wife :ethel})))
      (is (= (dissoc ethel :husband)
                (map->Person {:firstname "Ethel" :lastname "Mertz"})))
      (is (= {:foo 'bar} (meta (with-meta (A.) {:foo 'bar}))))
      (is (= 'bar (:foo (assoc (A.) :foo 'bar))))
      (is (= (set (keys letters)) #{:a :b :c}))
      (is (= (set (keys more-letters)) #{:a :b :c :d :e :f}))
      (is (= (set (keys (dissoc more-letters :d))) #{:a :b :c :e :f}))
      (is (= (set (keys (dissoc more-letters :d :e))) #{:a :b :c :f}))
      (is (= (set (keys (dissoc more-letters :d :e :f))) #{:a :b :c}))
      (is (not= (A'. nil) (B'. nil))))))

(deftype FnLike []
  IFn
  (-invoke [_] :a)
  (-invoke [_ a] :b)
  (-invoke [_ a b] :c))

(deftype FnLikeB [a]
  IFn
  (-invoke [_] a))

(deftest test-ifn
  (testing "Testing IFn implementations"
    (is (= :a ((FnLike.))))
    (is (= :b ((FnLike.) 1)))
    (is (= :c ((FnLike.) 1 2)))
    (is (= [:b :b :b] (map (FnLike.) [0 0 0])))
    (is (= 1 ((FnLikeB. 1))))
    ))

(deftest test-case
  (testing "Test case expr"
    (let [x 1]
      (is (= (case x 1 :one) :one)))
    (let [x 1]
      (is (= (case x 2 :two :default) :default)))
    (let [x 1]
      (is (= (try
                   (case x 3 :three)
                   (catch js/Error e
                     :fail))
                :fail)))
    (let [x 1]
      (is (= (case x
                   (1 2 3) :ok
                   :fail)
                :ok)))
    (let [x [:a :b]]
      (is (= (case x
                   [:a :b] :ok)
                :ok)))
    (let [a 'a]
      (is (= (case a
                   nil nil
                   & :amp
                   :none)
                :none)))
    (let [a '&]
      (is (= (case a
                   nil nil
                   & :amp
                   :none)
                :amp)))
    (let [foo 'a]
      (testing "multiple match"
        (is (= (case foo
                 (a b c) :sym
                 :none)
              :sym))
        (is (= (case foo
                 (b c d) :sym
                 :none)
              :none))))
    ))

(defprotocol IHasFirst
  (-get-first [this]))

(defprotocol IFindsFirst
  (-find-first [this other]))

(deftype First [xs]
  ISeqable
  (-seq [this] (seq xs))
  IIndexed
  (-nth [this i] (nth xs i))
  (-nth [this i not-found] (nth xs i not-found))
  IFn
  (-invoke [[x]] x)
  (-invoke [this x] this)
  Object
  (toString [[x]] (str x))
  IHasFirst
  (-get-first [[x]] x)
  IFindsFirst
  (-find-first [_ [x]] x))

(deftype DestructuringWithLocals [a]
  IFindsFirst
  (-find-first [_ [x y]]
    [x y a]))

(deftest test-protocol-method-destructuring
  (testing "Testing protocol method destructuring"
    (let [fv (First. [1 2 3])
          fs (First. "asdf")]
      (testing "basic operations"
        (is (= (fv) 1))
        (is (= (fs) \a))
        (is (= (str fs) \a))
        (is (= (-get-first fv) 1))
        (is (= (-get-first fs) \a))
        (is (= (-find-first fv [1]) 1))
        (is (identical? (fv 1) fv))))
    (let [t (DestructuringWithLocals. 1)]
      (testing "with locals"
        (is (= [2 3 1] (-find-first t [2 3])))))))

(defprotocol IProtocolWithDocStrings
  (-method1 [this] "some doc")
  (-method2 [this] ""))

;; =============================================================================
;; Tickets

(deftest test-383
  (testing "Testing CLJS-383"
    (let [f1 (fn f1 ([] 0) ([a] 1) ([a b] 2) ([a b c & more] 3))
          f2 (fn f2 ([x] :foo) ([x y & more] (apply f1 y more)))]
      (is (= 1 (f2 1 2))))
    (let [f (fn ([]) ([a & more] more))]
      (is (nil? (f :foo))))
    (is (nil? (array-seq (array 1) 1))))  )

(deftest test-513
  (testing "Testing CLJS-513"
    (let [sentinel (js-obj)]
      (is (identical? sentinel (try ([] 0) (catch js/Error _ sentinel)))))))

(defprotocol IFoo (foo [this]))

(deftest test-reify-meta
  (is (= (meta (with-meta (reify IFoo (foo [this] :foo)) {:foo :bar}))
            {:foo :bar})))

;; hashing bug in many JS runtimes CLJ-118
(deftest test-clj-118
  (let [g #{(conj #{:2} :alt)}
        h #{#{:2 :alt}}]
    (is (= g h)))
  (is (= (hash {:a 1 :b 2})
            (hash {:b 2 :a 1})))
  (is (= (hash (hash-map :a 1 :b 2))
            (hash (hash-map :b 2 :a 1))))
  (is (= (hash {:start 133 :end 134})
            (hash (apply hash-map [:start 133 :end 134]))))
  (is (= (hash :a)
            (hash (keyword "a")))))

(defprotocol IBar (-bar [this x]))

(defn baz [f]
  (reify
    IBar
    (-bar [_ x]
      (f x))))

(deftest test-405
  (is (= 2 (-bar (baz inc) 1))))

(let [x "original"]
  (defn original-closure-stmt [] x))

(deftest test-401-411
  (let [x "overwritten"]
    (is (= "original" (original-closure-stmt))))
  (is (= "original" (let [x "original"
                               oce (fn [] x)
                               x "overwritten"]
                           (oce)))))

(deftest test-letfn-shadowing
  (letfn [(x [] "original")
          (y [] (x))]
    (let [x (fn [] "overwritten")]
      (is (= "original" (y))))))

(deftest test-459
  (is (= (reduce-kv conj [] (sorted-map :foo 1 :bar 2))
        [:bar 2 :foo 1])))

(deftest test-kv-reduce
  (letfn [(kvr-test [data expect]
            (and
              (= :reduced
                (reduce-kv
                  (fn [_ _ _] (reduced :reduced))
                  [] data))
              (= (sort expect)
                (sort
                  (reduce-kv
                    (fn [r k v] (-> r (conj [k v])))
                    [] data)))))]
    (testing "Testing IKVReduce"
      (doseq [[data expect] [[(obj-map :k0 :v0 :k1 :v1) [[:k0 :v0] [:k1 :v1]]]
                             [(hash-map :k0 :v0 :k1 :v1) [[:k0 :v0] [:k1 :v1]]]
                             [(array-map :k0 :v0 :k1 :v1) [[:k0 :v0] [:k1 :v1]]]
                             [[:v0 :v1] [[0 :v0] [1 :v1]]]]]
        (is (kvr-test data expect)))
      (is (= {:init :val} (reduce-kv assoc {:init :val} nil))))))

(deftest test-data-conveying-exceptions
  (is (= {:foo 1}
             (try (throw (ex-info "asdf" {:foo 1}))
                  (catch ExceptionInfo e
                    (ex-data e)))))
  (is (instance? js/Error (ex-info "asdf" {:foo 1})))
  (is (= (pr-str (ex-info "abc" {:x 1})) "#ExceptionInfo{:message \"abc\", :data {:x 1}}"))
  (is (= (pr-str (ex-info "abc" {:x 1} "def")) "#ExceptionInfo{:message \"abc\", :data {:x 1}, :cause \"def\"}"))
  (is (not (instance? cljs.core.ExceptionInfo (js/Error.)))))

(deftest test-435
  (is (= (assoc {} 154618822656 1 261993005056 1)
            {154618822656 1 261993005056 1})))

(deftest test-458
  (is (= (get-in {:a {:b 1}} [:a :b :c] :nothing-there)
        :nothing-there)))

(deftest test-464
  (is (nil? (get-in {:foo {:bar 2}} [:foo :bar :baz]))))

(deftest test-symbol-meta
  (is (= (meta (with-meta 'foo {:tag 'int})) {:tag 'int})))

(deftest test-467
  (is (= (reduce-kv + 0 (apply hash-map (range 1000)))
        (reduce + (range 1000)))))

(deftest test-477
  (is (= [js/undefined 1 2] ((fn [& more] more) js/undefined 1 2)))
  (is (= [js/undefined 4 5] ((fn [a b & more] more) 1 2 js/undefined 4 5))))

(deftest test-493
  (is (nil? (get 42 :anything)))
  (is (= (get 42 :anything :not-found) :not-found))
  (is (nil? (first (map get [42] [:anything]))))
  (is (= (first (map get [42] [:anything] [:not-found])) :not-found)))

(deftest test-481
  (let [fs (atom [])]
    (doseq [x (range 4)
            :let [y (inc x)
                  f (fn [] y)]]
      (swap! fs conj f))
    (is (= (map #(%) @fs) '(1 2 3 4)))))

(def exists?-test-val 'foo)

(deftest test-495
  (testing "Testing CLJS-495, exists?"
    (is (false? (exists? js/jQuery)))
    (is (exists? exists?-test-val))))

(deftest test-496
  (is (= (char 65) \A))
  (is (= (char \A) \A)))

(deftype PositionalFactoryTest [x])

(deftest test-515
  (is (== 1 (.-x (->PositionalFactoryTest 1)))))

(deftest test-518
  (is (nil? (:test "test"))))

;; r1798 core fn protocol regression
(extend-type object
  ISeqable
  (-seq [coll]
    (map #(vector % (aget coll %)) (js-keys coll)))

  ILookup
  (-lookup
    ([coll k]
     (-lookup coll k nil))
    ([coll k not-found]
     (if-let [v (aget coll k)]
       v
       not-found))))

(deftest test-extend-to-object
  (is (= (seq (js-obj "foo" 1 "bar" 2)) '(["foo" 1] ["bar" 2])))
  (is (= (get (js-obj "foo" 1) "foo") 1))
  (is (= (get (js-obj "foo" 1) "bar" ::not-found) ::not-found))
  (is (= (reduce (fn [s [k v]] (+ s v)) 0 (js-obj "foo" 1 "bar" 2)) 3)))

(deftest test-541
  (letfn [(f! [x] (print \f) x)
          (g! [x] (print \g) x)]
    (is (= "ffgfg"
          (with-out-str
            (instance? Symbol (f! 'foo))
            (max (f! 5) (g! 10))
            (min (f! 5) (g! 10)))))))

(deftest test-582
  (is (= #{1 2} (set [1 2 2])))
  (is (= #{1 2} (hash-set 1 2 2)))
  (is (= #{1 2} (apply hash-set [1 2 2]))))

(deftest test-585
  (is (= (last (map identity (into [] (range 32)))) 31))
  (is (= (into #{} (range 32))
            (set (map identity (into [] (range 32)))))))

(def foo580)
(def foo580 {:a (fn []) :b (fn [] (foo580 :a))})

(deftest test-580
  (is (nil? (((:b foo580))))))

(deftest test-587
  (is (== (first (filter #(== % 9999) (range))) 9999)))

(deftest test-604
  (is (= () (concat nil [])))
  (is (= () (concat [] []))))

(deftest test-600
  (is (= "foobar" (apply str (concat "foo" "bar")))))

(deftest test-608
  (is (= '("") (re-seq #"\s*" ""))))

(deftype KeywordTest []
  ILookup
  (-lookup [o k] :nothing)
  (-lookup [o k not-found] not-found))

(deftest tset-638
  (is (= (:a (KeywordTest.)) :nothing)))

(deftest test-648
  (let [a (reify IHash (-hash [_] 42))
        b (reify IHash (-hash [_] 42))
        s (set (range 128))]
    (testing "Testing CLJS-648 (CLJ-1285)"
      (is (= (-> (conj s a b) transient (disj! a) persistent! (conj a))
             (-> (conj s a b) transient (disj! a) persistent! (conj a)))))))

(deftest test-660
  (testing "Testing CLJS-660, namespace handling"
    (is (= (-> 'a.b keyword ((juxt namespace name))) [nil "a.b"]))
    (is (= (-> 'a.b/c keyword ((juxt namespace name))) ["a.b" "c"]))
    (is (= (-> "a.b" keyword ((juxt namespace name))) [nil "a.b"]))
    (is (= (-> "a.b/c" keyword ((juxt namespace name))) ["a.b" "c"]))))

(deftest test-663
  (testing "Testing CLJS-663, invalid keywords"
    (is (= (keyword 123) nil))
    (is (= (keyword (js/Date.)) nil))))

(deftest test-647
  (let [keys #(vec (js-keys %))
        z "x"]
    (testing "Testing CLJS-647, js-keys"
      (assert (= ["x"]
                (keys (js-obj "x" "y"))
                (keys (js-obj (identity "x") "y"))
                (keys (js-obj z "y")))))))


(def some-x 1)
(def some-y 1)

(deftest test-583
  (is (= (count #{some-x some-y}) 1)))

(deftest test-584
  (is (= (count {some-x :foo some-y :bar}) 1)))

(deftest test-717
  (testing "Testing CLJS-717, JS literals"
    (is (array? #js [1 2 3]))
    (is (= (alength #js [1 2 3]) 3))
    (is (= (seq #js [1 2 3]) (seq [1 2 3])))
    (is (= (set (js-keys #js {:foo "bar" :baz "woz"})) #{"foo" "baz"}))
    (is (= (aget #js {:foo "bar"} "foo") "bar"))
    (is (= (aget #js {"foo" "bar"} "foo") "bar"))
    (is (array? (aget #js {"foo" #js [1 2 3]} "foo")))
    (is (= (seq (aget #js {"foo" #js [1 2 3]} "foo")) '(1 2 3)))))

(deftest test-725
  (testing "Testing CLJS-725, drop"
    (is (= (apply vector (drop-while (partial = 1) [1 2 3])) [2 3]))
    (is (= (apply list (drop-while (partial = 1) [1 2 3])) '(2 3)))
    (is (= (set (drop 1 #js [1 2 3])) #{2 3}))))

(deftest test-724
  (is (nil? (first (rest (rest (rest (range 3))))))))

(deftest test-730
  (testing "Testing CLJS-730, object? predicate"
    (is (true? (object? #js {})))
    (is (false? (object? nil)))))

(deftest test-count-hash-set
  (is
    (== (count (hash-set [1 4] [2 4] [3 4] [0 3] [1 3] [2 3] [3 3]
                 [0 2] [1 2] [2 2] [3 2] [4 2] [0 1] [1 1]
                 [2 1] [3 1] [1 0] [2 0] [3 0]))
      (count (list [1 4] [2 4] [3 4] [0 3] [1 3] [2 3] [3 3]
               [0 2] [1 2] [2 2] [3 2] [4 2] [0 1] [1 1]
               [2 1] [3 1] [1 0] [2 0] [3 0])))))

(defprotocol IWoz
  (-woz [this]))

(def noz [])

(deftest test-414
  (testing "Testing CLJS-414, specify"
    (is (= (specify noz IWoz (-woz [_] :boz)) noz))
    (is (not (identical? (specify noz IWoz (-woz [_] :boz)) noz)))
    (is (= (-woz (specify noz IWoz (-woz [this] this))) noz))
    (is (= (-woz (specify noz IWoz (-woz [_] :boz))) :boz))))

(deftest test-734
  (testing "Testing CLJS-734, transient operations"
    (is (= (-> (transient []) (conj! 1 2) persistent!) [1 2]))
    (is (= (-> (transient #{1 2 3}) (disj! 1 2) persistent!) #{3}))
    (is (= (-> (transient {}) (assoc! :a 1 :b 2) persistent!) {:a 1 :b 2}))
    (is (= (-> (transient {:a 1 :b 2 :c 3}) (dissoc! :a :b) persistent!) {:c 3}))))

(deftest test-767
  (testing "Testing CLJS-767, invalid assoc"
    (doseq [n [nil "-1" "" "0" "1" false true (js-obj)]]
      (is (= :fail (try (assoc [1 2] n 4)
                        (catch js/Error e :fail))))
      (is (= :fail (try (assoc (subvec [1 2 3] 2) n 4)
                     (catch js/Error e :fail))))
      (is (= :fail (try (assoc (range 1 3) n 4)
                     (catch js/Error e :fail)))))))

(deftest test-768
  (testing "Testing CLJS-768, invalid assoc!"
    (doseq [n [nil "-1" "" "0" "1" false true (js-obj)]]
      (is (= :fail (try (assoc! (transient [1 2]) n 4)
                        (catch js/Error e :fail)))))))

(defn cljs-739 [arr names]
  (let [name (first names)]
    (if name
      (recur (conj arr (fn [] (println name)))
        (rest names))
      arr)))

(deftest test-739
  (testing "Testing CLJS-739, with-out-str"
    (set! *print-newline* true)
    (is (= (with-out-str (doseq [fn (cljs-739 [] [:a :b :c :d])] (fn)))
          ":a\n:b\n:c\n:d\n"))
    (set! *print-newline* false)))

(deftest test-728
  (testing "Testing CLJS-728, lookup with default"
    (doseq [n [nil "-1" "" "0" "1" false true (js-obj)]]
      (is (nil? (get [1 2] n)))
      (is (= :fail (try (nth [1 2] n) (catch js/Error e :fail))))
      (is (= 4 (get [1 2] n 4)))
      (is (= :fail (try (nth [1 2] n 4) (catch js/Error e :fail))))

      (is (nil? (get (subvec [1 2] 1) n)))
      (is (= :fail (try (nth (subvec [1 2] 1) n) (catch js/Error e :fail))))
      (is (= 4 (get (subvec [1 2] 1) n 4)))
      (is (= :fail (try (nth (subvec [1 2] 1) n 4) (catch js/Error e :fail))))

      (is (nil? (get (transient [1 2]) n)))
      (is (= :fail (try (nth (transient [1 2]) n) (catch js/Error e :fail))))
      (is (= 4 (get (transient [1 2]) n 4)))
      (is (= :fail (try (nth (transient [1 2]) n 4) (catch js/Error e :fail))))

      (is (nil? (get (range 1 3) n)))
      (is (= :fail (try (nth (range 1 3) n) (catch js/Error e :fail))))
      (is (= 4 (get (range 1 3) n 4)))
      (is (= :fail (try (nth (range 1 3) n 4) (catch js/Error e :fail))))))
  )

(deftest test-778
  (testing "Testing CLJS-778, -rest, -next RSeq"
    (is (= (-rest (rseq [0])) ()))
    (is (nil? (-next (rseq [0]))))
    (is (= (set (rseq [0])) #{0}))))

(def cljs-780 (atom {:foo (with-meta [] {:bar '(1 2 3)})}))

(deftest test-780
  (let [_ (swap! cljs-780 update-in [:foo] vary-meta update-in [:bar] vec)
        x (-> @cljs-780 :foo meta :bar)]
    (testing "Testing CLJS-780, update-in + vary-meta"
      (is (vector? x))
      (is (= x [1 2 3])))) )

(deftest test-782
  (testing "Testing CLJS-782, UUID toString"
    (is (= (.toString #uuid "550e8400-e29b-41d4-a716-446655440000")
          "550e8400-e29b-41d4-a716-446655440000"))))

(deftest test-784
  (testing "Testing CLJS-784, conj on maps"
    (doseq [m [(array-map) (hash-map) (sorted-map)]]
      (is (= :ok
            (try
              (conj m "foo")
              (catch js/Error _
                :ok))))
      (is (= {:foo 1} (conj m [:foo 1])))
      (is (= {:foo 1} (conj m {:foo 1})))
      (is (= {:foo 1} (conj m (list [:foo 1])))))
    (doseq [mt [array-map hash-map sorted-map]]
      (is (= {:foo 1 :bar 2 :baz 3}
            (conj (mt :foo 1)
              ((fn make-seq [from-seq]
                 ;; this tests specifically for user defined seq's, that implement the bare minimum, i.e. no INext
                 (when (seq from-seq)
                   (reify
                     ISeqable
                     (-seq [this] this)
                     ISeq
                     (-first [this] (first from-seq))
                     (-rest [this] (make-seq (rest from-seq))))))
               [[:bar 2] [:baz 3]]))))))
  )

(deftest test-case-keyword
  (is (= (let [x "a"] (case x :a 1 "a")) "a")))

(deftest test-801
  (testing "Testing CLJS-801, str"
    (is (= "0atrue:key/wordsymb/olfalse[1 2 3 4]1234.56789"
          (str 0 "a" true nil :key/word 'symb/ol false [1 2 3 4] 1234.5678 0x09)))))

(defn case-recur [value]
  (case value
    :a (recur :b)
    :b 0))

(deftest test-812
  (testing "Testing CLJS-812, case with recur"
    (is (= (case-recur :a) 0))))

(deftest test-816
  (testing "Testing CLJS-816, rename-keys"
    (is (= (set/rename-keys {:a "one" :b "two"} {:a :z}) {:z "one" :b "two"}))
    (is (= (set/rename-keys {:a "one" :b "two"} {:a :z :c :y}) {:z "one" :b "two"}))
    (is (= (set/rename-keys {:a "one" :b "two" :c "three"} {:a :b :b :a})
          {:a "two" :b "one" :c "three"}))) )

(deftest test-881
  (testing "Testing CLJS-881, duplicate keys in array maps"
    (is (= [:foo] (keys (apply array-map [:foo 1 :foo 2]))))))

(deftest test-810
  (let [not-strings [true false nil 1 (fn [])]]
    (testing "Testing CLJS-810, exception on bad input to regex fns"
      (is (every? #(= :failed (try (re-find #"." %)
                                   (catch js/TypeError _ :failed))) not-strings))
      (is (every? #(= :failed (try (re-matches #"." %)
                                   (catch js/TypeError _ :failed))) not-strings))
      (is (every? #(= :failed (try (re-find #"nomatch" %)
                                   (catch js/TypeError _ :failed))) not-strings))
      (is (every? #(= :failed (try (re-matches #"nomatch" %)
                                   (catch js/TypeError _ :failed))) not-strings)))))

(deftest test-849
  (let [xs [44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24]]
    (testing "Testing CLJS-849, transient contains?"
      (is (loop [m (transient (zipmap xs (repeat 1)))
                 xs xs]
            (if-let [x (first xs)]
              (if (contains? m x)
                (recur (dissoc! m x) (next xs))
                false)
              true))))))

(deftest test-large-array-map
  (let [m (array-map 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13 14 14 15 15)]
    (testing "Testing large array maps"
      (is (instance? cljs.core/PersistentArrayMap m))
      (is (= (seq m) [[0 0] [1 1] [2 2] [3 3] [4 4] [5 5] [6 6] [7 7] [8 8] [9 9] [10 10] [11 11] [12 12] [13 13] [14 14] [15 15]])))))

(def test-map
  {:a 1
   :b 2
   #inst "2013-12-19T05:00:00.000-00:00" 3
   :d 4
   :e 5
   #inst "2013-12-06T05:00:00.000-00:00" 6
   :g 7
   :h 8
   :i 9
   :j 10})

(deftest test-716
  (testing "Testing CLJS-716, date as keys in maps"
    (is (= (test-map #inst "2013-12-19T05:00:00.000-00:00") 3))
    (is (= (test-map #inst "2013-12-06T05:00:00.000-00:00") 6))))

(deftest test-853
  (testing "Testing CLJS-853, function metadata"
    (is (= {:foo true} (meta ^:foo (fn []))))))

(deftest test-807 
  (testing "Testing CLJS-807, big int, float, big dec literals"
    (is (= -1 -1N))
    (is (= 9.007199254740996E15 9007199254740995N))
    (is (= 1.5 1.5M))
    (is (= 4.9E-324 5E-324M))))

(deftest test-transient-edge-case-1
  (let [v1 (vec (range 15 48))
        v2 (vec (range 40 57))
        v1 (persistent! (assoc! (conj! (pop! (transient v1)) :foo) 0 :quux))
        v2 (persistent! (assoc! (conj! (transient v2) :bar) 0 :quux))
        v  (into v1 v2)]
    (is (= v (vec (concat [:quux] (range 16 47) [:foo]
                    [:quux] (range 41 57) [:bar]))))))

(deftest test-transient-edge-case-2
  (is (loop [v  (transient [])
             xs (range 100)]
        (if-let [x (first xs)]
          (recur
            (condp #(%1 (mod %2 3)) x
              #{0 2} (conj! v x)
              #{1}   (assoc! v (count v) x))
            (next xs))
          (= (vec (range 100)) (persistent! v))))))

(deftest test-phm
  ;; PersistentHashMap & TransientHashMap
  (loop [m1 cljs.core.PersistentHashMap.EMPTY
         m2 (transient cljs.core.PersistentHashMap.EMPTY)
         i 0]
    (if (< i 100)
      (recur (assoc m1 i i) (assoc! m2 i i) (inc i))
      (let [m2 (persistent! m2)]
        (is (= (count m1) 100))
        (is (= (count m2) 100))
        (is (= m1 m2))
        (loop [i 0]
          (if (< i 100)
            (do (is (= (m1 i) i))
                (is (= (m2 i) i))
                (is (= (get m1 i) i))
                (is (= (get m2 i) i))
                (is (contains? m1 i))
                (is (contains? m2 i))
                (recur (inc i)))))
        (is (= (map vector (range 100) (range 100)) (sort-by first (seq m1))))
        (is (= (map vector (range 100) (range 100)) (sort-by first (seq m2))))
        (is (not (contains? (dissoc m1 3) 3))))))
  (let [tm (-> (->> (interleave (range 10) (range 10))
                    (apply assoc cljs.core.PersistentHashMap.EMPTY))
               (dissoc 3 5 7)
               transient)]
    (doseq [k [0 1 2 4 6 8 9]]
      (is (= k (get tm k))))
    (let [m (persistent! tm)]
      (is (= 2 (try (dissoc! tm 1) 1 (catch js/Error e 2))))
      (is (= 2 (try (assoc! tm 10 10) 1 (catch js/Error e 2))))
      (is (= 2 (try (persistent! tm) 1 (catch js/Error e 2))))
      (is (= 2 (try (count tm) 1 (catch js/Error e 2))))
      (is (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))
  (let [m (-> (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentHashMap.EMPTY))
            (dissoc 3 5 7))]
    (testing "Testing PHM dissoc"
      (is (= (count m) 7))
      (is (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))
  (let [m (-> (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentHashMap.EMPTY))
            (conj [:foo 1]))]
    (testing "Testing PHM conj"
      (is (= (count m) 11))
      (is (= m {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :foo 1}))))
  (let [m (-> (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentHashMap.EMPTY)
                transient)
            (conj! [:foo 1])
            persistent!)]
    (testing "Testing PHM conj!"
      (is (= (count m) 11))
      (is (= m {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :foo 1}))))
  (let [tm (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentHashMap.EMPTY)
                transient)]
    (testing "Testing transient PHM"
      (is (loop [tm tm ks [3 5 7]]
            (if-let [k (first ks)]
              (recur (dissoc! tm k) (next ks))
              (let [m (persistent! tm)]
                (and (= (count m) 7)
                     (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))))))
)

(deftype FixedHash [h v]
  IHash
  (-hash [this] h)
  IEquiv
  (-equiv [this other]
    (and (instance? FixedHash other) (= v (.-v other)))))

(def fixed-hash-foo (FixedHash. 0 :foo))
(def fixed-hash-bar (FixedHash. 0 :bar))

(deftest test-phm-fixed-hash
  (let [m (assoc cljs.core.PersistentHashMap.EMPTY
            fixed-hash-foo 1
            fixed-hash-bar 2)]
    (is (= (get m fixed-hash-foo) 1))
    (is (= (get m fixed-hash-bar) 2))
    (is (= (count m) 2))
    (let [m (dissoc m fixed-hash-foo)]
      (is (= (get m fixed-hash-bar) 2))
      (is (not (contains? m fixed-hash-foo)))
      (is (= (count m) 1))))

  (let [m (into cljs.core.PersistentHashMap.EMPTY ; make sure we're testing
            (zipmap (range 100) (range 100))) ; the correct map type
        m (assoc m fixed-hash-foo 1 fixed-hash-bar 2)]
    (is (= (count m) 102))
    (is (= (get m fixed-hash-foo) 1))
    (is (= (get m fixed-hash-bar) 2))
    (let [m (dissoc m 3 5 7 fixed-hash-foo)]
      (is (= (get m fixed-hash-bar) 2))
      (is (not (contains? m fixed-hash-foo)))
      (is (= (count m) 98))))

  (let [m (into cljs.core.PersistentHashMap.EMPTY ; make sure we're testing
            (zipmap (range 100) (range 100))) ; the correct map type
        m (transient m)
        m (assoc! m fixed-hash-foo 1)
        m (assoc! m fixed-hash-bar 2)
        m (persistent! m)]
    (is (= (count m) 102))
    (is (= (get m fixed-hash-foo) 1))
    (is (= (get m fixed-hash-bar) 2))
    (let [m (dissoc m 3 5 7 fixed-hash-foo)]
      (is (= (get m fixed-hash-bar) 2))
      (is (not (contains? m fixed-hash-foo)))
      (is (= (count m) 98)))))

(def array-map-conversion-threshold
  cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD)

(deftest test-pam
  (let [m (-> (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentArrayMap.EMPTY))
            (dissoc 3 5 7))]
    (is (= (count m) 7))
    (is (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9})))
  (let [m (-> (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentArrayMap.EMPTY))
            (conj [:foo 1]))]
    (is (= (count m) 11))
    (is (= m {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :foo 1})))
  (let [m (-> (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentArrayMap.EMPTY)
                transient)
            (conj! [:foo 1])
            persistent!)]
    (is (= (count m) 11))
    (is (= m {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :foo 1})))
  (let [tm (->> (interleave (range 10) (range 10))
             (apply assoc cljs.core.PersistentArrayMap.EMPTY)
             transient)]
    (loop [tm tm ks [3 5 7]]
      (if-let [k (first ks)]
        (recur (dissoc! tm k) (next ks))
        (let [m (persistent! tm)]
          (is (= (count m) 7))
          (is (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))))
  (let [tm (-> (->> (interleave (range 10) (range 10))
                 (apply assoc cljs.core.PersistentArrayMap.EMPTY))
             (dissoc 3 5 7)
             transient)]
    (doseq [k [0 1 2 4 6 8 9]]
      (is (= k (get tm k))))
    (let [m (persistent! tm)]
      (is (= 2 (try (dissoc! tm 1) 1 (catch js/Error e 2))))
      (is (= 2 (try (assoc! tm 10 10) 1 (catch js/Error e 2))))
      (is (= 2 (try (persistent! tm) 1 (catch js/Error e 2))))
      (is (= 2 (try (count tm) 1 (catch js/Error e 2))))
      (is (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))
  (let [m (apply assoc cljs.core.PersistentArrayMap.EMPTY
            (interleave (range (* 2 array-map-conversion-threshold))
              (range (* 2 array-map-conversion-threshold))))]
    (is (= (count m) (* 2 array-map-conversion-threshold)))
    (is (= (m array-map-conversion-threshold) array-map-conversion-threshold))
    (is (= m (into cljs.core.PersistentHashMap.EMPTY
                   (map #(vector % %)
                     (range (* 2 array-map-conversion-threshold)))))))
  )

(deftest test-literal-maps
  (loop [m1 {} m2 {} i 0]
    (if (< i 100)
      (recur (assoc m1 i i) (assoc m2 (str "foo" i) i) (inc i))
      (do (is (= m1 (into cljs.core.PersistentHashMap.EMPTY
                          (map vector (range 100) (range 100)))))
          (is (= m2 (into cljs.core.PersistentHashMap.EMPTY
                          (map vector
                            (map (partial str "foo") (range 100))
                            (range 100)))))
          (is (= (count m1) 100))
          (is (= (count m2) 100)))))
  )

(deftest test-461
  ;; CLJS-461: automatic map conversions
  (loop [i 0 m (with-meta {} {:foo :bar}) result []]
    (if (<= i (+ cljs.core.ObjMap.HASHMAP_THRESHOLD 2))
      (recur (inc i) (assoc m (str i) i) (conj result (meta m)))
      (let [n (inc (+ cljs.core.ObjMap.HASHMAP_THRESHOLD 2))
            expected (repeat n {:foo :bar})]
        (is (= result expected)))))
  (loop [i 0 m (with-meta {-1 :quux} {:foo :bar}) result []]
    (if (<= i (+ cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD 2))
      (recur (inc i) (assoc m i i) (conj result (meta m)))
      (let [n (inc (+ cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD 2))
            expected (repeat n {:foo :bar})]
        (is (= result expected))))))

(deftest test-transient-hash-set
  ;; TransientHashSet
  (loop [s (transient #{})
         i 0]
    (if (< i 100)
      (recur (conj! s i) (inc i))
      (loop [s s i 0]
        (if (< i 100)
          (if (zero? (mod i 3))
            (recur (disj! s i) (inc i))
            (recur s (inc i)))
          (let [s (persistent! s)]
            (is (= s (loop [s #{} xs (remove #(zero? (mod % 3)) (range 100))]
                           (if-let [x (first xs)]
                             (recur (conj s x) (next xs))
                             s))))
            (is (= s (set (remove #(zero? (mod % 3)) (range 100))))))))))
)

(deftest test-921-var-meta-name
  (testing "testing CLJS-921, :name var metadata should be unqualified"
    (is (= (-> (var first) meta :name) 'first))))

(deftype MyWatchable []
  IWatchable
  (-notify-watches [this oldval newval])
  (-add-watch [this key f])
  (-remove-watch [this key]))

(deftest test-920-watch-ops-return-ref
  (testing "tesing CLJS-920, add-watch/return-watch should return reference"
    (let [w (MyWatchable.)]
      (is (identical? (add-watch w :foo (fn [])) w))
      (is (identical? (remove-watch w :foo) w)))))

(deftype MyCustomAtom [^:mutable state]
  IDeref
  (-deref [_] state)
  IReset
  (-reset! [_ newval]
    (set! state newval)))

(deftest test-919-generic-cas
  (testing "testing CLJS-919, CAS should on custom atom types"
    (let [a0 (MyCustomAtom. 10)
          a1 (MyCustomAtom. 0)]
      (compare-and-set! a0 0 20)
      (compare-and-set! a1 0 20)
      (is (== @a0 10))
      (is (== @a1 20)))))

(deftest test-962-empty-literal-hashes
  (testing "CLJS-962: empty literals should produce collections with correct hash codes"
    (let [l ()
          v []
          s #{}
          m {}]
      (is (== (hash l) (hash v) (hash-ordered-coll ())))
      (is (== (hash s) (hash m) (hash-unordered-coll #{})))))
  (testing "CLJS-962: EMPTY collections should have correct hash codes"
    (let [l   (.-EMPTY List)
          pv  (.-EMPTY PersistentVector)
          phs (.-EMPTY PersistentHashSet)
          pts (.-EMPTY PersistentTreeSet)
          pam (.-EMPTY PersistentArrayMap)
          phm (.-EMPTY PersistentHashMap)
          ptm (.-EMPTY PersistentTreeMap)]
      (is (== (hash l) (hash pv) (hash-ordered-coll ())))
      (is (apply == (hash-unordered-coll #{}) (map hash [phs pts pam phm ptm]))))))

(deftest test-map-new-transducers
  (testing "Test distinct, interpose, map-indexed transducers"
    (is (= [1 2 3]
           (transduce (distinct) conj [] [1 1 2 2 3 3])))
    (is (= [1 :foo 2 :foo 3]
           (transduce (interpose :foo) conj [] [1 2 3])))
    (is (= [[0 1] [1 2] [2 3]]
           (transduce (map-indexed (fn [i x] [i x])) conj [] [1 2 3])))))

(deftest test-vec
  (let [v (vec #js [1 2 3 4])]
    (is (= (count v) 4))
    (is (= (first v) 1))
    (is (= (last v) 4))
    (is (= v [1 2 3 4]))))

(deftest test-phm-from-array
  (let [m (.fromArray PersistentHashMap #js [1 2 3 4] true)]
    (is (= (count m) 2))
    (is (contains? m 1))
    (is (contains? m 3))
    (is (= (get m 1) 2))
    (is (= (get m 3) 4))
    (is (= m {1 2 3 4}))))

(defn foo-var [f]
  (fn [x]
    (f x)))

(defn foo-set [x]
  (first x))

(deftest test-cljs-982-var-deref []
  (let [f (foo-var #'foo-set)]
    (is (= (f [1 2 3]) 1))
    (set! foo-set (fn [x] :oops))
    (is (= (f [1 2 3]) :oops))))

(deftest test-cljs-993
  (is (nil? (binding [*print-level* 4])))
  (is (= (binding [*print-level* 4] *print-level*) 4))
  (is (nil? (try
              (binding [*print-level* 4]
                (throw (js/Error.)))
              (catch js/Error e
                *print-level*)))))

(comment
  ;; ObjMap
  ;; (let [ks (map (partial str "foo") (range 500))
  ;;       m  (apply obj-map (interleave ks (range 500)))]
  ;;   (assert (instance? cljs.core.ObjMap m))
  ;;   (assert (= 500 (count m)))
  ;;   (assert (= 123 (m "foo123"))))

  ;; vars

  ;; (defn var-test
  ;;   "A docstring"
  ;;   [a b]
  ;;   (+ a b))

  ;; (let [var-meta (meta #'var-test)]
  ;;   (assert (= (:doc var-meta) "A docstring"))
  ;;   (assert (= (:arglists var-meta) '([a b]))))

  )
