(ns ^{:doc "Tests clojure.math to compare between JVM provided functions and the
      clojure.math implementations on a ClojureScript instance running on NodeJS.
      Tests are generative, but not run through the defspec framework to minimize
      i/o to the ClojureScript instance."
      :authors ["Michiel Borkent" "Paula Gearon"]}
    clojure.gen-math-test
    (:require [cljs.core.server]
              [cljs.repl.node]
              [clojure.core.server :as server]
              [clojure.edn :as edn]
              [clojure.java.io :as io]
              [clojure.test :as t :refer [deftest is]]
              [clojure.test.check.clojure-test :refer [defspec]]
              [clojure.test.check.generators :as gen]
              [clojure.test.check.properties :as prop]))

(def ^:const Number-MAX_SAFE_INTEGER  9007199254740991)
(def ^:const Number-MIN_SAFE_INTEGER -9007199254740991)
(defn Number-isSafeInteger
  [n]
  (and (>= n Number-MIN_SAFE_INTEGER)
       (<= n Number-MAX_SAFE_INTEGER)))

(def gen-small-integer
  "Generates a positive or negative integer bounded by the generator's
  `size` parameter. Shrinks to zero."
  (gen/sized (fn [size] (gen/choose (- size) size))))

(def reader (atom nil))
(def writer (atom nil))

(defn cljs-eval [expr]
  (-> (binding [*out* @writer
                *in* @reader]
        (println expr)
        (read-line))
      edn/read-string
      :val))

(t/use-fixtures :once
  (fn [f]
    (println "Launching test pREPL.")
    (let [server (server/start-server {:accept 'cljs.core.server/io-prepl
                                       :address "127.0.0.1"
                                       :port 0
                                       :name "clojure.math-repl"
                                       :args [:repl-env (cljs.repl.node/repl-env)]})
          port (-> server (.getLocalPort))]
      (println "Server opened on port" port)
      (with-open [socket (java.net.Socket. "127.0.0.1" port)
                  rdr (io/reader socket)
                  wrtr (io/writer socket)]
        (reset! reader rdr)
        (reset! writer wrtr)
        (println "Executing tests")
        (cljs-eval "(require 'clojure.math)")
        (f)
        (println "Tearing down test pREPL.")))))

(deftest sanity-test
  (is (= "6" (cljs-eval "(+ 1 2 3)"))))

(deftest cljs-match-sanity-test
  (is (= "1" (cljs-eval "(clojure.math/cos 0.0)"))))

(defn n==
  [a b]
  (or (and (Double/isNaN a) (Double/isNaN b))
      (and (number? a) (number? b) (== a b))
      (= a b)))

(defn maxi==
  [a b]
  (or (and (Double/isNaN a) (Double/isNaN b))
      (and (= a Number-MAX_SAFE_INTEGER) (= b Long/MAX_VALUE))
      (and (= a Number-MIN_SAFE_INTEGER) (= b Long/MIN_VALUE))
      (and (number? a) (number? b) (== a b))
      (= a b)))

(defmacro test-t->t
  [n jfn cfn gen & [equals]]
  (let [jmfn (symbol "Math" (str jfn))
        cmfn (name cfn)
        eq (or equals n==)]
    `(let [ds# (gen/sample ~gen ~n)]
       (is (every? identity
            (map ~eq
                 (read-string
                  (cljs-eval (str "(->> '" (pr-str ds#)
                                  " (map double)"
                                  " (map clojure.math/" ~cmfn "))")))
                 (map #(~jmfn %) ds#)))
           (str "data: " (pr-str ds#))))))

(defmacro test-double->double
  [n jfn cfn & [equals]]
  `(test-t->t ~n ~jfn ~cfn gen/double ~equals))

(defmacro test-t-t->double
  [n jfn cfn gen1 gen2 & [equals]]
  (let [jmfn (symbol "Math" (str jfn))
        cmfn (name cfn)
        eq (or equals n==)]
    `(let [ds# (gen/sample ~gen1 ~n)
           ds2# (gen/sample ~gen2 ~n)]
       (is (every? identity
            (map ~eq
                 (read-string
                  (cljs-eval (str "(->> (map #(vector %1 %2) '"
                                  (pr-str ds#) " '" (pr-str ds2#) ")"
                                  " (map #(try (apply clojure.math/" ~cmfn " %) (catch :default _ :exception))))")))
                 (map #(~jmfn %1 %2) ds# ds2#)))
           (str "data: " (pr-str (map vector ds# ds2#)))))))

(defmacro test-double-double->double
  [n jfn cfn & [equals]]
  `(test-t-t->double ~n ~jfn ~cfn gen/double gen/double ~equals))

(def safe-integer (gen/sized (fn [_] (gen/choose Number-MIN_SAFE_INTEGER Number-MAX_SAFE_INTEGER))))

(defn e==
  [a b]
  (or (and (number? a) (number? b) (== a b))
      (= a b)))

(defmacro test-zlong-long->long
  [n jfn cfn]
  (let [jmfn (symbol "Math" (str jfn))
        cmfn (name cfn)]
    `(let [lzs# (gen/sample safe-integer ~n)
           ls# (gen/sample (gen/such-that #(not= % 0) safe-integer) ~n)]
       (is (every? identity
            (map e==
                 (read-string
                  (cljs-eval (str "(->> (map #(vector (long %1) (long %2)) '"
                                  (pr-str lzs#) " '" (pr-str ls#) ")"
                                  " (map #(try (apply clojure.math/" ~cmfn " %) (catch :default _ :exception))))")))
                 (map #(~jmfn (long %1) (long %2)) lzs# ls#)))
           (str "data: " (pr-str (map vector lzs# ls#)))))))

;; Tests clojure.core/abs. This function has recently moved to core
(deftest abs-test
  (let [ds (gen/sample gen/double 100)]
    (is (every? identity
                (map #(or (= (double %1) %2) (and (Double/isNaN %1) (Double/isNaN %2)))
                     (read-string (cljs-eval (str "(->> '" (pr-str ds)
                                                  " (map double)"
                                                  " (map abs))")))
                     (map #(Math/abs %) ds)))  ;; This can change to clojure.core/math after Clojure 11
        (str "data: " (pr-str ds)))))

(def ^:const delta 1E-15)

(defn nd==
  [label a b]
  (or (and (Double/isNaN a) (Double/isNaN b))
      (== a b)
      (do
        (println label "variance:" a "\u2260" b)
        (< (Math/abs (- a b)) delta))))

(deftest sin-test
  (test-double->double 100 sin sin #(nd== "sin()" %1 %2)))

(deftest to-radians-test
  (test-double->double 100 toRadians to-radians))

(deftest to-degrees-test
  (test-double->double 100 toDegrees to-degrees))

(deftest ieee-remainder-test
  (test-double-double->double 100 IEEEremainder IEEE-remainder))

(deftest ceil-test
  (test-double->double 100 ceil ceil))

(deftest ceil-null-test
  (is (= ":exception" (cljs-eval (str "(try (clojure.math/ceil nil) (catch :default _ :exception))")))))

(deftest floor-test
  (test-double->double 100 floor floor))

(deftest floor-null-test
  (is (= ":exception" (cljs-eval (str "(try (clojure.math/floor nil) (catch :default _ :exception))")))))

(deftest copy-sign-test
  (test-double-double->double 100 copySign copy-sign))

(deftest rint-test
  (test-double->double 100 rint rint))

(deftest round-test
  (test-t->t 100 round round (gen/double* {:min Number-MIN_SAFE_INTEGER :max Number-MAX_SAFE_INTEGER}) maxi==))

(deftest floor-div-test
  (test-zlong-long->long 100 floorDiv floor-div))

(deftest floor-mod-test
  (test-zlong-long->long 100 floorMod floor-mod))

(deftest get-exponent-test
  (test-double->double 100 getExponent get-exponent))

(deftest ulp-test
  (test-double->double 100 ulp ulp))

(deftest signum-test
  (test-double->double 100 signum signum))

(deftest next-after-test
  (test-double-double->double 100 nextAfter next-after))

(deftest next-up-test
  (test-double->double 100 nextUp next-up))

(deftest next-down-test
  (test-double->double 100 nextDown next-down))

(def ^:const MAX-INT 0x7fffffff)

(deftest scalb-test
  (test-t-t->double 100 scalb scalb
                    gen/double
                    (gen/such-that
                     #(<= % MAX-INT)
                     (gen/resize (inc MAX-INT) gen-small-integer))))

;; utililties for the -exact tests
(def safe-integer (gen/choose Number-MIN_SAFE_INTEGER Number-MAX_SAFE_INTEGER))

(defn no-overflow?
  [f ^long x ^long y]
  (try
    (Number-isSafeInteger (f x y))
    (catch ArithmeticException _ false)))

(defmacro test-safe-safe->safe
  [n jfn cfn op]
  (let [jmfn (symbol "Math" (str jfn))
        cmfn (name cfn)]
    `(let [ls1# (gen/sample safe-integer ~n)
           ls2# (gen/sample safe-integer ~n)]
       (is (every? identity
                   (map e==
                        (read-string
                         (cljs-eval (str "(->> (map #(vector (long %1) (long %2)) '"
                                         (pr-str ls1#) " '" (pr-str ls2#) ")"
                                         " (map (fn [[a b]]"
                                         "        (try (clojure.math/" ~cmfn "  a b)"
                                         "          (catch :default _ :exception)))))")))
                        (map #(if (no-overflow? ~op %1 %2)
                                (~jmfn (long %1) (long %2))
                                :exception) ls1# ls2#)))
           (str "data: " (pr-str (map vector ls1# ls2#)))))))

(deftest add-exact-test
  (test-safe-safe->safe 100 addExact add-exact +))

(deftest subtract-exact
  (test-safe-safe->safe 100 subtractExact subtract-exact -))

(deftest multiply-exact
  (test-safe-safe->safe 100 multiplyExact multiply-exact *))

(defmacro test-safe->safe
  [n jfn cfn op]
  (let [jmfn (symbol "Math" (str jfn))
        cmfn (name cfn)]
    `(let [ls# (gen/sample safe-integer ~n)]
       (is (every? identity
                   (map e==
                        (read-string
                         (cljs-eval (str "(->> '" (pr-str ls#)
                                         " (map #(try (clojure.math/" ~cmfn " %)"
                                         "         (catch :default _ :exception))))")))
                        (map #(if (no-overflow? ~op % 1)
                                (~jmfn (long %))
                                :exception) ls#)))
           (str "data: " (pr-str (map vector ls#)))))))

(deftest increment-exact
  (test-safe->safe 100 incrementExact increment-exact +))

(deftest decrement-exact
  (test-safe->safe 100 decrementExact decrement-exact -))
