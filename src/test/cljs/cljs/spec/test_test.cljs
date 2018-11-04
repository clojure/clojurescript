(ns cljs.spec.test-test
  (:require-macros [cljs.spec.test.test-macros])
  (:require [cljs.test :as test :refer-macros [deftest testing
                                               is are run-tests]]
            [cljs.spec.alpha :as s]
            [cljs.spec.test.alpha :as stest]
            [cljs.spec.test.test-ns1]
            [cljs.spec.test.test-ns2]))

(s/fdef clojure.core/symbol
  :args (s/alt :separate (s/cat :ns string? :n string?)
               :str string?
               :sym symbol?)
  :ret symbol?)

(defn h-cljs-1812 [x] true)
(s/fdef h-cljs-1812 :args (s/cat :x int?) :ret true?)

(deftest test-cljs-1812
  (is (= (stest/unstrument `h-cljs-1812)
        []))

  (stest/check `h-cljs-1812 {:clojure.test.check/opts {:num-tests 1}})

  ; Calling h-cljs-1812 with an argument of the wrong type shouldn't throw,
  ; because the function should not have been instrumented by stest/check.
  (h-cljs-1812 "foo"))

;; Setup for CLJS-2142
(def ^:const pi 3.14159)
(defn area [r] (* pi r r))
(s/fdef area :args (s/cat :r number?))

(deftest test-cljs-2142
  (is (= `[area] (stest/instrument `[pi area]))))

(defn f-2391 [] 1)
(s/fdef f-2391 :args (s/cat) :ret #{2})

(deftest test-cljs-2391-a
  (is (= 1 (f-2391))))

(deftest test-cljs-2391-b
  (stest/instrument `f-2391 {:stub #{`f-2391}})
  (is (= 2 (f-2391))))

(deftest test-cljs-2391-c
  (stest/unstrument `f-2391)
  (is (= 1 (f-2391))))

(deftest test-cljs-2414
  (is (empty? (stest/instrument 'cljs.spec.test.test-macros$macros/add))))

(deftest test-cljs-2197
  (stest/instrument `symbol)
  (is (thrown? js/Error (symbol 3)))
  (is (thrown? js/Error (#'symbol 3)))
  (is (thrown? js/Error (apply symbol [3])))
  (stest/unstrument `symbol))

(defn arities
  ([a]
   (inc a))
  ([a b]
   (+ a b))
  ([a b c] 0))

(s/fdef arities
  :args (s/or :arity-1 (s/cat :a number?)
              :arity-2 (s/cat :a number? :b number?)
              :arity-3 (s/cat :a string? :b boolean? :c map?))
  :ret number?)

(deftest test-2397
  (stest/instrument `arities)
  (is (arities 1))
  (is (thrown? js/Error (arities "bad")))
  (stest/unstrument `arities))

(defn foo [& args] args)
(s/fdef foo :args (s/cat :args (s/* int?)))

(deftest test-2641
  (stest/instrument `foo)
  (is (= [1 2 3] (foo 1 2 3)))
  (is (thrown? js/Error (foo 1 :hello)))
  (stest/unstrument `foo))

(deftest test-2755
  (is (uri? (ffirst (s/exercise uri? 1)))))

(deftest test-cljs-2665
  (is (= '#{cljs.spec.test.test-ns1/x cljs.spec.test.test-ns1/y cljs.spec.test.test-ns2/z}
        (stest/enumerate-namespace '[cljs.spec.test.test-ns1 cljs.spec.test.test-ns2])))
  (is (= '#{cljs.spec.test.test-ns1/x cljs.spec.test.test-ns1/y cljs.spec.test.test-ns2/z}
        (stest/enumerate-namespace ['cljs.spec.test.test-ns1 'cljs.spec.test.test-ns2])))
  (is (= '#{cljs.spec.test.test-ns1/x cljs.spec.test.test-ns1/y}
        (stest/enumerate-namespace 'cljs.spec.test.test-ns1)))
  (is (= '#{cljs.spec.test.test-ns2/z}
        (stest/enumerate-namespace 'cljs.spec.test.test-ns2))))

(defn fn-2953 [x] ::ret-val)

(s/fdef fn-2953 :args (s/cat :x int?))

(deftest test-cljs-2953
  (stest/instrument `fn-2953)
  (is @#'stest/*instrument-enabled*)
  (is (= ::ret-val (stest/with-instrument-disabled
                     (is (nil? @#'stest/*instrument-enabled*))
                     (fn-2953 "abc"))))
  (is @#'stest/*instrument-enabled*))

(s/fdef cljs.core/= :args (s/+ any?))

(deftest test-cljs-2956
  (stest/instrument 'cljs.core/=)
  (is (true? (= 1)))
  (is (thrown? js/Error (=)))
  (stest/unstrument 'cljs.core/=))

(defn fn-2975 [x])

(deftest test-2975
  (testing "instrument and unstrument return empty coll when no fdef exists"
    (is (empty? (stest/instrument `fn-2975)))
    (is (empty? (stest/unstrument `fn-2975)))))

(defn fn-2995
  ([] (fn-2995 0))
  ([a] (fn-2995 a 1))
  ([a b] [a b]))

(s/fdef fn-2995
  :args (s/cat :a (s/? number?)
               :b (s/? number?)))

(deftest test-2995
  (stest/instrument `fn-2995)
  (testing "instrumented self-calling multi-arity function works"
    (is (= [0 1] (fn-2995 0 1)))
    (is (= [0 1] (fn-2995 0)))
    (is (= [0 1] (fn-2995 0)))
    (is (thrown? js/Error (fn-2995 "not a number")))))
