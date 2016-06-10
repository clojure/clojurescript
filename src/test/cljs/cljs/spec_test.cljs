(ns cljs.spec-test
  (:require [cljs.spec :as s]
            [cljs.test :as test :refer-macros [deftest is run-tests]]))

(s/def ::even? (s/and number? even?))
(s/def ::odd? (s/and number? odd?))

(def s2
  (s/cat :forty-two #{42}
    :odds (s/+ ::odd?)
    :m (s/keys :req-un [::a ::b ::c])
    :oes (s/& (s/* (s/cat :o ::odd? :e ::even?)) #(< (count %) 3))
    :ex (s/* (s/alt :odd ::odd? :even ::even?))))

(deftest test-roundtrip
  (let [xs [42 11 13 15 {:a 1 :b 2 :c 3} 1 2 3 42 43 44 11]]
    (is (= xs (s/unform s2 (s/conform s2 xs))))))

(defn adder
  ([a] a)
  ([a b] (+ a b)))

(s/fdef adder
  :args (s/cat :a integer? :b (s/? integer?))
  :ret integer?)

(s/instrument #'adder)

(deftest test-multi-arity-instrument
  (is (= 1 (adder 1)))
  (is (= 3 (adder 1 2)))
  (is (thrown? js/Error (adder "foo"))))

(defmulti testmm :type)
(defmethod testmm :default [_])
(defmethod testmm :good [_] "good")

(s/fdef testmm :args (s/cat :m map?) :ret string?)

(s/instrument #'testmm)

(deftest test-multifn-instrument
  (is (= "good" (testmm {:type :good})))
  (is (thrown? js/Error (testmm "foo"))))

(comment

  (s/conform s2 [42 11 13 15 {:a 1 :b 2 :c 3} 1 2 3 42 43 44 11])
  (s/unform s2
    (s/conform s2 [42 11 13 15 {:a 1 :b 2 :c 3} 1 2 3 42 43 44 11]))

  )