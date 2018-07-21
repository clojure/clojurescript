;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.spec-test
  (:require [cljs.spec.alpha :as s]
            [cljs.spec.test.alpha :as st]
            [cljs.test :as test :refer-macros [deftest is are run-tests]]
            [cljs.spec.gen.alpha :as gen]
            [clojure.test.check.generators]))

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

;;(st/instrument `adder)

(deftest test-multi-arity-instrument
  (is (= 1 (adder 1)))
  (is (= 3 (adder 1 2)))
  ;;(is (thrown? js/Error (adder "foo")))
  )

(defmulti testmm :type)
(defmethod testmm :default [_])
(defmethod testmm :good [_] "good")

(s/fdef testmm :args (s/cat :m map?) :ret string?)

;;(st/instrument `testmm)

(deftest test-multifn-instrument
  (is (= "good" (testmm {:type :good})))
  ;;(is (thrown? js/Error (testmm "foo")))
  )

(deftest int-in-test
  (is (s/valid? (s/int-in 1 3) 2))
  (is (not (s/valid? (s/int-in 1 3) 0))))

(deftest inst-in-test
  (is (s/valid? (s/inst-in #inst "1999" #inst "2001") #inst "2000"))
  (is (not (s/valid? (s/inst-in #inst "1999" #inst "2001") #inst "1492"))))

(deftest test-conform-unform
  (let [xs [42 11 13 15 {:a 1 :b 2 :c 3} 1 2 3 42 43 44 11]]
    (is (= xs (s/unform s2 (s/conform s2 xs))))))

(deftest test-assert
  (s/def ::even-number (s/and number? even?))
  ;; assertions off by default
  (is (= 42 (s/assert ::even-number 42)))
  (s/check-asserts true)
  (is (= 42 (s/assert ::even-number 42)))
  (is (thrown? js/Error (s/assert ::even-number 5))))

(deftest test-cljs-1754
  (is (boolean? (gen/generate (s/gen boolean?)))))

(s/fdef cljs-1757-x :args (s/cat ::first number?) :ret #(= % 2))
(defn cljs-1757-x [b] 2)

(deftest test-cljs-1757
  (is (s/exercise-fn `cljs-1757-x)))

(deftest test-cljs-1788
  (defmulti mm :mm/type)
  (s/def ::foo-1788 (s/multi-spec mm :mm/type))
  (is (= (s/form ::foo-1788)
         '(cljs.spec.alpha/multi-spec cljs.spec-test/mm :mm/type))))

(def h-cljs-1790 (derive (make-hierarchy) :a :b))
(defmulti spec-type-1790 identity :hierarchy #'h-cljs-1790)
(defmethod spec-type-1790 :b [_]
           (s/spec (constantly true)))

(deftest test-cljs-1790
  (s/def ::multi (s/multi-spec spec-type-1790 identity))
  (is (= :b (s/conform ::multi :b)))
  (is (= :a (s/conform ::multi :a))))

(deftest test-cljs-1944
  (is (not-empty (s/exercise (s/coll-of string? :kind set?)))))

;; Copied from Clojure spec tests

(def even-count? #(even? (count %)))

(defn submap?
  "Is m1 a subset of m2?"
  [m1 m2]
  (if (and (map? m1) (map? m2))
    (every? (fn [[k v]] (and (contains? m2 k)
                          (submap? v (get m2 k))))
      m1)
    (= m1 m2)))

(deftest conform-explain
  (let [a (s/and #(> % 5) #(< % 10))
        o (s/or :s string? :k keyword?)
        c (s/cat :a string? :b keyword?)
        either (s/alt :a string? :b keyword?)
        star (s/* keyword?)
        plus (s/+ keyword?)
        opt (s/? keyword?)
        andre (s/& (s/* keyword?) even-count?)
        m (s/map-of keyword? string?)
        mkeys (s/map-of (s/and keyword? (s/conformer name)) any?)
        mkeys2 (s/map-of (s/and keyword? (s/conformer name)) any? :conform-keys true)
        s (s/coll-of (s/spec (s/cat :tag keyword? :val any?)) :kind list?)
        v (s/coll-of keyword? :kind vector?)
        coll (s/coll-of keyword?)
        lrange (s/int-in 7 42)
        drange (s/double-in :infinite? false :NaN? false :min 3.1 :max 3.2)
        irange (s/inst-in #inst "1939" #inst "1946")]
    (are [spec x conformed ed]
      (let [co (s/conform spec x)
            e  (::s/problems (s/explain-data spec x))]
        (when (not= conformed co) (println "conform fail\n\texpect=" conformed "\n\tactual=" co))
        (when (not (every? true? (map submap? ed e)))
          (println "explain failures\n\texpect=" ed "\n\tactual failures=" e "\n\tsubmap?=" (map submap? ed e)))
        (and (= conformed co) (every? true? (map submap? ed e))))

      lrange 7 7 nil
      lrange 8 8 nil
      lrange 42 ::s/invalid [{:pred '(cljs.core/fn [%] (cljs.spec.alpha/int-in-range? 7 42 %)), :val 42}]

      irange #inst "1938" ::s/invalid [{:pred '(cljs.core/fn [%] (cljs.spec.alpha/inst-in-range? #inst "1939-01-01T00:00:00.000-00:00" #inst "1946-01-01T00:00:00.000-00:00" %)), :val #inst "1938"}]
      irange #inst "1942" #inst "1942" nil
      irange #inst "1946" ::s/invalid [{:pred '(cljs.core/fn [%] (cljs.spec.alpha/inst-in-range? #inst "1939-01-01T00:00:00.000-00:00" #inst "1946-01-01T00:00:00.000-00:00" %)), :val #inst "1946"}]

      drange 3.0 ::s/invalid [{:pred '(cljs.core/fn [%] (cljs.core/<= 3.1 %)), :val 3.0}]
      drange 3.1 3.1 nil
      drange 3.2 3.2 nil
      ;drange Double/POSITIVE_INFINITY ::s/invalid [{:pred '(not (isInfinite %)), :val Double/POSITIVE_INFINITY}]
      ;; can't use equality-based test for Double/NaN
      ;; drange Double/NaN ::s/invalid {[] {:pred '(not (isNaN %)), :val Double/NaN}}

      keyword? :k :k nil
      keyword? nil ::s/invalid [{:pred ::s/unknown :val nil}]
      keyword? "abc" ::s/invalid [{:pred ::s/unknown :val "abc"}]

      a 6 6 nil
      a 3 ::s/invalid '[{:pred (cljs.core/fn [%] (cljs.core/> % 5)), :val 3}]
      a 20 ::s/invalid '[{:pred (cljs.core/fn [%] (cljs.core/< % 10)), :val 20}]
      ;a nil "java.lang.NullPointerException" "java.lang.NullPointerException"
      ;a :k "java.lang.ClassCastException" "java.lang.ClassCastException"

      o "a" [:s "a"] nil
      o :a [:k :a] nil
      o 'a ::s/invalid '[{:pred cljs.core/string?, :val a, :path [:s]} {:pred cljs.core/keyword?, :val a :path [:k]}]

      c nil ::s/invalid '[{:reason "Insufficient input", :pred cljs.core/string?, :val (), :path [:a]}]
      c [] ::s/invalid '[{:reason "Insufficient input", :pred cljs.core/string?, :val (), :path [:a]}]
      c [:a] ::s/invalid '[{:pred cljs.core/string?, :val :a, :path [:a], :in [0]}]
      c ["a"] ::s/invalid '[{:reason "Insufficient input", :pred cljs.core/keyword?, :val (), :path [:b]}]
      c ["s" :k] '{:a "s" :b :k} nil
      c ["s" :k 5] ::s/invalid '[{:reason "Extra input", :pred (cljs.spec.alpha/cat :a cljs.core/string? :b cljs.core/keyword?), :val (5)}]

      (s/cat) nil {} nil
      (s/cat) [5] ::s/invalid '[{:reason "Extra input", :pred (cljs.spec.alpha/cat), :val (5), :in [0]}]

      either nil ::s/invalid '[{:reason "Insufficient input", :pred (cljs.spec.alpha/alt :a cljs.core/string? :b cljs.core/keyword?), :val () :via []}]
      either [] ::s/invalid '[{:reason "Insufficient input", :pred (cljs.spec.alpha/alt :a cljs.core/string? :b cljs.core/keyword?), :val () :via []}]
      either [:k] [:b :k] nil
      either ["s"] [:a "s"] nil
      either [:b "s"] ::s/invalid '[{:reason "Extra input", :pred (cljs.spec.alpha/alt :a cljs.core/string? :b cljs.core/keyword?), :val ("s") :via []}]

      star nil [] nil
      star [] [] nil
      star [:k] [:k] nil
      star [:k1 :k2] [:k1 :k2] nil
      star [:k1 :k2 "x"] ::s/invalid '[{:pred cljs.core/keyword?, :val "x" :via []}]
      star ["a"] ::s/invalid '[{:pred cljs.core/keyword?, :val "a" :via []}]

      plus nil ::s/invalid '[{:reason "Insufficient input", :pred cljs.core/keyword?, :val () :via []}]
      plus [] ::s/invalid '[{:reason "Insufficient input", :pred cljs.core/keyword?, :val () :via []}]
      plus [:k] [:k] nil
      plus [:k1 :k2] [:k1 :k2] nil
      plus [:k1 :k2 "x"] ::s/invalid '[{:pred cljs.core/keyword?, :val "x", :in [2]}]
      plus ["a"] ::s/invalid '[{:pred cljs.core/keyword?, :val "a" :via []}]

      opt nil nil nil
      opt [] nil nil
      opt :k ::s/invalid '[{:pred (cljs.core/fn [%] (cljs.core/or (cljs.core/nil? %) (cljs.core/sequential? %))), :val :k}]
      opt [:k] :k nil
      opt [:k1 :k2] ::s/invalid '[{:reason "Extra input", :pred (cljs.spec.alpha/? cljs.core/keyword?), :val (:k2)}]
      opt [:k1 :k2 "x"] ::s/invalid '[{:reason "Extra input", :pred (cljs.spec.alpha/? cljs.core/keyword?), :val (:k2 "x")}]
      opt ["a"] ::s/invalid '[{:pred cljs.core/keyword?, :val "a"}]

      andre nil nil nil
      andre [] nil nil
      andre :k ::s/invalid '[{:pred (cljs.core/fn [%] (cljs.core/or (cljs.core/nil? %) (cljs.core/sequential? %))), :val :k}]
      andre [:k] ::s/invalid '[{:pred cljs.spec-test/even-count?, :val [:k]}]
      andre [:j :k] [:j :k] nil

      m nil ::s/invalid '[{:pred cljs.core/map?, :val nil}]
      m {} {} nil
      m {:a "b"} {:a "b"} nil

      mkeys nil ::s/invalid '[{:pred cljs.core/map?, :val nil}]
      mkeys {} {} nil
      mkeys {:a 1 :b 2} {:a 1 :b 2} nil

      mkeys2 nil ::s/invalid '[{:pred cljs.core/map?, :val nil}]
      mkeys2 {} {} nil
      mkeys2 {:a 1 :b 2} {"a" 1 "b" 2} nil

      s '([:a 1] [:b "2"]) '({:tag :a :val 1} {:tag :b :val "2"}) nil

      v [:a :b] [:a :b] nil
      v '(:a :b) ::s/invalid '[{:pred cljs.core/vector? :val (:a :b)}]

      coll nil ::s/invalid '[{:path [], :pred cljs.core/coll?, :val nil, :via [], :in []}]
      coll [] [] nil
      coll [:a] [:a] nil
      coll [:a :b] [:a :b] nil
      coll (map identity [:a :b]) '(:a :b) nil
      ;;coll [:a "b"] ::s/invalid '[{:pred (coll-checker keyword?), :val [:a b]}]
      )))

(deftest coll-form
  (are [spec form]
    (= (s/form spec) form)
    (s/map-of int? any?)
    '(cljs.spec.alpha/map-of cljs.core/int? cljs.core/any?)

    (s/coll-of int?)
    '(cljs.spec.alpha/coll-of cljs.core/int?)

    (s/every-kv int? int?)
    '(cljs.spec.alpha/every-kv cljs.core/int? cljs.core/int?)

    (s/every int?)
    '(cljs.spec.alpha/every cljs.core/int?)

    (s/coll-of (s/tuple (s/tuple int?)))
    '(cljs.spec.alpha/coll-of (cljs.spec.alpha/tuple (cljs.spec.alpha/tuple cljs.core/int?)))

    (s/coll-of int? :kind vector?)
    '(cljs.spec.alpha/coll-of cljs.core/int? :kind cljs.core/vector?)

    (s/coll-of int? :gen #(gen/return [1 2]))
    '(cljs.spec.alpha/coll-of cljs.core/int? :gen (fn* [] (gen/return [1 2])))))

(defn check-conform-unform [spec vals expected-conforms]
  (let [actual-conforms (map #(s/conform spec %) vals)
        unforms (map #(s/unform spec %) actual-conforms)]
    (is (= actual-conforms expected-conforms))
    (is (= vals unforms))))

(deftest coll-conform-unform
  (check-conform-unform
    (s/coll-of (s/or :i int? :s string?))
    [[1 "x"]]
    [[[:i 1] [:s "x"]]])
  (check-conform-unform
    (s/every (s/or :i int? :s string?))
    [[1 "x"]]
    [[1 "x"]])
  (check-conform-unform
    (s/map-of int? (s/or :i int? :s string?))
    [{10 10 20 "x"}]
    [{10 [:i 10] 20 [:s "x"]}])
  (check-conform-unform
    (s/map-of (s/or :i int? :s string?) int? :conform-keys true)
    [{10 10 "x" 20}]
    [{[:i 10] 10 [:s "x"] 20}])
  (check-conform-unform
    (s/every-kv int? (s/or :i int? :s string?))
    [{10 10 20 "x"}]
    [{10 10 20 "x"}]))

(s/fdef foo.bar/cljs-2275
  :args (s/cat :k keyword?)
  :ret  string?)

(comment

  (run-tests)

  )
