;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.destructuring-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is are]]
            [clojure.string :as s]
            [clojure.set :as set]))

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

(deftest keywords-in-destructuring
  (let [m {:a 1 :b 2}]
    (let [{:keys [:a :b]} m]
      (is (= [1 2] [a b])))
    (let [{:keys [:a :b :c] :or {c 3}} m]
      (is (= [1 2 3] [a b c])))))

(deftest namespaced-keywords-in-destructuring
  (let [m {:a/b 1 :c/d 2}]
    (let [{:keys [:a/b :c/d]} m]
      (is (= [1 2] [b d])))
    (let [{:keys [:a/b :c/d :e/f] :or {f 3}} m]
      (is (= [1 2 3] [b d f])))))

(deftest namespaced-keys-in-destructuring
  (let [m {:a/b 1 :c/d 2}]
    (let [{:keys [a/b c/d]} m]
      (is (= [1 2] [b d])))
    (let [{:keys [a/b c/d e/f] :or {f 3}} m]
      (is (= [1 2 3] [b d f])))))

(deftest namespaced-syms-in-destructuring
  (let [{:syms [a/b c/d e/f] :or {f 3}} {'a/b 1 'c/d 2}]
    (is (= [1 2 3] [b d f]))))

(deftest namespaced-keys-syntax
  (let [{:a/keys [b c d] :or {d 3}} {:a/b 1 :a/c 2}]
    (is (= [1 2 3] [b c d]))))

(deftest namespaced-syms-syntax
  (let [{:a/syms [b c d] :or {d 3}} {'a/b 1 'a/c 2}]
    (is (= [1 2 3] [b c d]))))

(deftest resolve-keyword-ns-alias-in-destructuring
  (let [{:keys [::s/x ::s/y ::s/z] :or {z 3}} {:clojure.string/x 1 :clojure.string/y 2}]
    (is (= [1 2 3] [x y z]))))

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

(defn destructure-1216
  ([kvs] kvs)
  ([k v & args] [k v args]))

(defn foo-1216
  ([a] (foo-1216 a 10))
  ([a b & [c]] [a b c]))

(deftest test-cljs-1216
  (testing "varargs regression"
    (is (= (foo-1216 1) [1 10 nil]))
    (is (= (foo-1216 1 2) [1 2 nil]))
    (is (= (foo-1216 1 2 3) [1 2 3]))
    (is (= [1 2 [3 4]]
          (destructure-1216 1 2 3 4)))
    (is (= [1 2 [3 4]]
          (apply destructure-1216 [1 2 3 4])))
    (is (= (destructure-1216 1 2 3 4)[1 2 [3 4]]
          (apply destructure-1216 [1 2 3 4])))))

(defprotocol CLJS-1600-IFoo
  (foo-fn [_ {:keys [a b] :as x}]))

(defrecord CLJS-1600-Foo []
  CLJS-1600-IFoo
  (foo-fn [_ {:keys [a b] :as args}]
    args))

(deftest test-cljs-1600
  (let [foo (reify
              CLJS-1600-IFoo
              (foo-fn [_ {:keys [a b] :as args}]
                args))]
    (is (= (foo-fn (->CLJS-1600-Foo) {:a 1 :b 2})
          {:a 1 :b 2}))
    (is (= (foo-fn foo {:a 1 :b 2})
          {:a 1 :b 2})))
  ;; test that the destructuring works
  (let [foo (reify
              CLJS-1600-IFoo
              (foo-fn [_ {:keys [a b] :as args}]
                {:a a :b b}))]
    (is (= (foo-fn foo {:a 1 :b 2})
          {:a 1 :b 2})))
  (let [foo (reify
              CLJS-1600-IFoo
              (foo-fn [_ {:keys [a b c] :or {c 3}}]
                {:c c}))]
    (is (= (foo-fn foo {:a 1 :b 2})
          {:c 3}))))

(deftest test-cljs-3076
  (let [f (fn [& [a _]]
            a)]
    (is (nil? (f nil)))
    (is (= 1 (f 1)))
    (is (= 1 (f 1 2))))
  (let []))

(deftest test-pam-dupes?
  (is (false? (#'pam-dupes? #js [:a 1 :b 2 :c 3])))
  (is (true? (#'pam-dupes? #js [:a 1 :b 2 :a 3]))))

(deftest test-pam-new-size
  (is (== 6 (#'pam-new-size #js [:a 1 :b 2 :c 3])))
  (is (== 4 (#'pam-new-size #js [:a 1 :b 2 :a 3]))))

(deftest singleton-map-in-destructure-context
  (let [sample-map {:a 1 :b 2}
        {:keys [a] :as m1} (list sample-map)]
    (is (= m1 sample-map))
    (is (= a 1))))

(deftest trailing-map-destructuring
  (let [add  (fn [& {:keys [a b]}] (+ a b))
        addn (fn [n & {:keys [a b]}] (+ n a b))]
    (testing "that kwargs are applied properly given a map in place of the key/val pairs"
      (is (= 3 (add  :a 1 :b 2)))
      (is (= 3 (add  {:a 1 :b 2})))
      (is (= 13 (addn 10 :a 1 :b 2)))
      (is (= 13 (addn 10 {:a 1 :b 2})))
      (is (= 103 ((partial addn 100) :a 1 {:b 2})))
      (is (= 103 ((partial addn 100 :a 1) {:b 2})))
      (is (= 107 ((partial addn 100 :a 1) {:a 5 :b 2}))))
    (testing "built maps"
      (let [{:as m1} (list :a 1 :b 2)
            {:as m2} (list :a 1 :b 2 {:c 3})
            {:as m3} (list :a 1 :b 2 {:a 0})
            {:keys [a4] :as m4} (list nil)]
        (= m1 {:a 1 :b 2})
        (= m2 {:a 1 :b 2 :c 3})
        (= m3 {:a 0 :b 2})
        (= m1 (seq-to-map-for-destructuring (list :a 1 :b 2)))
        (= m2 (seq-to-map-for-destructuring (list :a 1 :b 2 {:c 3})))
        (= m3 (seq-to-map-for-destructuring (list :a 1 :b 2 {:a 0})))
        (= a4 nil)))))

(deftest keys-bang
  (let [sample-map {:a 1 :b 2}]
    (testing ":keys! happy path, binds and throws when key missing"
      (is (= 1 (let [{:keys! [a b]} sample-map] a)))
      (is (thrown? js/Error (let [{:keys! [a b]} {:a 1}] a))))
    (testing ":keys! with & bind and don't bind"
      (is (= 1 (let [{:keys! [a & :b]} sample-map] a)))
      (is (thrown? js/Error (let [{:keys! [a & :b]} {:a 1}] a))))
    (testing "nested maps with :keys! &"
      (let [sample-map {:a 1 :b {:a 2 :b 3 :c 4 :d 42}}
            {a :a {aa :a :as m :keys! [b c & :d]} :b} sample-map]
        (is (= m (:b sample-map)))
        (is (= a 1))
        (is (= aa 2))
        (is (= b 3))
        (is (= c 4))
        (is (thrown? js/Error (let [{a :a {aa :a :as m :keys! [b c & :d :e]} :b} sample-map] a)))
        (is (thrown? js/Error (let [{a :a {aa :a :as m :keys! [b c & :d]} :b} (update sample-map :b dissoc :c)] a)))))
    (testing "a broad range of qualified names/declarators with :keys! &"
      (let [sample-map {:foo/a 1 :b 2 :foo/c 3}
            {:keys! [foo/a & :b]} sample-map
            {:keys! [b & :foo/c]} sample-map
            sample-map2 {:foo/aa 1 :bb 2 :foo/cc 3}
            {:foo/keys! [aa & :foo/cc]} sample-map2]
        (is (= a 1))
        (is (= b 2))
        (is (thrown? js/Error (let [{:keys! [b & :foo/c]} (dissoc sample-map :b)] b)))
        (is (thrown? js/Error (let [{:keys! [b & :foo/c]} (dissoc sample-map :foo/c)] b)))
        (is (= 1 (let [{:foo/keys! [aa & :bb]} sample-map2] aa)))
        (is (= aa 1))
        (is (= 1 (let [{:keys! [::a & ::b]} {::a 1 , ::b 2}] a)))))))

(deftest syms-bang
  (let [sample-map '{a 1 b 2}]
    (testing ":syms! happy path, binds and throws when key missing"
      (is (= 1 (let [{:syms! [a b]} sample-map] a)))
      (is (thrown? js/Error (let [{:syms! [a b]} {:a 1}] a))))
    (testing ":syms! with & bind and don't bind"
      (is (= 1 (let [{:syms! [a & 'b]} sample-map] a)))
      (is (thrown? js/Error (let [{:syms! [a & 'b]} {:a 1}] a))))
    (testing "nested maps with :syms! &"
      (let [sample-map '{a 1 b {a 2 b 3 c 4 d 42}}
            {a 'a {aa 'a :as m :syms! [b c & 'd]} 'b} sample-map]
        (is (= m ('b sample-map)))
        (is (= a 1))
        (is (= aa 2))
        (is (= b 3))
        (is (= c 4))
        (is (thrown? js/Error (let [{a 'a {aa :a :as m :syms! [b c & 'd 'e]} 'b} sample-map] a)))
        (is (thrown? js/Error (let [{a 'a {aa :a :as m :syms! [b c & 'd]} 'b} (update sample-map 'b dissoc 'c)] a)))))
    (testing "a broad range of qualified names/declarators with :syms! &"
      (let [sample-map '{foo/a 1 b 2 foo/c 3}
            {:syms! [foo/a & 'b]} sample-map
            {:syms! [b & 'foo/c]} sample-map
            sample-map2 '{foo/aa 1 bb 2 foo/cc 3}
            {:foo/syms! [aa & 'foo/cc]} sample-map2]
        (is (= a 1))
        (is (= b 2))
        (is (thrown? js/Error (let [{:syms! [b & 'foo/c]} (dissoc sample-map 'b)] b)))
        (is (thrown? js/Error (let [{:syms! [b & 'foo/c]} (dissoc sample-map 'foo/c)] b)))
        (is (= aa 1))
        (is (= 1 (let [{:foo/syms! [aa & 'bb]} sample-map2] aa)))))))

(deftest strs-bang
  (let [sample-map {"a" 1 "b" 2}]
    (testing ":strs! happy path, binds and throws when key missing"
      (is (= 1 (let [{:strs! [a b]} sample-map] a)))
      (is (thrown? js/Error (let [{:strs! [a b]} {:a 1}] a))))
    (testing ":strs! with & bind and don't bind"
      (is (= 1 (let [{:strs! [a & "b"]} sample-map] a)))
      (is (thrown? js/Error (let [{:strs! [a & "b"]} {:a 1}] a))))
    (testing "nested maps with :strs! &"
      (let [sample-map {"a" 1 "b" {"a" 2 "b" 3 "c" 4 "d" 42}}
            {a "a" {aa "a" :as m :strs! [b c & "d"]} "b"} sample-map]
        (is (= m (get sample-map "b")))
        (is (= a 1))
        (is (= aa 2))
        (is (= b 3))
        (is (= c 4))
        (is (thrown? js/Error (let [{a "a" {aa "a" :as m :strs! [b c & "d" "e"]} "b"} sample-map] a)))
        (is (thrown? js/Error (let [{a "a" {aa "a" :as m :strs! [b c & "d"]} "b"} (update sample-map "b" dissoc "c")] a)))))))

(deftest select-directive
  (let [m {:a 1 :b 2 :c 3 :d 4
           'sa 10 'sb 20 'sc 30 'sd 40
           "stra" 100 "strb" 200 "strc" 300 "strd" 400
           :foo/x 1000 :foo/y 2000 :foo/z 3000
           ::x 10000 ::y 20000 ::z 30000
           :nested {:aa 1 'saa 10 "straa" 100}}

        {:keys [a b & :c :z]
         :keys! [d]
         :select keys-sel} m

        {:syms [sa sb & 'sc 'sz]
         :syms! [sd]
         :select syms-sel} m

        {:strs [stra strb & "strc" "strz"]
         :strs! [strd]
         :select strs-sel} m

        {:foo/keys [x & :y :zz]
         :foo/keys! [z]
         :select qkeys-sel} m

        {::keys [x & :y :zz]
         ::keys! [z]
         :select aqkeys-sel} m

        {{aa :aa saa 'saa
          :select nest-sel} :nested
         aqx ::x
         :select tl-sel} m

        {:keys! [a b & :c]
         :keys [d & :z]
         :or {}
         :select or-sel} m

        {:keys [a b c d]
         :syms [sa sb sc sd]
         :strs [stra strb strc strd]
         :foo/keys! [x y z]
         ::keys [x y z]
         nest :nested
         :as mm
         :select sel-mm} m]
    (are [expected result] (= expected result)
      keys-sel {:a 1 :b 2 :c 3 :d 4}
      syms-sel '{sa 10 sb 20 sc 30 sd 40}
      strs-sel {"stra" 100 "strb" 200 "strc" 300 "strd" 400}
      qkeys-sel {:foo/x 1000 :foo/z 3000}
      aqkeys-sel {::x 10000 ::z 30000}
      nest-sel '{:aa 1, saa 10}
      tl-sel '{:nested {:aa 1, saa 10} ::x 10000}
      or-sel {:a 1 :b 2 :c 3 :d 4}
      sel-mm mm))
  (testing "base cases"
    (is (nil? (let [{{a :a} :n :select s} nil] s))
        "if you haven't supplied a map, select won't make one for no reason")

    (testing "you get what you supplied if nothing else"
      (is (= {} (let [{{a :a} :n :select s} {}] s)))
      (is (= {:n nil} (let [{{a :a} :n :select s} {:n nil}] s)))
      (is (= {:n {}} (let [{{a :a} :n :select s} {:n {}}] s))))

    (testing "defaults can turn nothing into something"
      (is (= {:n {:a 42}} (let [{{a :a :or {a 42}} :n :select s} nil] s)))
      (is (= {:n {:a 42}} (let [{{a :a :or {a 42}} :n :select s} {:n nil}] s))))))

(deftest select-or-defaults
  (let [sample-map {:a 1, :b 2, :c  {:aa 10 :bb 20},
                    'd 4  'e 5  'f  {'dd 40 'ee 50},
                    "g" 6 "h" 7 "i" {"gg" 60 "hh" 70},}]
    (testing "happy path"
      (testing ":defaults"
        (is (empty? (let [{:defaults d :or {}} {}] d)))
        
        (is (= {:a 1} (let [{:keys [a] :defaults d :or {:a 1}} {}] d)))
        (is (= {:a 1} (let [{:keys [a] :defaults d :or {a 1}} {}] d)))
        (is (= {:a 1, 'b 2, "c" 3} (let [{b 'b, c "c", :keys [a] :defaults d :or {:a 1, 'b 2, "c" 3}} {}] d))))
      
      (testing ":keys + :select + :or + defaults"
        (let [{:keys [a b z & :c :d] {:keys! [aa & :bb]} :c
               :or {:d 42, z :or-z}
               :select m
               :defaults dfs} sample-map]
          (is (= 1 a))
          (is (= 2 b))
          (is (= 10 aa))
          (is (= {:z :or-z, :c {:aa 10, :bb 20}, :b 2, :d 42, :a 1} m))
          (is (= {:d 42, :z :or-z} dfs))))

      (testing ":syms + :select + :or + defaults"
        (let [{:syms [d e z & 'd 'f] {:syms! [dd & 'ee]} 'f
               :or {'d 42, z :or-z}
               :select m
               :defaults dfs} sample-map]
          (is (= 4 d))
          (is (= 5 e))
          (is (= 40 dd))
          (is (= '{f {dd 40, ee 50}, e 5, d 4, z :or-z} m))
          (is (= '{d 42, z :or-z} dfs))))

      (testing ":strs + :select + :or + defaults"
        (let [{:strs [g h z & "d" "i"] {:strs! [gg & "hh"]} "i"
               :or {"d" 42, z :or-z}
               :select m
               :defaults dfs} sample-map]
          (is (= 6 g))
          (is (= 7 h))
          (is (= 60 gg))
          (is (= {"d" 42, "z" :or-z, "i" {"gg" 60, "hh" 70}, "g" 6, "h" 7} m))
          (is (= {"d" 42, "z" :or-z} dfs))))

      (testing "mixed things after &"
        (is (= 1 (let [{:keys [a & 'b]} {:a 1}] a)))
        (is (= 1 (let [{:keys! [a & 'b "c"]} {:a 1, 'b 2, "c" 3}] a)))))))

(deftest excess
  (let [sample-map {:a 1, :b 2, :c  {:aa 10},
                    'd 4  'e 5  'f  {'dd 40 'ee 50},
                    "g" 6 "h" 7 "i" {"gg" 60 "hh" 70}}]
    (testing "happy path"
      (let [{:keys [a] :excess exa} (select-keys sample-map [:a :b :c])
            {:keys [a b c] :excess exnil} (select-keys sample-map [:a :b :c])
            {{:excess exnest} :c} sample-map
            {:keys [a c] :excess exkws} (select-keys sample-map [:a :b :c])
            {:syms [d f] :excess exsyms} (select-keys sample-map '[d e f])
            {:strs [g i] :excess exstrs} (select-keys sample-map ["g" "h" "i"])]
        (is (= {:b 2 :c {:aa 10}} exa))
        (is (= {:aa 10} exnest))
        (is (= {:b 2} exkws))
        (is (= '{e 5} exsyms))
        (is (= {"h" 7} exstrs))

        (testing ":excess predicative use"
          (is (nil? exnil))
          (is (nil? (let [{:excess ex} {}] ex)))
          (is (nil? (let [{:keys [a] :excess ex} nil] ex)))

          (let [{:keys [a] :excess ex-some} (select-keys sample-map [:a :b :c])
                {:keys [a b c] :excess ex-none} (assoc (select-keys sample-map [:a :b :c]) :b nil)]
            (is (some-vals ex-some))
            (is (not (some-vals ex-none)))))))

    (testing ":excess retains nil values"
      (let [{:keys [a] :excess ex-nil1} (assoc (select-keys sample-map [:a :b :c]) :b nil)
            {{:keys [aa] :excess ex-nil2} :c} (assoc-in sample-map [:c :bb] nil)]
        (is (= {:b nil :c {:aa 10}} ex-nil1))
        (is (= {:bb nil} ex-nil2))))

    (testing ":excess and :or to ensure that defaults do not show up"
      (let [{:keys [a z] :or {z 99} :excess exor} (select-keys sample-map [:a :b :c])]
        (is (= {:b 2 :c {:aa 10}} exor))))

    (testing "nested :excess, also with &"
      (let [{:keys [a] {:keys [aa] :excess exc} :c} sample-map
            {:keys! [a & :b :c]
             :syms! [& 'd 'e 'f]
             :strs! [&  "g" "h" "i"]
             {:keys [zz] :or {:zz 999} :excess exinner1} :c
             {:syms [yy] :or {'yy 999} :excess exinner2} 'f
             {:strs [xx] :or {"xx" 999} :excess exinner3} "i"
             :excess exouter} sample-map]
        (is (nil? exc))
        (is (= {:aa 10} exinner1))
        (is (= {'dd 40 'ee 50} exinner2))
        (is (= {"gg" 60 "hh" 70} exinner3))
        (is (= '{:c {:aa 10}, f {dd 40, ee 50}, "i" {"gg" 60, "hh" 70}} exouter))))

    (testing ":excess with namespace-qualification"
      (let [nsmap {:foo/x 1000, :foo/y 2000, ::z 3000}
            {:foo/keys [x] :excess exfoo} nsmap
            {::keys [z] :excess exauto} nsmap
            {:foo/keys [x] ::keys [z] :excess exmix} nsmap
            {:foo/keys [x y] ::keys [z] :excess exall} nsmap]
        (is (= {:foo/y 2000 ::z 3000} exfoo))
        (is (= {:foo/x 1000 :foo/y 2000} exauto))
        (is (= {:foo/y 2000} exmix))
        (is (nil? exall))))))

(deftest missing-directive
  (let [keys-map {:a 1 :b 2 :nest {:aa 10}}
        {:keys! [a b], :missing mkeys-nil} keys-map
        {:keys! [a b c], :missing mkeys-c} keys-map
        {:keys! [a b & :c], :missing mkeys-c&} keys-map
      
        syms-map '{a 1 b 2 nest {aa 10}}
        {:syms! [a b], :missing msyms-nil} syms-map
        {:syms! [a b c], :missing msyms-c} syms-map
        {:syms! [a b & 'c], :missing msyms-c&} syms-map

        strs-map {"a" 1 "b" 2 "nest" {"aa" 10}}
        {:strs! [a b], :missing mstrs-nil} strs-map
        {:strs! [a b c], :missing mstrs-c} strs-map
        {:strs! [a b & "c"], :missing mstrs-c&} strs-map

        q-map {:foo/a 1 :b 2 :foo/c 3 ::d 4}
        {:keys! [foo/a :foo/c] :missing mq-nil} q-map
        {:keys! [b & :foo/d] :missing mq-d} q-map
        {:keys! [foo/a & :foo/d] :missing mq-d&} q-map
        {:foo/keys! [a c] :missing q-nil} q-map
        {:foo/keys! [a & :foo/d] :missing q-d} q-map
        {:foo/keys! [a & :foo/d] :missing q-d&} q-map
        {::keys! [d] :missing aq-nil} q-map]

    (testing "1-level :missing keys for keys/qkeys/syms/strs"
      (is (nil? mkeys-nil))
      (is (= mkeys-c {:c nil}))
      (is (= mkeys-c& {:c nil}))

      (is (nil? msyms-nil))
      (is (= msyms-c '{c nil}))
      (is (= msyms-c& '{c nil}))

      (is (nil? mstrs-nil))
      (is (= mstrs-c {"c" nil}))
      (is (= mstrs-c& {"c" nil}))
      
      (is (nil? mq-nil))
      (is (= mq-d #:foo{:d nil}))
      (is (= mq-d& #:foo{:d nil}))
      (is (nil? q-nil))
      (is (= q-d #:foo{:d nil}))
      (is (= q-d& #:foo{:d nil}))
      (is (nil? aq-nil)))

    (testing "required nested map that also has required keys, covering the following cases:
               - :nest is missing
               - :nest is nil
               - :nest is a map missing :x and/or :y
               - :nest is a map having everything that's required"
      (let [sample-nest {:a 0 :b 0}]
        (are [input-map expected] (= expected
                                     (let [{:keys! [a b & :nest]
                                            {:keys! [x y]} :nest
                                            :missing mkeys-missing}
                                           input-map]
                                       mkeys-missing))

          sample-nest {:nest {:x nil, :y nil}}
          
          (assoc sample-nest :nest nil) {:nest {:x nil, :y nil}}
          
          (assoc sample-nest :nest {:x 1}) {:nest {:y nil}}

          (assoc sample-nest :nest {:x 1 :y 2}) nil)))

    (testing "that a nested map required keys is captured with the outer :missing"
      (let [{:keys! [a b], {:keys! [bb]} :nest :missing mkeys-outer} keys-map]
        (is (= mkeys-outer {:nest {:bb nil}}))))))

(comment

  (cljs.test/run-tests)

  )
