;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.interop-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is are]]))

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
    (is (= (.get {:foo "bar"} :bar :default) :default))
    (let [iter (.keys {:foo "bar" :baz "woz"})]
      (testing "map key iteration"
        (is (#{:foo :baz} (.-value (.next iter))))
        (is (#{:foo :baz} (.-value (.next iter))))
        (is (.-done (.next iter)))))
    (let [eiter (.entries {:foo "bar" :baz "woz"})]
      (testing "map entry iteration"
        (let [entries #{(seq #js [:foo "bar"]) (seq #js [:baz "woz"])}]
          (is (entries (seq (.-value (.next eiter)))))
          (is (entries (seq (.-value (.next eiter))))))
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