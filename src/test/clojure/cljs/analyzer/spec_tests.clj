;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.spec-tests
  (:require [cljs.analyzer.api :as ana :refer [no-warn]]
            [cljs.analyzer-tests :refer [analyze ns-env]]
            [cljs.analyzer.specs :as a]
            [clojure.test :as test :refer [deftest is]]
            [clojure.spec.alpha :as s]))

(deftest test-const
  (is (s/valid? ::a/node (analyze ns-env 1)))
  (is (s/valid? ::a/node (analyze ns-env 1.2)))
  (is (s/valid? ::a/node (analyze ns-env true)))
  (is (s/valid? ::a/node (analyze ns-env "foo")))
  (is (s/valid? ::a/node (analyze ns-env [])))
  (is (s/valid? ::a/node (analyze ns-env [1 2 3])))
  (is (s/valid? ::a/node (analyze ns-env {})))
  (is (s/valid? ::a/node (analyze ns-env {1 2 3 4})))
  (is (s/valid? ::a/node (analyze ns-env #{})))
  (is (s/valid? ::a/node (analyze ns-env #{1 2 3}))))

(deftest test-if
  (is (s/valid? ::a/node (analyze ns-env '(if true true))))
  (is (s/valid? ::a/node (analyze ns-env '(if true true false)))))

(deftest test-do
  (is (s/valid? ::a/node (analyze ns-env '(do))))
  (is (s/valid? ::a/node (analyze ns-env '(do 1))))
  (is (s/valid? ::a/node (analyze ns-env '(do 1 2 3)))))

(deftest test-let
  (is (s/valid? ::a/node (analyze ns-env '(let []))))
  (is (s/valid? ::a/node (analyze ns-env '(let [x 1]))))
  (is (s/valid? ::a/node (analyze ns-env '(let [x 1] x)))))

(deftest test-throw
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(throw (js/Error. "foo")))))))

(deftest test-def
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(def x)))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(def x 1)))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(def x (fn []))))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(def x (fn [y] y)))))))

(deftest test-fn
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(fn [])))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(fn [] 1)))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(fn [x])))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(fn [x] 1))))))

(deftest test-defn
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(defn x [])))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(defn x [] 1)))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(defn x [y] y))))))

(deftest test-new
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(new String)))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(new js/String)))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(String.)))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(js/String.))))))

(comment

  (test/run-tests)

  (s/valid? ::a/node (no-warn (analyze ns-env '(case x 1 :foo 2 :bar))))
  (s/explain ::a/node (no-warn (analyze ns-env '(case x 1 :foo 2 :bar))))

  )
