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
  (let [node (analyze ns-env [])]
    (is (= :vector (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env [1 2 3])))
  (is (s/valid? ::a/node (analyze ns-env {})))
  (let [node (analyze ns-env {1 2 3 4})]
    (is (= :map (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env #{})))
  (let [node (analyze ns-env #{1 2 3})]
    (is (= :set (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-if
  (let [node (analyze ns-env '(if true true))]
    (is (= :if (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env '(if true true false)))))

(deftest test-do
  (let [node (analyze ns-env '(do))]
    (is (= :do (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env '(do 1))))
  (is (s/valid? ::a/node (analyze ns-env '(do 1 2 3)))))

(deftest test-let
  (let [node (analyze ns-env '(let []))]
    (is (= :let (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env '(let [x 1]))))
  (is (s/valid? ::a/node (analyze ns-env '(let [x 1] x)))))

(deftest test-throw
  (let [node (no-warn (analyze ns-env '(throw (js/Error. "foo"))))]
    (is (= :throw (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-def
  (let [node (no-warn (analyze ns-env '(def x)))]
    (is (= :def (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env '(def x 1))))
  (is (s/valid? ::a/node (analyze ns-env '(def x (fn [])))))
  (is (s/valid? ::a/node (analyze ns-env '(def x (fn [y] y))))))

(deftest test-fn
  (let [node (no-warn (analyze ns-env '(fn [])))]
    (is (= :fn (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env '(fn [] 1))))
  (is (s/valid? ::a/node (analyze ns-env '(fn [x]))))
  (is (s/valid? ::a/node (analyze ns-env '(fn [x] 1)))))

(deftest test-defn
  (is (s/valid? ::a/node (analyze ns-env '(defn x []))))
  (is (s/valid? ::a/node (analyze ns-env '(defn x [] 1))))
  (is (s/valid? ::a/node (analyze ns-env '(defn x [y] y)))))

(deftest test-new
  (let [node (no-warn (analyze ns-env '(new String)))]
    (is (= :new (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env '(new js/String))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(String.)))))
  (is (s/valid? ::a/node (analyze ns-env '(js/String.)))))

(deftest test-deftype
  (let [node (no-warn (analyze ns-env '(deftype A [])))]
    (is (= :deftype (-> node :statements first :op)))
    (is (s/valid? ::a/node node))))

(deftest test-defrecord
  (let [node (no-warn (analyze ns-env '(defrecord A [])))
        body (:body node)]
    (is (= :defrecord (-> body :statements first :ret :op)))
    (is (s/valid? ::a/node node))))

(deftest test-host-call
  (let [node (analyze ns-env '(.substring "foo" 0 1))]
    (is (= :host-call (:op node)))
    (is (s/valid? ::a/node node)))
  (let [node (analyze ns-env '(. "foo" (substring 0 1)))]
    (is (= :host-call (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-host-field
  (let [node (analyze ns-env '(.-length "foo"))]
    (is (= :host-field (:op node)))
    (is (s/valid? ::a/node node)))
  (let [node (analyze ns-env '(. "foo" -length))]
    (is (= :host-field (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-invoke
  (let [node (no-warn (analyze ns-env '(count "foo")))]
    (is (= :invoke (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-loop
  (let [node (analyze ns-env '(loop []))]
    (is (= :loop (:op node)))
    (is (s/valid? ::a/node node)))
  (let [node (analyze ns-env '(loop [x 1] x))]
    (is (s/valid? ::a/node node)))
  (let [node (analyze ns-env '(loop [x 1] (recur (inc x))))]
    (is (s/valid? ::a/node node)))
  (let [node (no-warn
               (analyze ns-env
                 '(loop [x 100]
                    (if (pos? x)
                      (recur (dec x))
                      x))))]
    (is (s/valid? ::a/node node))))

(deftest test-recur
  (let [node (no-warn (analyze ns-env '(fn [x] (recur (inc x)))))]
    (is (s/valid? ::a/node node))))

(deftest test-case
  (let [node (no-warn (analyze ns-env '(case x 1 :foo 2 :bar)))]
    (is (s/valid? ::a/node node))))

;; letfn

;; local

; TODO: #js
;(deftest test-js-object
;  )

(comment

  (test/run-tests)

  )
