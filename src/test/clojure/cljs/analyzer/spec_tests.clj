;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.spec-tests
  (:require [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api :refer [no-warn]]
            [cljs.compiler.api :as comp-api]
            [cljs.analyzer-tests :refer [analyze ns-env]]
            [cljs.analyzer.specs :as a]
            [clojure.test :as test :refer [deftest is]]
            [clojure.spec.alpha :as s])
  (:import [java.io StringReader]))

(deftest test-binding
  (let [node (analyze ns-env '(let [x 1] x))
        binding (-> node :bindings first)]
    (is (= :binding (:op binding)))
    (is (s/valid? ::a/node binding))))

(deftest test-case
  (let [let-node (no-warn (analyze ns-env '(case x 1 :foo 2 :bar)))
        node     (-> let-node :body :ret)]
    (is (= :case (:op node)))
    (is (s/valid? ::a/node node))
    (let [nodes (-> node :nodes)
          case-node (first nodes)]
      (is (= :case-node (:op case-node)))
      (is (s/valid? ::a/node case-node))
      (let [case-tests (:tests case-node)
            case-test  (first case-tests)
            case-then  (:then case-node)]
        (is (= :case-test (:op case-test)))
        (is (s/valid? ::a/node case-test))
        (is (= :case-then (:op case-then)))
        (is (s/valid? ::a/node case-then))))))

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

(deftest test-def
  (let [node (no-warn (analyze ns-env '(def x)))]
    (is (= :def (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env '(def x 1))))
  (is (s/valid? ::a/node (analyze ns-env '(def x (fn [])))))
  (is (s/valid? ::a/node (analyze ns-env '(def x (fn [y] y))))))

(deftest test-defn
  (is (s/valid? ::a/node (analyze ns-env '(defn x []))))
  (is (s/valid? ::a/node (analyze ns-env '(defn x [] 1))))
  (is (s/valid? ::a/node (analyze ns-env '(defn x [y] y)))))

(deftest test-defrecord
  (let [node (no-warn (analyze ns-env '(defrecord A [])))
        body (:body node)]
    (is (= :defrecord (-> body :statements first :ret :op)))
    (is (s/valid? ::a/node node))))

(deftest test-deftype
  (let [node (no-warn (analyze ns-env '(deftype A [])))]
    (is (= :deftype (-> node :statements first :op)))
    (is (s/valid? ::a/node node))))

(deftest test-do
  (let [node (analyze ns-env '(do))]
    (is (= :do (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env '(do 1))))
  (is (s/valid? ::a/node (analyze ns-env '(do 1 2 3)))))

(deftest test-fn
  (let [node (no-warn (analyze ns-env '(fn [])))]
    (is (= :fn (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env '(fn [] 1))))
  (is (s/valid? ::a/node (analyze ns-env '(fn [x]))))
  (is (s/valid? ::a/node (analyze ns-env '(fn [x] 1)))))

(deftest test-fn-method
  (let [node (analyze ns-env '(fn ([]) ([x] x)))
        methods (:methods node)
        fn0 (first methods)
        fn1 (second methods)]
    (is (= :fn-method (:op fn0)))
    (is (s/valid? ::a/node fn0))
    (is (= :fn-method (:op fn1)))
    (is (s/valid? ::a/node fn1))))

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

(deftest test-if
  (let [node (analyze ns-env '(if true true))]
    (is (= :if (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env '(if true true false)))))

(deftest test-invoke
  (let [node (no-warn (analyze ns-env '(count "foo")))]
    (is (= :invoke (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-js
  (let [node (analyze ns-env '(js* "~{}" 1))]
    (is (= :js (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-js-array
  (let [node (analyze ns-env
               (ana-api/with-state (ana-api/empty-state)
                 (first (ana-api/forms-seq (StringReader. "#js [1 2 3]")))))]
    (is (= :js-array (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-js-object
  (let [node (analyze ns-env
               (ana-api/with-state (ana-api/empty-state)
                 (first (ana-api/forms-seq (StringReader. "#js {:foo 1 :bar 2}")))))]
    (is (= :js-object (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-js-var
  (let [node (analyze ns-env 'js/String)]
    (is (= :js-var (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-let
  (let [node (analyze ns-env '(let []))]
    (is (= :let (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env '(let [x 1]))))
  (is (s/valid? ::a/node (analyze ns-env '(let [x 1] x)))))

(deftest test-letfn
  (let [node (analyze ns-env '(letfn [(foo [] (bar)) (bar [] (foo))]))]
    (is (= :letfn (:op node)))
    (is (s/valid? ::a/node node))))

;; list, no longer needed, subsumed by :quote

(deftest test-local
  (let [node      (analyze ns-env '(fn [x] x))
        fn-method (-> node :methods first)
        body      (-> fn-method :body)
        ret       (:ret body)]
    (is (= :local (:op ret)))
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

(deftest test-map
  (let [node (no-warn (analyze ns-env '{:foo 1 :bar 2}))]
    (is (= :map (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-new
  (let [node (no-warn (analyze ns-env '(new String)))]
    (is (= :new (:op node)))
    (is (s/valid? ::a/node node)))
  (is (s/valid? ::a/node (analyze ns-env '(new js/String))))
  (is (s/valid? ::a/node (no-warn (analyze ns-env '(String.)))))
  (is (s/valid? ::a/node (analyze ns-env '(js/String.)))))

(deftest test-no-op
  (let [node (binding [ana/*unchecked-if* true]
               (no-warn (analyze ns-env '(set! *unchecked-if* false))))]
    (is (= :no-op (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-ns
  (let [node (no-warn
               (binding [ana/*cljs-ns* 'cljs.user]
                 (analyze ns-env '(ns foo (:require [goog.string])))))]
    (is (= :ns (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-ns*
  (let [node (no-warn
               (binding [ana/*cljs-ns* 'cljs.user]
                 (analyze ns-env '(ns* (:require '[goog.string])))))]
    (is (= :ns* (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-quote
  (let [node (analyze ns-env ''(1 2 3))]
    (is (= :quote (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-recur
  (let [node (no-warn (analyze ns-env '(fn [x] (recur (inc x)))))]
    (is (s/valid? ::a/node node))))

(deftest test-set
  (let [node (analyze ns-env #{1 2 3})]
    (is (= :set (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-set!
  (let [node (no-warn (analyze ns-env '(set! x 1)))]
    (is (= :set! (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-the-var
  (let [node (comp-api/with-core-cljs {}
               #(analyze ns-env '(var first)))]
    (is (= :the-var (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-throw
  (let [node (no-warn (analyze ns-env '(throw (js/Error. "foo"))))]
    (is (= :throw (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-try
  (let [node (no-warn (analyze ns-env '(try 1 (catch :default e) (finally))))]
    (is (= :try (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-var
  (let [node      (no-warn (analyze ns-env '(fn [] x)))
        fn-method (-> node :methods first)
        body      (-> fn-method :body)
        ret       (:ret body)]
    (is (= :var (:op ret)))
    (is (s/valid? ::a/node node))))

(deftest test-vector
  (let [node (no-warn (analyze ns-env '[1 2]))]
    (is (= :vector (:op node)))
    (is (s/valid? ::a/node node))))

(deftest test-with-meta
  (let [node (analyze ns-env ^{:meta 2} {:foo 1})]
    (is (= :with-meta (:op node)))
    (is (s/valid? ::a/node node))))

(comment

  (test/run-tests)

  )
