;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer-pass-tests
  (:require [cljs.analyzer :as ana]
            [cljs.analyzer-tests :as ana-tests :refer [analyze]]
            [clojure.test :as test :refer [deftest is testing]]))

(def simple-ops
  #{:var :js-var :local :invoke :const :host-field :host-call :js :quote})

(defn simple-op? [ast]
  (contains? simple-ops (:op ast)))

(defn simple-test-expr?
  ([ast]
   (simple-test-expr? (ana/empty-env) ast))
  ([env ast]
   (boolean
     (and (simple-ops (:op ast))
          ('#{boolean seq} (ana/infer-tag env ast))))))

(defn single-binding-let? [ast]
  (and (= :let (:op ast))
       (= 1 (count (-> ast :bindings)))))

(defn no-statements? [let-ast]
  (= [] (-> let-ast :body :statements)))

(defn returns-if? [let-ast]
  (= :if (-> let-ast :body :ret :op)))

(defn test=then? [if-ast]
  ;; remove :env, if same local will differ only by
  ;; :context (:expr | :statement)
  (= (dissoc (:test if-ast) :env)
     (dissoc (:then if-ast) :env)))

(defn test=else? [if-ast]
  ;; remove :env, if same local will differ only by
  ;; :context (:expr | :statement)
  (= (dissoc (:test if-ast) :env)
     (dissoc (:else if-ast) :env)))

(defn simple-and? [ast]
  (and (single-binding-let? ast)
       (no-statements? ast)
       (returns-if? ast)
       (test=else? (-> ast :body :ret))))

(defn simple-or? [ast]
  (and (single-binding-let? ast)
       (no-statements? ast)
       (returns-if? ast)
       (test=then? (-> ast :body :ret))))

(deftest test-helpers
  (testing "Testing and/or matching helpers"
    (let [ast (analyze (ana/empty-env) `(and true false))]
      (is (simple-op? (-> ast :bindings first :init)))
      (is (simple-test-expr? (-> ast :bindings first :init)))
      (is (single-binding-let? ast))
      (is (no-statements? ast))
      (is (returns-if? ast)))))

(deftest and-or-matchers
  (testing "Testing and/or ast matching"
    (let [ast (analyze (ana/empty-env) `(and true false))]
      (is (simple-and? ast)))
    (let [ast (analyze (ana/empty-env) `(or true false))]
      (is (simple-or? ast)))))

(comment
  (test/run-tests)

  (require '[clojure.pprint :refer [pprint]])

  (let [ast (analyze (ana/empty-env)
              `(and true false))]
    (-> ast :body :ret :op))

  (let [ast (analyze (ana/empty-env)
              `(and true false))]
    (-> ast :body :ret :env :locals))

  )
