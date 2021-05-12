;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer-pass-tests
  (:require [cljs.analyzer :as ana]
            [cljs.analyzer.passes.and-or :as and-or]
            [cljs.analyzer-tests :as ana-tests :refer [analyze]]
            [cljs.compiler-tests :as comp-tests :refer [emit]]
            [clojure.test :as test :refer [deftest is testing]]))

(deftest test-helpers
  (testing "Testing and/or matching helpers"
    (let [ast (analyze (ana/empty-env) `(and true false))]
      (is (and-or/simple-op? (-> ast :bindings first :init)))
      (is (and-or/simple-test-expr? (-> ast :bindings first :init)))
      (is (and-or/single-binding-let? ast))
      (is (and-or/simple-test-binding-let? ast))
      (is (and-or/no-statements? ast))
      (is (and-or/returns-if? ast)))))

(deftest and-or-matchers
  (testing "Testing and/or ast matching & optimizability"
    (let [ast (analyze (ana/empty-env) `(and true false))]
      (is (and-or/simple-and? ast))
      (is (and-or/optimizable-and? ast)))
    (let [ast (ana/no-warn
                (analyze (ana/empty-env) `(and ~(with-meta 'x {:tag 'boolean}) false)))]
      (is (and-or/simple-and? ast))
      (is (and-or/optimizable-and? ast)))
    (let [ast (analyze (ana/empty-env) `(or true false))]
      (is (and-or/simple-or? ast))
      (is (and-or/optimizable-or? ast)))
    (let [ast (ana/no-warn
                (analyze (ana/empty-env) `(or ~(with-meta 'x {:tag 'boolean}) false)))]
      (is (and-or/simple-or? ast))
      (is (and-or/optimizable-or? ast)))))

(deftest simple-let-optimizability
  (testing "Testing that simple lets with boolean binding can optimize"
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (->> `(let [x# true]
                           (and x# true))
                     (analyze expr-env))]
      (is (and-or/simple-test-expr? (-> ast :bindings first :init)))
      ;;
      )))

(deftest and-or-code-gen
  (testing "and/or optimization code gen"
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (->> `(and true false)
                     (analyze expr-env)
                     and-or/optimize-and)
          code     (with-out-str (emit ast))]
      (is (= code "(true) && (false)")))
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (->> `(or true false)
                     (analyze expr-env)
                     and-or/optimize-or)
          code     (with-out-str (emit ast))]
      (is (= code "(true) || (false)")))))

(deftest and-or-code-gen-pass
  (testing "and/or optimization code gen pass"
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (binding [ana/*passes* (conj ana/*passes* and-or/optimize)]
                     (->> `(and true false)
                       (analyze expr-env)))
          code     (with-out-str (emit ast))]
      (is (= code "(true) && (false)")))))

;; TODO: a couple of core predicate in compound expression
;; TODO: mixing and/or
;; TODO: boolean var
;; TODO: boolean js-var
;; TODO: host-field
;; TODO: host-call

(comment
  (test/run-tests)

  (require '[clojure.pprint :refer [pprint]])

  ;; Doesn't seem right
  ;; it's understandable why the outside doesn't optimize
  ;; but not why the inner part is not fully optimized
  (let [expr-env (assoc (ana/empty-env) :context :expr)
        ast      (->> `(and ~(with-meta 'x {:tag 'boolean}) false)
                   (analyze expr-env))]
    (and-or/optimizable-and? ast))

  ;; works
  (let [expr-env (assoc (ana/empty-env) :context :expr)
        ast      (->> `(and ~(with-meta 'x {:tag 'boolean}) false)
                   (analyze expr-env))]
    (emit (and-or/optimize-and ast)))

  ;; does not work
  (let [expr-env (assoc (ana/empty-env) :context :expr)
        ast      (binding [ana/*passes* (conj ana/*passes* and-or/optimize)]
                   (->> `(and ~(with-meta 'x {:tag 'boolean}) false)
                     (analyze expr-env)))]
    (emit ast))

  ;; ==========
  ;; variations
  (let [expr-env (assoc (ana/empty-env) :context :expr)
        ast      (binding [ana/*passes* (conj ana/*passes* and-or/optimize)]
                   (->> `(let [x# true]
                           (and x# true false))
                     (analyze expr-env)))]
    (with-out-str (emit ast)))

  (let [ast (binding [ana/*passes* (conj ana/*passes* and-or/optimize)]
              (analyze (assoc (ana/empty-env) :context :expr)
                `(fn []
                   (if (and true false false)
                     :a :b))))]
    (emit ast))

  (let [ast (binding [ana/*passes* (conj ana/*passes* and-or/optimize)]
              (analyze (assoc (ana/empty-env) :context :expr)
                `(fn []
                   (if (and x false false)
                     :a :b))))]
    (emit ast))

  )
