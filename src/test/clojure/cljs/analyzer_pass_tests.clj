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
            [cljs.compiler-tests :as comp-tests :refer [emit]]
            [clojure.test :as test :refer [deftest is testing]]))

(def simple-ops
  #{:var :js-var :local :invoke :const :host-field :host-call :js :quote})

(defn ->expr-env [ast]
  (assoc-in ast [:env :context] :expr))

(defn simple-op? [ast]
  (contains? simple-ops (:op ast)))

(defn simple-test-expr?
  [ast]
  (boolean
    (and (simple-op? ast)
         ('#{boolean seq} (:tag ast)))))

(defn single-binding-let? [ast]
  (and (= :let (:op ast))
       (= 1 (count (-> ast :bindings)))))

(defn no-statements? [let-ast]
  (= [] (-> let-ast :body :statements)))

(defn returns-if? [let-ast]
  (= :if (-> let-ast :body :ret :op)))

(defn simple-test-binding-let? [ast]
  (and (single-binding-let? ast)
       (no-statements? ast)
       (simple-test-expr? (-> ast :bindings first :init))
       (returns-if? ast)))

(defn test=then? [if-ast]
  ;; remove :env, if same, local will differ only by
  ;; :context (:expr | :statement)
  (= (dissoc (:test if-ast) :env)
     (dissoc (:then if-ast) :env)))

(defn test=else? [if-ast]
  ;; remove :env, if same, local will differ only by
  ;; :context (:expr | :statement)
  (= (dissoc (:test if-ast) :env)
     (dissoc (:else if-ast) :env)))

(defn simple-and? [ast]
  (and (simple-test-binding-let? ast)
       (test=else? (-> ast :body :ret))))

(defn simple-or? [ast]
  (and (simple-test-binding-let? ast)
       (test=then? (-> ast :body :ret))))

(defn optimizable-and? [ast]
  (and (simple-and? ast)
       (simple-test-expr? (-> ast :body :ret :then))))

(defn optimizable-or? [ast]
  (and (simple-or? ast)
       (simple-test-expr? (-> ast :body :ret :else))))

(defn optimize-and [ast]
  {:op :js
   :env (:env ast)
   :segs ["(" ") && (" ")"]
   :args [(-> ast :bindings first :init)
          (->expr-env (-> ast :body :ret :then))]
   :form (:form ast)
   :children [:args]
   :tag 'boolean})

(defn optimize-or [ast]
  {:op :js
   :env (:env ast)
   :segs ["(" ") || (" ")"]
   :args [(-> ast :bindings first :init)
          (->expr-env (-> ast :body :ret :else))]
   :form (:form ast)
   :children [:args]
   :tag 'boolean})

(defn optimize-and-or [env ast _]
  (cond
    (optimizable-and? ast) (optimize-and ast)
    (optimizable-or? ast)  (optimize-or ast)
    :else ast))

(deftest test-helpers
  (testing "Testing and/or matching helpers"
    (let [ast (analyze (ana/empty-env) `(and true false))]
      (is (simple-op? (-> ast :bindings first :init)))
      (is (simple-test-expr? (-> ast :bindings first :init)))
      (is (single-binding-let? ast))
      (is (simple-test-binding-let? ast))
      (is (no-statements? ast))
      (is (returns-if? ast)))))

(deftest and-or-matchers
  (testing "Testing and/or ast matching"
    (let [ast (analyze (ana/empty-env) `(and true false))]
      (is (simple-and? ast))
      (is (optimizable-and? ast)))
    (let [ast (analyze (ana/empty-env) `(or true false))]
      (is (simple-or? ast))
      (is (optimizable-or? ast)))))

(deftest and-or-code-gen
  (testing "and/or optimization code gen"
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (->> `(and true false)
                     (analyze expr-env)
                     optimize-and)
          code     (with-out-str (emit ast))]
      (is (= code "(true) && (false)")))
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (->> `(or true false)
                     (analyze expr-env)
                     optimize-or)
          code     (with-out-str (emit ast))]
      (is (= code "(true) || (false)")))))

(deftest and-or-code-gen-pass
  (testing "and/or optimization code gen pass"
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (binding [ana/*passes* (into [optimize-and-or] ana/*passes*)]
                     (->> `(and true false)
                       (analyze expr-env)))
          code     (with-out-str (emit ast))]
      (is (= code "(true) && (false)")))))

(comment
  (test/run-tests)

  (require '[clojure.pprint :refer [pprint]])

  (let [ast (binding [ana/*passes* (into [optimize-and-or] ana/*passes*)]
              (analyze (assoc (ana/empty-env) :context :expr)
                `(fn []
                   (if (and true false false)
                     :a :b))))]
    (emit ast))

  )
