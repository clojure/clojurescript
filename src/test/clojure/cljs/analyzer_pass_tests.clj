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
            [cljs.compiler :as comp]
            [cljs.compiler-tests :as comp-tests :refer [compile-form-seq emit]]
            [cljs.env :as env]
            [clojure.string :as string]
            [clojure.test :as test :refer [deftest is testing]]))

(defmacro with-and-or-pass [& body]
  `(binding [ana/*passes* (conj ana/default-passes and-or/optimize)]
     ~@body))

(deftest test-and-or-helpers
  (testing "Testing and/or matching helpers"
    (let [ast (analyze (ana/empty-env) `(and true false))]
      (is (and-or/simple-op? (-> ast :bindings first :init)))
      (is (and-or/simple-test-expr? (-> ast :bindings first :init)))
      (is (and-or/single-binding-let? ast))
      (is (and-or/simple-test-binding-let? ast))
      (is (and-or/no-statements? ast))
      (is (and-or/returns-if? ast)))))

(deftest test-and-or-matchers
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

(deftest test-and-or-code-gen
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

(deftest test-and-or-code-gen-pass
  (testing "and/or optimization code gen pass"
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (with-and-or-pass
                     (->> `(and true false)
                       (analyze expr-env)))
          code     (with-out-str (emit ast))]
      (is (= code "(true) && (false)")))
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (with-and-or-pass
                     (analyze expr-env
                       `(and true (or true false))))
          code     (with-out-str (emit ast))]
      (is (= code "(true) && ((true) || (false))")))
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (with-and-or-pass
                     (analyze expr-env
                       `(or true (and false true))))
          code     (with-out-str (emit ast))]
      (is (= code "(true) || ((false) && (true))")))
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          local    (gensym)
          ast      (with-and-or-pass
                     (analyze expr-env
                       `(let [~local true]
                          (and true (or ~local false)))))
          code     (with-out-str (emit ast))]
      (is (= code
            (string/replace
              "(function (){var $SYM = true;\nreturn (true) && (($SYM) || (false));\n})()"
              "$SYM" (str local)))))))

(deftest test-and-or-local
  (testing "and/or optimizable with boolean local"
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (with-and-or-pass
                     (->> `(let [x# true]
                             (and x# true false))
                       (analyze expr-env)))
          code     (with-out-str (emit ast))]
      (is (= 2 (count (re-seq #"&&" code)))))))

(deftest test-and-or-boolean-fn-arg
  (testing "and/or optimizable with boolean fn arg"
    (let [arg  (with-meta 'x {:tag 'boolean})
          ast  (with-and-or-pass
                 (analyze (assoc (ana/empty-env) :context :expr)
                   `(fn [~arg]
                      (and ~arg false false))))
          code (with-out-str (emit ast))]
      (is (= 2 (count (re-seq #"&&" code)))))))

(deftest test-and-or-boolean-var
  (testing "and/or optimizable with boolean var"
    (let [code (env/with-compiler-env (env/default-compiler-env)
                 (with-and-or-pass
                   (compile-form-seq
                     '[(ns foo.bar)
                       (def baz true)
                       (defn woz []
                         (and baz false))])))]
      (is (= 1 (count (re-seq #"&&" code)))))))

(deftest test-and-or-js-boolean-var
  (testing "and/or optimizable with js boolean var"
    (let [code (env/with-compiler-env (env/default-compiler-env)
                 (with-and-or-pass
                   (compile-form-seq
                     '[(ns foo.bar)
                       (defn baz []
                         (and ^boolean js/woz false))])))]
      (is (= 1 (count (re-seq #"&&" code)))))))

(deftest test-and-or-host-call
  (testing "and/or optimizable with host call"
    (let [code (env/with-compiler-env (env/default-compiler-env)
                 (with-and-or-pass
                   (compile-form-seq
                     '[(ns foo.bar)
                       (defn bar [x]
                         (and ^boolean (.woz x) false))])))]
      (is (= 1 (count (re-seq #"&&" code)))))))

(deftest test-and-or-host-field
  (testing "and/or optimizable with host field"
    (let [code (env/with-compiler-env (env/default-compiler-env)
                 (with-and-or-pass
                   (compile-form-seq
                     '[(ns foo.bar)
                       (defn bar [x]
                         (and ^boolean (.-woz x) false))])))]
      (is (= 1 (count (re-seq #"&&" code)))))))

(deftest test-core-predicates
  (testing "and/or optimizable with core predicates"
    (let [code (env/with-compiler-env (env/default-compiler-env)
                (comp/with-core-cljs {}
                  (fn []
                    (with-and-or-pass
                      (compile-form-seq
                        '[(ns foo.bar)
                          (defn bar []
                            (and (even? 1) false))])))))]
      (is (= 1 (count (re-seq #"&&" code)))))))

(comment
  (test/run-tests)

  (require '[clojure.pprint :refer [pprint]])

  )
