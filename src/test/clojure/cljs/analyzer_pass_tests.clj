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

(deftest test-and-or-code-gen-pass
  (testing "and/or optimization code gen pass"
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (->> `(and true false)
                     (analyze expr-env))
          code     (with-out-str (emit ast))]
      (is (= code "((true) && (false))")))
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (analyze expr-env
                     `(and true (or true false)))
          code     (with-out-str (emit ast))]
      (is (= code "((true) && (((true) || (false))))")))
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (analyze expr-env
                     `(or true (and false true)))
          code     (with-out-str (emit ast))]
      (is (= code "((true) || (((false) && (true))))")))
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          local    (gensym)
          ast      (analyze expr-env
                     `(let [~local true]
                        (and true (or ~local false))))
          code     (with-out-str (emit ast))]
      (is (= code
            (string/replace
              "(function (){var $SYM = true;\nreturn ((true) && ((($SYM) || (false))));\n})()"
              "$SYM" (str local)))))))

(deftest test-and-or-local
  (testing "and/or optimizable with boolean local"
    (let [expr-env (assoc (ana/empty-env) :context :expr)
          ast      (->> `(let [x# true]
                           (and x# true false))
                     (analyze expr-env))
          code     (with-out-str (emit ast))]
      (is (= 2 (count (re-seq #"&&" code)))))))

(deftest test-and-or-boolean-fn-arg
  (testing "and/or optimizable with boolean fn arg"
    (let [arg  (with-meta 'x {:tag 'boolean})
          ast  (analyze (assoc (ana/empty-env) :context :expr)
                 `(fn [~arg]
                    (and ~arg false false)))
          code (with-out-str (emit ast))]
      (is (= 2 (count (re-seq #"&&" code)))))))

(deftest test-and-or-boolean-var
  (testing "and/or optimizable with boolean var"
    (let [code (env/with-compiler-env (env/default-compiler-env)
                 (compile-form-seq
                   '[(ns foo.bar)
                     (def baz true)
                     (defn woz []
                       (and baz false))]))]
      (is (= 1 (count (re-seq #"&&" code)))))))

(deftest test-and-or-js-boolean-var
  (testing "and/or optimizable with js boolean var"
    (let [code (env/with-compiler-env (env/default-compiler-env)
                 (compile-form-seq
                   '[(ns foo.bar)
                     (defn baz []
                       (and ^boolean js/woz false))]))]
      (is (= 1 (count (re-seq #"&&" code)))))))

(deftest test-and-or-host-call
  (testing "and/or optimizable with host call"
    (let [code (env/with-compiler-env (env/default-compiler-env)
                 (compile-form-seq
                   '[(ns foo.bar)
                     (defn bar [x]
                       (and ^boolean (.woz x) false))]))]
      (is (= 1 (count (re-seq #"&&" code)))))))

(deftest test-and-or-host-field
  (testing "and/or optimizable with host field"
    (let [code (env/with-compiler-env (env/default-compiler-env)
                 (compile-form-seq
                   '[(ns foo.bar)
                     (defn bar [x]
                       (and ^boolean (.-woz x) false))]))]
      (is (= 1 (count (re-seq #"&&" code)))))))

(deftest test-core-predicates
  (testing "and/or optimizable with core predicates"
    (let [code (env/with-compiler-env (env/default-compiler-env)
                (comp/with-core-cljs {}
                  (fn []
                    (compile-form-seq
                      '[(ns foo.bar)
                        (defn bar []
                          (and (even? 1) false))]))))]
      (is (= 1 (count (re-seq #"&&" code)))))))

(comment
  (test/run-tests)

  (require '[clojure.pprint :refer [pprint]])

  )
