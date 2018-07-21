;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [cljs.apply-test]
            [cljs.primitives-test]
            [cljs.destructuring-test]
            [cljs.new-new-test]
            [cljs.printing-test]
            [cljs.seqs-test]
            [cljs.collections-test]
            [cljs.hashing-test]
            [cljs.core-test :as core-test]
            [cljs.reader-test]
            [cljs.binding-test]
            [cljs.ns-test]
            [clojure.string-test]
            [clojure.data-test]
            [clojure.walk-test]
            [cljs.macro-test]
            [cljs.letfn-test]
            [foo.ns-shadow-test]
            [cljs.top-level]
            [cljs.reducers-test]
            [cljs.keyword-test]
            [cljs.import-test]
            [cljs.ns-test.foo]
            [cljs.syntax-quote-test]
            [cljs.pprint]
            [cljs.pprint-test]
            [cljs.spec-test]
            [cljs.spec.test-test]
            [cljs.clojure-alias-test]
            [cljs.hash-map-test]
            [cljs.map-entry-test]
            [cljs.npm-deps-test]
            [cljs.predicates-test]
            [cljs.tagged-literals-test]
            [cljs.test-test]
            [static.core-test]
            [cljs.recur-test]
            [cljs.array-access-test]
            [cljs.inference-test]
            [cljs.extend-to-native-test]))

(set! *print-newline* false)
(set-print-fn! js/print)

(run-tests
  'cljs.apply-test
  'cljs.primitives-test
  'cljs.destructuring-test
  'cljs.new-new-test
  'cljs.printing-test
  'cljs.seqs-test
  'cljs.collections-test
  'cljs.hashing-test
  'cljs.core-test
  'cljs.reader-test
  'clojure.string-test
  'clojure.data-test
  'clojure.walk-test
  'cljs.letfn-test
  'cljs.reducers-test
  'cljs.binding-test
  'cljs.macro-test
  'cljs.top-level
  'cljs.keyword-test
  'cljs.ns-test
  'cljs.ns-test.foo
  'foo.ns-shadow-test
  'cljs.import-test
  'cljs.pprint
  'cljs.spec-test
  'cljs.spec.test-test
  'cljs.clojure-alias-test
  'cljs.hash-map-test
  'cljs.map-entry-test
  'cljs.npm-deps-test
  'cljs.pprint-test
  'cljs.predicates-test
  'cljs.syntax-quote-test
  'cljs.tagged-literals-test
  'cljs.test-test
  'static.core-test
  'cljs.recur-test
  'cljs.array-access-test
  'cljs.inference-test
  'cljs.extend-to-native-test)
