(ns test-runner
  (:require [cljs.test :refer-macros [run-tests]]
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
            [cljs.clojure-alias-test]
            [cljs.hash-map-test]
            [cljs.predicates-test]))

(set! *print-newline* false)
(set-print-fn! js/print)

(run-tests
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
  'cljs.clojure-alias-test
  'cljs.hash-map-test
  'cljs.pprint-test
  ;; TODO: CLJS-1799
  #_'cljs.predicates-test
  'cljs.syntax-quote-test)
