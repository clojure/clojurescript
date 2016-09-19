(ns test-runner
  (:require [cljs.test :refer-macros [run-tests]]
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
            [cljs.syntax-quote-test]
            [cljs.import-test]
            [cljs.ns-test.foo]
            [cljs.pprint]
            [cljs.clojure-alias-test]
            [cljs.hash-map-test]))

(set! *print-newline* false)
(set-print-fn! js/print)

(run-tests
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
  'cljs.syntax-quote-test
  'cljs.ns-test
  'cljs.ns-test.foo
  'foo.ns-shadow-test
  'cljs.import-test
  'cljs.pprint
  'cljs.clojure-alias-test
  'cljs.hash-map-test)
