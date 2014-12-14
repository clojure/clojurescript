(ns test-runner
  (:require [cljs.test :as test :refer-macros [run-tests]]
            [cljs.core-test :as core-test]
            [cljs.reader-test]
            [cljs.binding-test]
            [cljs.ns-test]
            [clojure.string-test]
            [clojure.data-test]
            [cljs.macro-test]
            [cljs.letfn-test]
            [foo.ns-shadow-test]
            [cljs.top-level]
            [cljs.reducers-test]
            [cljs.keyword-test]
            [cljs.import-test]))

(set! *print-newline* false)
(set-print-fn! js/print)

(core-test/test-stuff)
(run-tests
  'cljs.reader-test
  'clojure.string-test
  'clojure.data-test
  'cljs.letfn-test
  'cljs.reducers-test
  'cljs.binding-test
  'cljs.macro-test
  'cljs.top-level
  'cljs.keyword-test
  'cljs.ns-tst
  'foo.ns-shadow-test
  'cljs.import-test)
