(ns test-runner
  (:require [cljs.test :as test :refer-macros [run-tests]]
            [cljs.core-test :as core-test]
            [cljs.reader-test :as reader-test]
            [cljs.binding-test]
            [cljs.ns-test :as ns-test]
            [clojure.string-test]
            [clojure.data-test]
            [cljs.macro-test]
            [cljs.letfn-test]
            [foo.ns-shadow-test :as ns-shadow-test]
            [cljs.top-level]
            [cljs.reducers-test]
            [cljs.keyword-test]
            [cljs.import-test :as import-test]))

(set! *print-newline* false)
(set-print-fn! js/print)

(core-test/test-stuff)
(reader-test/test-reader)
(run-tests
  'clojure.string-test
  'clojure.data-test
  'cljs.letfn-test
  'cljs.reducers-test
  'cljs.binding-test
  'cljs.macro-test
  'cljs.top-level
  'cljs.keyword-test)
(ns-test/test-ns)
(ns-shadow-test/test-shadow)
(import-test/test-import)

(println "Tests completed without exception")


