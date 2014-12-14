(ns test-runner
  (:require [cljs.test :as test :refer-macros [run-tests]]
            [cljs.core-test :as core-test]
            [cljs.reader-test :as reader-test]
            [cljs.binding-test :as binding-test]
            [cljs.ns-test :as ns-test]
            [clojure.string-test]
            [clojure.data-test]
            [cljs.macro-test :as macro-test]
            [cljs.letfn-test :as letfn-test]
            [foo.ns-shadow-test :as ns-shadow-test]
            [cljs.top-level :as top-level]
            [cljs.reducers-test :as reducers-test]
            [cljs.keyword-test :as keyword-test]
            [cljs.import-test :as import-test]))

(set! *print-newline* false)
(set-print-fn! js/print)

(core-test/test-stuff)
(reader-test/test-reader)
(run-tests
  'clojure.string-test
  'clojure.data-test)
(binding-test/test-binding)
(binding-test/test-with-redefs)
(ns-test/test-ns)
(macro-test/test-macros)
(letfn-test/test-letfn)
(ns-shadow-test/test-shadow)
(top-level/test)
(reducers-test/test-all)
(keyword-test/test-keyword)
(import-test/test-import)

(println "Tests completed without exception")


