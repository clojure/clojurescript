(ns test-runner
  (:require [cljs.core-test :as core-test]
            [cljs.reader-test :as reader-test]
            [cljs.binding-test :as binding-test]
            [cljs.ns-test :as ns-test]
            [clojure.string-test :as string-test]
            [cljs.macro-test :as macro-test]
            [cljs.letfn-test :as letfn-test]))

(set! *print-fn* js/print)

(core-test/test-stuff)
(reader-test/test-reader)
(string-test/test-string)
(binding-test/test-binding)
(ns-test/test-ns)
(macro-test/test-macros)
(letfn-test/test-letfn)

(println "Tests completed without exception")


