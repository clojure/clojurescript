(ns test-runner
  (:require [cljs.core-test :as core-test]
            [cljs.reader-test :as reader-test]
            [clojure.string-test :as string-test]))

(set! *print-fn* js/print)

(core-test/test-stuff)
(reader-test/test-reader)
(string-test/test-string)

(println "Tests completed without exception")


