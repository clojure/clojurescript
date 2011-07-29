(ns cljs.test-runner
  (:require [cljs.core-test :as core-test]
            [cljs.reader-test :as reader-test]))

(core-test/test-stuff)
(reader-test/test-reader)

(println "Tests completed without exception")


