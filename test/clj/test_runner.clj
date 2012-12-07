(ns test-runner
  (require [clojure.test :as t]
           [cljs.compiler-test :as c]))

(t/run-tests 'cljs.compiler-test)

(println "Test completed without exception")
