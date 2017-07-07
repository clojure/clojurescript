(ns npm-deps-test.core
  (:require left-pad))

(enable-console-print!)

(println "Padded:" (left-pad 42 5 0))
