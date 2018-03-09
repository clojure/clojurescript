(ns package-json-resolution-test.core
  (:require [iterall]
            [graphql]))

(enable-console-print!)

(println "Is collection:" (iterall/isCollection #js [1 2]))
(println "GraphQL:" graphql)
