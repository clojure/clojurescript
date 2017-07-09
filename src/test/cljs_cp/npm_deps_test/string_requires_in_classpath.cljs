(ns npm-deps-test.string-requires-in-classpath
  "This tests string require of a lib that is not loaded
  by project local files from a classpath file."
  (:require ["lodash/array" :as array]))

(println "lodash/array is loaded:" (array/nth #js [true] 1))
