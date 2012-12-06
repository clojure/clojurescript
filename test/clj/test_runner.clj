  (require '[cljs.compiler-test :as comp-test])
  (require '[clojure.test :as test])
  (require '[cljs.closure :as closure])

(test/deftest a
  (comp-test/test-exclude-file-names))

(test/run-tests)

(println "Clojure Test Finished")
