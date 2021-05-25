(ns cljs.test-runner
  (:require [cljs.analyzer-api-tests]
            [cljs.analyzer-tests]
            [cljs.build-api-tests]
            [cljs.closure-tests]
            [cljs.compiler-tests]
            [cljs.externs-infer-tests]
            [cljs.externs-parsing-tests]
            [cljs.instant-tests]
            [cljs.module-graph-tests]
            [cljs.module-processing-tests]
            [cljs.source-map.base64-tests]
            [cljs.type-inference-tests]
            [cljs.util-tests]
            [clojure.test :refer [run-tests]]))

(defn -main []
  (let [{:keys [fail error]}
        (run-tests
          'cljs.analyzer-api-tests
          'cljs.analyzer-tests
          'cljs.build-api-tests
          'cljs.closure-tests
          'cljs.compiler-tests
          'cljs.externs-infer-tests
          'cljs.externs-parsing-tests
          'cljs.instant-tests
          'cljs.module-graph-tests
          'cljs.module-processing-tests
          'cljs.source-map.base64-tests
          'cljs.type-inference-tests
          'cljs.util-tests)]
    (if (or (not (zero? fail))
            (not (zero? error)))
      (System/exit 1)
      (System/exit 0))))
