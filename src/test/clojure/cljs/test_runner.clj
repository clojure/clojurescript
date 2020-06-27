(ns cljs.test-runner
  (:require [cljs.source-map.base64-tests]
            [cljs.analyzer-api-tests]
            [cljs.analyzer-tests]
            [cljs.build-api-tests]
            [cljs.closure-tests]
            [cljs.compiler-tests]
            [cljs.externs-infer-tests]
            [cljs.externs-parsing-tests]
            [cljs.module-graph-tests]
            [cljs.module-processing-tests]
            [cljs.module-graph-tests]
            [cljs.module-processing-tests]
            [cljs.type-inference-tests]
            [cljs.util-tests]
            [clojure.test :refer [run-all-tests]]))

(run-all-tests)
