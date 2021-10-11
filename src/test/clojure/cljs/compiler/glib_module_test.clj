(ns cljs.compiler.glib-module-test
  (:require [cljs.compiler :as comp]
            [cljs.compiler-tests :as comp-tests]
            [cljs.env :as env]
            [clojure.test :as test :refer [deftest is testing]]))

(deftest test-glib-module-compile
  (testing "glib modules compiled to Closure Compile expectations"
    (env/with-compiler-env (env/default-compiler-env)
      (comp-tests/compile-form-seq
        '[(ns test.foo
            (:require [goog.module.ModuleLoader :as module-loader]))
          (def EVENTS module-loader/EVENTS)]))))

(comment

  (println
    (env/with-compiler-env (env/default-compiler-env)
      (comp-tests/compile-form-seq
        '[(ns test.foo
            (:require [goog.module.ModuleLoader :as module-loader]))
          (def EVENTS module-loader/EVENTS)])))

  )
