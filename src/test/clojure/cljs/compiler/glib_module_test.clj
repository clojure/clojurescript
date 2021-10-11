(ns cljs.compiler.glib-module-test
  (:require [cljs.compiler :as comp]
            [cljs.compiler-tests :as comp-tests]
            [cljs.env :as env]
            [clojure.test :as test :refer [deftest is testing]]))

(deftest test-glib-module-compile
  (testing "glib modules compiled to Closure Compile expectations"
    (let [src (env/with-compiler-env (env/default-compiler-env)
                (comp-tests/compile-form-seq
                  '[(ns test.foo
                      (:import [goog.module ModuleLoader]))
                    (def module-loader (ModuleLoader.))]))]
      (is (re-find #"goog\.require\('goog\.module\.ModuleLoader'\)" src))
      (is (re-find #"test\.foo\.goog\$module\$goog\$module\$ModuleLoader = goog\.module\.get\('goog.module.ModuleLoader'\)" src)))))

(comment

  (test/run-tests)

  (println
    (env/with-compiler-env (env/default-compiler-env)
      (comp-tests/compile-form-seq
        '[(ns test.foo
            (:import [goog.module ModuleLoader]))
          (def module-loader (ModuleLoader.))])))

  )
