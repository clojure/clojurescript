(ns cljs.compiler.glib-module-test
  (:require [cljs.compiler-tests :as comp-tests]
            [cljs.env :as env]
            [clojure.test :as test :refer [deftest is testing]]))

(deftest test-glib-module-compile
  (testing "glib modules compiled to Closure Compile expectations"
    (let [src (env/with-compiler-env (env/default-compiler-env )
                (comp-tests/compile-form-seq
                  '[(ns test.foo
                      (:import [goog.module ModuleLoader]))
                    (def module-loader (ModuleLoader.))]))]
      (is (re-find #"goog\.require\('goog\.module\.ModuleLoader'\)" src))
      (is (re-find #"test\.foo\.goog\$module\$goog\$module\$ModuleLoader = goog\.module\.get\('goog.module.ModuleLoader'\)" src))
      (is (re-find #"test\.foo\.module_loader = \(new test\.foo\.goog\$module\$goog\$module\$ModuleLoader\(\)\)" src)))))

(deftest cljs-3330-global-goog-object&array
  (testing "migration path for goog.module impact on goog.object & goog.array"
    (let [src (env/with-compiler-env
                (env/default-compiler-env {:global-goog-object&array true})
                (comp-tests/compile-form-seq
                  '[(ns test.foo
                      (:require [goog.object :as gobj]
                                [goog.array :as garray]))
                    (def module-loader (ModuleLoader.))]))]
      (is (re-find #"goog\.require\('goog\.object\'\)" src))
      (is (re-find #"goog\.require\('goog\.array\'\)" src)))))

(comment

  (test/run-tests)

  )
