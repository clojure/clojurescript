(ns cljs.analyzer.glib-module-test
  (:require [cljs.analyzer :as ana]
            [cljs.analyzer-tests :as ana-tests]
            [clojure.test :as test :refer [deftest is testing]]
            [cljs.env :as env]))

(deftest glib-module-detect-test
  (testing "Basic glib module detection"
    (is (= :goog (get-in @ana-tests/test-cenv [:js-dependency-index (munge "goog.module.ModuleLoader") :module])))))

(deftest glib-module-predicate-test
  (testing "glib module detection predicate"
    (env/with-compiler-env ana-tests/test-cenv
      (is (ana/goog-module-dep? 'goog.module.ModuleLoader)))))

(deftest glib-module-classification-test
  (testing "glib module classification"
    (env/with-compiler-env ana-tests/test-cenv
      (is (= :goog-module (ana/ns->module-type 'goog.module.ModuleLoader))))))

(deftest glib-module-resolve-var-test
  (testing "glib module var resolution"
    (let [cenv   (env/default-compiler-env)
          ns-ast (ana-tests/analyze-forms cenv
                   '[(ns foo.core
                       (:require [goog.module.ModuleLoader :as module-loader]))])
          aenv   (assoc (ana/empty-env) :ns (ana/get-namespace cenv 'foo.core))]
      (is (= '{:name foo.core/goog$module$goog$module$ModuleLoader.EventType
               :ns foo.core
               :op :var}
             (env/with-compiler-env cenv
               (ana/resolve-var aenv 'module-loader/EventType)))))))

(deftest glib-module-resolve-import-test
  (testing "glib module resolve import helper test"
    (let [cenv   (env/default-compiler-env)
          ns-ast (ana-tests/analyze-forms cenv
                   '[(ns foo.core
                       (:require [goog.module.ModuleLoader :as module-loader]))])
          aenv   (assoc (ana/empty-env) :ns (ana/get-namespace cenv 'foo.core))]
      (is (= 'foo.core.goog$module$goog$module$ModuleLoader
             (env/with-compiler-env cenv
               (ana/resolve-import aenv 'goog.module.ModuleLoader)))))))

(deftest glib-module-resolve-import-var-test
  (testing "glib module :import var resolution"
    (let [cenv   (env/default-compiler-env)
          ns-ast (ana-tests/analyze-forms cenv
                   '[(ns foo.core
                       (:import [goog.module ModuleLoader]))])
          aenv   (assoc (ana/empty-env) :ns (ana/get-namespace cenv 'foo.core))]
      (is (= '{:name foo.core/goog$module$goog$module$ModuleLoader
               :ns foo.core
               :op :var}
            (env/with-compiler-env cenv
              (ana/resolve-var aenv 'ModuleLoader)))))))

(comment

  (test/run-tests)

  )
