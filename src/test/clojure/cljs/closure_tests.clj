;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.closure-tests
  (:refer-clojure :exclude [compile])
  (:use cljs.closure)
  (:use clojure.test)
  (:import [com.google.javascript.jscomp JSModule])
  (:require [cljs.build.api :as build]
            [cljs.closure :as closure]
            [cljs.test-util :as test]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(deftest test-make-preamble
  (testing "no options"
    (is (= "" (make-preamble {}))))
  (testing "nodejs"
    (testing "with default hashbang"
      (is (= "#!/usr/bin/env node\n" (make-preamble {:target :nodejs}))))
    (testing "with custom hashbang"
      (is (= "#!/bin/env node\n" (make-preamble {:target :nodejs
                                                 :hashbang "/bin/env node"}))))
    (testing "with no hashbang"
      (is (= "" (make-preamble {:target :nodejs
                                :hashbang false})))
      (testing "and preamble"
        (is (= "var preamble1 = require(\"preamble1\");\n"
              (make-preamble {:target :nodejs
                              :hashbang false
                              :preamble ["cljs/preamble1.js"]})))))
    (testing "with preamble"
      (is (= "#!/usr/bin/env node\nvar preamble1 = require(\"preamble1\");\n"
             (make-preamble {:target :nodejs
                             :preamble ["cljs/preamble1.js"]})))))
  (testing "preamble"
    (is (= "var preamble1 = require(\"preamble1\");\nvar preamble2 = require(\"preamble2\");\n"
           (make-preamble {:preamble ["cljs/preamble1.js"
                                      "cljs/preamble2.js"]})))))

(deftest test-check-sourcemap
  (testing "optimizations none"
    (is (check-source-map {:source-map true :optimizations :none}))
    (is (check-source-map {:source-map false :optimizations :none}))
    (is (thrown? AssertionError (check-source-map {:source-map "target/build/app.js.map" :optimizations :none})))))

(deftest test-cljs-1882-constants-table-is-sorted
  (let [out (str (System/getProperty "java.io.tmpdir") "/cljs-1882-out")
        project (test/project-with-modules out)
        modules (-> project :opts :modules)]
    (test/clean-outputs (:opts project))
    (build/build (build/inputs (:inputs project)) (:opts project))
    (let [compiler (closure/make-closure-compiler)
          module (JSModule. "module-c")]
      (.initOptions compiler (closure/make-options (:opts project)))
      (doseq [file ["cljs/core/constants.js"
                    "module_test/modules/a.js"
                    "cljs/core.js"]]
        (.add module (closure/js-source-file nil (io/file out file))))
      (.sortInputsByDeps module compiler)
      (is (= (->> (.getInputs module)
                  (map #(string/replace (.getName %) (str out "/") "")))
             ["cljs/core.js"
              "cljs/core/constants.js"
              "module_test/modules/a.js"])))))
