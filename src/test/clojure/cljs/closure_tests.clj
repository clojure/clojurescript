;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.closure-tests
  (:refer-clojure :exclude [compile])
  (:use cljs.closure clojure.test)
  (:require [cljs.build.api :as build]
            [cljs.closure :as closure]
            [cljs.js-deps :as deps]
            [cljs.util :as util]
            [cljs.test-util :as test]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.io File]
           [com.google.javascript.jscomp JSModule]))

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
  (let [out (.getPath (io/file (test/tmp-dir) "cljs-1882-out"))
        project (test/project-with-modules out)
        modules (-> project :opts :modules)]
    (test/delete-out-files out)
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
                  (map #(string/replace (.getName %) (str out File/separator) "")))
             ["cljs/core.js"
              "cljs/core/constants.js"
              "module_test/modules/a.js"])))))

(deftest test-string-provides
  (is (= ["CB0BFFB"] (deps/-provides "var x = 42;"))))

(deftest test-lib-rel-path-cljs-2152
  (let [ijs {:provides ["tabby"]
             :url (io/as-url (io/file "src/test/cljs/js_libs/tabby.js"))
             :lib-path "src/test/cljs/js_libs"}]
    (is (= (closure/lib-rel-path ijs) "tabby.js")))
  (let [ijs {:provides ["tabby"]
             :url (io/as-url (io/file "src/test/cljs/js_libs/tabby.js"))
             :lib-path (.getAbsolutePath (io/file "src/test/cljs/js_libs/tabby.js"))}]
    (is (= (closure/lib-rel-path ijs) "tabby.js"))))

(deftest test-index-node-modules
  (test/delete-node-modules)
  (spit (io/file "package.json") "{}")
  (closure/maybe-install-node-deps! {:npm-deps {:left-pad "1.1.3"}})
  (let [modules (closure/index-node-modules-dir)]
    (is (true? (some (fn [module]
                       (= module {:module-type :commonjs
                                  :file (.getAbsolutePath (io/file "node_modules/left-pad/index.js"))
                                  :provides ["left-pad"]})) modules))))
  (test/delete-node-modules)
  (spit (io/file "package.json") "{}")
  (closure/maybe-install-node-deps! {:npm-deps {:react "15.6.1"
                                                :react-dom "15.6.1"}})
  (let [modules (closure/index-node-modules-dir)]
    (is (true? (some (fn [module]
                       (= module {:module-type :commonjs
                                  :file (.getAbsolutePath (io/file "node_modules/react/react.js"))
                                  :provides ["react"]}))
                 modules)))
    (is (true? (some (fn [module]
                       (= module {:module-type :commonjs
                                  :file (.getAbsolutePath (io/file "node_modules/react/lib/React.js"))
                                  :provides ["react/lib/React.js" "react/lib/React"]}))
                 modules)))
    (is (true? (some (fn [module]
                       (= module {:module-type :commonjs
                                  :file (.getAbsolutePath (io/file "node_modules/react-dom/server.js"))
                                  :provides ["react-dom/server.js" "react-dom/server"]}))
                 modules))))
  (test/delete-node-modules)
  (spit (io/file "package.json") "{}")
  (closure/maybe-install-node-deps! {:npm-deps {:node-fetch "1.7.1"}})
  (let [modules (closure/index-node-modules-dir)]
    (is (true? (some (fn [module]
                       (= module {:module-type :commonjs
                                  :file (.getAbsolutePath (io/file "node_modules/node-fetch/lib/index.js"))
                                  :provides ["node-fetch/lib/index.js" "node-fetch/lib/index" "node-fetch/lib"]}))
                 modules))))
  (.delete (io/file "package.json"))
  (test/delete-node-modules))
