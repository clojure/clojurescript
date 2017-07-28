;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.build-api-tests
  (:refer-clojure :exclude [compile])
  (:import java.io.File)
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.test-util :as test]
            [cljs.build.api :as build]
            [cljs.closure :as closure]))

(deftest test-target-file-for-cljs-ns
  (is (= (.getPath (build/target-file-for-cljs-ns 'example.core-lib nil))
         "out/example/core_lib.js"))
  (is (= (.getPath (build/target-file-for-cljs-ns 'example.core-lib "output"))
         "output/example/core_lib.js")))

(deftest test-cljs-dependents-for-macro-namespaces
  (env/with-compiler-env (env/default-compiler-env)
    (swap! env/*compiler* assoc :cljs.analyzer/namespaces
                                { 'example.core
                                 {:require-macros {'example.macros 'example.macros
                                                   'mac 'example.macros}
                                  :name 'example.core}
                                 'example.util
                                 {:require-macros {'example.macros 'example.macros
                                                   'mac 'example.macros}
                                  :name 'example.util}
                                 'example.helpers
                                 {:require-macros {'example.macros-again 'example.macros-again
                                                   'mac 'example.macros-again}
                                  :name 'example.helpers }
                                 'example.fun
                                 {:require-macros nil
                                  :name 'example.fun }})
    (is (= (set (build/cljs-dependents-for-macro-namespaces ['example.macros]))
           #{'example.core 'example.util}))
    (is (= (set (build/cljs-dependents-for-macro-namespaces ['example.macros-again]))
           #{'example.helpers}))
    (is (= (set (build/cljs-dependents-for-macro-namespaces ['example.macros 'example.macros-again]))
           #{'example.core 'example.util 'example.helpers}))
    (is (= (set (build/cljs-dependents-for-macro-namespaces ['example.not-macros]))
           #{}))))

(def test-cenv (atom {}))
(def test-env (assoc-in (ana/empty-env) [:ns :name] 'cljs.user))

;; basic

(binding [ana/*cljs-ns* 'cljs.user
          ana/*analyze-deps* false]
  (env/with-compiler-env test-cenv
    (ana/no-warn
      (ana/analyze test-env
        '(ns cljs.user
           (:use [clojure.string :only [join]]))))))

;; linear

(binding [ana/*cljs-ns* 'cljs.user
          ana/*analyze-deps* false]
  (env/with-compiler-env test-cenv
    (ana/no-warn
      (ana/analyze test-env
        '(ns foo.core)))))

(binding [ana/*cljs-ns* 'cljs.user
          ana/*analyze-deps* false]
  (env/with-compiler-env test-cenv
    (ana/no-warn
      (ana/analyze test-env
        '(ns bar.core
           (:require [foo.core :as foo]))))))

(binding [ana/*cljs-ns* 'cljs.user
          ana/*analyze-deps* false]
  (env/with-compiler-env test-cenv
    (ana/no-warn
      (ana/analyze test-env
        '(ns baz.core
           (:require [bar.core :as bar]))))))

;; graph

(binding [ana/*cljs-ns* 'cljs.user
          ana/*analyze-deps* false]
  (env/with-compiler-env test-cenv
    (ana/no-warn
      (ana/analyze test-env
        '(ns graph.foo.core)))))

(binding [ana/*cljs-ns* 'cljs.user
          ana/*analyze-deps* false]
  (env/with-compiler-env test-cenv
    (ana/no-warn
      (ana/analyze test-env
        '(ns graph.bar.core
           (:require [graph.foo.core :as foo]))))))

(binding [ana/*cljs-ns* 'cljs.user
          ana/*analyze-deps* false]
  (env/with-compiler-env test-cenv
    (ana/no-warn
      (ana/analyze test-env
        '(ns graph.baz.core
           (:require [graph.foo.core :as foo]
                     [graph.bar.core :as bar]))))))

(deftest cljs-1469
  (let [srcs "samples/hello/src"
        [common-tmp app-tmp] (mapv #(File/createTempFile  % ".js")
                                   ["common" "app"])
        opts {:optimizations :simple
              :modules {:common {:entries #{"hello.foo.bar"}
                                 :output-to (.getAbsolutePath common-tmp)}
                        :app {:entries #{"hello.core"}
                              :output-to (.getAbsolutePath app-tmp)}}}]
    (.deleteOnExit common-tmp)
    (.deleteOnExit app-tmp)
    (is (every? #(zero? (.length %)) [common-tmp app-tmp])
      "The initial files are empty")
    (build/build srcs opts)
    (is (not (every? #(zero? (.length %)) [common-tmp app-tmp]))
      "The files are not empty after compilation")))

(deftest cljs-1500-test-modules
  (let [out (io/file (test/tmp-dir) "cljs-1500-out")
        project (test/project-with-modules (str out))
        modules (-> project :opts :modules)]
    (test/delete-out-files out)
    (build/build (build/inputs (:inputs project)) (:opts project))
    (is (re-find #"Loading modules A and B" (slurp (-> modules :cljs-base :output-to))))
    (is (re-find #"Module A loaded" (slurp (-> modules :module-a :output-to))))
    (is (re-find #"Module B loaded" (slurp (-> modules :module-b :output-to))))))

(deftest cljs-1883-test-foreign-libs-use-relative-path
  (let [out (io/file (test/tmp-dir) "cljs-1883-out")
        root (io/file "src" "test" "cljs_build")
        opts {:foreign-libs
              [{:file (str (io/file root "thirdparty" "add.js"))
                :provides  ["thirdparty.add"]}]
              :output-dir (str out)
              :target :nodejs}]
    (test/delete-out-files out)
    (build/build (build/inputs (io/file root "foreign_libs") (io/file root "thirdparty")) opts)
    (let [foreign-lib-file (io/file out (-> opts :foreign-libs first :file))]
      (is (.exists foreign-lib-file))
      (is (= (->> (slurp (io/file out "foreign_libs" "core.js"))
                  (re-matches #"(?s).*cljs\.core\.load_file\(\"([^\"]+)\"\);.*")
                  (second))
             (str foreign-lib-file))))))

(deftest cljs-1537-circular-deps
  (let [out-file (io/file "out/main.js")
        root "src/test/cljs_build"]
    (.delete out-file)
    (try
      (build/build (build/inputs
                     (io/file (str root "a.cljs"))
                     (io/file (str root "b.cljs")))
        {:main 'circular-deps.a
         :optimizations :none
         :output-to "out"})
      (is false)
      (catch Throwable e
        (is true)))))

(defn loader-test-project [output-dir]
  {:inputs (str (io/file "src" "test" "cljs_build" "loader_test"))
   :opts
   {:output-dir output-dir
    :optimizations :none
    :verbose true
    :modules
    {:foo
     {:output-to (str (io/file output-dir "foo.js"))
      :entries #{'loader-test.foo}}
     :bar
     {:output-to (str (io/file output-dir "bar.js"))
      :entries #{'loader-test.bar}}}}})

(deftest cljs-2077-test-loader
  (test/delete-out-files)
  (let [{:keys [inputs opts]} (merge-with merge (loader-test-project "out"))
        loader (io/file "out" "cljs" "loader.js")]
    (build/build (build/inputs (io/file inputs "bar.cljs") (io/file inputs "foo.cljs")) opts)
    (is (.exists loader))
    (is (not (nil? (re-find #"/loader_test/foo\.js" (slurp loader))))))
  (test/delete-out-files)
  (let [project (merge-with merge (loader-test-project "out")
                  {:opts {:optimizations :advanced
                          :source-map true}})]
    (build/build (build/inputs (:inputs project)) (:opts project)))
  (testing "string inputs in modules"
    (test/delete-out-files)
    (let [project (merge-with merge (loader-test-project "out")
                    {:opts {:optimizations :whitespace}})]
      (build/build (build/inputs (:inputs project)) (:opts project)))))

(deftest test-npm-deps
  (test/delete-node-modules)
  (spit (io/file "package.json") "{}")
  (testing "simplest case, require"
    (let [out (.getPath (io/file (test/tmp-dir) "npm-deps-test-out"))
          {:keys [inputs opts]} {:inputs (str (io/file "src" "test" "cljs_build"))
                                 :opts {:main 'npm-deps-test.core
                                        :output-dir out
                                        :optimizations :none
                                        :install-deps true
                                        :npm-deps {:left-pad "1.1.3"}
                                        :closure-warnings {:check-types :off}}}
          cenv (env/default-compiler-env)]
      (test/delete-out-files out)
      (build/build (build/inputs (io/file inputs "npm_deps_test/core.cljs")) opts cenv)
      (is (.exists (io/file out "node_modules/left-pad/index.js")))
      (is (contains? (:js-module-index @cenv) "left-pad"))))
  (let [cenv (env/default-compiler-env)
        out (.getPath (io/file (test/tmp-dir) "npm-deps-test-out"))
        {:keys [inputs opts]} {:inputs (str (io/file "src" "test" "cljs_build"))
                               :opts {:main 'npm-deps-test.string-requires
                                      :output-dir out
                                      :optimizations :none
                                      :install-deps true
                                      :npm-deps {:react "15.6.1"
                                                 :react-dom "15.6.1"
                                                 :lodash "4.17.4"}
                                      :closure-warnings {:check-types :off
                                                         :non-standard-jsdoc :off}}}]
    (testing "mix of symbol & string-based requires"
      (test/delete-out-files out)
      (test/delete-node-modules)
      (build/build (build/inputs (io/file inputs "npm_deps_test/string_requires.cljs")) opts cenv)
      (is (.exists (io/file out "node_modules/react/react.js")))
      (is (contains? (:js-module-index @cenv) "react"))
      (is (contains? (:js-module-index @cenv) "react-dom/server")))
    (testing "builds with string requires are idempotent"
      (build/build (build/inputs (io/file inputs "npm_deps_test/string_requires.cljs")) opts cenv)
      (is (not (nil? (re-find #"\.\./node_modules/react-dom/server\.js" (slurp (io/file out "cljs_deps.js"))))))
      (test/delete-out-files out)))
  (.delete (io/file "package.json"))
  (test/delete-node-modules))

(deftest test-preloads
  (let [out (.getPath (io/file (test/tmp-dir) "preloads-test-out"))
        {:keys [inputs opts]} {:inputs (str (io/file "src" "test" "cljs"))
                               :opts {:main 'preloads-test.core
                                      :preloads '[preloads-test.preload]
                                      :output-dir out
                                      :optimizations :none
                                      :closure-warnings {:check-types :off}}}
        cenv (env/default-compiler-env)]
    (test/delete-out-files out)
    (build/build (build/inputs
                   (io/file inputs "preloads_test/core.cljs"))
                 opts cenv)
    (is (.exists (io/file out "preloads_test/preload.cljs")))
    (is (contains? (get-in @cenv [::ana/namespaces 'preloads-test.preload :defs]) 'preload-var))))

(deftest test-libs-cljs-2152
  (let [out (.getPath (io/file (test/tmp-dir) "libs-test-out"))
        {:keys [inputs opts]} {:inputs (str (io/file "src" "test" "cljs_build"))
                               :opts {:main 'libs-test.core
                                      :output-dir out
                                      :libs ["src/test/cljs/js_libs"]
                                      :optimizations :none
                                      :closure-warnings {:check-types :off}}}
        cenv (env/default-compiler-env)]
    (test/delete-out-files out)
    (build/build (build/inputs
                   (io/file "src/test/cljs_build/libs_test/core.cljs") (io/file "src/test/cljs/js_libs")
                   (io/file inputs "libs_test/core.cljs")
                   (io/file "src/test/cljs/js_libs"))
      opts cenv)
    (is (.exists (io/file out "tabby.js")))))

(defn collecting-warning-handler [state]
  (fn [warning-type env extra]
    (when (warning-type ana/*cljs-warnings*)
      (when-let [s (ana/error-message warning-type extra)]
        (swap! state conj s)))))

(deftest test-emit-node-requires-cljs-2213
  (test/delete-node-modules)
  (spit (io/file "package.json") "{}")
  (testing "simplest case, require"
    (let [ws (atom [])
          out (.getPath (io/file (test/tmp-dir) "emit-node-requires-test-out"))
          {:keys [inputs opts]} {:inputs (str (io/file "src" "test" "cljs_build"))
                                 :opts {:main 'emit-node-requires-test.core
                                        :output-dir out
                                        :optimizations :none
                                        :target :nodejs
                                        :install-deps true
                                        :npm-deps {:react "15.6.1"
                                                   :react-dom "15.6.1"}
                                        :closure-warnings {:check-types :off
                                                           :non-standard-jsdoc :off}}}
          cenv (env/default-compiler-env opts)]
      (test/delete-out-files out)
      (ana/with-warning-handlers [(collecting-warning-handler ws)]
        (build/build (build/inputs (io/file inputs "emit_node_requires_test/core.cljs")) opts cenv))
      ;; wasn't processed by Closure
      (is (not (.exists (io/file out "node_modules/react/react.js"))))
      (is (.exists (io/file out "emit_node_requires_test/core.js")))
      (is (true? (boolean (re-find #"emit_node_requires_test\.core\.node\$module\$react_dom\$server = require\('react-dom/server'\);"
                            (slurp (io/file out "emit_node_requires_test/core.js"))))))
      (is (true? (boolean (re-find #"emit_node_requires_test\.core\.node\$module\$react_dom\$server\.renderToString"
                            (slurp (io/file out "emit_node_requires_test/core.js"))))))
      (is (empty? @ws))))
  (testing "Node native modules, CLJS-2218"
    (let [ws (atom [])
          out (.getPath (io/file (test/tmp-dir) "emit-node-requires-test-out"))
          {:keys [inputs opts]} {:inputs (str (io/file "src" "test" "cljs_build"))
                                 :opts {:main 'emit-node-requires-test.native-modules
                                        :output-dir out
                                        :optimizations :none
                                        :target :nodejs
                                        :closure-warnings {:check-types :off}}}
          cenv (env/default-compiler-env opts)]
      (test/delete-out-files out)
      (test/delete-node-modules)
      (ana/with-warning-handlers [(collecting-warning-handler ws)]
        (build/build (build/inputs (io/file inputs "emit_node_requires_test/native_modules.cljs")) opts cenv))
      (is (.exists (io/file out "emit_node_requires_test/native_modules.js")))
      (is (true? (boolean (re-find #"emit_node_requires_test\.native_modules\.node\$module\$path\.isAbsolute"
                            (slurp (io/file out "emit_node_requires_test/native_modules.js"))))))
      (is (empty? @ws))))
  (.delete (io/file "package.json"))
  (test/delete-node-modules))

(deftest test-emit-global-requires-cljs-2214
  (testing "simplest case, require"
    (let [ws (atom [])
          out (.getPath (io/file (test/tmp-dir) "emit-global-requires-test-out"))
          {:keys [inputs opts]} {:inputs (str (io/file "src" "test" "cljs_build"))
                                 :opts {:main 'emit-node-requires-test.core
                                        :output-dir out
                                        :optimizations :none
                                        ;; Doesn't matter what :file is used here, as long at it exists
                                        :foreign-libs [{:file "src/test/cljs_build/thirdparty/add.js"
                                                        :provides ["react"]
                                                        :global-exports '{react React}}
                                                       {:file "src/test/cljs_build/thirdparty/add.js"
                                                        :provides ["react-dom"]
                                                        :requires ["react"]
                                                        :global-exports '{react-dom ReactDOM}}
                                                       {:file "src/test/cljs_build/thirdparty/add.js"
                                                        :provides ["react-dom/server"]
                                                        :requires ["react-dom"]
                                                        :global-exports '{react-dom/server ReactDOMServer}}]}}
          cenv (env/default-compiler-env)]
      (test/delete-out-files out)
      (ana/with-warning-handlers [(collecting-warning-handler ws)]
        (build/build (build/inputs (io/file inputs "emit_global_requires_test/core.cljs")) opts cenv))
      (is (.exists (io/file out "emit_global_requires_test/core.js")))
      (is (true? (boolean (re-find #"emit_global_requires_test\.core\.global\$module\$react_dom\$server = goog\.global\.ReactDOMServer;"
                            (slurp (io/file out "emit_global_requires_test/core.js"))))))
      (is (true? (boolean (re-find #"emit_global_requires_test\.core\.global\$module\$react_dom\$server\.renderToString"
                            (slurp (io/file out "emit_global_requires_test/core.js"))))))
      (is (empty? @ws)))))

(deftest test-data-readers
  (let [out (.getPath (io/file (test/tmp-dir) "data-readers-test-out"))
        {:keys [inputs opts]} {:inputs (str (io/file "src" "test" "cljs"))
                               :opts {:main 'data-readers-test.core
                                      :output-dir out
                                      :optimizations :none
                                      :closure-warnings {:check-types :off}}}
        cenv (env/default-compiler-env)]
    (test/delete-out-files out)
    (build/build (build/inputs (io/file inputs "data_readers_test")) opts cenv)
    (is (contains? (-> @cenv ::ana/data-readers) 'test/custom-identity))))

(deftest test-cljs-2249
  (let [out (io/file (test/tmp-dir) "cljs-2249-out")
        root (io/file "src" "test" "cljs_build")
        opts {:output-dir (str out)
              :target :nodejs}]
    (test/delete-out-files out)
    (build/build (build/inputs (io/file root "foreign_libs_cljs_2249")) opts)
    (is (.exists (io/file out "calculator_global.js")))
    (test/delete-out-files out)
    (closure/build (build/inputs (io/file root "foreign_libs_cljs_2249")) opts)
    (is (.exists (io/file out "calculator_global.js")))))

(deftest test-node-modules-cljs-2246
  (test/delete-node-modules)
  (.delete (io/file "package-lock.json"))
  (spit (io/file "package.json") (json/json-str {:dependencies {:left-pad "1.1.3"}
                                                 :devDependencies {:module-deps "*"
                                                                   :resolve "*"
                                                                   :browser-resolve "*"}}))
  (sh/sh "npm" "install")
  (let [ws (atom [])
        out (.getPath (io/file (test/tmp-dir) "node-modules-opt-test-out"))
        {:keys [inputs opts]} {:inputs (str (io/file "src" "test" "cljs_build"))
                               :opts {:main 'node-modules-opt-test.core
                                      :output-dir out
                                      :optimizations :none
                                      :closure-warnings {:check-types :off}}}
        cenv (env/default-compiler-env opts)]
    (test/delete-out-files out)
    (ana/with-warning-handlers [(collecting-warning-handler ws)]
      (build/build (build/inputs (io/file inputs "node_modules_opt_test/core.cljs")) opts cenv))
    (is (.exists (io/file out "node_modules/left-pad/index.js")))
    (is (contains? (:js-module-index @cenv) "left-pad"))
    (is (empty? @ws)))
  (.delete (io/file "package.json"))
  (.delete (io/file "package-lock.json"))
  (test/delete-node-modules))

(deftest test-deps-api-cljs-2255
  (test/delete-node-modules)
  (spit (io/file "package.json") "{}")
  (build/install-node-deps! {:left-pad "1.1.3"})
  (is (.exists (io/file "node_modules/left-pad/package.json")))
  (test/delete-node-modules)
  (spit (io/file "package.json") "{}")
  (build/install-node-deps! {:react "15.6.1"
                             :react-dom "15.6.1"})
  (let [modules (build/get-node-deps '[react "react-dom/server"])]
    (is (true? (some (fn [module]
                       (= module {:module-type :es6
                                  :file (.getAbsolutePath (io/file "node_modules/react/react.js"))
                                  :provides ["react"
                                             "react/react.js"
                                             "react/react"]}))
                 modules)))
    (is (true? (some (fn [module]
                       (= module {:module-type :es6
                                  :file (.getAbsolutePath (io/file "node_modules/react/lib/React.js"))
                                  :provides ["react/lib/React.js" "react/lib/React"]}))
                 modules)))
    (is (true? (some (fn [module]
                       (= module {:module-type :es6
                                  :file (.getAbsolutePath (io/file "node_modules/react-dom/server.js"))
                                  :provides ["react-dom/server.js" "react-dom/server"]}))
                 modules))))
  (test/delete-node-modules)
  (.delete (io/file "package.json")))
