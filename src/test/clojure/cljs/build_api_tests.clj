;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.build-api-tests
  (:refer-clojure :exclude [compile])
  (:use clojure.test)
  (:import java.io.File)
  (:require [clojure.java.io :as io]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.test-util :as test]
            [cljs.build.api :as build :refer [build]]))

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
    (build srcs opts)
    (is (not (every? #(zero? (.length %)) [common-tmp app-tmp]))
      "The files are not empty after compilation")))

(deftest cljs-1500-test-modules
  (let [out (io/file (test/tmp-dir) "cljs-1500-out")
        project (test/project-with-modules (str out))
        modules (-> project :opts :modules)]
    (test/delete-out-files out)
    (build (build/inputs (:inputs project)) (:opts project))
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
    (build (build/inputs (io/file root "foreign_libs") (io/file root "thirdparty")) opts)
    (let [foreign-lib-file (io/file out (-> opts :foreign-libs first :file))]
      (is (.exists foreign-lib-file))
      (is (= (->> (slurp (io/file out "foreign_libs" "core.js"))
                  (re-matches #"(?s).*cljs\.core\.load_file\(\"([^\"]+)\"\);.*")
                  (second))
             (str foreign-lib-file))))))

(deftest cljs-1537-circular-deps
  (let [out-file (io/file "out/main.js")]
    (.delete out-file)
    (try
      (build (build/inputs "src/test/cljs_build")
        {:main 'circular-deps.a
         :optimizations :none
         :verbose true
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
  (let [project (merge-with merge (loader-test-project "out"))
        loader (io/file "out" "cljs" "loader.js")]
    (build (build/inputs (:inputs project)) (:opts project))
    (is (.exists loader))
    (is (not (nil? (re-find #"/loader_test/foo\.js" (slurp loader))))))
  (test/delete-out-files)
  (let [project (merge-with merge (loader-test-project "out")
                  {:opts {:optimizations :advanced
                          :source-map true}})]
      (build (build/inputs (:inputs project)) (:opts project))))
