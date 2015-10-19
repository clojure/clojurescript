(ns cljs.build-api-tests
  (:refer-clojure :exclude [compile])
  (:use cljs.build.api)
  (:use clojure.test)
  (:import java.io.File)
  (:require [clojure.java.io :as io]
            [cljs.env :as env]
            [cljs.analyzer :as ana]))

(deftest test-target-file-for-cljs-ns
  (is (= (.getPath (target-file-for-cljs-ns 'example.core-lib nil))
         "out/example/core_lib.js"))
  (is (= (.getPath (target-file-for-cljs-ns 'example.core-lib "output"))
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
    (is (= (set (cljs-dependents-for-macro-namespaces ['example.macros]))
           #{'example.core 'example.util}))
    (is (= (set (cljs-dependents-for-macro-namespaces ['example.macros-again]))
           #{'example.helpers}))
    (is (= (set (cljs-dependents-for-macro-namespaces ['example.macros 'example.macros-again]))
           #{'example.core 'example.util 'example.helpers}))
    (is (= (set (cljs-dependents-for-macro-namespaces ['example.not-macros]))
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

(deftest test-cljs-ns-dependencies
  (is (= (env/with-compiler-env test-cenv
           (cljs-ns-dependents 'clojure.string))
        '(cljs.user)))
  (is (= (env/with-compiler-env test-cenv
           (cljs-ns-dependents 'foo.core))
        '(bar.core baz.core)))
  (is (= (env/with-compiler-env test-cenv
           (cljs-ns-dependents 'graph.foo.core))
        '(graph.bar.core graph.baz.core)))
  (is (= (cljs-ns-dependents test-cenv 'graph.foo.core)
        '(graph.bar.core graph.baz.core))))

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
