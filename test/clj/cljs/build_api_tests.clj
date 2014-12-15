(ns cljs.build-api-tests
  (:use cljs.build.api)
  (:use clojure.test)
  (:require
   [cljs.env :as env]
   [cljs.analyzer]))

(deftest test-target-file-for-cljs-ns
  (is (= (.getPath (target-file-for-cljs-ns nil 'example.core-lib))
         "out/example/core_lib.js"))
  (is (= (.getPath (target-file-for-cljs-ns "output" 'example.core-lib))
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
