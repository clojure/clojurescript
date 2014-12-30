(ns cljs.compiler-tests
  (:use clojure.test)
  (:require [cljs.compiler :as c])
  (:require [cljs.env :as e])
  (:require [cljs.util :as util])
  (:import (java.io File)))

(deftest should-recompile
  (let [src (File. "test/hello.cljs")
        dst (File/createTempFile "compilertest" ".cljs")
        opt {:optimize-constants true}
        optmod {:optimize-constants true :elide-asserts false}]
    (with-redefs [util/*clojurescript-version* {:major 0 :minor 0 :qualifier 42}]
      (e/with-compiler-env (e/default-compiler-env)
        (.setLastModified dst (- (.lastModified src) 100))
        (is (c/requires-compilation? src dst opt))
        (c/compile-file src dst opt)
        (is (not (c/requires-compilation? src dst opt)))
        (is (c/requires-compilation? src dst optmod))
        (c/compile-file src dst optmod)
        (is (not (c/requires-compilation? src dst optmod)))))))

