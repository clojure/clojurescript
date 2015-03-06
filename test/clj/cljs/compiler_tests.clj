(ns cljs.compiler-tests
  (:use clojure.test)
  (:require [cljs.analyzer :as a])
  (:require [cljs.compiler :as c])
  (:require [cljs.env :as e])
  (:require [cljs.util :as util]
            [cljs.compiler :as comp])
  (:import (java.io File)))

(def ns-env (assoc-in (a/empty-env) [:ns :name] 'cljs.user))

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

(deftest fn-scope-munge
  (is (= (c/munge
           (get-in
             (a/analyze ns-env
               '(defn foo []
                  (fn bar [])))
             [:init :name]))
         'cljs$user_SLASH_foo))
  (is (= (c/munge
           (get-in
             (a/analyze ns-env
               '(defn foo []
                  (fn bar [])))
             [:init :children 0 :children 0 :name]))
          'cljs$user_SLASH_foo_$_bar))
  (is (= (c/munge
           (get-in
             (a/analyze ns-env
               '(fn []
                  (fn console [])))
             [:children 0 :children 0 :name]))
         'cljs$user_SLASH_console)))

(comment
  (c/munge
    (get-in
      (a/analyze ns-env
        '(defn foo []
           (fn bar [])))
      [:init :children 0 :children 0 :name]))
  )