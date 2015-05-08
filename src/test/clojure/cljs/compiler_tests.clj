(ns cljs.compiler-tests
  (:use clojure.test)
  (:require [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.env :as env]
            [cljs.util :as util])
  (:import [java.io File]))

(def aenv (assoc-in (ana/empty-env) [:ns :name] 'cljs.user))
(def cenv (env/default-compiler-env))

#_(deftest should-recompile
  (let [src (File. "test/hello.cljs")
        dst (File/createTempFile "compilertest" ".cljs")
        opt {:optimize-constants true}
        optmod {:optimize-constants true :elide-asserts false}]
    (with-redefs [util/*clojurescript-version* {:major 0 :minor 0 :qualifier 42}]
      (env/with-compiler-env (env/default-compiler-env)
        (.setLastModified dst (- (.lastModified src) 100))
        (is (comp/requires-compilation? src dst opt))
        (comp/compile-file src dst opt)
        (is (not (comp/requires-compilation? src dst opt)))
        (is (comp/requires-compilation? src dst optmod))
        (comp/compile-file src dst optmod)
        (is (not (comp/requires-compilation? src dst optmod)))))))

(deftest fn-scope-munge
  (is (= (comp/munge
           (get-in
             (ana/analyze aenv
               '(defn foo []
                  (fn bar [])))
             [:init :name]))
         'cljs$user$foo))
  (is (= (comp/munge
           (get-in
             (ana/analyze aenv
               '(defn foo []
                  (fn bar [])))
             [:init :children 0 :children 0 :name]))
          'cljs$user$foo_$_bar))
  (is (= (comp/munge
           (get-in
             (ana/analyze aenv
               '(fn []
                  (fn console [])))
             [:children 0 :children 0 :name]))
         'cljs$user$console)))

(deftest test-js-negative-infinity
  (= (with-out-str
       (comp/emit
         (ana/analyze (assoc aenv :context :expr) 'js/-Infinity)))
     "-Infinity"))

(comment
  (env/with-compiler-env cenv
    (comp/emit
      (ana/analyze aenv
        '(defn foo ([a]) ([a b])))))
  )

;; CLJS-1225

(comment
  (binding [ana/*cljs-static-fns* true]
    (env/with-compiler-env cenv
      (comp/emit
        (ana/analyze aenv
          '(defn incme []
             (let [incme (fn [a queue & args])]
               (println (incme 1 [1] 1 1))))))))
  )