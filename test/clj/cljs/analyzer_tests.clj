(ns cljs.analyzer-tests
  (:require [clojure.java.io :as io]
            [cljs.analyzer :as a]
            [cljs.env :as e]
            [cljs.env :as env]
            [cljs.analyzer.api :as ana-api])
  (:use clojure.test))

;;******************************************************************************
;;  cljs-warnings tests
;;******************************************************************************

(def warning-forms
  {:undeclared-var (let [v (gensym)] `(~v 1 2 3))
   :fn-arity '(do (defn x [a b] (+ a b))
                  (x 1 2 3 4))})

(defn warn-count [form]
  (let [counter (atom 0)
        tracker (fn [warning-type env & [extra]]
                  (when (warning-type a/*cljs-warnings*)
                    (swap! counter inc)))]
    (a/with-warning-handlers [tracker]
      (a/analyze (a/empty-env) form))
    @counter))

(deftest no-warn
  (is (every? zero? (map (fn [[name form]] (a/no-warn (warn-count form))) warning-forms))))

(deftest all-warn
  (is (every? #(= 1 %) (map (fn [[name form]] (a/all-warn (warn-count form))) warning-forms))))

;; =============================================================================
;; NS parsing

(def ns-env (assoc-in (a/empty-env) [:ns [:name]] 'cljs.user))

(deftest spec-validation
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require {:foo :bar})))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns & options] and lib.ns specs supported in :require / :require-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [:foo :bar])))
          (catch Exception e
            (.getMessage e)))
        "Library name must be specified as a symbol in :require / :require-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [baz.woz :as woz :refer [] :plop])))
          (catch Exception e
            (.getMessage e)))
        "Only :as alias and :refer (names) options supported in :require"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [baz.woz :as woz :refer [] :plop true])))
          (catch Exception e
            (.getMessage e)))
        "Only :as and :refer options supported in :require / :require-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [baz.woz :as woz :refer [] :as boz :refer []])))
          (catch Exception e
            (.getMessage e)))
        "Each of :as and :refer options may only be specified once in :require / :require-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:refer-clojure :refer [])))
          (catch Exception e
            (.getMessage e)))
        "Only [:refer-clojure :exclude (names)] form supported"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz :exclude []])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [baz.woz :as []])))
          (catch Exception e
            (.getMessage e)))
        ":as must be followed by a symbol in :require / :require-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [baz.woz :as woz] [noz.goz :as woz])))
          (catch Exception e
            (.getMessage e)))
        ":as alias must be unique"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:unless [])))
          (catch Exception e
            (.getMessage e)))
        "Only :refer-clojure, :require, :require-macros, :use and :use-macros libspecs supported"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require baz.woz) (:require noz.goz)))
          (catch Exception e
            (.getMessage e)))
        "Only one ")))

;; =============================================================================
;; Inference tests

(def test-cenv (atom {}))
(def test-env (assoc-in (a/empty-env) [:ns :name] 'cljs.core))

(a/no-warn
  (e/with-compiler-env test-cenv
    (binding [a/*analyze-deps* false]
      (a/analyze-file (io/file "src/cljs/cljs/core.cljs")))))

(deftest basic-inference
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '1)))
         'number))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '"foo")))
         'string))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(make-array 10))))
         'array))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(js-obj))))
         'object))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '[])))
         'cljs.core/IVector))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '{})))
         'cljs.core/IMap))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '#{})))
         'cljs.core/ISet))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env ())))
         'cljs.core/IList))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(fn [x] x))))
         'function)))

(deftest if-inference
  (is (= (a/no-warn
           (e/with-compiler-env test-cenv
             (:tag (a/analyze test-env '(if x "foo" 1)))))
         '#{number string})))

(deftest method-inference
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(.foo js/bar))))
         'any)))

(deftest fn-inference
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env
                   '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
                      (x :one)))))
        'number))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env
                   '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
                      (x :one :two)))))
        'string))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env
                   '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
                      (x :one :two :three)))))
        'cljs.core/IList)))

(deftest lib-inference
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(+ 1 2))))
         'number))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(alength (array)))))
         'number))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(aclone (array)))))
         'array))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(count [1 2 3]))))
         'number))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(into-array [1 2 3]))))
         'array))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(js-obj))))
         'object))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(-conj [] 1))))
         'clj))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(conj [] 1))))
         'clj))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(assoc nil :foo :bar))))
         'clj))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(dissoc {:foo :bar} :foo))))
         '#{clj clj-nil})))

(deftest test-always-true-if
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(if 1 2 "foo"))))
         'number)))

;; will only work if the previous test works
(deftest test-count
  (is (= (cljs.env/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(count []))))
         'number)))

(deftest test-numeric
  (is (= (a/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (a/analyze test-env '(dec x)))))
         'number))
  (is (= (a/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (a/analyze test-env '(int x)))))
         'number))
  (is (= (a/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (a/analyze test-env '(unchecked-int x)))))
         'number))
  (is (= (a/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (a/analyze test-env '(mod x y)))))
         'number))
  (is (= (a/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (a/analyze test-env '(quot x y)))))
         'number))
  (is (= (a/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (a/analyze test-env '(rem x y)))))
         'number))
  (is (= (a/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (a/analyze test-env '(bit-count n)))))
         'number)))

;; =============================================================================
;; Catching errors during macroexpansion

(deftest test-defn-error
  (is (.startsWith
        (try
          (a/analyze test-env '(defn foo 123))
          (catch Exception e
            (.getMessage e)))
        "Parameter declaration 123 should be a vector at line")))

;; =============================================================================
;; ns desugaring

(deftest test-cljs-975
  (let [spec '((:require [bar :refer [baz] :refer-macros [quux]] :reload))]
    (is (= (a/desugar-ns-specs spec)
           '((:require-macros (bar :refer [quux]) :reload)
             (:require (bar :refer [baz]) :reload))))))

;; =============================================================================
;; Namespace metadata

(deftest test-namespace-metadata
  (binding [a/*cljs-ns* a/*cljs-ns*]
    (is (= (do (a/analyze ns-env '(ns weeble {:foo bar}))
               (meta a/*cljs-ns*))
           {:foo 'bar}))

    (is (= (do (a/analyze ns-env '(ns ^{:foo bar} weeble))
               (meta a/*cljs-ns*))
           {:foo 'bar}))

    (is (= (do (a/analyze ns-env '(ns ^{:foo bar} weeble {:baz quux}))
               (meta a/*cljs-ns*))
           {:foo 'bar :baz 'quux}))

    (is (= (do (a/analyze ns-env '(ns ^{:foo bar} weeble {:foo baz}))
               (meta a/*cljs-ns*))
           {:foo 'baz}))

    (is (= (meta (:name (a/analyze ns-env '(ns weeble {:foo bar}))))
           {:foo 'bar}))

    (is (= (meta (:name (a/analyze ns-env '(ns ^{:foo bar} weeble))))
           {:foo 'bar}))

    (is (= (meta (:name (a/analyze ns-env '(ns ^{:foo bar} weeble {:baz quux}))))
           {:foo 'bar :baz 'quux}))

    (is (= (meta (:name (a/analyze ns-env '(ns ^{:foo bar} weeble {:foo baz}))))
           {:foo 'baz}))))

(def test-deps-env (atom
                    {:cljs.analyzer/namespaces
                     {'file-reloading {:requires {'utils 'utils}}
                      'core           {:requires {'file-reloading 'file-reloading}}
                      'dev            {:requires {'client 'client
                                                  'core 'core}}
                      'client         {:requires {'utils 'utils}}
                      'utils          {:requires {}}}}))

(deftest test-ns-dependents
  (binding [env/*compiler* test-deps-env]
    (is (= (set (a/ns-dependents 'client))
           #{'dev}))
    (is (= (set (a/ns-dependents 'core))
           #{'dev}))
    (is (= (set (a/ns-dependents 'dev))
           #{}))

    ;; if 'file-reloading returns 'core
    (is (= (set (a/ns-dependents 'file-reloading))
           #{'dev 'core}))

    ;; how can 'utils include 'file-reloading but not 'core
    ;; FAILS with
    ;; actual: (not (= #{file-reloading dev client} #{file-reloading dev client core}))
    (is (= (set (a/ns-dependents 'utils))
           #{'file-reloading 'dev 'client 'core}))))
