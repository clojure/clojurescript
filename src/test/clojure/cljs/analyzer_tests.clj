;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer-tests
  (:require [clojure.java.io :as io]
            [cljs.util :as util]
            [cljs.env :as e]
            [cljs.env :as env]
            [cljs.analyzer :as a]
            [cljs.analyzer.api :as ana-api]
            [cljs.compiler :as comp]
            [cljs.closure :as closure]
            [cljs.externs :as externs]
            [cljs.analyzer :as ana]
            [clojure.string :as string]
            [cljs.test-util :refer [unsplit-lines]])
  (:use clojure.test))

(defn collecting-warning-handler [state]
  (fn [warning-type env extra]
    (when (warning-type a/*cljs-warnings*)
      (when-let [s (a/error-message warning-type extra)]
        (swap! state conj s)))))

;;******************************************************************************
;;  cljs-warnings tests
;;******************************************************************************

(def warning-forms
  {:undeclared-var (let [v (gensym)] `(~v 1 2 3))
   :fn-arity '(do (defn x [a b] (+ a b))
                  (x 1 2 3 4))
   :keyword-arity '(do (:argumentless-keyword-invocation))})

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

(def ns-env (assoc-in (a/empty-env) [:ns :name] 'cljs.user))

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
        "Only :as alias, :refer (names) and :rename {from to} options supported in :require"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:require [baz.woz :as woz :refer [] :plop true])))
          (catch Exception e
            (.getMessage e)))
        "Only :as, :refer and :rename options supported in :require / :require-macros"))
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
        "Only [:refer-clojure :exclude (names)] and optionally `:rename {from to}` specs supported"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:refer-clojure :rename [1 2])))
          (catch Exception e
            (.getMessage e)))
        "Only [:refer-clojure :exclude (names)] and optionally `:rename {from to}` specs supported"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz :exclude []])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz :only])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz :only [1 2 3]])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz :rename [1 2]])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [foo.bar :rename {baz qux}])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:use [baz.woz :only [foo] :only [bar]])))
          (catch Exception e
            (.getMessage e)))
        "Each of :only and :rename options may only be specified once in :use / :use-macros"))
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
          (a/analyze ns-env '(ns foo.bar (:require [foo.bar :rename {baz qux}])))
          (catch Exception e
            (.getMessage e)))
        "Renamed symbol baz not referred"))
  (is (.startsWith
        (try
          (a/analyze ns-env '(ns foo.bar (:unless [])))
          (catch Exception e
            (.getMessage e)))
        "Only :refer-clojure, :require, :require-macros, :use, :use-macros, and :import libspecs supported. Got (:unless []) instead."))
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
      (a/analyze-file (io/file "src/main/cljs/cljs/core.cljs")))))

(deftest basic-inference
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '1)))
         'number))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '"foo")))
         'string))
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '\a)))
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
         'js)))

(deftest fn-inference
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env
  ;                 '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
  ;                    (x :one)))))
  ;      'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env
  ;                 '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
  ;                    (x :one :two)))))
  ;      'string))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env
  ;                 '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
  ;                    (x :one :two :three)))))
  ;      'cljs.core/IList))
  )

(deftest lib-inference
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(+ 1 2))))
         'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(alength (array)))))
  ;       'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(aclone (array)))))
  ;       'array))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(-count [1 2 3]))))
  ;      'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(count [1 2 3]))))
  ;       'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(into-array [1 2 3]))))
  ;       'array))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(js-obj))))
  ;       'object))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(-conj [] 1))))
  ;       'clj))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(conj [] 1))))
  ;       'clj))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(assoc nil :foo :bar))))
  ;       'clj))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(dissoc {:foo :bar} :foo))))
  ;       '#{clj clj-nil}))
  )

(deftest test-always-true-if
  (is (= (e/with-compiler-env test-cenv
           (:tag (a/analyze test-env '(if 1 2 "foo"))))
         'number)))

;; will only work if the previous test works
(deftest test-count
  ;(is (= (cljs.env/with-compiler-env test-cenv
  ;         (:tag (a/analyze test-env '(count []))))
  ;       'number))
  )

(deftest test-numeric
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(dec x)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(int x)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(unchecked-int x)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(mod x y)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(quot x y)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(rem x y)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (a/analyze test-env '(bit-count n)))))
  ;       'number))
  )

;; =============================================================================
;; Catching errors during macroexpansion

(deftest test-defn-error
  (is (.startsWith
        (try
          (a/analyze test-env '(defn foo 123))
          (catch Exception e
            (.getMessage e)))
        "Parameter declaration \"123\" should be a vector")))

;; =============================================================================
;; ns desugaring

(deftest test-cljs-975
  (let [spec '((:require [bar :refer [baz] :refer-macros [quux]] :reload))]
    (is (= (set (a/desugar-ns-specs spec))
           (set '((:require-macros (bar :refer [quux]) :reload)
                  (:require (bar :refer [baz]) :reload)))))))

(deftest test-rewrite-cljs-aliases
  (is (= (a/rewrite-cljs-aliases
           '((:require-macros (bar :refer [quux]) :reload)
             (:require (clojure.spec.alpha :as s :refer [fdef]) :reload)))
         '((:require-macros (bar :refer [quux]) :reload)
           (:require (cljs.spec.alpha :as s :refer [fdef])
                     (cljs.spec.alpha :as clojure.spec.alpha) :reload))))
  (is (= (a/rewrite-cljs-aliases
           '((:refer-clojure :exclude [first])
              (:require-macros (bar :refer [quux]) :reload)
              (:require (clojure.spec.alpha :as s) :reload)))
         '((:refer-clojure :exclude [first])
           (:require-macros (bar :refer [quux]) :reload)
           (:require (cljs.spec.alpha :as s) (cljs.spec.alpha :as clojure.spec.alpha) :reload))))
  (is (= (a/rewrite-cljs-aliases
           '((:require-macros (bar :refer [quux]) :reload)
             (:require clojure.spec.alpha :reload)))
         '((:require-macros (bar :refer [quux]) :reload)
           (:require (cljs.spec.alpha :as clojure.spec.alpha) :reload)))))

;; =============================================================================
;; Namespace metadata

(deftest test-namespace-metadata
  (binding [a/*cljs-ns* a/*cljs-ns*]
    (is (= (do (a/analyze ns-env '(ns weeble.ns {:foo bar}))
               (meta a/*cljs-ns*))
           {:foo 'bar}))

    (is (= (do (a/analyze ns-env '(ns ^{:foo bar} weeble.ns))
               (meta a/*cljs-ns*))
           {:foo 'bar}))

    (is (= (do (a/analyze ns-env '(ns ^{:foo bar} weeble.ns {:baz quux}))
               (meta a/*cljs-ns*))
           {:foo 'bar :baz 'quux}))

    (is (= (do (a/analyze ns-env '(ns ^{:foo bar} weeble.ns {:foo baz}))
               (meta a/*cljs-ns*))
           {:foo 'baz}))

    (is (= (meta (:name (a/analyze ns-env '(ns weeble.ns {:foo bar}))))
           {:foo 'bar}))

    (is (= (meta (:name (a/analyze ns-env '(ns ^{:foo bar} weeble.ns))))
           {:foo 'bar}))

    (is (= (meta (:name (a/analyze ns-env '(ns ^{:foo bar} weeble.ns {:baz quux}))))
           {:foo 'bar :baz 'quux}))

    (is (= (meta (:name (a/analyze ns-env '(ns ^{:foo bar} weeble.ns {:foo baz}))))
           {:foo 'baz}))))

(deftest test-cljs-1105
  ;; munge turns - into _, must preserve the dash first
  (is (not= (a/gen-constant-id :test-kw)
            (a/gen-constant-id :test_kw))))

(deftest test-symbols-munge-cljs-1432
  (is (not= (a/gen-constant-id :$)
            (a/gen-constant-id :.)))
  (is (not= (a/gen-constant-id '$)
            (a/gen-constant-id '.))))

(deftest test-unicode-munging-cljs-1457
  (is (= (a/gen-constant-id :C♯) 'cst$kw$C_u266f_)
      (= (a/gen-constant-id 'C♯) 'cst$sym$C_u266f_)))

;; Constants

(deftest test-constants
 (is (.startsWith
        (try
          (a/analyze test-env '(do (def ^:const foo 123)  (def foo 246)))
          (catch Exception e
            (.getMessage e)))
        "Can't redefine a constant"))
  (is (.startsWith
        (try
          (a/analyze test-env '(do (def ^:const foo 123)  (set! foo 246)))
          (catch Exception e
            (.getMessage e)))
        "Can't set! a constant")))

(deftest test-cljs-1508-rename
  (binding [a/*cljs-ns* a/*cljs-ns*]
    (let [parsed-ns (e/with-compiler-env test-cenv
                      (a/analyze test-env
                        '(ns foo.core
                           (:require [clojure.set :as set :refer [intersection] :rename {intersection foo}]))))]
      (is (nil? (-> parsed-ns :uses (get 'foo))))
      (is (nil? (-> parsed-ns :uses (get 'intersection))))
      (is (some? (-> parsed-ns :renames (get 'foo))))
      (is (= (-> parsed-ns :renames (get 'foo))
             'clojure.set/intersection)))
    (is (e/with-compiler-env test-cenv
          (a/analyze test-env
            '(ns foo.core
               (:use [clojure.set :only [intersection] :rename {intersection foo}])))))
    (is (= (e/with-compiler-env (atom {::a/namespaces
                                       {'foo.core {:renames '{foo clojure.set/intersection}}}})
             (a/resolve-var {:ns {:name 'foo.core}} 'foo))
            '{:name clojure.set/intersection
              :ns   clojure.set}))
    (let [rwhen (e/with-compiler-env (atom (update-in @test-cenv [::a/namespaces]
                                             merge {'foo.core {:rename-macros '{always cljs.core/when}}}))
                  (a/resolve-macro-var {:ns {:name 'foo.core}} 'always))]
      (is (= (-> rwhen :name)
             'cljs.core/when)))
    (let [parsed-ns (e/with-compiler-env test-cenv
                      (a/analyze test-env
                        '(ns foo.core
                           (:refer-clojure :rename {when always
                                                    map  core-map}))))]
      (is (= (-> parsed-ns :excludes) #{}))
      (is (= (-> parsed-ns :rename-macros) '{always cljs.core/when}))
      (is (= (-> parsed-ns :renames) '{core-map cljs.core/map})))
    (is (thrown? Exception (e/with-compiler-env test-cenv
                             (a/analyze test-env
                               '(ns foo.core
                                  (:require [clojure.set :rename {intersection foo}]))))))))

(deftest test-cljs-1274
  (let [test-env (assoc-in (a/empty-env) [:ns :name] 'cljs.user)]
    (binding [a/*cljs-ns* a/*cljs-ns*]
      (is (thrown-with-msg? Exception #"Can't def ns-qualified name in namespace foo.core"
            (a/analyze test-env '(def foo.core/foo 43))))
      (is (a/analyze test-env '(def cljs.user/foo 43))))))

(deftest test-cljs-1763
  (let [parsed (a/parse-ns-excludes {} '())]
    (is (= parsed
           {:excludes #{}
            :renames {}}))
    (is (set? (:excludes parsed)))))

(deftest test-cljs-1785-js-shadowed-by-local
  (let [ws (atom [])]
    (a/with-warning-handlers [(collecting-warning-handler ws)]
      (a/analyze ns-env
        '(fn [foo]
           (let [x js/foo]
             (println x)))))
    (is (.startsWith (first @ws) "js/foo is shadowed by a local"))))

(deftest test-cljs-2005
  (let [ws (atom [])]
    (try
      (a/with-warning-handlers [(collecting-warning-handler ws)]
        (a/analyze (a/empty-env)
          '(defn myfun
             ([x] x)
             ([x] x))))
      (catch Exception _))
    (is (.startsWith (first @ws) "myfun: Can't have 2 overloads with same arity"))))

(deftest test-canonicalize-specs
  (is (= (a/canonicalize-specs '((quote [clojure.set :as set])))
         '([clojure.set :as set])))
  (is (= (a/canonicalize-specs '(:exclude (quote [map mapv])))
         '(:exclude [map mapv])))
  (is (= (a/canonicalize-specs '(:require (quote [clojure.set :as set])))
         '(:require [clojure.set :as set])))
  (is (= (a/canonicalize-specs '(:require (quote clojure.set)))
         '(:require [clojure.set])))
  (is (= (a/canonicalize-specs '(:refer-clojure :exclude '[map] :rename '{map core-map}))
         '(:refer-clojure :exclude [map] :rename {map core-map}))))

(deftest test-canonicalize-import-specs
  (is (= (a/canonicalize-import-specs '(:import (quote [goog Uri])))
         '(:import [goog Uri])))
  (is (= (a/canonicalize-import-specs '(:import (quote (goog Uri))))
         '(:import (goog Uri))))
  (is (= (a/canonicalize-import-specs '(:import (quote goog.Uri)))
         '(:import goog.Uri))))

(deftest test-cljs-1346
  (testing "`ns*` special form conformance"
    (let [test-env (a/empty-env)]
      (is (= (-> (a/parse-ns '((require '[clojure.set :as set]))) :requires)
            '#{cljs.core clojure.set})))
    (binding [a/*cljs-ns* a/*cljs-ns*
              a/*cljs-warnings* nil]
      (let [test-env (a/empty-env)]
        (is (= (-> (a/analyze test-env '(require '[clojure.set :as set])) :requires vals set)
              '#{clojure.set})))
      (let [test-env (a/empty-env)]
        (is (= (-> (a/analyze test-env '(require '[clojure.set :as set :refer [union intersection]])) :uses keys set)
              '#{union intersection})))
      (let [test-env (a/empty-env)]
        (is (= (-> (a/analyze test-env '(require '[clojure.set :as set]
                                          '[clojure.string :as str]))
                 :requires vals set)
              '#{clojure.set clojure.string})))
      (let [test-env (a/empty-env)]
        (is (= (-> (a/analyze test-env '(require-macros '[cljs.test :as test])) :require-macros vals set)
              '#{cljs.test})))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(require-macros '[cljs.test :as test  :refer [deftest is]]))]
        (is (= (-> parsed :require-macros vals set)
              '#{cljs.test}))
        (is (= (-> parsed :use-macros keys set)
              '#{is deftest})))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(require '[cljs.test :as test :refer-macros [deftest is]]))]
        (is (= (-> parsed :requires vals set)
              '#{cljs.test}))
        (is (= (-> parsed :require-macros vals set)
              '#{cljs.test}))
        (is (= (-> parsed :use-macros keys set)
              '#{is deftest})))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(use '[clojure.set :only [intersection]]))]
        (is (= (-> parsed :uses keys set)
              '#{intersection}))
        (is (= (-> parsed :requires)
              '{clojure.set clojure.set})))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(use-macros '[cljs.test :only [deftest is]]))]
        (is (= (-> parsed :use-macros keys set)
              '#{deftest is}))
        (is (= (-> parsed :require-macros)
              '{cljs.test cljs.test}))
        (is (nil? (-> parsed :requires))))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(import '[goog.math Long Integer]))]
        (is (= (-> parsed :imports)
              (-> parsed :requires)
              '{Long goog.math.Long
                Integer goog.math.Integer})))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(refer-clojure :exclude '[map mapv]))]
        (is (= (-> parsed :excludes)
              '#{map mapv})))
      (let [test-env (a/empty-env)
            parsed (a/analyze test-env '(refer-clojure :exclude '[map mapv] :rename '{mapv core-mapv}))]
        (is (= (-> parsed :excludes)
              '#{map mapv})))))
  (testing "arguments to require should be quoted"
    (binding [a/*cljs-ns* a/*cljs-ns*
              a/*cljs-warnings* nil]
      (is (thrown-with-msg? Exception #"Arguments to require must be quoted"
            (a/analyze test-env
              '(require [clojure.set :as set]))))
      (is (thrown-with-msg? Exception #"Arguments to require must be quoted"
            (a/analyze test-env
              '(require clojure.set))))))
  (testing "`:ns` and `:ns*` should throw if not `:top-level`"
    (binding [a/*cljs-ns* a/*cljs-ns*
              a/*cljs-warnings* nil]
      (are [analyzed] (thrown-with-msg? Exception
                        #"Namespace declarations must appear at the top-level."
                        analyzed)
          (a/analyze test-env
          '(def foo
             (ns foo.core
               (:require [clojure.set :as set]))))
        (a/analyze test-env
          '(fn []
             (ns foo.core
               (:require [clojure.set :as set]))))
        (a/analyze test-env
          '(map #(ns foo.core
                   (:require [clojure.set :as set])) [1 2])))
      (are [analyzed] (thrown-with-msg? Exception
                        #"Calls to `require` must appear at the top-level."
                        analyzed)
        (a/analyze test-env
          '(def foo
             (require '[clojure.set :as set])))
        (a/analyze test-env
          '(fn [] (require '[clojure.set :as set])))
        (a/analyze test-env
          '(map #(require '[clojure.set :as set]) [1 2]))))))

(deftest test-gen-user-ns
  ;; note: can't use `with-redefs` because direct-linking is enabled
  (let [s   "src/cljs/foo.cljs"
        sha (util/content-sha s)]
    (is (= (a/gen-user-ns s) (symbol (str "cljs.user.foo" (apply str (take 7 sha)))))))
  (let [a   "src/cljs/foo.cljs"
        b   "src/cljs/foo.cljc"]
    ;; namespaces should have different names because the filename hash will be different
    (is (not= (a/gen-user-ns a) (a/gen-user-ns b)))
    ;; specifically, only the hashes should differ
    (let [nsa (str (a/gen-user-ns a))
          nsb (str (a/gen-user-ns b))]
      (is (not= (.substring nsa (- (count nsa) 7)) (.substring nsb (- (count nsb) 7))))
      (is (= (.substring nsa 0 (- (count nsa) 7)) (.substring nsb 0 (- (count nsb) 7)))))))

(deftest test-cljs-1536
  (let [parsed (e/with-compiler-env test-cenv
                 (a/analyze (assoc test-env :def-emits-var true)
                   '(def x 1)))]
    (is (some? (:var-ast parsed))))
  (let [parsed (e/with-compiler-env test-cenv
                 (a/analyze (assoc test-env :def-emits-var true)
                   '(let [y 1] (def y 2))))]
    (is (some? (-> parsed :expr :ret :var-ast)))))

(deftest test-has-extern?-basic
  (let [externs (externs/externs-map
                  (closure/load-externs
                    {:externs ["src/test/externs/test.js"]
                     :use-only-custom-externs true}))]
    (is (true? (a/has-extern? '[Foo] externs)))
    (is (true? (a/has-extern? '[Foo wozMethod] externs)))
    (is (false? (a/has-extern? '[foo] externs)))
    (is (false? (a/has-extern? '[Foo gozMethod] externs)))
    (is (true? (a/has-extern? '[baz] externs)))
    (is (false? (a/has-extern? '[Baz] externs)))))

(deftest test-has-extern?-defaults
  (let [externs (externs/externs-map)]
    (is (true? (a/has-extern? '[console] externs)))
    (is (true? (a/has-extern? '[console log] externs)))
    (is (true? (a/has-extern? '[Number isNaN] externs)))))

(def externs-cenv
  (atom
    {::a/externs
     (externs/externs-map
       (closure/load-externs
         {:externs ["src/test/externs/test.js"]}))}))

(deftest test-js-tag
  (let [externs (externs/externs-map
                  (closure/load-externs
                    {:externs ["src/test/externs/test.js"]}))]
    (is (= 'js/Console (a/js-tag '[console] :tag externs)))
    (is (= 'js/Function (a/js-tag '[console log] :tag externs)))
    (is (= 'js/Boolean (a/js-tag '[Number isNaN] :ret-tag externs)))
    (is (= 'js/Foo (a/js-tag '[baz] :ret-tag externs)))))

(deftest test-externs-infer
  (is (= 'js/Foo
         (-> (binding [a/*cljs-ns* a/*cljs-ns*]
               (e/with-compiler-env externs-cenv
                 (a/analyze (a/empty-env) 'js/baz)))
           :info :ret-tag)))
  (is (= 'js/Foo
         (-> (binding [a/*cljs-ns* a/*cljs-ns*]
               (e/with-compiler-env externs-cenv
                 (a/analyze (a/empty-env) '(js/baz))))
           :tag)))
  (is (= 'js
         (-> (binding [a/*cljs-ns* a/*cljs-ns*]
               (e/with-compiler-env externs-cenv
                 (a/analyze (a/empty-env) '(js/woz))))
           :tag)))
  (is (= 'js
         (-> (binding [a/*cljs-ns* a/*cljs-ns*]
               (e/with-compiler-env externs-cenv
                 (a/analyze (a/empty-env) '(def foo (js/woz)))))
           :tag)))
  (is (= 'js
          (-> (binding [a/*cljs-ns* a/*cljs-ns*]
                (e/with-compiler-env externs-cenv
                  (a/analyze (a/empty-env) '(def foo js/boz))))
            :tag)))
  (is (nil? (-> (binding [a/*cljs-ns* a/*cljs-ns*]
                  (a/no-warn
                    (e/with-compiler-env externs-cenv
                      (a/analyze (a/empty-env)
                        '(let [z (.baz ^js/Foo.Bar x)]
                           z)))))
              :tag meta :prefix))))

(deftest test-cljs-2023
  (let [form (with-meta 'js/goog.DEBUG {:tag 'boolean})]
    (is (= (-> (ana-api/analyze (a/empty-env) form) :tag) 'boolean))))

(deftest test-cljs-1992 ;; declare after def should have no effect
  (let [test-cenv (e/default-compiler-env)]
    (e/with-compiler-env test-cenv
      (a/analyze-form-seq
        '[(ns test.cljs-1992)
          (defn test-fn [a b c] :foo)
          (declare test-fn)]
        ))

    (let [def (get-in @test-cenv [::a/namespaces 'test.cljs-1992 :defs 'test-fn])]
      (is (:fn-var def)))))

(deftest test-cljs-2101
  (let [test-cenv (e/default-compiler-env)]
    (e/with-compiler-env test-cenv
      (a/analyze-form-seq
        ['(ns test.cljs-2101)
         `(do
            ;; Splice in 32 forms in order to consume first chunk in chunked sequence
            ~@(range 32)
            (def ~'x32 1)
            ;; The previous def must be analyzed for subsequent var special to succeed
            (def ~'x33 (var ~'x32)))]))))

(deftest test-cljs-2139
  (let [ws (atom [])]
    (try
      (a/with-warning-handlers [(collecting-warning-handler ws)]
        (a/analyze (a/empty-env)
          '(defn foo [] x)))
      (catch Exception _))
    (is (= ["Use of undeclared Var cljs.user/x"] @ws))))

(deftest test-cljs-2148
  (binding [ana/*checked-arrays* :warn]
    (let [ws (atom [])]
      (try
        (a/with-warning-handlers [(collecting-warning-handler ws)]
          (e/with-compiler-env test-cenv
            (a/analyze (a/empty-env)
              '(aget (js-obj) "a"))))
        (catch Exception _))
      (is (= ["cljs.core/aget, arguments must be an array followed by numeric indices, got [object string] instead (consider goog.object/get for object access)"] @ws)))
    (let [ws (atom [])]
      (try
        (a/with-warning-handlers [(collecting-warning-handler ws)]
          (e/with-compiler-env test-cenv
            (a/analyze (a/empty-env)
              '(aget (js-obj) "foo" "bar"))))
        (catch Exception _))
      (is (= ["cljs.core/aget, arguments must be an array followed by numeric indices, got [object string string] instead (consider goog.object/getValueByKeys for object access)"] @ws)))
    (let [ws (atom [])]
      (try
        (a/with-warning-handlers [(collecting-warning-handler ws)]
          (e/with-compiler-env test-cenv
            (a/analyze (a/empty-env)
              '(aset (js-obj) "a" 2))))
        (catch Exception _))
      (is (= ["cljs.core/aset, arguments must be an array, followed by numeric indices, followed by a value, got [object string number] instead (consider goog.object/set for object access)"] @ws)))
    (let [ws (atom [])]
      (try
        (a/with-warning-handlers [(collecting-warning-handler ws)]
          (e/with-compiler-env test-cenv
            (a/analyze (a/empty-env)
              '(let [^objects arr (into-array [1 2 3])]
                 (aget arr 0)))))
        (catch Exception _))
      (is (empty? @ws)))
    (let [ws (atom [])]
      (try
        (a/with-warning-handlers [(collecting-warning-handler ws)]
          (e/with-compiler-env test-cenv
            (a/analyze (a/empty-env)
              '(and true (or (aget (js-obj "foo" 1) "foo") 2)))))
        (catch Exception _))
      (is (= 1 (count @ws))))))

(deftest test-cljs-2037
  (let [test-env (assoc-in (a/empty-env) [:ns :name] 'cljs.user)]
    (binding [a/*cljs-ns* a/*cljs-ns*
              a/*analyze-deps* false]
      (is (thrown-with-msg? Exception #"Alias str already exists in namespace cljs.user, aliasing clojure.string"
            (a/analyze test-env '(do
                                   (require '[clojure.string :as str])
                                   (require '[clojure.set :as str])))))
      (is (thrown-with-msg? Exception #"Alias str already exists in namespace cljs.user, aliasing clojure.string"
            (a/analyze test-env '(do
                                   (require-macros '[clojure.string :as str])
                                   (require-macros '[clojure.set :as str])))))
      (is (a/analyze test-env '(do
                                 (require '[clojure.string :as str])
                                 (require '[clojure.string :as str])
                                 (require 'clojure.string)))))))

(deftest test-cljs-2182
  (let [cenv (atom @test-cenv)]
    (is (thrown-with-msg? Exception
          #"Argument to resolve must be a quoted symbol"
          (e/with-compiler-env test-cenv
            (a/analyze test-env '(resolve foo.core)))))))

(deftest test-cljs-2387
  (a/no-warn
    (e/with-compiler-env test-cenv
      (a/analyze-file (io/file "src/test/cljs_build/analyzer_test/no_defs.cljs"))))
  (is (= {} (get-in @test-cenv [::a/namespaces 'analyzer-test.no-defs :defs]))))

(deftest test-cljs-2475
  (is (thrown-with-msg? Exception #"recur argument count mismatch, expected: 2 args, got: 1"
        (a/analyze test-env '(loop [x 1 y 2] (recur 3))))))

(deftest test-cljs-2476
  (doseq [invalid-try-recur-form '[(loop [] (try (recur)))
                                   (loop [] (try (catch js/Error t (recur))))
                                   (loop [] (try (catch :default t (recur))))
                                   (loop [] (try (finally (recur))))]]
    (is (thrown-with-msg? Exception
          #"Can't recur here"
          (a/analyze test-env invalid-try-recur-form)))))

(comment
  (binding [a/*cljs-ns* a/*cljs-ns*]
    (a/no-warn
      (e/with-compiler-env externs-cenv
        (a/analyze (a/empty-env)
          '(let [React (js/require "react")]
             React)))))

  ;; FIXME: we don't preserve tag information
  (binding [a/*cljs-ns* a/*cljs-ns*]
    (a/no-warn
      (e/with-compiler-env externs-cenv
        (let [aenv (a/empty-env)
              _ (a/analyze aenv '(ns foo.core))
              aenv' (assoc-in aenv [:ns :name] 'foo.core)
              _ (a/analyze aenv' '(def x 1))]
          (dissoc (a/analyze-symbol (assoc-in aenv [:ns :name] 'foo.core) 'x) :env)
          ;(get-in @externs-cenv [::a/namespaces 'foo.core])
          ))))
  )

(def core-inferred
  ["var setTimeout;" "var process;" "process.hrtime;"
   "goog.isArrayLike;" "Java.type;" "Object.out;" "Object.out.println;"
   "Object.error;" "Object.error.println;"])

(defn infer-test-helper
  [{:keys [forms externs warnings warn js-dependency-index with-core? opts]}]
  (let [test-cenv (atom
                    (cond->
                      (if with-core?
                        (env/default-compiler-env*
                          (closure/add-externs-sources (merge {:infer-externs true} opts)))
                        {::a/externs
                         (externs/externs-map
                           (closure/load-externs {:externs (or externs [])}))})
                      js-dependency-index (assoc :js-dependency-index js-dependency-index)))
        wrap      (if with-core?
                    #(comp/with-core-cljs nil %)
                    #(do (%)))]
    (a/with-warning-handlers [(collecting-warning-handler (or warnings (atom [])))]
      (binding [a/*analyze-deps* false
                a/*cljs-ns* a/*cljs-ns*]
        (e/with-compiler-env test-cenv
          (wrap
            (fn []
              (binding [a/*analyze-deps* true
                        a/*cljs-warnings*
                        (assoc a/*cljs-warnings*
                          :infer-warning (if (nil? warn) true warn))]
                (a/analyze-form-seq forms))
              (with-out-str
                (comp/emit-externs
                  (reduce util/map-merge {}
                    (map (comp :externs second)
                      (get @test-cenv ::a/namespaces))))))))))))

(deftest test-basic-infer
  (let [res (infer-test-helper
              {:forms '[(ns foo.core)
                        (defn bar [a] (js/parseInt a))
                        (def c js/React.Component)
                        (js/console.log "Hello world!")
                        (fn [& args]
                          (.apply (.-log js/console) js/console (into-array args)))
                        (js/console.log js/Number.MAX_VALUE)
                        (js/console.log js/Symbol.iterator)]})]
    (is (= (unsplit-lines ["var React;" "React.Component;"]) res))))

(deftest test-method-infer
  (let [res (infer-test-helper
              {:forms '[(defn foo [^js/React.Component c]
                          (.render c))]})]
    (is (= (unsplit-lines ["var React;" "React.Component;" "React.Component.prototype.render;"])
           res))))

(deftest test-minimal-infer
  (let [res (infer-test-helper
              {:forms '[(js/console.log (.wozMethod (js/baz)))]
               :externs ["src/test/externs/test.js"]})]
    (is (string/blank? res))))

(deftest test-type-hint-minimal-infer
  (let [res (infer-test-helper
              {:forms ''[(defn afun [^js/Foo x]
                           (.wozMethod x))]
               :externs ["src/test/externs/test.js"]})]
    (is (string/blank? res))))

(deftest test-type-hint-infer-unknown-method-in-chain
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(defn afun [^js/Foo.Bar x]
                          (let [z (.baz x)]
                            (.wozz z)))]
               :externs ["src/test/externs/test.js"]
               :warnings ws})]
    (is (= (unsplit-lines ["Foo.Boo.prototype.wozz;"]) res))
    (is (= 1 (count @ws)))
    (is (string/starts-with?
          (first @ws)
          "Cannot resolve property wozz for inferred type js/Foo.Boo"))))

(deftest test-type-hint-infer-unknown-property-in-chain
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(defn afun [^js/Foo.Bar x]
                          (let [z (.baz x)]
                            (.-wozz z)))]
               :externs ["src/test/externs/test.js"]
               :warnings ws})]
    (is (= (unsplit-lines ["Foo.Boo.prototype.wozz;"]) res))
    (is (= 1 (count @ws)))
    (is (string/starts-with?
          (first @ws)
          "Cannot resolve property wozz for inferred type js/Foo.Boo"))))

(deftest test-type-hint-infer-unknown-method
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(defn baz [^js/Foo a]
                           (.gozMethod a))]
               :externs ["src/test/externs/test.js"]
               :warnings ws})]
    (is (= (unsplit-lines ["Foo.prototype.gozMethod;"]) res))
    (is (= 1 (count @ws)))
    (is (string/starts-with?
          (first @ws)
          "Cannot resolve property gozMethod for inferred type js/Foo"))))

(deftest test-infer-unknown-method-from-externs
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(.gozMethod (js/baz))]
               :externs ["src/test/externs/test.js"]
               :warnings ws})]
    (is (= (unsplit-lines ["Foo.prototype.gozMethod;"]) res))
    (is (= 1 (count @ws)))
    (is (string/starts-with?
          (first @ws)
          "Cannot resolve property gozMethod for inferred type js/Foo"))))

(deftest test-infer-js-require
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns foo.core)
                        (def React (js/require "react"))
                        (.log js/console (.-Component React))]
               :externs ["src/test/externs/test.js"]
               :warnings ws})]
    (is (= (unsplit-lines ["var require;" "Object.Component;"]) res))
    (is (= 1 (count @ws)))
    (is (string/starts-with?
          (first @ws)
          "Adding extern to Object for property Component"))))

(deftest test-set-warn-on-infer
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns warn-on-infer-test.app)
                        (set! *warn-on-infer* true)
                        (defn wrap-baz [x]
                          (.baz x))]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :warn false
               :with-core? true})]
    (is (= 1 (count @ws)))
    (is (string/starts-with? (first @ws) "Cannot infer target type"))))

(deftest test-cljs-1970-infer-with-cljs-literals
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-1970.core)
                        (set! *warn-on-infer* true)
                        (defn foo [] (list))
                        (defn bar [] (vector))]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :with-core? true})]
    (is (zero? (count @ws)))))

(deftest test-cljs-1918-infer-with-case-keywords
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-1918.core)
                        (defn foo [x]
                          (cljs.core/case x
                            :foo 1
                            nil))]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :with-core? true})]
    (is (zero? (count @ws)))))

(deftest test-cljs-2385-infer-priority
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-1918.core)
                        (defn thing [{:as this}]
                          (.componentDidUpdate ^js/Thing this))]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :with-core? true})]
    (is (string/includes? res "Thing.prototype.componentDidUpdate;"))
    (is (zero? (count @ws)))))

(deftest test-cljs-2392-broken-inferred-externs
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-1918.core
                          (:require [cljs.nodejs]
                                    [cljs.nodejscli]))]
               :warnings ws
               :with-core? true
               :opts {:target :nodejs}})]
    (not (string/includes? res "COMPILED"))
    (not (string/includes? res "goog"))
    (is (zero? (count @ws)))))

(deftest test-cljs-2678-global-exports-infer
  (let [ws  (atom [])
        res (infer-test-helper
              {:js-dependency-index {"react" {:global-exports '{react React}}}
               :forms '[(ns foo.core
                          (:require [react :as react]))
                        (.log js/console react/Component)]
               :warnings ws
               :warn false})]
    (is (= (unsplit-lines ["Object.Component;"]) res))))
