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
            [clojure.set :as set]
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

(defn analyze
  ([env form]
   (env/ensure (a/analyze env form)))
  ([env form name]
   (env/ensure (a/analyze env form name)))
  ([env form name opts]
   (env/ensure (a/analyze env form name opts))))

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
      (analyze (a/empty-env) form))
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
          (analyze ns-env '(ns foo.bar (:require {:foo :bar})))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns & options] and lib.ns specs supported in :require / :require-macros"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:require [:foo :bar])))
          (catch Exception e
            (.getMessage e)))
        "Library name must be specified as a symbol in :require / :require-macros"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:require [baz.woz :as woz :refer [] :plop])))
          (catch Exception e
            (.getMessage e)))
        "Only :as alias, :refer (names) and :rename {from to} options supported in :require"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:require [baz.woz :as woz :refer [] :plop true])))
          (catch Exception e
            (.getMessage e)))
        "Only :as, :refer and :rename options supported in :require / :require-macros"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:require [baz.woz :as woz :refer [] :as boz :refer []])))
          (catch Exception e
            (.getMessage e)))
        "Each of :as and :refer options may only be specified once in :require / :require-macros"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:refer-clojure :refer [])))
          (catch Exception e
            (.getMessage e)))
        "Only [:refer-clojure :exclude (names)] and optionally `:rename {from to}` specs supported"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:refer-clojure :rename [1 2])))
          (catch Exception e
            (.getMessage e)))
        "Only [:refer-clojure :exclude (names)] and optionally `:rename {from to}` specs supported"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:use [baz.woz :exclude []])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:use [baz.woz])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:use [baz.woz :only])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:use [baz.woz :only [1 2 3]])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:use [baz.woz :rename [1 2]])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:use [foo.bar :rename {baz qux}])))
          (catch Exception e
            (.getMessage e)))
        "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:use [baz.woz :only [foo] :only [bar]])))
          (catch Exception e
            (.getMessage e)))
        "Each of :only and :rename options may only be specified once in :use / :use-macros"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:require [baz.woz :as []])))
          (catch Exception e
            (.getMessage e)))
        ":as must be followed by a symbol in :require / :require-macros"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:require [baz.woz :as woz] [noz.goz :as woz])))
          (catch Exception e
            (.getMessage e)))
        ":as alias must be unique"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:require [foo.bar :rename {baz qux}])))
          (catch Exception e
            (.getMessage e)))
        "Renamed symbol baz not referred"))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:unless [])))
          (catch Exception e
            (.getMessage e)))
        "Only :refer-clojure, :require, :require-macros, :use, :use-macros, and :import libspecs supported. Got (:unless []) instead."))
  (is (.startsWith
        (try
          (analyze ns-env '(ns foo.bar (:require baz.woz) (:require noz.goz)))
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
           (:tag (analyze test-env '1)))
         'number))
  (is (= (e/with-compiler-env test-cenv
           (:tag (analyze test-env '"foo")))
         'string))
  (is (= (e/with-compiler-env test-cenv
           (:tag (analyze test-env '\a)))
        'string))
  (is (= (e/with-compiler-env test-cenv
           (:tag (analyze test-env '(make-array 10))))
         'array))
  (is (= (e/with-compiler-env test-cenv
           (:tag (analyze test-env '(js-obj))))
         'object))
  (is (= (e/with-compiler-env test-cenv
           (:tag (analyze test-env '[])))
         'cljs.core/IVector))
  (is (= (e/with-compiler-env test-cenv
           (:tag (analyze test-env '{})))
         'cljs.core/IMap))
  (is (= (e/with-compiler-env test-cenv
           (:tag (analyze test-env '#{})))
         'cljs.core/ISet))
  (is (= (e/with-compiler-env test-cenv
           (:tag (analyze test-env ())))
         'cljs.core/IList))
  (is (= (e/with-compiler-env test-cenv
           (:tag (analyze test-env '(fn [x] x))))
         'function)))

(deftest if-inference
  (is (= (a/no-warn
           (e/with-compiler-env test-cenv
             (:tag (analyze test-env '(if x "foo" 1)))))
         '#{number string})))

(deftest method-inference
  (is (= (e/with-compiler-env test-cenv
           (:tag (analyze test-env '(.foo js/bar))))
         'js)))

(deftest fn-inference
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env
  ;                 '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
  ;                    (x :one)))))
  ;      'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env
  ;                 '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
  ;                    (x :one :two)))))
  ;      'string))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env
  ;                 '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
  ;                    (x :one :two :three)))))
  ;      'cljs.core/IList))
  )

(deftest lib-inference
  (is (= (e/with-compiler-env test-cenv
           (:tag (analyze test-env '(+ 1 2))))
         'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env '(alength (array)))))
  ;       'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env '(aclone (array)))))
  ;       'array))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env '(-count [1 2 3]))))
  ;      'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env '(count [1 2 3]))))
  ;       'number))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env '(into-array [1 2 3]))))
  ;       'array))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env '(js-obj))))
  ;       'object))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env '(-conj [] 1))))
  ;       'clj))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env '(conj [] 1))))
  ;       'clj))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env '(assoc nil :foo :bar))))
  ;       'clj))
  ;(is (= (e/with-compiler-env test-cenv
  ;         (:tag (analyze test-env '(dissoc {:foo :bar} :foo))))
  ;       '#{clj clj-nil}))
  )

(deftest test-always-true-if
  (is (= (e/with-compiler-env test-cenv
           (:tag (analyze test-env '(if 1 2 "foo"))))
         'number)))

;; will only work if the previous test works
(deftest test-count
  ;(is (= (cljs.env/with-compiler-env test-cenv
  ;         (:tag (analyze test-env '(count []))))
  ;       'number))
  )

(deftest test-numeric
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (analyze test-env '(dec x)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (analyze test-env '(int x)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (analyze test-env '(unchecked-int x)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (analyze test-env '(mod x y)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (analyze test-env '(quot x y)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (analyze test-env '(rem x y)))))
  ;       'number))
  ;(is (= (a/no-warn
  ;         (cljs.env/with-compiler-env test-cenv
  ;           (:tag (analyze test-env '(bit-count n)))))
  ;       'number))
  )

;; =============================================================================
;; Catching errors during macroexpansion

(deftest test-defn-error
  (is (.startsWith
        (try
          (analyze test-env '(defn foo 123))
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
    (is (= (do (analyze ns-env '(ns weeble.ns {:foo bar}))
               (meta a/*cljs-ns*))
           {:foo 'bar}))

    (is (= (do (analyze ns-env '(ns ^{:foo bar} weeble.ns))
               (meta a/*cljs-ns*))
           {:foo 'bar}))

    (is (= (do (analyze ns-env '(ns ^{:foo bar} weeble.ns {:baz quux}))
               (meta a/*cljs-ns*))
           {:foo 'bar :baz 'quux}))

    (is (= (do (analyze ns-env '(ns ^{:foo bar} weeble.ns {:foo baz}))
               (meta a/*cljs-ns*))
           {:foo 'baz}))

    (is (= (meta (:name (analyze ns-env '(ns weeble.ns {:foo bar}))))
           {:foo 'bar}))

    (is (= (meta (:name (analyze ns-env '(ns ^{:foo bar} weeble.ns))))
           {:foo 'bar}))

    (is (= (meta (:name (analyze ns-env '(ns ^{:foo bar} weeble.ns {:baz quux}))))
           {:foo 'bar :baz 'quux}))

    (is (= (meta (:name (analyze ns-env '(ns ^{:foo bar} weeble.ns {:foo baz}))))
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
          (analyze test-env '(do (def ^:const foo 123)  (def foo 246)))
          (catch Exception e
            (.getMessage e)))
        "Can't redefine a constant"))
  (is (.startsWith
        (try
          (analyze test-env '(do (def ^:const foo 123)  (set! foo 246)))
          (catch Exception e
            (.getMessage e)))
        "Can't set! a constant")))

(deftest test-cljs-1508-rename
  (binding [a/*cljs-ns* a/*cljs-ns*]
    (let [parsed-ns (e/with-compiler-env test-cenv
                      (analyze test-env
                        '(ns foo.core
                           (:require [clojure.set :as set :refer [intersection] :rename {intersection foo}]))))]
      (is (nil? (-> parsed-ns :uses (get 'foo))))
      (is (nil? (-> parsed-ns :uses (get 'intersection))))
      (is (some? (-> parsed-ns :renames (get 'foo))))
      (is (= (-> parsed-ns :renames (get 'foo))
             'clojure.set/intersection)))
    (is (e/with-compiler-env test-cenv
          (analyze test-env
            '(ns foo.core
               (:use [clojure.set :only [intersection] :rename {intersection foo}])))))
    (is (= (e/with-compiler-env (atom {::a/namespaces
                                       {'foo.core {:renames '{foo clojure.set/intersection}}}})
             (select-keys (a/resolve-var {:ns {:name 'foo.core}} 'foo)
                          [:name :ns]))
           '{:name clojure.set/intersection
             :ns   clojure.set}))
    (let [rwhen (e/with-compiler-env (atom (update-in @test-cenv [::a/namespaces]
                                             merge {'foo.core {:rename-macros '{always cljs.core/when}}}))
                  (a/resolve-macro-var {:ns {:name 'foo.core}} 'always))]
      (is (= (-> rwhen :name)
             'cljs.core/when)))
    (let [parsed-ns (e/with-compiler-env test-cenv
                      (analyze test-env
                        '(ns foo.core
                           (:refer-clojure :rename {when always
                                                    map  core-map}))))]
      (is (= (-> parsed-ns :excludes) #{}))
      (is (= (-> parsed-ns :rename-macros) '{always cljs.core/when}))
      (is (= (-> parsed-ns :renames) '{core-map cljs.core/map})))
    (is (thrown? Exception (e/with-compiler-env test-cenv
                             (analyze test-env
                               '(ns foo.core
                                  (:require [clojure.set :rename {intersection foo}]))))))))

(deftest test-cljs-1274
  (let [test-env (assoc-in (a/empty-env) [:ns :name] 'cljs.user)]
    (binding [a/*cljs-ns* a/*cljs-ns*]
      (is (thrown-with-msg? Exception #"Can't def ns-qualified name in namespace foo.core"
            (analyze test-env '(def foo.core/foo 43))))
      (is (analyze test-env '(def cljs.user/foo 43))))))

(deftest test-cljs-1702
  (let [ws (atom [])]
    (a/with-warning-handlers [(collecting-warning-handler ws)]
      (e/with-compiler-env test-cenv
        (a/analyze-form-seq
          '[(ns test.cljs-1702-a)
            (def ^:private a 3)
            (def ^:private b 3)
            (defn- test-fn-a [a] a)
            (defn- test-fn-b [a] b)])
        (a/analyze-form-seq
          '[(ns test.cljs-1702-b)
            (test.cljs-1702-a/test-fn-a 1)
            (#'test.cljs-1702-a/test-fn-b 1)
            test.cljs-1702-a/a
            @#'test.cljs-1702-a/b]))
      (is (= ["var: test.cljs-1702-a/test-fn-a is not public"
              "var: test.cljs-1702-a/a is not public"] @ws)))))

(deftest test-cljs-1763
  (let [parsed (a/parse-ns-excludes {} '())]
    (is (= parsed
           {:excludes #{}
            :renames {}}))
    (is (set? (:excludes parsed)))))

(deftest test-cljs-1785-js-shadowed-by-local
  (let [ws (atom [])]
    (a/with-warning-handlers [(collecting-warning-handler ws)]
      (analyze ns-env
        '(fn [foo]
           (let [x js/foo]
             (println x)))))
    (is (.startsWith (first @ws) "js/foo is shadowed by a local"))))

(deftest test-cljs-2005
  (let [ws (atom [])]
    (try
      (a/with-warning-handlers [(collecting-warning-handler ws)]
        (analyze (a/empty-env)
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
        (is (= (-> (analyze test-env '(require '[clojure.set :as set])) :requires vals set)
              '#{clojure.set})))
      (let [test-env (a/empty-env)]
        (is (= (-> (analyze test-env '(require '[clojure.set :as set :refer [union intersection]])) :uses keys set)
              '#{union intersection})))
      (let [test-env (a/empty-env)]
        (is (= (-> (analyze test-env '(require '[clojure.set :as set]
                                          '[clojure.string :as str]))
                 :requires vals set)
              '#{clojure.set clojure.string})))
      (let [test-env (a/empty-env)]
        (is (= (-> (analyze test-env '(require-macros '[cljs.test :as test])) :require-macros vals set)
              '#{cljs.test})))
      (let [test-env (a/empty-env)
            parsed (analyze test-env '(require-macros '[cljs.test :as test  :refer [deftest is]]))]
        (is (= (-> parsed :require-macros vals set)
              '#{cljs.test}))
        (is (= (-> parsed :use-macros keys set)
              '#{is deftest})))
      (let [test-env (a/empty-env)
            parsed (analyze test-env '(require '[cljs.test :as test :refer-macros [deftest is]]))]
        (is (= (-> parsed :requires vals set)
              '#{cljs.test}))
        (is (= (-> parsed :require-macros vals set)
              '#{cljs.test}))
        (is (= (-> parsed :use-macros keys set)
              '#{is deftest})))
      (let [test-env (a/empty-env)
            parsed (analyze test-env '(use '[clojure.set :only [intersection]]))]
        (is (= (-> parsed :uses keys set)
              '#{intersection}))
        (is (= (-> parsed :requires)
              '{clojure.set clojure.set})))
      (let [test-env (a/empty-env)
            parsed (analyze test-env '(use-macros '[cljs.test :only [deftest is]]))]
        (is (= (-> parsed :use-macros keys set)
              '#{deftest is}))
        (is (= (-> parsed :require-macros)
              '{cljs.test cljs.test}))
        (is (nil? (-> parsed :requires))))
      (let [test-env (a/empty-env)
            parsed (analyze test-env '(import '[goog.math Long Integer]))]
        (is (= (-> parsed :imports)
              (-> parsed :requires)
              '{Long goog.math.Long
                Integer goog.math.Integer})))
      (let [test-env (a/empty-env)
            parsed (analyze test-env '(refer-clojure :exclude '[map mapv]))]
        (is (= (-> parsed :excludes)
              '#{map mapv})))
      (let [test-env (a/empty-env)
            parsed (analyze test-env '(refer-clojure :exclude '[map mapv] :rename '{mapv core-mapv}))]
        (is (= (-> parsed :excludes)
              '#{map mapv})))))
  (testing "arguments to require should be quoted"
    (binding [a/*cljs-ns* a/*cljs-ns*
              a/*cljs-warnings* nil]
      (is (thrown-with-msg? Exception #"Arguments to require must be quoted"
            (analyze test-env
              '(require [clojure.set :as set]))))
      (is (thrown-with-msg? Exception #"Arguments to require must be quoted"
            (analyze test-env
              '(require clojure.set))))))
  (testing "`:ns` and `:ns*` should throw if not `:top-level`"
    (binding [a/*cljs-ns* a/*cljs-ns*
              a/*cljs-warnings* nil]
      (are [analyzed] (thrown-with-msg? Exception
                        #"Namespace declarations must appear at the top-level."
                        analyzed)
          (analyze test-env
          '(def foo
             (ns foo.core
               (:require [clojure.set :as set]))))
        (analyze test-env
          '(fn []
             (ns foo.core
               (:require [clojure.set :as set]))))
        (analyze test-env
          '(map #(ns foo.core
                   (:require [clojure.set :as set])) [1 2])))
      (are [analyzed] (thrown-with-msg? Exception
                        #"Calls to `require` must appear at the top-level."
                        analyzed)
        (analyze test-env
          '(def foo
             (require '[clojure.set :as set])))
        (analyze test-env
          '(fn [] (require '[clojure.set :as set])))
        (analyze test-env
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
                 (analyze (assoc test-env :def-emits-var true)
                   '(def x 1)))]
    (is (some? (:var-ast parsed))))
  (let [parsed (e/with-compiler-env test-cenv
                 (analyze (assoc test-env :def-emits-var true)
                   '(let [y 1] (def y 2))))]
    (is (some? (-> parsed :body :ret :var-ast)))))

(def analyze-ops-cenv (atom @test-cenv))

(defn ana' [form]
  (e/with-compiler-env analyze-ops-cenv
    (analyze test-env form)))

(defmacro ana [form]
  `(ana' '~form))

(defn prs-ana [fstr]
  (e/with-compiler-env analyze-ops-cenv
    (let [[form] (a/forms-seq*
                   (java.io.StringReader. fstr))]
      (ana' form))))

(def juxt-op-val (juxt :op :val))

(deftest analyze-ops
  ;constants
  (is (empty? (-> (ana 1) :children)))
  (is (= (-> (ana 1) juxt-op-val) [:const 1]))
  (is (empty? (-> (ana :a) :children)))
  (is (= (-> (ana :a) juxt-op-val) [:const :a]))
  (is (= (-> (ana ::a) juxt-op-val) [:const ::a]))
  (is (= (-> (ana "abc") juxt-op-val) [:const "abc"]))
  ;variables
      ; FIXME deviates from tools.analyzer, :name is always unqualified
  (is (= [:var 'cljs.core 'cljs.core/inc 'inc] (-> (ana inc) ((juxt :op :ns :name :form)))))
  (is (= [:var 'cljs.core 'cljs.core/inc 'cljs.core/inc] (-> (ana cljs.core/inc) ((juxt :op :ns :name :form)))))
  ;; dotted :var
  (is (= [:host-field 'bar :host-field 'foo :var 'cljs.core/inc 'cljs.core/inc]
         (-> (ana inc.foo.bar)
             ((juxt :op 
                    :field
                    (comp :op :target)
                    (comp :field :target)
                    (comp :op :target :target)
                    (comp :name :target :target)
                    (comp :name :info :target :target))))))
  ;; dotted :local
  (is (= [:host-field 'c :host-field 'b :local 'a 'a]
         (-> (ana (let [a 1] a.b.c)) :body :ret
             ((juxt :op 
                    :field
                    (comp :op :target)
                    (comp :field :target)
                    (comp :op :target :target)
                    (comp :name :target :target)
                    (comp :name :info :target :target))))))
  ;do
  (is (= (-> (ana (do 1 2)) :op) :do))
  (is (= (-> (ana (do 1 2)) :children) [:statements :ret]))
  ;   :statements
  (is (vector? (-> (ana (do)) :statements)))
  (is (vector? (-> (ana (do 1)) :statements)))
  (is (vector? (-> (ana (do 1 2)) :statements)))
  (is (= (-> (ana (do 1 2)) :statements first :op) :const))
  ;   :ret
  (is (= (-> (ana (do)) :ret juxt-op-val) [:const nil]))
  (is (= (-> (ana (do nil)) :ret juxt-op-val) [:const nil]))
  (is (= (-> (ana (do 1)) :ret juxt-op-val) [:const 1]))
  (is (= (-> (ana (do 1 2)) :ret juxt-op-val) [:const 2]))
  ;let
  (is (= (-> (ana (let [])) :op) :let))
  (is (= (-> (ana (let [a 1] a)) :children) [:bindings :body]))
  ;  :bindings
  (is ((every-pred vector? empty?) (-> (ana (let [])) :bindings)))
  (is (vector? (-> (ana (let [a 1] a)) :bindings)))
  (is (vector? (-> (ana (let [a 1 b 2] a)) :bindings)))
  (is (= (-> (ana (let [a 1] a)) :bindings first :op) :binding))
  (is (= (-> (ana (let [a 1] a)) :bindings first :init :op) :const))
  ;  :body
  (is (= (-> (ana (let [a 1] a)) :body :op) :do))
  ;local
  (is (empty? (-> (ana (let [a 1] a)) :body :ret :children)))
  (is (= (-> (ana (let [a 1] a)) :body :ret :op) :local))
  (is (= (-> (ana (let [a 1] a)) :body :ret :name) 'a))
  (is (= (-> (ana (let [a 1] a)) :body :ret :form) 'a))
  (is (map? (-> (ana (let [a 1] a)) :body :ret :env)))
  ;; dotted :local
  (is (= [:host-field 'c :host-field 'b :local 'a] 
         (-> (ana (let [a 1] a.b.c)) :body :ret
             ((juxt :op 
                    :field
                    (comp :op :target)
                    (comp :field :target)
                    (comp :op :target :target)
                    (comp :name :target :target))))))
  ;local shadow
  (is (= 'alert
         (a/no-warn (-> (ana (let [alert 1] js/alert)) :body 
                        :env :locals
                        (get 'alert)
                        :name))))
  (is (= [:local 'alert]
         (a/no-warn (-> (ana (let [alert 1] js/alert)) :body :ret 
                        ((juxt :op :name))))))
  ;loop
  (is (= (-> (ana (loop [])) :op) :loop))
  (is (= (-> (ana (loop [a 1])) :bindings first :op) :binding))
  (is (= (-> (ana (loop [a 1] a)) :bindings first :init :op) :const))
  (is (= (-> (ana (loop [a 1] a)) :body :ret :local) :loop))
  (is (= (-> (ana (loop [a 1] (recur 1))) :children) [:bindings :body]))
  ;recur
  (is (= (-> (ana (loop [a 1] (recur 1))) :body :ret :op) :recur))
  (is (= (-> (ana (loop [a 1] (recur 1))) :body :ret :children) [:exprs]))
  ;    :exprs
  (is ((every-pred vector? empty?) (-> (ana (loop [] (recur))) :body :ret :exprs)))
  (is (vector? (-> (ana (loop [a 1] (recur 1))) :body :ret :exprs)))
  (is (vector? (-> (ana (loop [a 1 b 2] (recur 1 2))) :body :ret :exprs)))
  (is (= (-> (ana (loop [a 1] (recur 1))) :body :ret :exprs first :op) :const))
  (is (= (-> (ana (loop [a 1 b 2] (recur 1 2))) :body :ret :exprs second :op) :const))
  ;try
  (is (= (-> (ana (try)) :op) :try))
  (is (= (-> (ana (try)) :children) [:body :catch])) ;; not sure why :catch?
  (is (= (-> (ana (try (catch :default e))) :children) [:body :catch]))
  (is (= (-> (ana (try (catch :default e) (finally))) :children) [:body :catch :finally]))
  (is (= (-> (ana (try (finally))) :children) [:body :catch :finally])) ;; not sure why :catch?
  ;   :name
  (is (symbol? (-> (ana (try (catch :default a))) :name)))
  (is (nil? (-> (ana (try)) :name)))
  ;   :catch
  (is (keyword? (-> (ana (try (catch :default a))) :catch :op)))
  ;   :finally
  (is (= (-> (ana (try (finally 1))) :finally :op) :do))
  (is (= (-> (ana (try (finally 1))) :finally :ret :op) :const))
  ;TODO case 
  (is (= (-> (ana (case 1)) :op) :let))
  (is (= (-> (ana (case 1)) :body :ret :op) :case))
  (is (= (-> (ana (case 1)) :body :ret :children) [:test :nodes :default]))
  ;;   :test
  (is (= (-> (ana (case 1)) :body :ret :test :op) :local))
  ;;   :nodes
  (is (vector? (-> (ana (case 1)) :body :ret :nodes)))
  (is (vector? (-> (ana (case 1 :a 1)) :body :ret :nodes)))
  (is (vector? (-> (ana (case 1 (:a :b) 1)) :body :ret :nodes)))
  ;;       :tests
  (is (vector?
        (-> (ana (case 1 :a 1)) :body :ret :nodes first :tests)))
  (is (vector?
        (-> (ana (case 1 :a 1 :b 2)) :body :ret :nodes first :tests)))
  (is (= (-> (ana (case 1 :a 1)) :body :ret :nodes first :tests first :op)
         :case-test))
  (is (= (-> (ana (case 1 :a 1)) :body :ret :nodes first :tests first :test juxt-op-val)
         [:const "a"]))
  (is (= (-> (ana (case 1 :a 1 :b 2)) :body :ret :nodes second :tests first :test juxt-op-val)
         [:const "b"]))
  (is (= (-> (ana (case 1 :a 1 (:b :faz) 2)) :body :ret :nodes (nth 1) :tests second :test juxt-op-val)
         [:const "faz"]))
  ;;       :thens
  (is (= (-> (ana (case 1 :a 3)) :body :ret :nodes first :then :op)
         :case-then))
  (is (= (-> (ana (case 1 :a 3)) :body :ret :nodes first :then :then juxt-op-val)
         [:const 3]))
  (is (= (-> (ana (case 1 :a 3 :b 4)) :body :ret :nodes second :then :then juxt-op-val)
         [:const 4]))
  (is (= (-> (ana (case 1 :a 3 (:b :c) 4)) :body :ret :nodes (nth 1) :then :then juxt-op-val)
         [:const 4]))
  ;;   :default
  (is (= :throw (-> (ana (case 1)) :body :ret :default :op)))
  (is (= [:const 2] (-> (ana (case 1 2)) :body :ret :default juxt-op-val)))
  ;def
;TODO :meta node
  (is (= :def (-> (ana (def a)) :op)))
  (is (= [:var] (-> (ana (def a)) :children)))
  (is (= [:var :init] (-> (ana (def a 1)) :children)))
  ;   :ns/:name
  (is (= ['cljs.core 'cljs.core/a] (-> (ana (def a 1)) ((juxt :ns :name)))))
  ;   :var
  (is (= [:var 'cljs.core 'cljs.core/a 'a] 
         (-> (ana (def a 1)) :var
             ((juxt :op :ns :name :form)))))
  ;   :init
  (is (-> (ana (def a)) (contains? :init) false?))
  (is (= [:const 1] (-> (ana (def a 1)) :init juxt-op-val)))
  ;deftype
  (is (= :deftype (-> (ana (deftype A [])) :statements first :op)))
  (is (= [:body] (-> (ana (deftype A [])) :statements first :children)))
  ;   :body
  (is (= :do (-> (ana (deftype A [a] Object (toString [this] a))) :statements first :body :op)))
        ; field reference
  (is (= [:local :field]
         (-> (ana (deftype A [a] Object (toString [this] a))) 
             :statements first :body :ret :val :methods
             first :body :ret :body :ret 
             ((juxt :op :local)))))
  ;defrecord
  (is (= :defrecord (-> (ana (defrecord Ab [])) :body :statements first :ret :op)))
  (is (= [:body] (-> (ana (defrecord Ab [])) :body :statements first :ret :children)))
  ;   :body
  (is (= :do (-> (ana (defrecord Ab [] Object (toString [this] "a"))) :body :statements first :ret :body :op)))
  ;fn
  (is (= :fn (-> (ana (fn [])) :op)))
  (is (= [:methods] (-> (ana (fn [])) :children)))
  (is (= [:local :methods] (-> (ana (fn a [])) :children)))
  ;   :local
  (is (-> (ana (fn [])) (contains? :local) false?))
  (is (=
       [:binding 'b :fn]
       (-> (ana (fn b [& a]))
           :local
           ((juxt :op :name :local)))))
  (is (=
       [:local 'b :fn]
       (-> (ana (fn b [] b))
           :methods
           first
           :body
           :ret
           ((juxt :op :name :local)))))
  (is (=
       [:binding 'b :fn]
       (-> (ana (fn b [] b))
           :methods
           first
           :body
           :ret
           :env
           :locals
           (get 'b)
           ((juxt :op :name :local)))))
  ;   :variadic?
  (is (true? (-> (ana (fn [& a])) :variadic?)))
  (is (false? (-> (ana (fn [])) :variadic?)))
  ;   :methods
  (is (vector? (-> (ana (fn [])) :methods)))
  (is (vector? (-> (ana (fn ([]) ([a]))) :methods)))
  ;fn-method
  (is (= :fn-method (-> (ana (fn [])) :methods first :op)))
  (is (= [:params :body] (-> (ana (fn [])) :methods first :children)))
  (is (= [:params :body] (-> (ana (fn [a])) :methods first :children)))
  ;   :fixed-arity
  (is (= 0 (-> (ana (fn [])) :methods first :fixed-arity)))
  (is (= 1 (-> (ana (fn [a])) :methods first :fixed-arity)))
  (is (= 2 (-> (ana (fn [a b & c])) :methods first :fixed-arity)))
  ;   :variadic?
  (is (true? (-> (ana (fn [a b & c])) :variadic?)))
  (is (false? (-> (ana (fn [a b])) :variadic?)))
  ;   :body
  (is (= [:const 1] (-> (ana (fn [] 1)) :methods first :body :ret juxt-op-val)))
  ;   :params
  (is (vector?
         (-> (ana (fn [])) :methods first :params)))
  (is (vector?
         (-> (ana (fn [a b])) :methods first :params)))
  (is (= [:binding 'a :arg] 
         (-> (ana (fn [a b])) :methods first :params
             first ((juxt :op :name :local)))))
  (is (= [:binding 'b :arg] 
         (-> (ana (fn [a b])) :methods first :params
             second ((juxt :op :name :local)))))
  ;if
  (is (= :if (-> (ana (if 1 2)) :op)))
  (is (= :if (-> (ana (if 1 2 3)) :op)))
  (is (= [:test :then :else] (-> (ana (if 1 2 3)) :children)))
  (is (= [:test :then :else] (-> (ana (if 1 2)) :children)))
  ;   :test
  (is (= [:const 1] (-> (ana (if 1 2)) :test juxt-op-val)))
  (is (= [:const 1] (-> (ana (if 1 2 3)) :test juxt-op-val)))
  ;   :then
  (is (= [:const 2] (-> (ana (if 1 2)) :then juxt-op-val)))
  (is (= [:const 2] (-> (ana (if 1 2 3)) :then juxt-op-val)))
  ;   :else
  (is (= [:const nil] (-> (ana (if 1 2)) :else juxt-op-val)))
  (is (= [:const 3]   (-> (ana (if 1 2 3)) :else juxt-op-val)))
  ;invoke
  (is (= :invoke (-> (ana (:a 1)) :op)))
  (is (= [:fn :args] (-> (ana (:a 1)) :children)))
  (is ((every-pred vector? empty?) (-> (ana (#'str)) :args)))
  (is (vector? (-> (ana (:a 1)) :args)))
  (is (vector? (-> (ana (:a 1 2)) :args)))
  ;   :fn
  (is (= :the-var (-> (ana (#'str)) :fn :op)))
  ;   :invoke
  (is (= [:const 1] (-> (ana (:a 1)) :args first juxt-op-val)))
  (is (= [:const 2] (-> (ana (:a 1 2)) :args second juxt-op-val)))
  ;js-array
  (is (= :js-array (-> (prs-ana "#js ['a]") :op)))
  (is (= [:items] (-> (prs-ana "#js ['a]") :children)))
  (is (vector? (-> (prs-ana "#js ['a]") :items)))
  (is (= 'array (-> (prs-ana "#js ['a]") :tag)))
  (is (= [:const :a] (-> (prs-ana "#js [:a]") :items first juxt-op-val)))
  ;js-object
  (is (= :js-object (-> (prs-ana "#js {:a 1}]") :op)))
;; FIXME :keys should be an expression too
  (is (= [:vals] (-> (prs-ana "#js {:a 1}") :children)))
  (is (vector? (-> (prs-ana "#js {:a 1}") :vals)))
  (is (= :a (-> (prs-ana "#js {:a 1}") :keys first)))
  (is (vector? (-> (prs-ana "#js {:a 1}") :keys)))
  (is (= [:const 1] (-> (prs-ana "#js {:a 1}") :vals first juxt-op-val)))
  ;js*
  (is (= :js (-> (ana (js* "~{}" 'food)) :op)))
  (is (= [:args] (-> (ana (js* "~{}" 'food)) :children)))
  (is (vector? (-> (ana (js* "~{}" 'food)) :args)))
  (is (= [:const 'food] (-> (ana (js* "~{}" 'food)) :args first :expr juxt-op-val)))
;; FIXME why not a vector?
  ;(is (vector? (-> (ana (js* "~{} / ~{}" 1 2)) :segs)))
  (is (= ["" " / " ""] (-> (ana (js* "~{} / ~{}" 1 2)) :segs)))
  ;letfn
  (is (= :letfn
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :op)))
  (is (= [:bindings :body]
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :children)))
  ;   :bindings
  (is (vector?
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :bindings)))
  (is (vector?
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :bindings)))
  (is (= :binding
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :bindings
             first
             :op)))
  (is (= :fn
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :bindings
             first
             :init
             :op)))
  (is (= :arg
         (-> (ana (letfn [(my-inc [a] a)]
                    (my-inc 1)))
             :bindings
             first
             :init
             :methods
             first
             :body
             :ret
             :local)))
  ;   :body
  (is (= :invoke
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :body :ret :op)))
  (is (= [:local :letfn]
         (-> (ana (letfn [(my-inc [a] (inc a))]
                    (my-inc 1)))
             :body :ret :fn ((juxt :op :local)))))
  ;map
  (is (= :map (-> (ana {:a 1}) :op)))
  (is (= [:keys :vals] (-> (ana {:a 1}) :children)))
  ;   :keys
  (is ((every-pred vector? empty?) (-> (ana {}) :keys)))
  (is (vector? (-> (ana {:a 1}) :keys)))
  (is (= [:const :a] (-> (ana {:a 1}) :keys first juxt-op-val)))
  ;   :vals
  (is ((every-pred vector? empty?) (-> (ana {}) :vals)))
  (is (vector? (-> (ana {:a 1}) :vals)))
  (is (= [:const 1] (-> (ana {:a 1}) :vals first juxt-op-val)))
  ;new
  (is (= :new
         (-> (ana (do (deftype Person [a]) (Person. 1)))
             :ret
             :op)))
  (is (= [:class :args]
         (-> (ana (do (deftype Person [a]) (Person. 1)))
             :ret
             :children)))
  ;   :class
  (is (= [:var 'cljs.core 'cljs.core/Person]
         (-> (ana (do (deftype Person [a]) (Person. 1)))
             :ret
             :class
             ((juxt :op :ns :name)))))
  ;   :args
  (is ((every-pred vector? empty?)
         (-> (ana (do (deftype Noarg []) (Noarg.)))
             :ret
             :args)))
  (is (= [:const 1]
         (-> (ana (do (deftype Person [a]) (Person. 1)))
             :ret
             :args
             first
             juxt-op-val)))
  ;set
  (is (= :set (-> (ana #{:a :b}) :op)))
  (is (= [:items] (-> (ana #{:a :b}) :children)))
  ;   :items
  (is ((every-pred vector? empty?)  (-> (ana #{}) :items)))
  (is (vector? (-> (ana #{:a}) :items)))
  (is (vector? (-> (ana #{:a :c :b}) :items)))
  (is (= [:const :a] (-> (ana #{:a}) :items first juxt-op-val)))
  ;set!
  (is (= :set!
         (-> (ana (do (def a 1) (set! a "Hi!")))
             :ret :op)))
  (is (= [:target :val]
         (-> (ana (do (def a 1) (set! a "Hi!")))
             :ret :children)))
  ;   :target
  (is (= [:var 'cljs.core 'cljs.core/a]
         (-> (ana (do (def a 1) (set! a "Hi!")))
             :ret :target ((juxt :op :ns :name)))))
  ;   :val
  (is (= [:const "Hi!"]
         (-> (ana (do (def a 1) (set! a "Hi!")))
             :ret :val juxt-op-val)))
  ;the-var
  (is (= :the-var (-> (ana #'+) :op)))
  (is (= [:var :sym :meta] (-> (ana #'+) :children)))
  ;   :var
  (is (= [:var 'cljs.core 'cljs.core/+]
         (-> (ana #'+) :var ((juxt :op :ns :name)))))
  ;   :sym
  (is (= 'cljs.core/+ (-> (ana #'+) :sym :expr :val)))
  ;   :meta
  (is (= :map (-> (ana #'+) :meta :op)))
  (is (empty?
        ; ensure at least these entries are present
        (set/difference
          #{:name :tag :arglists
            :line :column :end-line :end-column
            :ns :file :doc :test :top-fn}
          (->> (ana #'+) :meta :keys (map :val)))))
  (run!
    (fn [[k v]]
        ; ensure entries map to sane things
      (case (:val k)
        :name (is (= '+ (-> v :expr :val)))
        :tag (is (= 'number (-> v :expr :val)))
        :arglists (is (= :quote (:op v)))
        ;:row (is (= :quote (:op v)))
        (:line :column :end-line :end-column) (number? (:val v))
        :ns (is (symbol? (-> v :expr :val)))
        :file (is (string? (-> v :expr :val)))
        :doc (is (string? (-> v :expr :val)))
        :test (is (= :if (:op v)))
        :top-fn (do (is (= :const (:op (:expr v))))
                    (is (map? (:val (:expr v)))))
        ;default
        nil))
    (apply zipmap (-> (ana #'+) :meta ((juxt :keys :vals)))))
  ;throw
  (is (= :throw (-> (ana (throw (js/Error. "bad"))) :op)))
  (is (= [:exception] (-> (ana (throw (js/Error. "bad"))) :children)))
  ;   :exception
  (is (= [:js-var 'js 'js/Error] (-> (ana (throw (js/Error. "bad"))) :exception 
                                  :class
                                  ((juxt :op :ns :name)))))
  ;vector
  (is (= :vector (-> (ana [1]) :op)))
  (is (= [:items] (-> (ana [1]) :children)))
  ;   :items
  (is ((every-pred vector? empty?) (-> (ana []) :items)))
  (is (vector? (-> (ana [1]) :items)))
  (is (= [:const 1] (-> (ana [1]) :items first juxt-op-val)))
  ;with-meta
  (is (= :with-meta (-> (ana ^:blah (fn [])) :op)))
  (is (= [:meta :expr] (-> (ana ^:blah (fn [])) :children)))
  (is (= :const (-> (ana '^:foo a) :expr :op)))
  ;   :meta
  (is (= :map (-> (ana ^:blah (fn [])) :meta :op)))
  (is (= [:const :blah] (-> (ana ^:blah (fn [])) :meta :keys first juxt-op-val)))
  (is (= [:const true] (-> (ana ^:blah (fn [])) :meta :vals first juxt-op-val)))
  ;(is (= [:const :foo] (-> (ana '^:foo a) :expr :meta :keys first juxt-op-val)))
  ;(is (= [:const true] (-> (ana '^:foo a) :expr :meta :vals first juxt-op-val)))
  ;   :expr
  (is (= :fn (-> (ana ^:blah (fn [])) :expr :op)))
  ;(is (= :const (-> (ana '^:foo a) :expr :expr :op)))
  ;host-field
  (is (= :host-field (-> (ana (.-field 'a)) :op)))
  (is (= [:target] (-> (ana (.-field 'a)) :children)))
  (is (= 'field (-> (ana (.-field 'a)) :field)))
  ;   :target
  (is (= [:const 'a] (-> (ana (.-field 'a)) :target :expr juxt-op-val)))
  ;host-call
  (is (= :host-call (-> (ana (.call 'a)) :op)))
  (is (= [:target :args] (-> (ana (.call 'a)) :children)))
  (is (= 'call (-> (ana (.call 'a)) :method)))
  ;   :target
  (is (= [:const 'a] (-> (ana (.call 'a)) :target :expr juxt-op-val)))
  ;   :args
  (is ((every-pred vector? empty?) (-> (ana (.call 'a)) :args)))
  (is (= [:const 1] (-> (ana (.call 'a 1)) :args first juxt-op-val)))
  ;ns
  (is (binding [a/*cljs-ns* 'cljs.user]
        (= :ns (-> (ana (ns fazz.foo)) :op))))
  ;ns*
  (is (binding [a/*cljs-ns* 'cljs.user]
        (= :ns* (-> (ana (refer-clojure :exclude '[locking])) :op))))
  ;quote
  (is (= :quote (-> (ana (quote a)) :op)))
  (is (= [:expr] (-> (ana (quote a)) :children)))
  (is (map? (-> (ana (quote a)) :env)))
  (is (= 'quote (-> (ana (quote a)) :form first)))
  (is (= (:op (ana '(1 2 3))) :quote))
  ;   :expr
  (is (= [:const 'a] (-> (ana (quote a)) :expr juxt-op-val)))
  ;js-var
  (is (= [:js-var 'js/console 'js] (-> (ana js/console) ((juxt :op :name :ns)))))
  (is (map? (-> (ana js/console) :env)))
  (is (= 'js/-Infinity (-> (ana js/-Infinity) :form)))
  ;; TODO dotted :js-var (?)
#_
  (is (= [:js-var 'js/console 'js]
         (-> (ana js/console) ((juxt :op :name :ns)))))
  ;munging
  (is (=
       [false 'a]
       (-> 
         (ana (let [a (println 1)
                    b (println 2)]
                [a b]))
         :bindings first 
         ((juxt #(contains? % :ns) :name)))))
  ;shadowing
  (is (=
       'a
       (-> 
         (ana (let [a (println 1)
                    a (println 2)]
                [a a]))
         :bindings second 
         :shadow
         :name)))
  (is (=
       'a
       (-> 
         (ana (let [a (println 1)
                    a (println 2)
                    a (println 3)
                    ]
                [a a a]))
         :bindings (nth 2) 
         :shadow
         :shadow
         :name)))
  ;ns
  (is 
    (binding [a/*analyze-deps* false]
    (binding [a/*cljs-ns* 'cljs.user]
      (ana 
        (ns my.ns.foo
          (:require [clojure.repl]
                    [clojure.string]
                    [goog.string])
          (:import [goog.string StringBuffer]))))))
  ;nested metadata
  (is (= :baz
        (-> (ana ''#{^{:foo :baz} a})
          :expr
          :val
          second
          first
          meta
          :foo))))

(deftest quote-args-error-test
  (is (.startsWith
        (try
          (ana (quote))
          (catch Exception e
            (.getMessage e)))
        "Wrong number of args to quote"))
  (is (.startsWith
        (try
          (ana (quote a b))
          (catch Exception e
            (.getMessage e)))
        "Wrong number of args to quote"))
  (is (.startsWith
        (try
          (ana (quote a b c d))
          (catch Exception e
            (.getMessage e)))
        "Wrong number of args to quote")))

(deftest var-args-error-test
  (is (.startsWith
        (try
          (ana (var))
          (catch Exception e
            (.getMessage e)))
        "Wrong number of args to var"))
  (is (.startsWith
        (try
          (ana (var a b))
          (catch Exception e
            (.getMessage e)))
        "Wrong number of args to var"))
  (is (.startsWith
        (try
          (ana (var nil))
          (catch Exception e
            (.getMessage e)))
        "Argument to var must be symbol")))

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
                 (analyze (a/empty-env) 'js/baz)))
           :info :ret-tag)))
  (is (= 'js/Foo
         (-> (binding [a/*cljs-ns* a/*cljs-ns*]
               (e/with-compiler-env externs-cenv
                 (analyze (a/empty-env) '(js/baz))))
           :tag)))
  (is (= 'js
         (-> (binding [a/*cljs-ns* a/*cljs-ns*]
               (e/with-compiler-env externs-cenv
                 (analyze (a/empty-env) '(js/woz))))
           :tag)))
  (is (= 'js
         (-> (binding [a/*cljs-ns* a/*cljs-ns*]
               (e/with-compiler-env externs-cenv
                 (analyze (a/empty-env) '(def foo (js/woz)))))
           :tag)))
  (is (= 'js
          (-> (binding [a/*cljs-ns* a/*cljs-ns*]
                (e/with-compiler-env externs-cenv
                  (analyze (a/empty-env) '(def foo js/boz))))
            :tag)))
  (is (nil? (-> (binding [a/*cljs-ns* a/*cljs-ns*]
                  (a/no-warn
                    (e/with-compiler-env externs-cenv
                      (analyze (a/empty-env)
                        '(let [z (.baz ^js/Foo.Bar x)]
                           z)))))
              :tag meta :prefix))))

(deftest test-cljs-1871
  (let [ws (atom [])]
    (try
      (a/with-warning-handlers [(collecting-warning-handler ws)]
        (analyze (ana/empty-env)
          '(do (declare ^{:arglists '([x y])} foo)
               (defn foo [x]))))
      (catch Exception _))
    (is (string/includes? (first @ws) "declared arglists ([x y]) mismatch defined arglists ([x])"))))

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
        (analyze (a/empty-env)
          '(defn foo [] x)))
      (catch Exception _))
    (is (= ["Use of undeclared Var cljs.user/x"] @ws))))

(deftest test-cljs-2148
  (binding [ana/*checked-arrays* :warn]
    (let [ws (atom [])]
      (try
        (a/with-warning-handlers [(collecting-warning-handler ws)]
          (e/with-compiler-env test-cenv
            (analyze (a/empty-env)
              '(aget (js-obj) "a"))))
        (catch Exception _))
      (is (= ["cljs.core/aget, arguments must be an array followed by numeric indices, got [object string] instead (consider goog.object/get for object access)"] @ws)))
    (let [ws (atom [])]
      (try
        (a/with-warning-handlers [(collecting-warning-handler ws)]
          (e/with-compiler-env test-cenv
            (analyze (a/empty-env)
              '(aget (js-obj) "foo" "bar"))))
        (catch Exception _))
      (is (= ["cljs.core/aget, arguments must be an array followed by numeric indices, got [object string string] instead (consider goog.object/getValueByKeys for object access)"] @ws)))
    (let [ws (atom [])]
      (try
        (a/with-warning-handlers [(collecting-warning-handler ws)]
          (e/with-compiler-env test-cenv
            (analyze (a/empty-env)
              '(aset (js-obj) "a" 2))))
        (catch Exception _))
      (is (= ["cljs.core/aset, arguments must be an array, followed by numeric indices, followed by a value, got [object string number] instead (consider goog.object/set for object access)"] @ws)))
    (let [ws (atom [])]
      (try
        (a/with-warning-handlers [(collecting-warning-handler ws)]
          (e/with-compiler-env test-cenv
            (analyze (a/empty-env)
              '(let [^objects arr (into-array [1 2 3])]
                 (aget arr 0)))))
        (catch Exception _))
      (is (empty? @ws)))
    (let [ws (atom [])]
      (try
        (a/with-warning-handlers [(collecting-warning-handler ws)]
          (e/with-compiler-env test-cenv
            (analyze (a/empty-env)
              '(and true (or (aget (js-obj "foo" 1) "foo") 2)))))
        (catch Exception _))
      (is (= 1 (count @ws))))))

(deftest test-cljs-2037
  (let [test-env (assoc-in (a/empty-env) [:ns :name] 'cljs.user)]
    (binding [a/*cljs-ns* a/*cljs-ns*
              a/*analyze-deps* false]
      (is (thrown-with-msg? Exception #"Alias str already exists in namespace cljs.user, aliasing clojure.string"
            (analyze test-env '(do
                                   (require '[clojure.string :as str])
                                   (require '[clojure.set :as str])))))
      (is (thrown-with-msg? Exception #"Alias str already exists in namespace cljs.user, aliasing clojure.string"
            (analyze test-env '(do
                                   (require-macros '[clojure.string :as str])
                                   (require-macros '[clojure.set :as str])))))
      (is (analyze test-env '(do
                                 (require '[clojure.string :as str])
                                 (require '[clojure.string :as str])
                                 (require 'clojure.string)))))))

(deftest test-cljs-2182
  (let [cenv (atom @test-cenv)]
    (is (thrown-with-msg? Exception
          #"Argument to resolve must be a quoted symbol"
          (e/with-compiler-env test-cenv
            (analyze test-env '(resolve foo.core)))))))

(deftest test-cljs-2387
  (a/no-warn
    (e/with-compiler-env test-cenv
      (a/analyze-file (io/file "src/test/cljs_build/analyzer_test/no_defs.cljs"))))
  (is (= {} (get-in @test-cenv [::a/namespaces 'analyzer-test.no-defs :defs]))))

(deftest test-cljs-2475
  (is (thrown-with-msg? Exception #"recur argument count mismatch, expected: 2 args, got: 1"
        (analyze test-env '(loop [x 1 y 2] (recur 3))))))

(deftest test-cljs-2476
  (doseq [invalid-try-recur-form '[(loop [] (try (recur)))
                                   (loop [] (try (catch js/Error t (recur))))
                                   (loop [] (try (catch :default t (recur))))
                                   (loop [] (try (finally (recur))))]]
    (is (thrown-with-msg? Exception
          #"Can't recur here"
          (analyze test-env invalid-try-recur-form)))))

(comment
  (binding [a/*cljs-ns* a/*cljs-ns*]
    (a/no-warn
      (e/with-compiler-env externs-cenv
        (analyze (a/empty-env)
          '(let [React (js/require "react")]
             React)))))

  ;; FIXME: we don't preserve tag information
  (binding [a/*cljs-ns* a/*cljs-ns*]
    (a/no-warn
      (e/with-compiler-env externs-cenv
        (let [aenv (a/empty-env)
              _ (analyze aenv '(ns foo.core))
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

(deftest test-cljs-2767-deftype-defrecord
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-2767.core)
                        (defrecord Foo [])]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :with-core? true})]
    (is (empty? @ws))
    (is (not (string/includes? res "cljs.core"))))
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-2767.core)
                        (deftype Foo [])]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :with-core? true})]
    (is (empty? @ws))
    (is (not (string/includes? res "cljs.core")))))

(deftest test-cljs-2790-defrecord-fields
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-2790.core)
                        (defrecord Foo [a b])]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :with-core? true})]
    (is (empty? @ws))
    (is (not (string/includes? res "cljs.core")))))

(deftest test-locals-mapped-to-sym
  (testing "analyze should be robust to :locals mapping to symbols"
    (is (= [:local 'a] (-> (analyze (assoc-in test-env [:locals 'a] 'foo) 'a)
                           ((juxt :op :name)))))))

(deftest test-cljs-2814
  (is (= "global$module$react" (a/munge-global-export 'react)))
  (is (= "global$module$_CIRCA_material_ui$core$styles" (a/munge-global-export "@material-ui/core/styles")))
  (is (= "node$module$_CIRCA_material_ui$core$styles" (ana/munge-node-lib "@material-ui/core/styles"))))

(deftest test-cljs-2819
  (let [ws (atom [])]
    (a/with-warning-handlers [(collecting-warning-handler ws)]
      (analyze ns-env
        '(def *foo* 1)))
    (is (string/starts-with? (first @ws) "*foo* not declared dynamic and thus"))))

(deftest test-cljs-2923
  (is (thrown-with-msg? Exception
                        #"Malformed assignment, expecting \(set! target val\)"
                        (analyze test-env '(set! *warn-on-reflection*))))
  (is (analyze test-env '(set! *warn-on-reflection* true)))
  (is (thrown-with-msg? Exception
                        #"Malformed assignment, expecting \(set! target val\)"
                        (analyze test-env '(set! *warn-on-reflection* true false)))))
