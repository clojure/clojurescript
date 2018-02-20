;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns self-host.test
  (:require [cljs.test :as test
             :refer-macros [run-tests deftest testing is async]]
            [cljs.js :as cljs]
            [cljs.analyzer :as ana]
            [clojure.string :as string]
            [cljs.stacktrace :as st]
            [cljs.nodejs :as nodejs]))

(set! (.-user js/cljs) #js {})

(nodejs/enable-util-print!)

(defn latch [m f]
  (let [r (atom 0)]
    (add-watch r :latch
      (fn [_ _ o n]
        (when (== n m) (f))))
    r))

(defn inc! [r]
  (swap! r inc))

(def vm (js/require "vm"))
(def fs (js/require "fs"))
(def st (cljs/empty-state))

(defn node-eval [{:keys [name source]}]
  (if-not js/COMPILED
    (.runInThisContext vm source (str (munge name) ".js"))
    (js/eval source)))

(def libs
  {'bootstrap-test.core :cljs
   'bootstrap-test.macros :clj
   'bootstrap-test.helper :clj})

(defn node-load [{:keys [name macros]} cb]
  (if (contains? libs name)
    (let [path (str "src/test/self/" (cljs/ns->relpath name)
                    "." (cljs.core/name (get libs name)))]
      (.readFile fs path "utf-8"
        (fn [err src]
          (cb (if-not err
                {:lang :clj :source src}
                (.error js/console err))))))
    (cb nil)))

(defn elide-env [env ast opts]
  (dissoc ast :env))

(defn var-ast
  "Given an already derefed compiler state plus the symbols of a
  namespace and a var (e.g. 'clojure.string and 'trim) , return the var
  AST representation or nil if not found, probably because not required
  yet.

  The 1-arity function does the splitting in case of a fully qualified
  symbol (e.g. 'clojure.string/trim)."
  ([st sym]
   (var-ast st (symbol (namespace sym)) (symbol (name sym))))
  ([st ns-sym sym]
   (get-in st [:cljs.analyzer/namespaces ns-sym :defs sym])))

(defn file->lang
  "Converts a file path to a :lang keyword by inspecting the file
   extension."
  [file-path]
  (if (string/ends-with? file-path ".js")
    :js
    :clj))

(defn str-evals-to
  "Checks that a string evaluates to an expected value."
  ([st l expected s]
   (str-evals-to st l expected nil))
  ([st l expected s opts]
   (cljs/eval-str st
     s
     nil
     (merge
       {:context :expr
        :eval    node-eval}
       opts)
     (fn [{:keys [error value]}]
       (is (nil? error))
       (is (= expected value))
       (inc! l)))))

;; NOTE: can't set passes because callbacks happen _inside_ binding
;; do so will effect other tests

(deftest test-require-updates-*loading*
  (async done
    (let [l (latch 4 done)]
      (cljs/require
        {}
        'load1.core
        :reload-all
        {:load (fn [_ cb] (cb {:lang   :clj
                               :source "(ns load1.core)"}))
         :eval (constantly nil)}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is value)
          (is (= #{'load1.core} @cljs/*loaded*))
          (inc! l)))
      (cljs/require
        {}
        'load2.core
        :reload-all
        {:macros-ns true
         :load      (fn [_ cb] (cb {:lang   :clj
                                    :source "(ns load2.core)"}))
         :eval      (constantly nil)}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is value)
          (is (= #{'load2.core$macros} @cljs/*loaded*))
          (inc! l)))
      (cljs/require
        {}
        'load3.core
        :reload-all
        {:load (fn [_ cb] (cb {:lang   :js
                               :source ""}))
         :eval (constantly nil)}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is value)
          (is (= #{'load3.core} @cljs/*loaded*))
          (inc! l)))
      (cljs/require
        {}
        'load4.core
        :reload-all
        {:macros-ns true
         :load      (fn [_ cb] (cb {:lang   :js
                                    :source ""}))
         :eval      (constantly nil)}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is value)
          (is (= #{'load4.core$macros} @cljs/*loaded*))
          (inc! l))))))

(deftest test-analyze-str
  (async done
    (let [l (latch 3 done)]
      (cljs/analyze-str st "(+ 1 1)" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= :js (:op value)))
          (inc! l)))
      (cljs/analyze-str st "(defprotocol IFoo)" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (inc! l)))
      (cljs/analyze-str st "(fn [] (let [x 7 y] (prn y)))" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? value))
          (is (= "Could not analyze " (ex-message error)))
          (inc! l))))))

(deftest test-compile-str
  (async done
    (let [l (latch 7 done)]
      (cljs/compile-str st "(+ 1 1)"
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= "((1) + (1));\n" value))
          (inc! l)))
      (cljs/compile-str st "(fn [])" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= "(function (){\nreturn null;\n})" value))
          (inc! l)))
      (cljs/compile-str st "(if cljs.core.first 1 2)" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= "(cljs.core.truth_(cljs.core.first)?(1):(2))" value))
          (inc! l)))
      (cljs/compile-str st "(.toString \"a\")" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= "\"a\".toString()" value))
          (inc! l)))
      (cljs/compile-str st "(do (defn foo [a b] (+ a b)) (foo 1 2))" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (string/index-of value "cljs.user.foo.call(null,(1),(2))"))
          (inc! l)))
      (cljs/compile-str st "(do (defn foo [a b] (+ a b)) (foo 1 2))" nil
        {:context :expr
         :static-fns true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (string/index-of value "cljs.user.foo((1),(2))"))
          (inc! l)))
      (cljs/compile-str st "(fn [] (let [x 7 y] (prn y)))" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? value))
          (is (= "Could not compile " (ex-message error)))
          (inc! l))))))

(deftest test-eval-str
  (async done
    (let [l (latch 9 done)]
      (cljs/eval-str st "(+ 1 1)" nil
        {:eval node-eval}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== 2 value))
          (inc! l)))
      (cljs/eval-str st "(def x 1)" nil
        {:eval node-eval
         :context :expr
         :def-emits-var true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (var? value))
          (inc! l)))
      (cljs/eval-str st "(fn [])" nil
        {:eval node-eval
         :context :expr
         :def-emits-var true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (fn? value))
          (inc! l)))
      (cljs/eval-str st "((fn [a b] (+ a b)) 1 2)" nil
        {:eval node-eval
         :context :expr
         :def-emits-var true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== 3 value))
          (inc! l)))
      (cljs/eval-str st "(ns foo.bar)" nil
          {:eval node-eval
           :context :expr
           :def-emits-var true}
          (fn [{:keys [error value]}]
            (is (nil? error))
            (is (not (nil? js/foo.bar)))
            (inc! l)))
      (cljs/eval-str st "(defn foo [a b] (+ a b))" nil
        {:eval node-eval
         :context :expr
         :def-emits-var true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== 3 (js/cljs.user.foo 1 2)))
          (inc! l)))
      (cljs/eval-str st "(do (defn foo [a b] (+ a b)) (foo 1 2))" nil
        {:eval node-eval
         :context :expr
         :def-emits-var true
         :static-fns true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== 3 value))
          (inc! l)))
      (cljs/eval-str st "(def foo (let [x 1] (let [x (inc x)] x)))" nil
        {:eval node-eval
         :context :statement
         :def-emits-var true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (inc! l)))
      (cljs/eval-str st "(with-out-str (doseq [x [1 2]] (println x)))" nil
        {:eval node-eval
         :context :expr}
        (fn [{:keys [error value]}]
          (is (= "1\n2\n" value))
          (inc! l))))))

(deftest test-disable-analyze-deps
  (async done
    (let [l (latch 4 done)]
      (cljs/analyze-str st
        "(ns analyze-deps-as.core (:require [analyze-me.core :refer [abc]]))"
        nil
        {:context      :expr
         :eval         cljs.js/js-eval
         :analyze-deps false
         :load         (fn [_ cb]
                         (cb {:lang   :clj
                              :source "(ns analyze-me.core)"}))}
        (fn [{:keys [error]}]
          (is (nil? error))
          (inc! l)))
      (cljs/eval st
        '(ns analyze-deps-e.core (:require [analyze-me.core :refer [abc]]))
        {:context      :expr
         :eval         cljs.js/js-eval
         :analyze-deps false
         :load         (fn [_ cb]
                         (cb {:lang   :clj
                              :source "(ns analyze-me.core)"}))}
        (fn [{:keys [error]}]
          (is (nil? error))
          (inc! l)))
      (cljs/compile-str st
        "(ns analyze-deps-c.core (:require [analyze-me.core :refer [abc]]))"
        nil
        {:context      :expr
         :eval         cljs.js/js-eval
         :analyze-deps false
         :load         (fn [_ cb]
                         (cb {:lang   :clj
                              :source "(ns analyze-me.core)"}))}
        (fn [{:keys [error]}]
          (is (nil? error))
          (inc! l)))
      (cljs/eval-str st
        "(ns analyze-deps-es.core (:require [analyze-me.core :refer [abc]]))"
        nil
        {:context      :expr
         :eval         cljs.js/js-eval
         :analyze-deps false
         :load         (fn [_ cb]
                         (cb {:lang   :clj
                              :source "(ns analyze-me.core)"}))}
        (fn [{:keys [error]}]
          (is (nil? error))
          (inc! l))))))

(deftest test-disable-load-macros
  (async done
    (let [l (latch 4 done)]
      (cljs/analyze-str st
        "(ns load-macros-as.core (:require-macros [load-me.core]))"
        nil
        {:context     :expr
         :eval        cljs.js/js-eval
         :load-macros false
         :load        (fn [_ _]
                        (throw (ex-info "unexpected" {})))}
        (fn [{:keys [error]}]
          (is (nil? error))
          (inc! l)))
      (cljs/eval st
        '(ns load-macros-e.core (:require-macros [load-me.core]))
        {:context     :expr
         :eval        cljs.js/js-eval
         :load-macros false
         :load        (fn [_ _]
                        (throw (ex-info "unexpected" {})))}
        (fn [{:keys [error]}]
          (is (nil? error))
          (inc! l)))
      (cljs/compile-str st
        "(ns load-macros-c.core (:require-macros [load-me.core]))"
        nil
        {:context     :expr
         :eval        cljs.js/js-eval
         :load-macros false
         :load        (fn [_ _]
                        (throw (ex-info "unexpected" {})))}
        (fn [{:keys [error]}]
          (is (nil? error))
          (inc! l)))
      (cljs/eval-str st
        "(ns load-macros-es.core (:require-macros [load-me.core]))"
        nil
        {:context     :expr
         :eval        cljs.js/js-eval
         :load-macros false
         :load        (fn [_ _]
                        (throw (ex-info "unexpected" {})))}
        (fn [{:keys [error]}]
          (is (nil? error))
          (inc! l))))))

(deftest test-load-and-invoke-macros
  (async done
    (let [l (latch 12 done)]
      ;; Normal require macros
      (let [st (cljs/empty-state)]
        (cljs/eval-str st
          "(ns cljs.user (:require-macros foo.core))"
          nil
          {:eval node-eval
           :load (fn [_ cb] (cb {:lang :clj :source "(ns foo.core) (defmacro add [a b] `(+ ~a ~b))"}))}
          (fn [{:keys [value error]}]
            (is (nil? error))
            (cljs/eval-str st
              "(foo.core/add 1 2)"
              nil
              {:eval    node-eval
               :context :expr}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= 3 value))
                (inc! l))))))
      (let [st (cljs/empty-state)]
        ;; Refer macro symbol
        (cljs/eval-str st
          "(ns cljs.user (:require-macros [foo.core :refer [add]]))"
          nil
          {:eval node-eval
           :load (fn [_ cb] (cb {:lang :clj :source "(ns foo.core) (defmacro add [a b] `(+ ~a ~b))"}))}
          (fn [{:keys [value error]}]
            (is (nil? error))
            (cljs/eval-str st
              "(add 1 3)"
              nil
              {:eval    node-eval
               :context :expr}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= 4 value))
                (inc! l))))))
      (let [st (cljs/empty-state)]
        ;; Alias the macro namespace
        (cljs/eval-str st
          "(ns cljs.user (:require-macros [foo.core :as foo]))"
          nil
          {:eval node-eval
           :load (fn [_ cb] (cb {:lang :clj :source "(ns foo.core) (defmacro add [a b] `(+ ~a ~b))"}))}
          (fn [{:keys [value error]}]
            (is (nil? error))
            (cljs/eval-str st
              "(foo/add 1 5)"
              nil
              {:eval    node-eval
               :context :expr}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= 6 value))
                (inc! l))))))
      (let [st (cljs/empty-state)]
        ;; Use instead of require
        (cljs/eval-str st
          "(ns cljs.user (:use-macros [foo.core :only [add]]))"
          nil
          {:eval node-eval
           :load (fn [_ cb] (cb {:lang :clj :source "(ns foo.core) (defmacro add [a b] `(+ ~a ~b))"}))}
          (fn [{:keys [value error]}]
            (is (nil? error))
            (cljs/eval-str st
              "(add 1 8)"
              nil
              {:eval    node-eval
               :context :expr}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= 9 value))
                (inc! l))))))
      (let [st (cljs/empty-state)]
        ;; Employ inline macro specification sugar (include)
        (cljs/eval-str st
          "(ns cljs.user (:require [foo.core :include-macros true]))"
          nil
          {:eval node-eval
           :load (fn [{:keys [macros]} cb]
                   (if macros
                     (cb {:lang :clj :source "(ns foo.core) (defmacro add [a b] `(+ ~a ~b))"})
                     (cb {:lang :clj :source "(ns foo.core)"})))}
          (fn [{:keys [value error]}]
            (is (nil? error))
            (cljs/eval-str st
              "(foo.core/add 1 13)"
              nil
              {:eval    node-eval
               :context :expr}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= 14 value))
                (inc! l))))))
      (let [st (cljs/empty-state)]
        ;; Employ inline macro specification sugar (include with alias)
        (cljs/eval-str st
          "(ns cljs.user (:require [foo.core :as foo :include-macros true]))"
          nil
          {:eval node-eval
           :load (fn [{:keys [macros]} cb]
                   (if macros
                     (cb {:lang :clj :source "(ns foo.core) (defmacro add [a b] `(+ ~a ~b))"})
                     (cb {:lang :clj :source "(ns foo.core)"})))}
          (fn [{:keys [value error]}]
            (is (nil? error))
            (cljs/eval-str st
              "(foo/add 1 21)"
              nil
              {:eval    node-eval
               :context :expr}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= 22 value))
                (inc! l))))))
      (let [st (cljs/empty-state)]
        ;; Employ inline macro specification sugar (refer)
        (cljs/eval-str st
          "(ns cljs.user (:require [foo.core :refer-macros [add]]))"
          nil
          {:eval node-eval
           :load (fn [{:keys [macros]} cb]
                   (if macros
                     (cb {:lang :clj :source "(ns foo.core) (defmacro add [a b] `(+ ~a ~b))"})
                     (cb {:lang :clj :source "(ns foo.core)"})))}
          (fn [{:keys [value error]}]
            (is (nil? error))
            (cljs/eval-str st
              "(add 1 34)"
              nil
              {:eval    node-eval
               :context :expr}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= 35 value))
                (inc! l))))))
      (let [st (cljs/empty-state)]
        ;; Employ inline macro specification sugar (refer with alias)
        (cljs/eval-str st
          "(ns cljs.user (:require [foo.core :as foo :refer-macros [add]]))"
          nil
          {:eval node-eval
           :load (fn [{:keys [macros]} cb]
                   (if macros
                     (cb {:lang :clj :source "(ns foo.core) (defmacro add [a b] `(+ ~a ~b))"})
                     (cb {:lang :clj :source "(ns foo.core)"})))}
          (fn [{:keys [value error]}]
            (is (nil? error))
            (cljs/eval-str st
              "(+ (add 2 3) (foo/add 5 8))"
              nil
              {:eval    node-eval
               :context :expr}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= 18 value))
                (inc! l))))))
      (let [st (cljs/empty-state)]
        ;; Rely on implicit macro loading (ns loads its own macros)
        (cljs/eval-str st
          "(ns cljs.user (:require foo.core))"
          nil
          {:eval node-eval
           :load (fn [{:keys [macros]} cb]
                   (if macros
                     (cb {:lang :clj :source "(ns foo.core) (defmacro add [a b] `(+ ~a ~b))"})
                     (cb {:lang :clj :source "(ns foo.core (:require-macros foo.core))"})))}
          (fn [{:keys [value error]}]
            (is (nil? error))
            (cljs/eval-str st
              "(foo.core/add 100 200)"
              nil
              {:eval    node-eval
               :context :expr}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= 300 value))
                (inc! l))))))
      (let [st (cljs/empty-state)]
        ;; Rely on implicit macro inference (ns loads its own macros)
        (cljs/eval-str st
          "(ns cljs.user (:require [foo.core :refer [add]]))"
          nil
          {:eval node-eval
           :load (fn [{:keys [macros]} cb]
                   (if macros
                     (cb {:lang :clj :source "(ns foo.core) (defmacro add [a b] `(+ ~a ~b))"})
                     (cb {:lang :clj :source "(ns foo.core (:require-macros foo.core))"})))}
          (fn [{:keys [value error]}]
            (is (nil? error))
            (cljs/eval-str st
              "(add 110 210)"
              nil
              {:eval    node-eval
               :context :expr}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= 320 value))
                (inc! l))))))
      (let [st (cljs/empty-state)]
        ;; Rely on implicit macro inference for renames (ns loads its own macros)
        (cljs/eval-str st
          "(ns cljs.user (:require [foo.core :refer [add] :rename {add plus}]))"
          nil
          {:eval node-eval
           :load (fn [{:keys [macros]} cb]
                   (if macros
                     (cb {:lang :clj :source "(ns foo.core) (defmacro add [a b] `(+ ~a ~b))"})
                     (cb {:lang :clj :source "(ns foo.core (:require-macros foo.core))"})))}
          (fn [{:keys [value error]}]
            (is (nil? error))
            (cljs/eval-str st
              "(plus 110 210)"
              nil
              {:eval    node-eval
               :context :expr}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= 320 value))
                (inc! l))))))
      (let [st (cljs/empty-state)]
        ;; Rely on implicit macro loading (ns loads its own macros), with an alias
        ;; CLJS-1657
        (cljs/eval-str st
          "(ns cljs.user (:require [foo.core :as foo]))"
          nil
          {:eval node-eval
           :load (fn [{:keys [macros]} cb]
                   (if macros
                     (cb {:lang :clj :source "(ns foo.core) (defmacro add [a b] `(+ ~a ~b))"})
                     (cb {:lang :clj :source "(ns foo.core (:require-macros foo.core))"})))}
          (fn [{:keys [value error]}]
            (is (nil? error))
            (cljs/eval-str st
              "(foo/add 300 500)"
              nil
              {:eval    node-eval
               :context :expr}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= 800 value))
                (inc! l)))))))))

(deftest test-eval-str-with-require-macros
  (async done
    (let [l (latch 2 done)]
      (cljs/eval-str st
        "(ns cljs.user (:require-macros [cljs.user.macros]))"
        nil
        {:eval node-eval
         :load (fn [_ cb] (cb {:lang :clj :source "(ns cljs.user.macros)"}))}
        (fn [{:keys [value error]}]
          (is (nil? error))
          (inc! l)))
      (cljs/eval-str st
        "(ns cljs.user (:require-macros [cljs.user.macros :as cljs-user-macros]))"
        nil
        {:eval node-eval
         :load (fn [_ cb] (cb {:lang :clj :source "(ns cljs.user.macros)"}))}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (inc! l))))))

(deftest test-CLJS-1330
  (async done
    (cljs/eval-str st
      "(.toString 1)"
      nil
      {:eval node-eval}
      (fn [{:keys [error value]}]
        (is (= "1" value))
        (done)))))

(deftest test-CLJS-1551
  (async done
    (let [l (latch 3 done)]
      (cljs/eval-str st
        "(if-let [x true y true] 3)"
        nil
        {:eval node-eval}
        (fn [{:keys [error value]}]
          (is (nil? value))
          (is (= "if-let requires exactly 2 forms in binding vector at line 1 " (ex-message (ex-cause error))))
          (inc! l)))
      (cljs/eval-str st
        "(if-let [x true] 1 2 3)"
        nil
        {:eval node-eval}
        (fn [{:keys [error value]}]
          (is (nil? value))
          (is (= "if-let requires 1 or 2 forms after binding vector at line 1 " (ex-message (ex-cause error))))
          (inc! l)))
      (cljs/eval-str st
        "(if-let '(x true) 1)"
        nil
        {:eval node-eval}
        (fn [{:keys [error value]}]
          (is (nil? value))
          (is (= "if-let requires a vector for its binding at line 1 " (ex-message (ex-cause error))))
          (inc! l))))))

(deftest test-CLJS-1573
  (async done
    (let [l (latch 4 done)]
      (cljs/compile-str st
        "\"90°\""
        nil
        {:eval    node-eval
         :context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= "\"90\\u00b0\"" value))
          (inc! l)))
      (cljs/compile-str st
        "\"Ϊ\""
        nil
        {:eval    node-eval
         :context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= "\"\\u03aa\"" value))
          (inc! l)))
      (cljs/compile-str st
        "\"ሴ\""
        nil
        {:eval    node-eval
         :context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= "\"\\u1234\"" value))
          (inc! l)))
      (cljs/eval-str st
        "\"90°\""
        nil
        {:eval node-eval
         :context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= "90°" value))
          (inc! l))))))

(deftest test-CLJS-1577
  (async done
    (let [l (latch 3 done)]
      (cljs/analyze-str st
        "`.x"
        nil
        {:eval    node-eval
         :context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= '.x (:form value)))
          (inc! l)))
      (cljs/compile-str st
        "`.x"
        nil
        {:eval    node-eval
         :context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (string/starts-with? value "new cljs.core.Symbol(null,\".x\",\".x\","))
          (inc! l)))
      (cljs/eval-str st
        "`.x"
        nil
        {:eval    node-eval
         :context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= '.x value))
          (inc! l))))))

(deftest test-CLJS-1584
  (async done
    (cljs/eval-str st
      "(condp = 1 1 2)"
      nil
      {:eval    node-eval
       :context :expr}
      (fn [{:keys [error value]}]
        (is (nil? error))
        (is (= 2 value))
        (done)))))

(deftest test-CLJS-1585
  (async done
    (cljs/eval-str st
      "(ns alias-load.core (:require [aliased.core :as alias]))"
      nil
      {:ns      'cljs.user
       :context :expr
       :eval    cljs.js/js-eval
       :load    (fn [_ cb]
                  (cb {:lang :clj :source "(ns aliased.core)"}))}
      (fn [{:keys [error value]}]
        (is (nil? error))
        (cljs.js/eval-str st
          "::alias/bar"
          nil
          {:ns      'alias-load.core
           :context :expr
           :eval    cljs.js/js-eval}
          (fn [{:keys [error value]}]
            (is (nil? error))
            (is (= :aliased.core/bar value))
            (done)))))))

(deftest test-CLJS-1589
  (async done
    (cljs/eval-str st
      "(case 1 nil nil :x)"
      nil
      {:eval    node-eval
       :context :expr}
      (fn [{:keys [error value]}]
        (is (nil? error))
        (is (= :x value))
        (done)))))

(deftest test-CLJS-1612
  (async done
    (let [st (cljs/empty-state)
          l  (latch 10 done)]
      (cljs/eval st '(ns foo.core
                       (:require [bar.core :as bar]))
        {:load (fn [{:keys [macros]} cb]
                 (if macros
                   (cb {:lang :clj :source "(ns bar.core) (defmacro add [a b] `(+ ~a ~b))"})
                   (cb {:lang :clj :source "(ns bar.core (:refer-macros bar.core)) (defn sub [a b] (- a b))"})))}
        (fn [_] (inc! l)))
      (testing "various syntax quote patterns"
        (str-evals-to st l 'foo.core/foo "`foo" {:ns 'foo.core})
        (str-evals-to st l 'bar.core/sub "`bar/sub" {:ns 'foo.core})
        (str-evals-to st l 'bar.core/add "`bar/add" {:ns 'foo.core})
        (str-evals-to st l 'bar.core/undeclared "`bar/undeclared" {:ns 'foo.core}))
      (testing "core macros under syntax quote"
        (str-evals-to st l 13
          "(do (defmulti bar (fn [x y] [x y])) 13)" {:ns 'foo.core})
        (str-evals-to st l 17
          "(do (deftype FnLikeB [a] IFn (-invoke [_] a)) 17)" {:ns 'foo.core})
        (str-evals-to st l [10 4]
          "(let [{:keys [a b] :or {b 4}} {:a 10}] [a b])" {:ns 'foo.core})
        (str-evals-to st l [[nil]]
          "(js->clj (make-array nil 1 1))" {:ns 'foo.core})
        (str-evals-to st l [1 1 1 1 1]
          "(let [an-array (int-array 5 0)] (js->clj (amap an-array idx ret (+ 1 (aget an-array idx)))))" {:ns 'foo.core})))))

(deftest test-eval-str-with-require
  (async done
    (let [l (latch 3 done)]
      (cljs/eval-str st
        "(ns foo.bar (:require [bootstrap-test.core]))\n(bootstrap-test.core/foo 3 4)"
        nil
        {:eval node-eval
         :load node-load}
        (fn [{:keys [value error]}]
          (is (nil? error))
          (is (== 7 value))
          (inc! l)))
      (cljs/eval-str st
        "(ns foo.bar (:require-macros [bootstrap-test.macros :refer [foo]]))\n(foo 4 4)"
        nil
        {:eval node-eval
         :load node-load}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== 16 value))
          (inc! l)))
      (cljs/eval-str st
        "(ns foo.bar)\n(first [1 2 3])"
        nil
        {:eval node-eval
         :load node-load}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== 1 value))
          (inc! l))))))

(deftest test-cljs-1651
  (let [st (cljs/empty-state)]
    (async done
      (cljs/eval-str st
        "(defn double [x] (* 2 x))"
        nil
        {:eval node-eval
         :context :expr}
        (fn [{:keys [value error]}]
          (is (nil? error))
          (cljs/eval-str st
            "[(double 3) (apply double [3])]"
            nil
            {:eval node-eval
             :context :expr}
            (fn [{:keys [value error]}]
              (is (= value [6 6]))
              (done))))))))

(deftest test-cljs-1854
  (let [st (cljs/empty-state)]
    (async done
      (cljs/eval st
        '(require 'foo.core1854)
        {:eval    node-eval
         :context :expr
         :load    (fn [_ cb] (cb {:lang :clj :source "(ns foo.core1854) (def ^:const x 1)"}))}
        (fn [{:keys [value error]}]
          (is (nil? error))
          (cljs/eval st
            'foo.core1854/x
            {:eval node-eval
             :context :expr}
            (fn [{:keys [value error]}]
              (is (nil? error))
              (is (= value 1))))
          (cljs/eval st
            '(require 'foo.core1854 :reload)
            {:eval    node-eval
             :context :expr
             :load    (fn [_ cb] (cb {:lang :clj :source "(ns foo.core1854) (def ^:const x 2)"}))}
            (fn [{:keys [value error]}]
              (is (nil? error))
              (cljs/eval st
                'foo.core1854/x
                {:eval node-eval
                 :context :expr}
                (fn [{:keys [value error]}]
                  (is (nil? error))
                  (is (= value 2))))
              (cljs/eval st
                '(require 'bar.core1854 :reload-all)
                {:eval    node-eval
                 :context :expr
                 :load    (fn [{:keys [name]} cb]
                            (case name
                              bar.core1854 (cb {:lang :clj :source "(ns bar.core1854 (:require [foo.core1854]))"})
                              foo.core1854 (cb {:lang :clj :source "(ns foo.core1854) (def ^:const x 3)"})))}
                (fn [{:keys [value error]}]
                  (is (nil? error))
                  (cljs/eval st
                    'foo.core1854/x
                    {:eval node-eval
                     :context :expr}
                    (fn [{:keys [value error]}]
                      (is (nil? error))
                      (is (= value 3))
                      (done))))))))))))

(deftest test-cljs-1874
  (async done
    (let [st (cljs/empty-state)
          l (latch 1 done)]
      (cljs/eval st '(ns foo.core
                       (:require-macros [bar.core]))
        {:load (fn [_ cb]
                 (cb {:lang   :clj
                      :source "(ns bar.core) (defmacro add [a b] `(+ ~a ~b))"}))}
        (fn [_]
          (is (false? (:fn-var (var-ast @st 'bar.core$macros/add))))
          (inc! l))))))

(deftest test-cljs-1949
  (async done
    (let [st (cljs/empty-state)
          l (latch 1 done)]
      (cljs/eval-str
        st
        "(.catch (js/Promise. #(%2 \"x\")) #(println %))"
        nil
        {:context :expr
         :eval    node-eval}
        (fn [{:keys [error] :as m}]
          (is (nil? error))
          (inc! l))))))

(deftest test-cljs-2024
  (async done
    (let [st (cljs/empty-state)
          l (latch 1 done)]
      (cljs/eval-str
        st
        "(find-ns-obj 'a.x)"
        nil
        {:context :expr
         :eval node-eval}
        (fn [{:keys [error] :as m}]
          (is (nil? error))
          (inc! l))))))

(deftest test-cljs-2122
  (async done
    (let [st (cljs/empty-state)
          l (latch 2 done)]
      (cljs/eval-str
        st
        "1"
        nil
        {:context :expr
         :eval node-eval}
        (fn [{:keys [error] :as m}]
          (is (nil? error))
          (is (every? symbol? (keys (get-in @st [:cljs.analyzer/namespaces]))))
          (inc! l)))
      (cljs/eval-str
        st
        "1"
        "A string name"
        {:context :expr
         :eval node-eval}
        (fn [{:keys [error] :as m}]
          (is (nil? error))
          (is (every? symbol? (keys (get-in @st [:cljs.analyzer/namespaces]))))
          (inc! l))))))

(deftest test-string-requires-cljs-2232
  (async done
    (let [st (cljs/empty-state)
          l (latch 4 done)]
      (cljs/compile-str
        (atom @st)
        "(ns foo.core (:require [path]))"
        nil
        {:context :expr
         :target :nodejs
         :eval node-eval}
        (fn [{:keys [error value] :as m}]
          (is (nil? error))
          (is (some? (re-find #"foo\.core\.node\$module\$path = require\('path'\);" value)))
          (inc! l)))
      (cljs/eval-str
        (atom @st)
        "(ns foo.core (:require [path])) (path/basename \"/foo/bar\")"
        nil
        {:context :expr
         :target :nodejs
         :eval node-eval}
        (fn [{:keys [error value] :as m}]
          (is (nil? error))
          (is (= value "bar"))
          (inc! l)))
      (cljs/analyze-str
        (atom @st)
        "(ns foo.core (:require [path]))"
        nil
        {:context :expr
         :target :nodejs
         :load (fn [_ cb]
                 (cb {:lang   :js
                      :source ""}))}
        (fn [{:keys [error value] :as m}]
          (is (nil? error))
          (is (= (:deps value) '[path]))
          (inc! l)))
      (let [st (cljs/empty-state)]
        (cljs/eval
          st
          '(ns foo.core (:require [path]))
          {:context :expr
           :target :nodejs
           :eval node-eval}
          (fn [{:keys [error value] :as m}]
            (is (nil? error))
            (cljs/eval
              st
              '(path/basename "/foo/bar")
              {:context :expr
               :ns 'foo.core
               :target :nodejs
               :eval node-eval}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= value "bar"))
                (inc! l)))))))))

(deftest test-global-exports-cljs-2243
  (async done
    (let [calculator-load (fn [_ cb]
                            (cb {:lang   :js
                                 :source "global.Calculator = {
    add: function (a, b) {
        return a + b;
    },
    subtract: function (a, b) {
        return a - b;
    }
};"}))
          st (cljs/empty-state)
          l (latch 4 done)]
      (swap! st assoc :js-dependency-index {"calculator" {:global-exports '{calculator Calculator}}})
      (cljs/compile-str
        (atom @st)
        "(ns foo.core (:require [calculator]))"
        nil
        {:context :expr
         :load calculator-load
         :eval node-eval}
        (fn [{:keys [error value] :as m}]
          (is (nil? error))
          (is (some? (re-find #"foo\.core\.global\$module\$calculator = goog.global.Calculator;" value)))
          (inc! l)))
      (cljs/eval-str
        (atom @st)
        "(ns foo.core (:require [calculator])) (calculator/add 1 2)"
        nil
        {:context :expr
         :load calculator-load
         :eval node-eval}
        (fn [{:keys [error value] :as m}]
          (is (nil? error))
          (is (= value 3))
          (inc! l)))
      (cljs/analyze-str
        (atom @st)
        "(ns foo.core (:require [calculator]))"
        nil
        {:context :expr
         :load calculator-load}
        (fn [{:keys [error value] :as m}]
          (is (nil? error))
          (is (= (:deps value) '[calculator]))
          (inc! l)))
      (let [st (atom @st)]
        (cljs/eval
          st
          '(ns foo.core (:require [calculator]))
          {:context :expr
           :load calculator-load
           :eval node-eval}
          (fn [{:keys [error value] :as m}]
            (is (nil? error))
            (cljs/eval
              st
              '(calculator/add 1 2)
              {:context :expr
               :ns 'foo.core
               :eval node-eval}
              (fn [{:keys [error value]}]
                (is (nil? error))
                (is (= value 3))
                (inc! l)))))))))

(deftest test-cljs-2287
  (async done
    (let [st (cljs/empty-state)
          l (latch 2 done)]
      (cljs/eval-str
        (atom @st)
        "(ns foo.core (:require [path]))"
        nil
        {:context :expr
         :target :nodejs
         :def-emits-var true
         :eval identity}
        (fn [{{:keys [source]} :value}]
          (is (some? (re-find #"foo\.core\.node\$module\$path = require\('path'\);\snull;" source)))
          (inc! l)))
      (let [calculator-load (fn [_ cb]
                              (cb {:lang   :js
                                   :source "global.Calculator = {
    add: function (a, b) {
        return a + b;
    },
    subtract: function (a, b) {
        return a - b;
    }
};"}))]
        (swap! st assoc :js-dependency-index {"calculator" {:global-exports '{calculator Calculator}}})
        (cljs/eval-str
          (atom @st)
          "(ns foo.core (:require [calculator])) (calculator/add 1 2)"
          nil
          {:context :expr
           :def-emits-var true
           :load calculator-load
           :eval identity}
          (fn [{{:keys [source]} :value}]
            (is (some? (re-find #"foo\.core\.global\$module\$calculator = goog.global.Calculator;\snull;" source)))
            (inc! l)))))))

(deftest test-cljs-2261
  (async done
    (let [st (cljs/empty-state)
          l  (latch 2 done)]
      (cljs/eval st '(ns bar.core2261a
                       (:require [foo.core2261a :refer-macros [cake]]))
        {:ns      'cljs.user
         :eval    node-eval
         :context :expr
         :load    (fn [{:keys [macros]} cb]
                    (if macros
                      (cb {:lang   :clj
                           :source "(ns foo.core2261a) (defmacro cake [] `(->X))"})
                      (cb {:lang   :clj
                           :source "(ns foo.core2261a) (defrecord X [])"})))}
        (fn [{:keys [error]}]
          (is (nil? error))
          (cljs/eval-str st "(pr-str (cake))" nil
            {:ns      'bar.core2261a
             :eval    node-eval
             :context :expr}
            (fn [{:keys [error value]}]
              (is (nil? error))
              (is (= "#foo.core2261a.X{}" value))
              (inc! l)))))
      (cljs/eval st '(ns bar.core2261b
                       (:require [foo.core2261b :refer-macros [cake]]))
        {:ns      'cljs.user
         :eval    node-eval
         :context :expr
         :load    (fn [{:keys [macros]} cb]
                    (if macros
                      (cb {:lang   :clj
                           :source "(ns foo.core2261b) (defmacro cake [] `(X.))"})
                      (cb {:lang   :clj
                           :source "(ns foo.core2261b) (defrecord X [])"})))}
        (fn [{:keys [error]}]
          (is (nil? error))
          (cljs/eval-str st "(pr-str (cake))" nil
            {:ns      'bar.core2261b
             :eval    node-eval
             :context :expr}
            (fn [{:keys [error value]}]
              (is (nil? error))
              (is (= "#foo.core2261b.X{}" value))
              (inc! l))))))))

(deftest test-cljs-2266
  (async done
    (let [st (cljs/empty-state)
          l  (latch 1 done)]
      (cljs.js/eval-str st "(require 'clojure.x)" nil
        {:eval node-eval
         :load (fn [{:keys [name macros]} cb]
                 (cb (when (and (= name 'cljs.x)
                             (not macros))
                       {:lang   :clj
                        :source "(ns cljs.x)"})))}
        (fn [{:keys [error]}]
          (is (nil? error))
          (inc! l))))))

(deftest test-cljs-2303
  (async done
    (let [st (cljs/empty-state)
          load (fn [{:keys [name macros]} cb]
                 (cb (when (and (= name 'cljs.x)
                             (not macros))
                       {:lang   :clj
                        :source "(ns cljs.x)"})))
          l  (latch 1 done)]
      (cljs.js/eval-str st "(require 'clojure.x)" nil
        {:eval node-eval
         :load load}
        (fn [{:keys [error]}]
          (is (nil? error))
          (cljs.js/eval-str st "(require 'clojure.x)" nil
            {:eval node-eval
             :load load}
            (fn [{:keys [error]}]
              (is (nil? error))
              (inc! l))))))))

(deftest test-cljs-2354
  (async done
    (let [st (cljs/empty-state)
          load (fn [{:keys [name macros]} cb]
                 (cb (when (and (= name 'cljs.x)
                             (not macros))
                       {:lang   :clj
                        :source "(ns cljs.x)"})))
          l  (latch 1 done)]
      (cljs.js/compile-str st "(require 'clojure.x)" nil
        {:load load}
        (fn [{:keys [error value] :as m}]
          (is (nil? error))
          (is (re-find #"goog\.require\('cljs.x'\)" value))
          (inc! l))))))

(deftest test-cljs-2356
  (async done
    (let [st (cljs/empty-state)
          load (fn [{:keys [name macros]} cb]
                 (cb (cond
                       (= name 'circular.a)
                       {:lang   :clj
                        :source "(ns circular.a (:require circular.b))"}

                       (= name 'circular.b)
                       {:lang   :clj
                        :source "(ns circular.b (:require circular.a))"})))
          l  (latch 2 done)]
      (binding [ana/*cljs-dep-set* (with-meta #{} {:dep-path []})]
        (cljs.js/compile-str st "(ns circular.a (:require circular.b))" nil
          {:load load}
          (fn [{:keys [error value] :as m}]
            (is (some? error))
            (is (= "Circular dependency detected circular.a -> circular.b -> circular.a"
                   (.-message error)))
            (inc! l))))
      (binding [ana/*cljs-dep-set* (with-meta #{} {:dep-path []})]
        (cljs.js/eval-str st "(ns circular.a (:require circular.b))" nil
          {:load load
           :eval node-eval}
          (fn [{:keys [error value] :as m}]
            (is (some? error))
            (is (= "Circular dependency detected circular.a -> circular.b -> circular.a"
                   (.-message error)))
            (inc! l)))))))

(deftest test-self-host-self-require
  (async done
    (let [st (cljs/empty-state)
          l (latch 1 done)
          load (fn [{:keys [name macros]} cb]
                 (cb {:lang :clj
                      :source "(ns foo.core)"}))]
      (binding [ana/*cljs-dep-set* (with-meta #{} {:dep-path []})]
        (cljs.js/eval-str st "(ns foo.core)" nil
          {:eval node-eval}
          (fn [{:keys [error value] :as m}]
            (is (nil? error))
            (cljs.js/eval-str st "(require 'foo.core :reload)" nil
              {:load load
               :eval node-eval
               :def-emits-var true
               :ns 'foo.core}
              (fn [{:keys [error value] :as m}]
                (is (nil? error))
                (inc! l)))))))))

(deftest test-cljs-2367
  (async done
    (let [st (cljs/empty-state)
          l  (latch 2 done)]
      (cljs.js/eval st
        '(require (quote foo-2367.core))
        {:context       :expr
         :def-emits-var true
         :eval          node-eval
         :load          (fn [_ cb]
                          (cb {:lang   :clj
                               :source "(ns foo-2367.core) (def b (def a 3))"}))}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (cljs.js/eval st
            'foo-2367.core/b
            {:context :expr
             :eval    node-eval}
            (fn [{:keys [error value]}]
              (is (nil? error))
              (is (= 3 value))
              (inc! l)))))
      (cljs.js/eval st
        '(require-macros (quote bar-2367.core))
        {:context       :expr
         :def-emits-var true
         :eval          node-eval
         :load          (fn [_ cb]
                          (cb {:lang   :clj
                               :source "(ns bar-2367.core) (def b (def a 4)) (defmacro c [] b)"}))}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (cljs.js/eval st
            '(bar-2367.core/c)
            {:context :expr
             :eval    node-eval}
            (fn [{:keys [error value]}]
              (is (nil? error))
              (is (= 4 value))
              (inc! l))))))))

(deftest test-mapping-stacktrace
  (async done
    (let [l (latch 1 done)]
      (testing "it should correctly map from canonical representation (smoke test)."
        (let [st (cljs/empty-state)]
          (cljs/eval-str st
            "(ns cljs.user (:require foo.bar :reload))"
            'cljs.user
            {:source-map true
             :ns 'cljs.user
             :target :nodejs
             :eval node-eval
             :load (fn [{:keys [name]} cb]
                     (cb (when (= name 'foo.bar)
                           {:lang :clj
                            :source "(ns foo.bar)\n(defn broken-first [] (ffirst 0))"
                            :file "foo/bar.cljs"})))}
            (fn [{:keys [ns value error file]}]
              (let [sms (:source-maps @st)]
                (is (= [{:function "broken-first"
                         :file "foo/bar.cljs"
                         :line 2
                         :column 7}]
                       (st/mapped-stacktrace
                        [{:file "foo/bar.js"
                          :function "broken-first"
                          :line 2
                          :column 0}]
                        sms)))
                (inc! l)))))))))

(deftest test-mapping-stacktrace-with-underscore
  (async done
    (let [l (latch 1 done)]
      (testing "it should correctly map when file names contain underscore"
        (let [st (cljs/empty-state)]
          (cljs/eval-str st
            "(ns cljs.user (:require foo.with-underscore :reload))"
            'cljs.user
            {:source-map true
             :ns 'cljs.user
             :target :nodejs
             :eval node-eval
             :load (fn [{:keys [name]} cb]
                     (cb (when (= name 'foo.with-underscore)
                           {:lang :clj
                            :source "(ns foo.with-underscore)\n(defn broken-first [] (ffirst 0))"
                            :file "foo/with_underscore.cljs"})))}
            (fn [{:keys [ns value error file]}]
              (let [sms (:source-maps @st)]
                (is (= [{:function "broken-first"
                         :file "foo/with_underscore.cljs"
                         :line 2
                         :column 7}]
                       (st/mapped-stacktrace
                        [{:file "foo/with_underscore.js"
                          :function "broken-first"
                          :line 2
                          :column 0}]
                        sms)))
                (inc! l)))))))))

(deftest test-append-source-map-with-nil-name
  (async done
    (let [
          l (latch 1 done)]
      (testing "it should correctly use cljs-{js/Date as number} when name to cljs.js/eval-str is nil"
        (let [st (cljs/empty-state)]
          (cljs/eval-str st
            "(ns cljs.user (:require foo.bar :reload))"
            nil
            {:source-map true
             :ns 'cljs.user
             :target :nodejs
             :eval node-eval
             :load (fn [{:keys [name]} cb]
                     (cb (when (= name 'foo.bar)
                           {:lang :clj
                            :source "(ns foo.bar)"
                            :file "foo/bar.cljs"})))}
            (fn [{:keys [ns value error file]}]
              (let [cljs-timestamp? #(let [[c t] (string/split % "-")]
                                       (and (= "cljs" c) (not (js/isNaN (js/parseInt t)))))
                    sms (:source-maps @st)]
                (is (some cljs-timestamp? (keys sms)))
                (inc! l)))))))))

(defn -main [& args]
  (run-tests))

(set! *main-cli-fn* -main)

(comment
  )
