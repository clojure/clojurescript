;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.compiler-tests
  (:use clojure.test)
  (:require [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.compiler.api :as comp-api]
            [cljs.env :as env]
            [cljs.util :as util]
            [cljs.tagged-literals :as tags]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]))

(defn analyze
  ([env form]
   (env/ensure (ana/analyze env form)))
  ([env form name]
   (env/ensure (ana/analyze env form name)))
  ([env form name opts]
   (env/ensure (ana/analyze env form name opts))))

(defn emit [ast]
  (env/ensure (comp/emit ast)))

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
             (analyze aenv
               '(defn foo []
                  (fn bar [])))
             [:init :name]))
         'cljs$user$foo))
  (is (= (comp/munge
           (get-in
             (analyze aenv
               '(defn foo []
                  (fn bar [])))
             [:init :methods 0 :body :ret :local]))
          'cljs$user$foo_$_bar))
  (is (= (comp/munge
           (get-in
             (analyze aenv
               '(fn []
                  (fn console [])))
             [:methods 0 :body :ret :local]))
         'cljs$user$console)))

(deftest test-js-negative-infinity
  (is (= (with-out-str
           (emit
             (analyze (assoc aenv :context :expr) 'js/-Infinity)))
          "-Infinity")))

(deftest test-cljs-2352
  (are [form result]
      (= (with-out-str
           (emit
             (analyze (assoc aenv :context :expr) form)))
         result)
    Double/NaN "NaN"
    Double/POSITIVE_INFINITY "Infinity"
    Double/NEGATIVE_INFINITY "-Infinity"))

(deftest test-munge-dotdot
  (is (= 'cljs.core._DOT__DOT_ (comp/munge 'cljs.core/..)))
  (is (= "cljs.core._DOT__DOT_" (comp/munge "cljs.core/..")))
  (is (= 'cljs.core._DOT__DOT_
         (ana/no-warn
           (env/with-compiler-env cenv
             (comp/munge
               (:info (analyze {:ns {:name 'cljs.core}} 'cljs.core/..))))))))

(deftest test-resolve-dotdot
  (is (= '{:name cljs.core/..
           :ns   cljs.core}
         (ana/no-warn
           (env/with-compiler-env cenv
             (select-keys
               (ana/resolve-var {:ns {:name 'cljs.core}} '..)
               [:name :ns]))))))

(deftest test-cljs-428
  (letfn [(check-docs [docs]
            (is (= 1 (count (re-seq #"\*/" docs)))))]
    (check-docs (with-out-str
                  (env/ensure
                    (comp/emit-comment "/* multiline comments */" nil))))
    (check-docs (with-out-str
                  (emit
                    (analyze aenv
                      '(defn foo "foo is */ like this /*/" [] (+ 1 1))))))))

(comment
  (env/with-compiler-env cenv
    (emit
      (analyze aenv
        '(defn foo ([a]) ([a b])))))
  )

(defn capture-warnings* [f]
  (let [capture (atom [])
        tracker (fn [warning-type env & [extra]]
                  (when (warning-type ana/*cljs-warnings*)
                    (let [err (ana/error-message warning-type extra)
                          msg (ana/message env (str "WARNING: " err))]
                      (swap! capture conj [warning-type msg]))))]
    (ana/with-warning-handlers [tracker]
      (f))
    @capture))

(defmacro capture-warnings [& body]
  `(capture-warnings* (fn [] ~@body)))

(deftest or-doesnt-create-bindings
  (let [cenv (atom @cenv)]
    (binding [ana/*cljs-static-fns* true
              ana/*analyze-deps* false]
      (env/with-compiler-env cenv
        (ana/analyze-file (File. "src/main/cljs/cljs/core.cljs"))
        (let [warnings (-> (capture-warnings
                             (with-out-str
                               (emit
                                 (analyze aenv
                                   '(let [{:keys [a] :or {b 2}} {:a 1}] [a b]))))))]
          (is (= (ffirst warnings) :undeclared-var))
          (is (.startsWith (-> warnings first second)
                "WARNING: Use of undeclared Var cljs.user/b")))))))

(deftest no-warn-on-emit-invoke-protocol-method
  (let [define-foo #(assoc-in % [::ana/namespaces 'cljs.user :defs 'foo]
                              {:ns 'cljs.user
                               :name 'cljs.user/foo
                               :fn-var true
                               :method-params '([x])
                               :protocol 'cljs.user/Foo})
        aenv-with-foo (define-foo aenv)
        cenv-with-foo (define-foo @cenv)]
    (binding [ana/*cljs-static-fns* true]
      (are [form]
        (empty?
         (capture-warnings
          (env/with-compiler-env (atom cenv-with-foo)
            (with-out-str
              (emit
               (analyze aenv-with-foo form))))))

        '(cljs.user/foo nil)
        '(cljs.user/foo 0)
        '(cljs.user/foo (inc 0))
        '(cljs.user/foo "")
        '(cljs.user/foo true)
        '(cljs.user/foo false)
        '(cljs.user/foo (nil? nil))
        '(cljs.user/foo (fn [x] x))
        `(cljs.user/foo ~(tags/->JSValue {}))
        `(cljs.user/foo ~(tags/->JSValue []))
        '(cljs.user/foo (make-array 0))))))

(deftest test-cljs-1643
  (is (thrown-with-msg? Exception #"is not a valid ClojureScript constant."
        (comp/emit-constant clojure.core/inc))))

(def test-cljs-1925-code
  '(do
     (defprotocol X
       (x [x]))

     (defprotocol Y
       (y [y]))

     (extend-protocol X
       js/RegExp
       (x [x]
         (y x)))

     (extend-protocol Y
       js/RegExp
       (y [y]
         :y))))

(def specify-test-code
  '(do
     (defprotocol IBug
       (bug [this other] "A sample protocol"))

     (defn MyBug [])
     (specify! (.-prototype MyBug)
       IBug
       (bug [this other]
         "bug")
       Object
       (foo [this]
         (bug this 3)))))

(deftest test-cljs-1925
  (let [opts {:static-fns true}
        cenv (env/default-compiler-env opts)]
    (is (= [] (binding [ana/*unchecked-if* false
                        ana/*cljs-static-fns* true]
                (capture-warnings
                  (env/with-compiler-env cenv
                    (with-out-str
                      (emit
                        (comp/with-core-cljs
                          opts
                          (fn [] (analyze aenv test-cljs-1925-code nil opts)))))))))))
  (let [opts {:static-fns true}
        cenv (env/default-compiler-env opts)]
    (is (= [] (binding [ana/*unchecked-if* false
                        ana/*cljs-static-fns* true]
                (capture-warnings
                  (env/with-compiler-env cenv
                    (with-out-str
                      (emit
                        (comp/with-core-cljs
                          opts
                          (fn [] (analyze aenv specify-test-code nil opts))))))))))))


(deftest test-optimized-invoke-emit
  (let [out-file
        (io/file "target/invoke_test.js")]
    (comp-api/with-core-cljs
      (comp-api/compile-file
        (io/file "src/test/cljs/cljs/invoke_test.cljs")
        out-file
        {:static-fns true}))

    (let [content (slurp out-file)]
      ;; test for fn( not fn.call(, omitting arguments in test because they are not relevant
      ;; should emit variadic invokes
      (is (str/includes? content "cljs.invoke_test.variadic_fn.cljs$core$IFn$_invoke$arity$variadic("))
      ;; should emit optimized invokes
      (is (str/includes? content "cljs.invoke_test.multi_fn.cljs$core$IFn$_invoke$arity$1("))
      ;; closure js code must never use .call(
      (is (str/includes? content "goog.string.urlEncode("))
      ;; js/goog.string.urlDecode should not use .call
      (is (str/includes? content "goog.string.urlDecode("))
      ;; We should NOT emit a let binding for simple (:dont-bind-this js/x)
      (is (str/includes? content
                         (str "new cljs.core.Keyword(null,\"dont-bind-this\",\"dont-bind-this\","
                              "-140451389).cljs$core$IFn$_invoke$arity$1(x);")))
      ;; CLJS-2046: Emit bindings for expressions like: (@m a0) or ((:x m) a0)
      ;; The test: ((complement funexpr0) normal-arg)
      (is (re-find #"(?m)^.*var fexpr.*=.*cljs.core.complement\(funexpr0\);$"
                   content))
      ;; CLJS-855: Emit binding for expressions like:
      ;; (hofinvoke (inv-arg0))
      (is (re-find #"(?m)^.*var .*=.*inv_arg0.cljs.core.IFn._invoke.arity.0 \?.*$"
                   content))

      ;; Now test both (855,2046) together:
      ;; ((complement funexpr1) (inv-arg1))
      (is (re-find #"(?m)^.*var fexpr.*=.*cljs.core.complement\(funexpr1\);$"
                   content))
      (is (re-find #"(?m)^.*var .*=.*inv_arg1.cljs.core.IFn._invoke.arity.0 \?.*$"
                   content))
      ;; CLJS-1871: A declare hinted with :arglists meta should result in static dispatch
      (is (str/includes? content "cljs.invoke_test.declared_fn("))
      ;; CLJS-2950: Direct field access for keyword lookup on records
      (is (str/includes? content "cljs.invoke_test.foo_record.foo_field_a;")))))
#_(test-vars [#'test-optimized-invoke-emit])

(deftest test-cljs-3077
  (let [opts {}
        cenv (env/default-compiler-env opts)

        test-compile
        (fn [code]
          (env/with-compiler-env cenv
            (with-out-str
              (emit
                (comp/with-core-cljs
                  opts
                  (fn [] (analyze aenv code nil opts)))))))

        snippet1
        (test-compile
          '(defn wrapper1 [foo]
             (let [x 1]
               (prn (fn inner [] foo))
               (recur (inc foo)))))

        snippet2
        (test-compile
          '(defn wrapper2 [foo]
             (loop [x 1]
               (prn (fn inner [] x))
               (recur (inc x))
               )))

        snippet3
        (test-compile
          '(defn no-wrapper1 [foo]
             (let [x 1]
               (prn (fn inner [] foo)))))]

    ;; FIXME: not exactly a clean way to test if function wrappers are created or not
    ;; captures foo,x
    (is (str/includes? snippet1 "(function (foo,x){"))
    ;; captures x
    (is (str/includes? snippet2 "(function (x){"))
    ;; no capture, no loop or recur
    (is (not (str/includes? snippet3 "(function (foo,x){")))
    (is (not (str/includes? snippet3 "(function (foo){")))
    (is (not (str/includes? snippet3 "(function (x){")))
    ))

;; CLJS-1225

(comment
  (binding [ana/*cljs-static-fns* true]
    (env/with-compiler-env cenv
      (emit
        (analyze aenv
          '(defn incme []
             (let [incme (fn [a queue & args])]
               (println (incme 1 [1] 1 1))))))))
  )

(comment
  ;; combining boolean hint w/ static fns

  (binding [ana/*cljs-static-fns* true]
    (env/with-compiler-env cenv
      (emit
        (analyze aenv
          '(defn foo [x]
             (if ^boolean (goog.array/isEmpty x)
               true
               false))))))
  )
