;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.invoke-test
  (:require [goog.string :as gstr]))

(defn variadic-fn [& args])

(variadic-fn 1 2 3)

(defn multi-fn
  ([a] a)
  ([a b] a))

(defn hof-fn-expr-should-be-bound
  [funexpr0 normal-arg]
  ((complement funexpr0) normal-arg))

(defn hof-arg-should-be-bound
  [hofinvoke inv-arg0]
  (hofinvoke (inv-arg0)))

(defn hof-fn-expr+args-should-be-bound
  [funexpr1 inv-arg1]
  ((complement funexpr1) (inv-arg1)))

;; A keyword should not be bound in a let:
(def foo (delay
           (:dont-bind-this js/x)))

(multi-fn 2)

(gstr/urlEncode "foo")

(js/goog.string.urlDecode "bar")

(declare ^{:arglists '([a b])} declared-fn)

(declared-fn 1 2)

(defrecord Foo [foo-field-a foo-field-b])

(def foo-record (->Foo 1 2))

(:foo-field-a foo-record)
