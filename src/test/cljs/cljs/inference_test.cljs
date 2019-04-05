;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.inference-test
  (:require-macros [cljs.inference-util])
  (:require [cljs.test :refer [deftest is]]
            [cljs.pprint]))

(deftest test-cljs-2825
  (cljs.inference-util/truth_-not-called?
    (not nil)
    (object? nil)
    (char? nil)
    (any? nil)
    (native-satisfies? ICounted nil)
    (var? nil)
    (iterable? nil)
    (cloneable? nil)
    (inst? nil)
    (reduced? nil)
    (counted? nil)
    (indexed? nil)
    (fn? nil)
    (empty? nil)
    (coll? nil)
    (set? nil)
    (associative? nil)
    (ifind? nil)
    (sequential? nil)
    (sorted? nil)
    (reduceable? nil)
    (map? nil)
    (record? nil)
    (vector? nil)
    (chunked-seq? nil)
    (boolean? nil)
    (seq? nil)
    (seqable? nil)
    (boolean nil)
    (ifn? nil)
    (integer? nil)
    (int? nil)
    (pos-int? nil)
    (nat-int? nil)
    (float? nil)
    (double? nil)
    (infinite? nil)
    (contains? [] nil)
    (list? nil)
    (reversible? nil)
    (keyword? nil)
    (keyword-identical? :a :a)
    (symbol-identical? 'a 'a)
    (ident? nil)
    (simple-ident? nil)
    (qualified-ident? nil)
    (simple-symbol? nil)
    (qualified-symbol? nil)
    (simple-keyword? nil)
    (qualified-keyword? nil)
    (every? any? [])
    (not-every? any? [])
    (not-any? any? [])
    (even? 0)
    (odd? 0)
    (volatile? nil)
    (equiv-map {} {})
    (map-entry? nil)
    (key-test :a :a)
    (regexp? nil)
    (print-meta? {} nil)
    (delay? nil)
    (uuid? nil)
    (tagged-literal? nil)
    (cljs.pprint/float? nil)))

(deftest cljs-2866-test
  ;; Here we are testing that in the JavaScript emitted,
  ;; the gensym generated for curr is being passed to dec
  (is (zero? ((fn [x]
                (while (pos? @x)
                  (let [curr @x]
                    (when (number? curr)
                      (reset! x (dec curr)))))
                @x) (atom 1)))))

(def foo-var-3068 (merge))

(deftest cljs-3068-test
  ;; Here we are essentially testing that valid JavaScript is
  ;; emitted. Without the fix, tests fail with invalid JavaScript
  ;; that cannot be parse by GCC, etc.
  (if foo-var-3068 foo-var-3068)
  (is true))
