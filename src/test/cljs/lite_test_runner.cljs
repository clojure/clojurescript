;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns lite-test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [cljs.apply-test]
            [cljs.primitives-test]
            [cljs.destructuring-test]
            [cljs.new-new-test]
            [cljs.printing-test]
            [cljs.seqs-test]
            ;; [cljs.collections-test]
            [cljs.hashing-test]
            [cljs.core-test]
            ;; [cljs.chunked-seq] ;; doesn't exist in :lite-mode
            [cljs.interop-test]
            [cljs.iterator-test]
            [cljs.reader-test]
            [cljs.binding-test]
            [cljs.parse-test]
            [cljs.ns-test]
            [clojure.set-test]
            [clojure.string-test]
            [clojure.data-test]
            [clojure.datafy-test]
            [clojure.edn-test]
            [clojure.walk-test]
            [clojure.math-test]
            [cljs.macro-test]
            [cljs.letfn-test]
            [foo.ns-shadow-test]
            [cljs.top-level]
            [cljs.reducers-test]
            [cljs.keyword-test]
            [cljs.import-test]
            [cljs.ns-test.foo]
            [cljs.syntax-quote-test]
            [cljs.pprint]
            [cljs.pprint-test]
            [cljs.spec-test]
            [cljs.specials-test]
            [cljs.spec.test-test]
            [cljs.clojure-alias-test]
            ;; [cljs.hash-map-test]
            ;; [cljs.map-entry-test]
            ;; [cljs.metadata-test]
            ;; [cljs.npm-deps-test]
            [cljs.other-functions-test]
            [cljs.predicates-test]
            ;; [cljs.tagged-literals-test]
            [cljs.test-test]
            [static.core-test]
            [cljs.recur-test]
            [cljs.array-access-test]
            [cljs.inference-test]
            [cljs.walk-test]
            [cljs.repl-test]
            ;; [cljs.extend-to-native-test]
            [cljs.var-test]
            [cljs.lite-collections-test]))

(set! *print-newline* false)

;; When testing Windows we default to Node.js
(if (exists? js/print)
  (set-print-fn! js/print)
  (enable-console-print!))

(run-tests
  'cljs.apply-test
  'cljs.primitives-test
  'cljs.destructuring-test
  'cljs.printing-test
  'cljs.new-new-test
  'cljs.seqs-test
  'cljs.hashing-test
  'cljs.interop-test ;; ES6 stuff
  #_'cljs.iterator-test
  'cljs.reader-test
  'cljs.binding-test
  'cljs.parse-test
  'cljs.ns-test
  'clojure.set-test
  'clojure.string-test
  #_'clojure.data-test ;; 1 failure #object[Error Error: No item 1 in vector of length 1]
  'clojure.datafy-test
  'clojure.edn-test
  'clojure.walk-test
  'clojure.math-test
  'cljs.macro-test
  'cljs.letfn-test
  'foo.ns-shadow-test
  'cljs.top-level
  'cljs.reducers-test ;; missing IReduce, IKVReduce
  'cljs.keyword-test
  'cljs.import-test
  'cljs.ns-test.foo
  'cljs.syntax-quote-test
  'cljs.pprint
  'cljs.pprint-test
  #_'cljs.spec-test ;; this one is strange
  'cljs.specials-test
  'cljs.spec.test-test
  'cljs.clojure-alias-test
  #_'cljs.metadata-test ;; another RSeq one
  #_'cljs.npm-deps-test
  'cljs.other-functions-test
  'cljs.predicates-test
  #_'cljs.tagged-literals-test ;; duplicate keys in ObjMap/HashMap
  'cljs.test-test
  'static.core-test
  'cljs.recur-test
  'cljs.array-access-test
  'cljs.inference-test
  'cljs.walk-test
  'cljs.repl-test
  #_'cljs.extend-to-native-test ;; this one breaks a bunchof things
  'cljs.var-test
  'cljs.core-test
  'cljs.lite-collections-test
  )
