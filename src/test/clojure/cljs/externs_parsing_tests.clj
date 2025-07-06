;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.externs-parsing-tests
  (:require [cljs.closure :as closure]
            [cljs.analyzer :as ana]
            [cljs.env :as env]
            [cljs.externs :as externs]
            [clojure.java.io :as io]
            [clojure.test :as test :refer [deftest is testing]])
  (:import [com.google.javascript.jscomp CommandLineRunner]))

(deftest cljs-3121
  (let [externs (externs/parse-externs
                  (closure/js-source-file "goog/string/string.js"
                    (io/input-stream (io/resource "goog/string/string.js"))))]
    (is (every?
          (fn [xs]
            (= (count (distinct xs))
               (count xs)))
          externs))))

(deftest cljs-3176
  (let [ns (externs/analyze-goog-file "goog/date/date.js")
        v  (get-in ns [:defs 'getWeekNumber])]
    (is (= 3 (-> v :method-params first count))))
  (let [ns (externs/analyze-goog-file "goog/date/date.js" 'goog.date.month)]
    (is (= 13 (-> ns :defs count)))))

(deftest cljs-3170&3189
  (let [ns (externs/analyze-goog-file "goog/object/object.js")]
    (is (= 'any (get-in ns [:defs 'get :ret-tag])))
    (is (= 'array (get-in ns [:defs 'getKeys :ret-tag])))))

(comment
  ;; works
  (get-in (externs/analyze-goog-file "goog/object/object.js")
    [:defs 'containsKey :ret-tag])
  )

(deftest test-parse-super
  (let [info (->
               (filter
                 (fn [s]
                   (= "externs.zip//w3c_dom2.js" (.getName s)))
                 (externs/default-externs))
               first externs/parse-externs externs/index-externs
               (find 'HTMLDocument) first meta)]
    (is (= 'Document (:super info)))))

(deftest test-parse-closure-type-annotations
  (let [externs (::ana/externs @(env/default-compiler-env))]
    (testing "JS global console has tag Console"
      (let [info (externs/info externs '[console])]
        (is (= 'Console (:tag info)))))
    (testing "JS global crypto has tag webCrypto.Crypto from:
    @type {!webCrypto.Crypto|undefined}"
      (let [info (externs/info externs '[crypto])]
        (is (= 'webCrypto.Crypto (:tag info)))))
    (testing "Generic return type on crypto methods returns ClojureScript relevant
    type info:"
      (testing "@return {!Promise<!ArrayBuffer>}"
        (let [info (externs/info externs '[webCrypto SubtleCrypto prototype encrypt])]
          (is (= 'Promise (:ret-tag info)))))
      (testing "@return {!Promise<!webCrypto.CryptoKey|!webCrypto.CryptoKeyPair>}"
        (let [info (externs/info externs '[webCrypto SubtleCrypto prototype deriveKey])]
          (is (= 'Promise (:ret-tag info)))))
      (testing "@return {!Int8Array|!Uint8Array|!Uint8ClampedArray|!Int16Array|!Uint16Array|!Int32Array|!Uint32Array|!BigInt64Array|!BigUint64Array}"
        (let [info (externs/info externs '[webCrypto Crypto prototype getRandomValues])]
          (is (= 'any (:ret-tag info))))))))

(comment

  (let [externs (::ana/externs @(env/default-compiler-env))]
    (externs/info externs '[webCrypto Crypto prototype getRandomValues]))

  (externs/parse-externs
    (externs/resource->source-file (io/resource "goog/object/object.js")))

  (externs/analyze-goog-file "goog/object/object.js")

  (test/run-tests)

  (externs/analyze-goog-file "goog/date/date.js" 'goog.date.month)

  )
