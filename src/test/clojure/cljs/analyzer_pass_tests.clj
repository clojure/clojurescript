;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer-pass-tests
  (:require [cljs.analyzer.api :as ana-api]
            [cljs.analyzer-tests :as ana-tests :refer [analyze]]
            [clojure.test :as test :refer [deftest is testing]]))

(def simple-ops
  #{:var :js-var :local :invoke :const :host-field :host-call :js :quote})

(defn simple-op? [ast]
  (contains? simple-ops (:op ast)))

(defn simple-test-expr?
  ([ast]
   (simple-test-expr? (ana-api/empty-env) ast))
  ([env ast]
   (boolean
     (and (simple-ops (:op ast))
          ('#{boolean seq} (cljs.analyzer/infer-tag env ast))))))

(defn simple-and? [ast]
  (and (= :let (:op ast))
       (= 1 (count (-> ast :bindings)))
       (= [] (-> ast :body :statements))
       (= :if (-> ast :body :ret :op))))

(defn simple-or? [ast]
  (and (= :let (:op ast))
       (= 1 (count (-> ast :bindings)))
       (= [] (-> ast :body :statements))
       (= :if (-> ast :body :ret :op))))

(deftest test-helpers
  (testing "Testing and/or optimization helpers"
    (let [ast (analyze (ana-api/empty-env) `(and true false))]
      (is (simple-op? (-> ast :bindings first :init))))
    (let [ast (analyze (ana-api/empty-env) `(and true false))]
      (simple-test-expr? (-> ast :bindings first :init)))))

(comment
  (test/run-tests)

  (let [ast (analyze (ana-api/empty-env)
              `(and true false))]
    (-> ast :body :ret :op))

  (let [ast (analyze (ana-api/empty-env)
              `(and true false))]
    (-> ast :body :ret :env :locals))

  (let [ast (analyze (ana-api/empty-env)
              `(or true false))]
    (-> ast :body :ret))

  )
