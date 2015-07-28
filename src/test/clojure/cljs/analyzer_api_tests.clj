(ns cljs.analyzer-api-tests
  (:require [cljs.analyzer.api :as ana-api])
  (:use clojure.test))

(deftest cljs-warning-test
  (is (ana-api/warning-enabled? :undeclared-var)
      "Undeclared-var warning is enabled by default")
  (is (not (ana-api/no-warn (ana-api/warning-enabled? :undeclared-var)))
      "Disabled when all warnings are disabled"))

(def warning-form
  '(do (defn x [a b] (+ a b))
       (x 1 2 3 4)))

(defn warning-handler [counter]
  (fn [warning-type env extra]
    (when (ana-api/warning-enabled? warning-type)
      (swap! counter inc))))

(def test-cenv (atom {}))
(def test-env (ana-api/empty-env))

(deftest with-warning-handlers-test
  (let [counter (atom 0)]
    (ana-api/analyze test-cenv test-env warning-form nil
                     {:warning-handlers [(warning-handler counter)]})
    (is (= 1 @counter))))

(deftest vary-warning-handlers-test
  (let [counter (atom 0)]
    (cljs.analyzer/all-warn
      (ana-api/analyze test-cenv test-env warning-form nil
                       {:warning-handlers [(warning-handler counter)]}))
    (is (= 1 @counter))))
