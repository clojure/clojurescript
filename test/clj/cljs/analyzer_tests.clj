(ns cljs.analyzer-tests
  (:require [cljs.analyzer :as a])
  (:use clojure.test))

;;******************************************************************************
;;  cljs-warnings tests
;;******************************************************************************

(defn make-counter []
  (let [counter (atom 0)]
    {:counter counter
     :f (fn [& args]
          (swap! counter inc))}))

(def warning-forms
  {:undeclared-var (let [v (gensym)] `(~v 1 2 3))
   :fn-arity '(do (defn x [a b] (+ a b))
                  (x 1 2 3 4))})

(defn warn-count [form]
  (let [{:keys [counter f]} (make-counter)
        tracker (fn [warning-type env & [extra]]
                  (println "Warning: " warning-type)
                  (println "\tenabled? " (warning-type a/*cljs-warnings*)))]
    (a/with-warning-handlers [f]
      (a/analyze (a/empty-env) form))
    @counter))

(deftest no-warn
  (is (every? zero? (map (fn [[name form]] (a/no-warn (warn-count form))) warning-forms))))

(deftest all-warn
  (is (every? #(= 1 %) (map (fn [[name form]] (a/all-warn (warn-count form))) warning-forms))))
