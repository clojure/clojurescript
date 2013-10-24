(ns cljs.ns-test
  (:refer-clojure :exclude [+ for])
  (:require-macros [clojure.core :as lang])
  (:require [cljs.ns-test.foo :refer [baz]]
            [clojure.set :as s])
  (:use [cljs.ns-test.bar :only [quux]]))

(def + -)

(defn test-ns []
  (assert (= 4 (clojure.core/+ 2 1 1)))
  (assert (= 0 (cljs.ns-test/+ 2 1 1)))
  (assert (= 0 (+ 2 1 1)))
  (assert (= 123 (baz)))
  (assert (= 123 (quux)))

  (assert (= (range 5) (lang/for [x (range 5)] x)))
  (assert (= #{1 2 3} (s/union #{1} #{2 3})))
  :ok)
