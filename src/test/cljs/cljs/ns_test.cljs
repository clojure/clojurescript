(ns cljs.ns-test
  (:refer-clojure :exclude [+ for] :rename {mapv core-mapv})
  (:require-macros [clojure.core :as lang :refer [when when-let] :rename {when always
                                                                          when-let always-let}]
                   [cljs.test :refer [deftest is]])
  (:require [cljs.test]
            [cljs.ns-test.foo :refer [baz]]
            [clojure.set :as s :refer [intersection] :rename {intersection itsc}])
  (:use [cljs.ns-test.bar :only [quux]]))

(def + -)

(deftest test-ns
  (is (= 4 (clojure.core/+ 2 1 1)))
  (is (= 0 (cljs.ns-test/+ 2 1 1)))
  (is (= 0 (+ 2 1 1)))
  (is (= 123 (baz)))
  (is (= 123 (quux)))

  (is (= (range 5) (lang/for [x (range 5)] x)))
  (is (= #{1 2 3} (s/union #{1} #{2 3}))))

(deftest test-cljs-1508
  (is (= (itsc #{1 2 3} #{2}) #{2}))
  (is (= #'itsc #'clojure.set/intersection))
  (is (= itsc clojure.set/intersection))
  (is (= (always true 42) 42))
  (is (= (core-mapv inc [1 2]) [2 3]))
  (is (= (always-let [foo 42] foo) 42)))
