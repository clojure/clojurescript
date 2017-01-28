(ns cljs.test-test
  (:require [cljs.test :refer-macros [deftest testing is] :as ct]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn- nan?
  [x]
  (and (number? x)
       (js/isNaN x)))

(deftest js-line-and-column-test
  (is (= [2 3] (ct/js-line-and-column "foo:bar:2:3")))
  (is (= [2 3] (ct/js-line-and-column "foo:2:3")))
  (is (= [2 3] (ct/js-line-and-column "2:3")))
  (let [[line column] (ct/js-line-and-column "foo:bogus:3")]
    (is (nan? line))
    (is (== 3 column)))
  (let [[line column] (ct/js-line-and-column "foo:2:bogus")]
    (is (== 2 line))
    (is (nan? column)))
  (let [[line column] (ct/js-line-and-column "foo:bogus:bogus")]
    (is (nan? line))
    (is (nan? column)))
  (let [[line column] (ct/js-line-and-column "foo:3")]
    (is (nan? line))
    (is (== 3 column)))
  (let [[line column] (ct/js-line-and-column "foo")]
    (is (nan? line))
    (is (nan? column))))
