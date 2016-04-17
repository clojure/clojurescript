(ns cljs.syntax-quote-test
  (:require [cljs.test :as test :refer-macros [deftest is]]))

(deftest test-syntax-quote
  (is (= 'cljs.syntax-quote-test/foo `foo))
  (is (= 'cljs.test/test-vars `test/test-vars))
  (is (= 'cljs.test/deftest `test/deftest))
  (is (= 'cljs.test/foo `test/foo)))
