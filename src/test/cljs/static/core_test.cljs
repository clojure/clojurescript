(ns static.core-test
  (:require [cljs.test :refer-macros [deftest is]]))

; The purpose of this test namespace is to ensure
; that the use of a reserved JavaScript keyword
; (`static`) in the namespace is handled properly.

(deftest foo-test
  (is (= 1 1)))
