;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns static.core-test
  (:require [cljs.test :refer-macros [deftest is]]))

; The purpose of this test namespace is to ensure
; that the use of a reserved JavaScript keyword
; (`static`) in the namespace is handled properly.

(deftest foo-test
  (is (= 1 1)))
