;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.letfn-test
  (:require [cljs.test :refer-macros [deftest is]]))

(deftest test-letfn
  (letfn [(ev? [x]
            (if (zero? x)
              true
              (od? (dec x))))
          (od? [x]
            (if (zero? x)
              false
              (ev? (dec x))))]
    (is (ev? 0))
    (is (ev? 10))
    (is (not (ev? 1)))
    (is (not (ev? 11)))
    (is (not (od? 0)))
    (is (not (od? 10)))
    (is (od? 1))
    (is (od? 11))))
