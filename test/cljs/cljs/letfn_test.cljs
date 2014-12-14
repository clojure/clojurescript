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
