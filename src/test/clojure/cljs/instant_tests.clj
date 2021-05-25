;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.instant-tests
  (:require
   [cljs.instant :as inst]
   [clojure.test :refer [deftest is]]))

(deftest read-instant-instant-test
  ;; Clojure uses hybrid Julian / Gregorian, while Instant is proleptic Gregorian
  (is (not= #inst "1500" (inst/read-instant-instant "1500")))
  (is (not= (inst-ms #inst "1500") (inst-ms (inst/read-instant-instant "1500"))))
  (is (= -14831769600000 (inst-ms (inst/read-instant-instant "1500"))))
  (is (= "#inst \"1500-01-01T00:00:00.123456789-00:00\""
        (pr-str (inst/read-instant-instant "1500-01-01T00:00:00.123456789-00:00"))))
  (is (= "#inst \"2020-01-01T05:00:00.000000000-00:00\""
        (pr-str (inst/read-instant-instant "2020-01-01T00:00:00.000-05:00")))))
