;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.source-map.base64-tests
  (:require
    [clojure.test :refer [deftest is]]
    [cljs.source-map.base64 :as base64]))

(deftest encode-test
  (doseq [n (range 64)]
    (is (= (get base64/int->char n) (base64/encode n))))
  (is (thrown-with-msg? Error #"Must be between 0 and 63: 64" (base64/encode 64))))
