;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.import-test
  (:require [cljs.test :refer-macros [deftest is]])
  (:import goog.math.Long
           [goog.math Vec2 Vec3]
           [goog.math Integer]))

(deftest test-import
  (is (fn? Long))
  (is (.equals (Long. 4 6) (.add (Long. 1 2) (Long. 3 4))))
  (is (= "12" (str (Long.fromInt 12))))
  (is (not (nil? (Vec2. 1 2))))
  (is (not (nil? (Vec3. 1 2 3))))
  (is (.equals (Integer.fromString "10") (goog.math.Integer.fromString "10"))))
