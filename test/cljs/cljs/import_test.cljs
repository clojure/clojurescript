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
