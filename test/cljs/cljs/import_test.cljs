(ns cljs.import-test
  (:import goog.math.Long
           [goog.math Vec2 Vec3]
           [goog.math Integer]))

(defn test-import []
  (assert (fn? Long))
  (assert (.equals (Long. 4 6) (.add (Long. 1 2) (Long. 3 4))))
  (assert (= "12" (str (Long.fromInt 12))))
  (assert (not (nil? (Vec2. 1 2))))
  (assert (not (nil? (Vec3. 1 2 3))))
  (assert (.equals (Integer.fromString "10") (goog.math.Integer.fromString "10"))))
