(ns cljs.import-test
  (:import goog.math.Long
           cljs.import-test.foo.Bar
           cljs.import-test.foo.Quux))

(defn test-import []
  (assert (fn? Long))
  (assert (.equals (Long. 4 6) (.add (Long. 1 2) (Long. 3 4))))
  (assert (= "12" (str (Long/fromInt 12))))
  (assert (= 12 (.-x (Bar. 12))))
  (assert (= 12 (.-x (Quux. 12)))))
