(ns cljs.compiler-test
  (:require [clojure.test :as t]
            [cljs.compiler :as c]))

(t/deftest test-exclude-file-names
  (t/are [x y] (= x y)
       nil (c/exclude-file-names nil nil)
       nil (c/exclude-file-names nil [])
       nil (c/exclude-file-names nil [""])))
