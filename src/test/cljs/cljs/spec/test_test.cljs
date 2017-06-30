(ns cljs.spec.test-test
  (:require [cljs.test :as test :refer-macros [deftest is are run-tests]]
            [cljs.spec.alpha :as s]
            [cljs.spec.test.alpha :as stest]))

(defn h-cljs-1812 [x] true)
(s/fdef h-cljs-1812 :args (s/cat :x int?) :ret true?)

(deftest test-cljs-1812
  (is (= (stest/unstrument `h-cljs-1812)
        []))

  (stest/check `h-cljs-1812 {:clojure.test.check/opts {:num-tests 1}})

  ; Calling h-cljs-1812 with an argument of the wrong type shouldn't throw,
  ; because the function should not have been instrumented by stest/check.
  (h-cljs-1812 "foo"))

;; Setup for CLJS-2142
(def ^:const pi 3.14159)
(defn area [r] (* pi r r))
(s/fdef area :args (s/cat :r number?))

(deftest test-cljs-2142
  (is (= `[area] (stest/instrument `[pi area]))))
