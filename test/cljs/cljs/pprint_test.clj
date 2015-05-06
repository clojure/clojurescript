(ns cljs.pprint-test
  (:require [cljs.test :refer [deftest is]]))

(defmacro simple-tests [name & test-pairs]
  `(deftest ~name
     ~@(for [[x y] (partition 2 test-pairs)]
         `(cond
            (cljs.core/regexp? ~y) (is (.exec ~y ~x))
            (cljs.core/string? ~y) (is (= ~x ~y))
            :else (is (= ~x ~y))))))

(defmacro code-block
  "Read a string then print it with code-dispatch and succeed if it comes out the same"
  [test-name & blocks]
  `(simple-tests ~test-name
     ~@(apply concat
              (for [block blocks]
                `[(clojure.string/split-lines
                    (with-out-str
                      (cljs.pprint/with-pprint-dispatch cljs.pprint/code-dispatch
                                            (cljs.pprint/pprint (cljs.reader/read-string ~block)))))
                  (clojure.string/split-lines ~block)]))))

