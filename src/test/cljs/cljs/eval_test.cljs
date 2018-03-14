(ns cljs.eval-test
  (:require [cljs.test :refer [deftest is]]))

;;; This test namespace should only be loaded by environments that set up cljs.core/*eval*

(def addition-list-1 (list + 1 2))
(def addition-list-2 (list + 1 'a))
(def addition-list-3 (list (fn [a b] (+ a b)) 1 2))
(defn square [x] (* x x))
(defn cube [x] (* x x x))

(deftest test-eval
  (is (== 1 (eval 1)))
  (is (== 3 (eval '(+ 1 2))))
  (is (== 17 (eval '(let [a 10] (+ 3 4 a)))))
  (is (= 'a (:name (meta (eval '(def a 3))))))
  (is (== 3 (eval 'a)))
  (is (== 3 (eval addition-list-1)))
  (is (== 4 (eval addition-list-2)))
  (is (== 13 (eval (concat addition-list-1 [10]))))
  (is (= 'lucky-number (:name (meta (eval (list 'def 'lucky-number (concat addition-list-1 [20])))))))
  (is (== 23 (eval 'lucky-number)))
  (is (== 64 ((eval (list comp square cube)) 2)))
  (is (== 5 ((eval (eval +)) 2 3)))
  (is (== 3 (eval addition-list-3)))
  (is (== 4 (eval (list #'inc 3)))))
