;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.apply-test
  (:require [clojure.test :refer [deftest is]]))

(defn fn-returning-this
  [x]
  (this-as this
    this))

(deftest js-fns-test
  (is (= 1 (apply js/parseInt ["1"])))
  (is (= 1 (apply js/parseInt "1" nil)))
  (is (= 1 (apply js/parseInt "1" [])))
  (is (= 15 (apply js/parseInt "F" [16])))
  (is (identical? fn-returning-this (apply fn-returning-this [0]))
      "apply should supply the this object to be the function itself"))

(deftest data-structures-test
  (is (= 1 (apply #{1} [1])))
  (is (= nil (apply #{1} [2])))
  (is (= 1 (apply #{1} 1 [2])))
  (is (= 2 (apply #{} 1 [2])))
  (is (thrown? js/Error (apply #{} []))
      "We should still get wrong arity errors"))

(def meta-f (with-meta (fn [& a] a) {}))

;; more data structure test:
(deftest meta-fn-test
  (is (= nil (apply meta-f [])))
  (is (= '(1)) (apply meta-f [1]))
  (is (= '(1)) (apply meta-f 1 []))
  (is (= '(1 2)) (apply meta-f 1 2 []))
  (is (= '(1 2 3)) (apply meta-f 1 2 3 []))
  (is (= '(1 2 3 4)) (apply meta-f 1 2 3 4 []))
  (is (= '(1 2 3 4 5)) (apply meta-f 1 2 3 4 5 []))
  (is (= (range 1 8)) (apply meta-f 1 2 3 4 5 [6 7]))
  (is (= (range 21) (apply meta-f (range 21)))
        "Should properly call the last IFn arity with 20 args with last being a seq")
  (is (= (range 22) (apply meta-f (range 22)))
        "Should properly call the last IFn arity with 20 args with last being a seq")
  (is (= (range 22) (.apply meta-f nil (to-array (range 22))))
        ".apply should also handle >20 arguments"))

(deftest multi-arity-test
  (is (= 2 (apply (fn ([a] a) ([a b] b)) 1 [2])))
  (is (= 1 (apply (fn ([a] a) ([a b] b)) 1 [])))
  (is (= 1 (apply (fn ([a] a) ([a b] b)) 1 nil)))
  (is (thrown? js/Error (apply (fn ([a] a) ([a b] b)) 1 2 3 nil)))
  (is (thrown? js/Error (apply (fn ([a b] a)
                                 ([a b c] a)) [1]))))

(deftest single-arity-variadic-test
  (doseq [f [(fn [& r] r)
             (fn [a & r] (list* a r))
             (fn [a b & r] (list* a b r))
             (fn [a b c & r] (list* a b c r))
             (fn [a b c d & r] (list* a b c d r))
             (fn [a b c d e & r] (list* a b c d e r))]]
    (is (= (range 10) (apply f (range 10))))
    (is (= (range 10) (apply f 0 (range 1 10))))
    (is (= (range 10) (apply f 0 1 (range 2 10))))
    (is (= (range 10) (apply f 0 1 2 (range 3 10))))
    (is (= (range 10) (apply f 0 1 2 3 (range 4 10))))
    (is (= (range 10) (apply f 0 1 2 3 4 (range 5 10))))
    (is (= (range 10) (apply f 0 1 2 3 4 5 (range 6 10)))))
  (is (nil? (apply (fn [a & b] b) [1]))
      "rest should stay nil")
  (is (nil? (apply (fn [a & b] b) 1 []))
      "rest should be nil'd")
  (is (= '(2) (apply (fn [a & b] b) 1 [2]))
      "rest should be nil'd")
  (is (= (range 30)
         (apply (fn [_ _ _ _ _ a b c d e _ _ _ _ _ f g h i j k _ _ _ _ _ & R]
                  (let [a (array)]
                    (copy-arguments a)
                    (concat (.slice a 0 26) R)))
                (range 30)))
      "Variadic function are not limited to 20 params"))

(deftest multi-arity-variadic-test
  (doseq [f [(fn ([]) ([& r] r))
             (fn ([a]) ([a & r] (list* a r)))
             (fn ([a]) ([a b & r] (list* a b r)))
             (fn ([a]) ([a b c & r] (list* a b c r)))
             (fn ([a]) ([a b c d & r] (list* a b c d r)))
             (fn ([a]) ([a b c d e & r] (list* a b c d e r)))]]
    (is (= (range 10) (apply f (range 10))))
    (is (= (range 10) (apply f 0 (range 1 10))))
    (is (= (range 10) (apply f 0 1 (range 2 10))))
    (is (= (range 10) (apply f 0 1 2 (range 3 10))))
    (is (= (range 10) (apply f 0 1 2 3 (range 4 10))))
    (is (= (range 10) (apply f 0 1 2 3 4 (range 5 10))))
    (is (= (range 10) (apply f 0 1 2 3 4 5 (range 6 10)))))
  (is (= 1 (apply (fn ([a] a) ([a & b] b)) [1])))
  (is (= '(2) (apply (fn ([a] a) ([a & b] b)) 1 [2])))
  (is (= 1 (apply (fn ([a] a) ([a & b] b)) 1 [])))
  (is (= 1 (apply (fn ([a] a) ([a & b] b)) [1])))
  (is (= '(2 3 4 5 6) (apply (fn ([a] a) ([a & b] b)) 1 2 3 4 5 [6])))
  (is (= '(2 3 4 5) (apply (fn ([a] a) ([a & b] b)) 1 2 3 4 5 [])))
  (is (= (range 30)
         (apply (fn ([a])
                  ([_ _ _ _ _ a b c d e _ _ _ _ _ f g h i j k _ _ _ _ _ & R]
                   (let [a (array)]
                     (copy-arguments a)
                     (concat (.slice a 0 26) R))))
                (range 30)))
      "Variadic function are not limited to 20 params"))

(deftest incorrect-invokes-m-arity-test
  ;; This is the fault of .call currently not throwing. Can't be caught by apply:
  #_(is (thrown? js/Error
                 (apply (fn ([a b] a)
                          ([a b & c] a)) [1]))
        "Apply should throw on wrong arity."))

(deftest incorrect-invokes-dispatcher-test
  ;; The dispatcher needs to look at arguments.length:
  #_(is (thrown? js/Error (.call (fn [a b & b] a) nil 1))
        "Dispatcher should complain about wrong arity"))
