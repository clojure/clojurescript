;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.inference-util)

(defmacro truth_-called?
  "Returns whether cljs.core/truth_ is called when evaluating a form as the
  test of an if."
  [form]
  `(let [called?# (volatile! false)]
     (with-redefs [cljs.core/truth_ (fn [x#]
                                      (vreset! called?# true)
                                      (cljs.core/truth_ x#))]
       (if ~form 1 2))
     @called?#))

(defmacro truth_-not-called?
  "Returns whether cljs.core/truth_ is not called when evaluating ecah of
  forms as the test of an if."
  [& forms]
  `(do
     ~@(map (fn [form]
              `(cljs.test/is (not (truth_-called? ~form))))
         forms)))
