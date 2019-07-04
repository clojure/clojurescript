;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.specials-test
  (:require [cljs.test :refer-macros [deftest is]]))

(defprotocol IFoo3125
  (-mutate [this]))

(defrecord Foo3125 [^:mutable x]
  IFoo3125
  (-mutate [this] (* 3 (set! x (inc x)))))

(def ^:dynamic *test-cljs-3125* 4)

(deftest test-cljs-3125
  (is (== 12 (let [o #js {}] (* 6 (set! (.-a o) 2)))))
  (is (== 12 (let [o #js {}] (* 6 (set! o -a 2)))))
  (is (== 15 (* 3 (set! *test-cljs-3125* (inc *test-cljs-3125*)))))
  (is (== 18 (-mutate (->Foo3125 5)))))
