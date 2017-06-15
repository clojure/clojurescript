;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.recur-test
  (:require [cljs.test :refer-macros [deftest is]]))

;; Setup for CLJS-2085

(defprotocol ISearch
  (search [this coll]))

;; Passing this in the recur call here will cause a warning to be emitted
(defrecord Search1 [needle]
  ISearch
  (search [this coll]
    (when (seq coll)
      (if (= needle (first coll))
        needle
        (recur this (rest coll))))))

;; This code will be accepted as is
(defrecord Search2 [needle]
  ISearch
  (search [_ coll]
    (when (seq coll)
      (if (= needle (first coll))
        needle
        (recur (rest coll))))))

;; This code will also be accepted as is; the recur is to a loop
(defrecord Search3 [needle]
  ISearch
  (search [this coll]
    (loop [coll coll]
      (when (seq coll)
        (if (= needle (first coll))
          needle
          (recur (rest coll)))))))

;; This code should not cause a warning to be emitted
(defrecord Search4 [needle]
  ISearch
  (search [this coll]
    (let [search-fn (fn [coll]
                      (when (seq coll)
                        (if (= needle (first coll))
                          needle
                          (recur (rest coll)))))]
      (search-fn coll))))

(deftest cljs-2085-test
  (is (= 1 (-> (->Search1 1) (search [:a 1 "b"]))))
  (is (nil? (-> (->Search1 :z) (search [:a 1 "b"]))))
  (is (= 1 (-> (->Search2 1) (search [:a 1 "b"]))))
  (is (nil? (-> (->Search2 :z) (search [:a 1 "b"]))))
  (is (= 1 (-> (->Search3 1) (search [:a 1 "b"]))))
  (is (nil? (-> (->Search3 :z) (search [:a 1 "b"]))))
  (is (= 1 (-> (->Search4 1) (search [:a 1 "b"]))))
  (is (nil? (-> (->Search4 :z) (search [:a 1 "b"])))))
