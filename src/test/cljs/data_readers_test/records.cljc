;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns data-readers-test.records
  #?(:cljs (:require-macros [data-readers-test.records])))

(defrecord Foo [a b])

(assert (= (Foo. 1 2) #data_readers_test.records.Foo{:a 1 :b 2}))
(assert (= (Foo. 1 2)
           '#data_readers_test.records.Foo{:a 1 :b 2}))
(assert (= (Foo. 1 2)
           (second ''#data_readers_test.records.Foo{:a 1 :b 2})))
(assert (= (Foo. 'a 'b)
           (let [a 1
                 b 2]
             #data_readers_test.records.Foo{:a a :b b}))
        (pr-str
          (let [a 1
                b 2]
            #data_readers_test.records.Foo{:a a :b b})))
(assert (= (Foo. 'a 'b)
           (let [a 1
                 b 2]
             '#data_readers_test.records.Foo{:a a :b b}))
        (pr-str
          (let [a 1
                b 2]
            '#data_readers_test.records.Foo{:a a :b b})))
