(ns data-readers-test.records
  #?(:cljs (:require-macros [data-readers-test.records])))

(defrecord Foo [a b])

(assert (= (Foo. 1 2) #data_readers_test.records.Foo{:a 1 :b 2}))