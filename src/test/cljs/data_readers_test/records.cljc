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
