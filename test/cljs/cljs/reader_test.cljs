(ns cljs.reader-test
  (:require [cljs.reader :as reader]))

(defn test-reader
  []
  (assert (= 1 (reader/read-string "1")))
  (assert (= 2 (reader/read-string "#_nope 2")))
  (assert (= -1 (reader/read-string "-1")))
  (assert (= -1.5 (reader/read-string "-1.5")))
  (assert (= [3 4] (reader/read-string "[3 4]")))
  (assert (= "foo" (reader/read-string "\"foo\"")))
  (assert (= :hello (reader/read-string ":hello")))
  (assert (= 'goodbye (reader/read-string "goodbye")))
  (assert (= #{1 2 3} (reader/read-string "#{1 2 3}")))
  (assert (= '(7 8 9) (reader/read-string "(7 8 9)")))
  (assert (= '(deref foo) (reader/read-string "@foo")))
  (assert (= '(quote bar) (reader/read-string "'bar")))
  (assert (= \a (reader/read-string "\\a")))
  (assert (= {:tag 'String} (meta (reader/read-string "^String {:a 1}"))))
  (assert (= [:a 'b #{'c {:d [:e :f :g]}}]
               (reader/read-string "[:a b #{c {:d [:e :f :g]}}]")))
  (assert (= nil (reader/read-string "nil")))
  (assert (= true (reader/read-string "true")))
  (assert (= false (reader/read-string "false")))
  
  :ok)
