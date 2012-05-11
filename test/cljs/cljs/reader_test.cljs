(ns cljs.reader-test
  (:require [cljs.reader :as reader]
            [goog.object :as o]))

(deftype T [a b])
(defrecord R [a b])

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
  (assert (= 'foo/bar (reader/read-string "foo/bar")))
  (assert (= \a (reader/read-string "\\a")))
  (assert (= {:tag 'String} (meta (reader/read-string "^String {:a 1}"))))
  (assert (= [:a 'b #{'c {:d [:e :f :g]}}]
               (reader/read-string "[:a b #{c {:d [:e :f :g]}}]")))
  (assert (= :foo/bar (reader/read-string ":foo/bar")))
  (assert (= nil (reader/read-string "nil")))
  (assert (= true (reader/read-string "true")))
  (assert (= false (reader/read-string "false")))
  (assert (= "string" (reader/read-string "\"string\"")))
  (assert (= "escape chars \t \r \n \\ \" \b \f" (reader/read-string "\"escape chars \\t \\r \\n \\\\ \\\" \\b \\f\"")))

  (println (pr-str (reader/read-string "#cljs.reader_test.R[1 2]")))
  (println (pr-str (reader/read-string "#cljs.reader_test.R{:a 1 :b 2}")))
  (println (pr-str (new (.-R cljs.reader_test) 1 2)))

  (let [n 'R
        k cljs.reader_test.R]
    (println (pr-str (js* "(new ~{k}(1,2))"))))

  (println (pr-str (new (.-R (.-reader_test js/cljs)) 1 2)))

  (let [a "cljs"
        b "reader_test"
        c "R"
        #_d #_"window['~{a}']"]
    (println (js* "Function('return this')()")))
  
  (assert (= (R. 1 2) (reader/read-string "#cljs.reader_test.R[1 2]")))
  
  :ok)
