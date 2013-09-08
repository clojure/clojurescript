(ns clojure.string-test
  (:require [clojure.string :as s]))

(defn test-string
  []
  ;; reverse
  (assert (= "" (s/reverse "")))
  (assert (= "tab" (s/reverse "bat")))
  ;; replace
  (assert (= "faabar" (s/replace "foobar" \o \a)))
  (assert (= "barbarbar" (s/replace "foobarfoo" "foo" "bar")))
  (assert (= "FOObarFOO" (s/replace "foobarfoo" #"foo" s/upper-case)))
  (assert (= "barbar)foo" (s/replace "foo(bar)foo" "foo(" "bar")))
  ;; join
  (assert (= "" (s/join nil)))
  (assert (= "" (s/join [])))
  (assert (= "1" (s/join [1])))
  (assert (= "12" (s/join [1 2])))
  (assert (= "1,2,3" (s/join \, [1 2 3])))
  (assert (= "" (s/join \, [])))
  (assert (= "1 and-a 2 and-a 3" (s/join " and-a " [1 2 3])))
  ;; capitalize
  (assert (= "FOOBAR" (s/upper-case "Foobar")))
  (assert (= "foobar" (s/lower-case "FooBar")))
  (assert (= "Foobar" (s/capitalize "foobar")))
  (assert (= "Foobar" (s/capitalize "FOOBAR")))
  ;; split
  (assert (= ["a" "b"] (s/split "a-b" #"-")))
  (assert (= ["a" "b" "c"] (s/split "a-b-c" #"-" -1)))
  (assert (= ["a" "b" "c"] (s/split "a-b-c" #"-" 0)))
  (assert (= ["a-b-c"] (s/split "a-b-c" #"-" 1)))
  (assert (= ["a" "b-c"] (s/split "a-b-c" #"-" 2)))
  (assert (= ["a" "b" "c"] (s/split "a-b-c" #"-" 3)))
  (assert (= ["a" "b" "c"] (s/split "a-b-c" #"-" 4)))
  (assert (vector? (s/split "abc" #"-")))
  (assert (= ["a-b-c"] (s/split "a-b-c" #"x" 2)))
  (assert (= ["" "a" "b" "c" ""] (s/split "abc" (re-pattern "") 5)))
  (assert (= ["a"] (s/split "ab" #"b")))
  (assert (= [] (s/split "ab" #"ab")))
  ;; split-lines
  (let [result (s/split-lines "one\ntwo\r\nthree")]
    (assert (= ["one" "two" "three"] result))
    (assert (vector? result)))
  (assert (= (list "foo") (s/split-lines "foo")))
  ;; blank
  (assert (s/blank? nil))
  (assert (s/blank? ""))
  (assert (s/blank? " "))
  (assert (s/blank? " \t \n  \r "))
  (assert (not (s/blank? "  foo  ")))
  ;; escape
  (assert (= "&lt;foo&amp;bar&gt;"
             (s/escape "<foo&bar>" {\& "&amp;" \< "&lt;" \> "&gt;"})))
  (assert (= " \\\"foo\\\" "
             (s/escape " \"foo\" " {\" "\\\""})))
  (assert (= "faabor"
             (s/escape "foobar" {\a \o, \o \a})))
  ;; replace-first
  (assert (= "barbarfoo" (s/replace-first "foobarfoo" "foo" "bar")))
  (assert (= "barbarfoo" (s/replace-first "foobarfoo" #"foo" "bar")))
  (assert (= "z.ology" (s/replace-first "zoology" \o \.)))
  (assert (= "FOObarfoo" (s/replace-first "foobarfoo" #"foo" s/upper-case)))
  ;; trim
  (assert (= "foo " (s/triml " foo ")))
  (assert (= "" (s/triml "   ")))
  (assert (= " foo" (s/trimr " foo ")))
  (assert (= "" (s/trimr "   ")))
  (assert (= "foo" (s/trim "  foo  \r\n")))
  ;; trim-newline
  (assert (= "foo" (s/trim-newline "foo\n")))
  (assert (= "foo" (s/trim-newline "foo\r\n")))
  (assert (= "foo" (s/trim-newline "foo")))
  (assert (= "foo\r " (s/trim-newline "foo\r ")))
  (assert (= "" (s/trim-newline "")))
  :ok)

(comment

(deftest char-sequence-handling
  (are [result f args] (let [[^CharSequence s & more] args]
                         (= result (apply f (StringBuffer. s) more)))
       "paz" s/reverse ["zap"]
       "foo:bar" s/replace ["foo-bar" \- \:]
       "ABC" s/replace ["abc" #"\w" s/upper-case]
       "faa" s/replace ["foo" #"o" (StringBuffer. "a")]
       "baz::quux" s/replace-first ["baz--quux" #"--" "::"]
       "baz::quux" s/replace-first ["baz--quux" (StringBuffer. "--") (StringBuffer. "::")]
       "zim-zam" s/replace-first ["zim zam" #" " (StringBuffer. "-")]
       "Pow" s/capitalize ["POW"]
       "BOOM" s/upper-case ["boom"]
       "whimper" s/lower-case ["whimPER"]
       ["foo" "bar"] s/split ["foo-bar" #"-"]
       "calvino" s/trim ["  calvino  "]
       "calvino  " s/triml ["  calvino  "]
       "  calvino" s/trimr ["  calvino  "]
       "the end" s/trim-newline ["the end\r\n\r\r\n"]
       true s/blank? [" "]
       ["a" "b"] s/split-lines ["a\nb"]
       "fa la la" s/escape ["fo lo lo" {\o \a}]))
)
