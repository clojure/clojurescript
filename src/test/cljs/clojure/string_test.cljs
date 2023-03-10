;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns clojure.string-test
  (:require [cljs.test :as test
             :refer-macros [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer-macros [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]
            [clojure.string :as s]))

(deftest test-api
  (testing "Testing string reverse"
    (is (= "" (s/reverse "")))
    (is (= "tab" (s/reverse "bat")))
    (is (= "c\uD834\uDD1Ea" (s/reverse "a\uD834\uDD1Ec"))) ;; U+1D11E MUSICAL SYMBOL G CLEF
    )

  (testing "Testing string replace"
    (is (= "faabar" (s/replace "foobar" \o \a)))
    (is (= "barbarbar" (s/replace "foobarfoo" "foo" "bar")))
    (is (= "foobarfoo" (s/replace "foobarfoo" #"ooo" s/upper-case)))
    (is (= "FOObarFOO" (s/replace "foobarfoo" #"foo" s/upper-case)))
    (is (= "barbar)foo" (s/replace "foo(bar)foo" "foo(" "bar")))
    (is (= "FOO-ObarFOO-O"
           (s/replace "foobarfoo" #"f(o)o" (fn [[m g1]] (s/upper-case (str m "-" g1))))))
    (is (= "faabarfaa" (s/replace "FOObarfoo" #"(?i)foo" "faa")))
    (is (= "aaa\nccc" (s/replace "aaa\nbbb" #"(?m)^bbb" "ccc"))))

  (testing "Testing string join"
    (is (= "" (s/join nil)))
    (is (= "" (s/join [])))
    (is (= "1" (s/join [1])))
    (is (= "12" (s/join [1 2])))
    (is (= "1,2,3" (s/join \, [1 2 3])))
    (is (= "" (s/join \, [])))
    (is (= "1 and-a 2 and-a 3" (s/join " and-a " [1 2 3]))))

  (testing "Testing string capitalize"
    (is (= "FOOBAR" (s/upper-case "Foobar")))
    (is (= "foobar" (s/lower-case "FooBar")))
    (is (= "Foobar" (s/capitalize "foobar")))
    (is (= "Foobar" (s/capitalize "FOOBAR"))))

  (testing "Testing string split"
    (is (= ["a" "b"] (s/split "a-b" #"-")))
    (is (= ["a" "b" "c"] (s/split "a-b-c" #"-" -1)))
    (is (= ["a" "b" "c"] (s/split "a-b-c" #"-" 0)))
    (is (= ["a-b-c"] (s/split "a-b-c" #"-" 1)))
    (is (= ["a" "b-c"] (s/split "a-b-c" #"-" 2)))
    (is (= ["a" "b" "c"] (s/split "a-b-c" #"-" 3)))
    (is (= ["a" "b" "c"] (s/split "a-b-c" #"-" 4)))
    (is (vector? (s/split "abc" #"-")))
    (is (= ["a-b-c"] (s/split "a-b-c" #"x" 2)))
    (is (= ["" "a" "b" "c" ""] (s/split "abc" (re-pattern "") 5)))
    (is (= ["a"] (s/split "ab" #"b")))
    (is (= [] (s/split "ab" #"ab"))))

  (testing "Testing string split lines"
    (let [result (s/split-lines "one\ntwo\r\nthree")]
      (is (= ["one" "two" "three"] result))
      (is (vector? result)))
    (is (= (list "foo") (s/split-lines "foo"))))

  (testing "Testing string blank?"
    (is (s/blank? nil))
    (is (s/blank? ""))
    (is (s/blank? " "))
    (is (s/blank? " \t \n  \r "))
    (is (not (s/blank? "  foo  "))))

  (testing "Testing string escape"
    (is (= "&lt;foo&amp;bar&gt;"
              (s/escape "<foo&bar>" {\& "&amp;" \< "&lt;" \> "&gt;"})))
    (is (= " \\\"foo\\\" "
              (s/escape " \"foo\" " {\" "\\\""})))
    (is (= "faabor"
              (s/escape "foobar" {\a \o, \o \a})))
    (is (= "aaa"
              (s/escape "foo" (fn [c] \a)))))

  (testing "Testing string replace-first"
    (is (= "barbarfoo" (s/replace-first "foobarfoo" "foo" "bar")))
    (is (= "barbarfoo" (s/replace-first "foobarfoo" #"foo" "bar")))
    (is (= "z.ology" (s/replace-first "zoology" \o \.)))
    (is (= "FOObarfoo" (s/replace-first "foobarfoo" #"foo" s/upper-case))))

  (testing "Testing string trim"
    (is (= "foo " (s/triml " foo ")))
    (is (= "" (s/triml "   ")))
    (is (= " foo" (s/trimr " foo ")))
    (is (= "" (s/trimr "   ")))
    (is (= "foo" (s/trim "  foo  \r\n"))))

  (testing "Testing string trim-newline"
    (is (= "foo" (s/trim-newline "foo\n")))
    (is (= "foo" (s/trim-newline "foo\r\n")))
    (is (= "foo" (s/trim-newline "foo")))
    (is (= "foo\r " (s/trim-newline "foo\r ")))
    (is (= "" (s/trim-newline ""))))

  (testing "Testing string trim-newline"
    (is (= "foo" (s/trim-newline "foo\n")))
    (is (= "foo" (s/trim-newline "foo\r\n")))
    (is (= "foo" (s/trim-newline "foo")))
    (is (= "foo\r " (s/trim-newline "foo\r ")))
    (is (= "" (s/trim-newline ""))))

  (testing "Testing string index-of"
    (let [sb "tacos"]
    (is (= 2  (s/index-of sb "c")))
    (is (= 2  (s/index-of sb \c)))
    (is (= 1  (s/index-of sb "ac")))
    (is (= 3  (s/index-of sb "o" 2)))
    (is (= 3  (s/index-of sb  \o  2)))
    (is (= 3  (s/index-of sb "o" -100)))
    (is (= nil (s/index-of sb "z")))
    (is (= nil (s/index-of sb \z)))
    (is (= nil (s/index-of sb "z" 2)))
    (is (= nil (s/index-of sb \z  2)))
    (is (= nil (s/index-of sb "z" 100))
    (is (= nil (s/index-of sb "z" -10))))))

  (testing "Testing string last-index-of"
    (let [sb "banana"]
      (is (= 4 (s/last-index-of sb "n")))
      (is (= 4 (s/last-index-of sb \n)))
      (is (= 3 (s/last-index-of sb "an")))
      (is (= 4 (s/last-index-of sb "n" )))
      (is (= 4 (s/last-index-of sb "n" 5)))
      (is (= 4 (s/last-index-of sb \n  5)))
      (is (= 4 (s/last-index-of sb "n" 500)))
      (is (= nil (s/last-index-of sb "z")))
      (is (= nil (s/last-index-of sb "z" 1)))
      (is (= nil (s/last-index-of sb \z  1)))
      (is (= nil (s/last-index-of sb "z" 100)))
      (is (= nil (s/last-index-of sb "z" -10)))))

  (testing "Testing string starts-with?"
    (is (s/starts-with? "clojure west" "clojure"))
    (is (not (s/starts-with? "conj" "clojure"))))

  (testing "Testing string ends-with?"
    (is (s/ends-with? "Clojure West" "West"))
    (is (not (s/ends-with? "Conj" "West"))))

  (testing "Testing string includes?"
    (let [sb "Clojure Applied Book"]
      (is (s/includes? sb "Applied"))
      (is (not (s/includes? sb "Living"))))))

(defspec test-cljs-2300
  ;; The reference implementation is the implementation prior to the change.
  ;; Since some JavaScript implementations fail to properly change case for
  ;; some characters (for example, the upper case of  "ß" is "SS"), we limit
  ;; this test to strings comprising only printable ASCII characters.
  (let [ref-impl           (fn [s]
                             (if (< (count s) 2)
                               (s/upper-case s)
                               (str (s/upper-case (subs s 0 1))
                                 (s/lower-case (subs s 1)))))
        char-codes->string (fn [xs]
                             (apply (.-fromCharCode js/String) xs))]
    (prop/for-all [s (gen/fmap char-codes->string
                       (gen/not-empty (gen/vector (gen/choose 0x20 0x7E))))]
      (= (ref-impl s) (s/capitalize s)))))

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
