;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.pprint-test
  (:refer-clojure :exclude [prn])
  (:require-macros
    [cljs.pprint-test :refer [simple-tests code-block]])
  (:require
    [cljs.test :as t :refer-macros [deftest is are]]
    [cljs.pprint :refer [pprint cl-format get-pretty-writer prn print-table
                         *print-pprint-dispatch* simple-dispatch
                         *print-right-margin* *print-miser-width*
                         write code-dispatch]
     :refer-macros [with-pprint-dispatch]]
    [cljs.reader :as reader])

  (:import [goog.string StringBuffer]))

(def format cl-format)

(simple-tests xp-fill-test
  (binding [*print-pprint-dispatch* simple-dispatch
            *print-right-margin* 38
            *print-miser-width* nil]
    (cl-format nil "(let ~:<~@{~:<~w ~_~w~:>~^ ~:_~}~:>~_ ...)~%"
               '((x 4) (*print-length* nil) (z 2) (list nil))))
  "(let ((x 4) (*print-length* nil)\n      (z 2) (list nil))\n ...)\n"

  (binding [*print-pprint-dispatch* simple-dispatch
            *print-right-margin* 22]
    (cl-format nil "(let ~:<~@{~:<~w ~_~w~:>~^ ~:_~}~:>~_ ...)~%"
               '((x 4) (*print-length* nil) (z 2) (list nil))))
  "(let ((x 4)\n      (*print-length*\n       nil)\n      (z 2)\n      (list nil))\n ...)\n"
)

(simple-tests mandatory-fill-test
  (cl-format nil
             "<pre>~%~<Usage: ~:I~@{*~a*~^~:@_~}~:>~%</pre>~%"
             [ "hello" "gooodbye" ])
  "<pre>
Usage: *hello*
       *gooodbye*
</pre>
")

(simple-tests prefix-suffix-test
  (binding [*print-pprint-dispatch* simple-dispatch
            *print-right-margin* 10, *print-miser-width* 10]
    (cl-format nil "~<{~;LIST ~@_~W ~@_~W ~@_~W~;}~:>" '(first second third)))
  "{LIST\n first\n second\n third}"
)

(simple-tests pprint-test
  (binding [*print-pprint-dispatch* simple-dispatch]
    (write '(defn foo [x y]
              (let [result (* x y)]
                (if (> result 400)
                  (cl-format true "That number is too big")
                  (cl-format true "The  result of ~d x ~d is ~d" x y result))))
           :stream nil))
  "(defn
 foo
 [x y]
 (let
  [result (* x y)]
  (if
   (> result 400)
   (cl-format true \"That number is too big\")
   (cl-format true \"The  result of ~d x ~d is ~d\" x y result))))"

  (with-pprint-dispatch code-dispatch                       ;;fail
    (write '(defn foo [x y]
              (let [result (* x y)]
                (if (> result 400)
                  (cl-format true "That number is too big")
                  (cl-format true "The  result of ~d x ~d is ~d" x y result))))
           :stream nil))
  "(defn foo [x y]
  (let [result (* x y)]
    (if (> result 400)
      (cl-format true \"That number is too big\")
      (cl-format true \"The  result of ~d x ~d is ~d\" x y result))))"

  (binding [*print-pprint-dispatch* simple-dispatch
            *print-right-margin* 15]
    (write '(fn (cons (car x) (cdr y))) :stream nil))
  "(fn\n (cons\n  (car x)\n  (cdr y)))"

  ;;TODO Fails because of spacing; may be due to the mutating clj difference from cljs
  ;;or could simply be a bug in our code
  (with-pprint-dispatch code-dispatch
    (binding [*print-right-margin* 52]
      (write
        '(add-to-buffer this (make-buffer-blob (str (char c)) nil))
        :stream nil)))
  "(add-to-buffer\n  this \n  (make-buffer-blob (str (char c)) nil))"
  #_"(add-to-buffer\n  this\n  (make-buffer-blob (str (char c)) nil))"
  )

;; Not a valid test now that we use tools.reader - David
;(simple-tests pprint-reader-macro-test
;  ;;I'm not sure this will work without significant work on cljs. Short story, cljs
;  ;;reader only takes valid EDN, so #(* % %) won't work.
;  ;;see http://stackoverflow.com/a/25712675/546321 for more details
;  #_(with-pprint-dispatch code-dispatch
;    (write (reader/read-string "(map #(first %) [[1 2 3] [4 5 6] [7]])")
;           :stream nil))
;  #_"(map #(first %) [[1 2 3] [4 5 6] [7]])"
;
;  ;;TODO Not sure what to do about this test due to the reader handling of `@`
;  (with-pprint-dispatch code-dispatch
;    (write (reader/read-string "@@(ref (ref 1))")
;           :stream nil))
;  "(deref (deref (ref (ref 1))))"
;  #_"@@(ref (ref 1))"
;
;  (with-pprint-dispatch code-dispatch
;    (write (reader/read-string "'foo")
;           :stream nil))
;  "'foo"
;  )

(simple-tests xp-miser-test
  (binding [*print-pprint-dispatch* simple-dispatch
            *print-right-margin* 10, *print-miser-width* 9]
    (cl-format nil "~:<LIST ~@_~W ~@_~W ~@_~W~:>" '(first second third)))
  "(LIST\n first\n second\n third)"

  (binding [*print-pprint-dispatch* simple-dispatch
            *print-right-margin* 10, *print-miser-width* 8]
    (cl-format nil "~:<LIST ~@_~W ~@_~W ~@_~W~:>" '(first second third)))
  "(LIST first second third)"
)

(code-block code-block-tests
  "(defn cl-format
  \"An implementation of a Common Lisp compatible format function\"
  [stream format-in & args]
  (let [compiled-format (if (string? format-in)
                          (compile-format format-in)
                          format-in)
        navigator (init-navigator args)]
    (execute-format stream compiled-format navigator)))"

 "(defn pprint-defn [writer alis]
  (if (next alis)
    (let [[defn-sym defn-name & stuff] alis
          [doc-str stuff] (if (string? (first stuff))
                            [(first stuff) (next stuff)]
                            [nil stuff])
          [attr-map stuff] (if (map? (first stuff))
                             [(first stuff) (next stuff)]
                             [nil stuff])]
      (pprint-logical-block
        writer
        :prefix
        \"(\"
        :suffix
        \")\"
        (cl-format true \"~w ~1I~@_~w\" defn-sym defn-name)
        (if doc-str (cl-format true \" ~_~w\" doc-str))
        (if attr-map (cl-format true \" ~_~w\" attr-map))
        (cond
          (vector? (first stuff)) (single-defn
                                    stuff
                                    (or doc-str attr-map))
          :else (multi-defn stuff (or doc-str attr-map)))))
    (pprint-simple-code-list writer alis)))"
)

(simple-tests print-length-tests
  (binding [*print-length* 1] (with-out-str (pprint '(a b c d e f))))
  "(a ...)\n"
  (binding [*print-length* 2] (with-out-str (pprint '(a b c d e f))))
  "(a b ...)\n"
  (binding [*print-length* 6] (with-out-str (pprint '(a b c d e f))))
  "(a b c d e f)\n"
  (binding [*print-length* 8] (with-out-str (pprint '(a b c d e f))))
  "(a b c d e f)\n"

  (binding [*print-length* 1] (with-out-str (pprint [1 2 3 4 5 6])))
  "[1 ...]\n"
  (binding [*print-length* 2] (with-out-str (pprint [1 2 3 4 5 6])))
  "[1 2 ...]\n"
  (binding [*print-length* 6] (with-out-str (pprint [1 2 3 4 5 6])))
  "[1 2 3 4 5 6]\n"
  (binding [*print-length* 8] (with-out-str (pprint [1 2 3 4 5 6])))
  "[1 2 3 4 5 6]\n"

  (binding [*print-length* 1] (with-out-str (pprint (sorted-set 1 2 3 4 5 6))))
  "#{1 ...}\n"
  (binding [*print-length* 2] (with-out-str (pprint (sorted-set 1 2 3 4 5 6))))
  "#{1 2 ...}\n"
  (binding [*print-length* 6] (with-out-str (pprint (sorted-set 1 2 3 4 5 6))))
  "#{1 2 3 4 5 6}\n"
  (binding [*print-length* 8] (with-out-str (pprint (sorted-set 1 2 3 4 5 6))))
  "#{1 2 3 4 5 6}\n"

  (binding [*print-length* 1] (with-out-str (pprint (sorted-map 1 2, 3 4, 5 6, 7 8, 9 10, 11 12))))
  "{1 2, ...}\n"
  (binding [*print-length* 2] (with-out-str (pprint (sorted-map 1 2, 3 4, 5 6, 7 8, 9 10, 11 12))))
  "{1 2, 3 4, ...}\n"
  (binding [*print-length* 6] (with-out-str (pprint (sorted-map 1 2, 3 4, 5 6, 7 8, 9 10, 11 12))))
  "{1 2, 3 4, 5 6, 7 8, 9 10, 11 12}\n"
  (binding [*print-length* 8] (with-out-str (pprint (sorted-map 1 2, 3 4, 5 6, 7 8, 9 10, 11 12))))
  "{1 2, 3 4, 5 6, 7 8, 9 10, 11 12}\n"

  ;;TODO Not sure if JS objs are handled the way we want; need to further investigate
  (binding [*print-length* 1] (with-out-str (pprint (int-array [1 2 3 4 5 6]))))
  "#js [1 ...]\n"
  (binding [*print-length* 2] (with-out-str (pprint (int-array [1 2 3 4 5 6]))))
  "#js [1 2 ...]\n"
  (binding [*print-length* 6] (with-out-str (pprint (int-array [1 2 3 4 5 6]))))
  "#js [1 2 3 4 5 6]\n"
  (binding [*print-length* 8] (with-out-str (pprint (int-array [1 2 3 4 5 6]))))
  "#js [1 2 3 4 5 6]\n"

  )

(simple-tests print-margin-tests
  (binding [cljs.pprint/*print-right-margin* 20]
    (with-out-str (pprint (sorted-map 1 (sorted-map 12345 123456), 3 (sorted-map 4 5, 6 7)))))
  "{1 {12345 123456},\n 3 {4 5, 6 7}}\n"

  (binding [cljs.pprint/*print-right-margin* 8]
    (with-out-str (pprint (sorted-set 123 456 789))))
  "#{123\n  456\n  789}\n"
)

(simple-tests print-namespace-maps-tests
  (binding [*print-namespace-maps* true] (with-out-str (pprint {:user/a 1})))
  "#:user{:a 1}\n"
  (binding [*print-namespace-maps* false] (with-out-str (pprint {:user/a 1})))
  "{:user/a 1}\n"
  )

;;----------------------------------------------------------------------------
;; clj-format tests
;;----------------------------------------------------------------------------

(simple-tests d-tests
  (cl-format nil "~D" 0) "0"
  (cl-format nil "~D" 2e6) "2000000"
  (cl-format nil "~D" 2000000) "2000000"
  (cl-format nil "~:D" 2000000) "2,000,000"
  ;(cl-format nil "~D" 1/2) "1/2" ;no ratio
  (cl-format nil "~D" 'fred) "fred"
)

(simple-tests cardinal-tests
  (cl-format nil "~R" 0) "zero"
  (cl-format nil "~R" 4) "four"
  (cl-format nil "~R" 15) "fifteen"
  (cl-format nil "~R" -15) "minus fifteen"
  (cl-format nil "~R" 25) "twenty-five"
  (cl-format nil "~R" 20) "twenty"
  (cl-format nil "~R" 200) "two hundred"
  (cl-format nil "~R" 203) "two hundred three"

  (cl-format nil "~R" 44879032)
  "forty-four million, eight hundred seventy-nine thousand, thirty-two"

  (cl-format nil "~R" -44879032)
  "minus forty-four million, eight hundred seventy-nine thousand, thirty-two"

  (cl-format nil "~R = ~:*~:D" 44000032)
  "forty-four million, thirty-two = 44,000,032"

  ;;js/Number.MAX_SAFE_INTEGER - js starts munging larger values
  (cl-format nil "~R = ~:*~:D" 9007199254740991)
  "nine quadrillion, seven trillion, one hundred ninety-nine billion, two hundred fifty-four million, seven hundred forty thousand, nine hundred ninety-one = 9,007,199,254,740,991"

  ;;js/Number.MIN_SAFE_INTEGER - js starts munging smaller values
  (cl-format nil "~R = ~:*~:D" -9007199254740991)
  "minus nine quadrillion, seven trillion, one hundred ninety-nine billion, two hundred fifty-four million, seven hundred forty thousand, nine hundred ninety-one = -9,007,199,254,740,991"

  (cl-format nil "~R = ~:*~:D" 2e6)
  "two million = 2,000,000"

  (cl-format nil "~R = ~:*~:D" 200000200000)
  "two hundred billion, two hundred thousand = 200,000,200,000"
)

(simple-tests ordinal-tests
  (cl-format nil "~:R" 0) "zeroth"
  (cl-format nil "~:R" 4) "fourth"
  (cl-format nil "~:R" 15) "fifteenth"
  (cl-format nil "~:R" -15) "minus fifteenth"
  (cl-format nil "~:R" 25) "twenty-fifth"
  (cl-format nil "~:R" 20) "twentieth"
  (cl-format nil "~:R" 200) "two hundredth"
  (cl-format nil "~:R" 203) "two hundred third"

  (cl-format nil "~:R" 44879032)
  "forty-four million, eight hundred seventy-nine thousand, thirty-second"

  (cl-format nil "~:R" -44879032)
  "minus forty-four million, eight hundred seventy-nine thousand, thirty-second"

  (cl-format nil "~:R = ~:*~:D" 44000032)
  "forty-four million, thirty-second = 44,000,032"

  ;;js/Number.MAX_SAFE_INTEGER - js starts munging larger values
  (cl-format nil "~:R = ~:*~:D" 9007199254740991)
  "nine quadrillion, seven trillion, one hundred ninety-nine billion, two hundred fifty-four million, seven hundred forty thousand, nine hundred ninety-first = 9,007,199,254,740,991"

  ;;js/Number.MIN_SAFE_INTEGER - js starts munging smaller values
  (cl-format nil "~:R = ~:*~:D" -9007199254740991)
  "minus nine quadrillion, seven trillion, one hundred ninety-nine billion, two hundred fifty-four million, seven hundred forty thousand, nine hundred ninety-first = -9,007,199,254,740,991"

  (cl-format nil "~:R = ~:*~:D" 2e6)
  "two millionth = 2,000,000"
)

(simple-tests ordinal1-tests
  (cl-format nil "~:R" 1) "first"
  (cl-format nil "~:R" 11) "eleventh"
  (cl-format nil "~:R" 21) "twenty-first"
  (cl-format nil "~:R" 20) "twentieth"
  (cl-format nil "~:R" 220) "two hundred twentieth"
  (cl-format nil "~:R" 200) "two hundredth"
  (cl-format nil "~:R" 999) "nine hundred ninety-ninth"
)

(simple-tests roman-tests
  (cl-format nil "~@R" 3) "III"
  (cl-format nil "~@R" 4) "IV"
  (cl-format nil "~@R" 9) "IX"
  (cl-format nil "~@R" 29) "XXIX"
  (cl-format nil "~@R" 429) "CDXXIX"
  (cl-format nil "~@:R" 429) "CCCCXXVIIII"
  (cl-format nil "~@:R" 3429) "MMMCCCCXXVIIII"
  (cl-format nil "~@R" 3429) "MMMCDXXIX"
  (cl-format nil "~@R" 3479) "MMMCDLXXIX"
  (cl-format nil "~@R" 3409) "MMMCDIX"
  (cl-format nil "~@R" 300) "CCC"
  (cl-format nil "~@R ~D" 300 20) "CCC 20"
  (cl-format nil "~@R" 5000) "5,000"
  (cl-format nil "~@R ~D" 5000 20) "5,000 20"
  (cl-format nil "~@R" "the quick") "the quick"
)

(simple-tests c-tests
  (cl-format nil "~{~c~^, ~}~%" "hello") "h, e, l, l, o\n"
  (cl-format nil "~{~:c~^, ~}~%" "hello") "h, e, l, l, o\n"
  (cl-format nil "~@C~%" \m) "\\m\n"
  (cl-format nil "~@C~%" (char 222)) "\\Þ\n"

  ;;chars that are specially printed in cljs
  (cl-format nil "~@C~%" (char 8)) "\\backspace\n"
  (cl-format nil "~@C~%" (char 9)) "\\tab\n"
  (cl-format nil "~@C~%" (char 10)) "\\newline\n"
  (cl-format nil "~@C~%" (char 12)) "\\formfeed\n"
  (cl-format nil "~@C~%" (char 13)) "\\return\n"
  (cl-format nil "~@C~%" (char 34)) "\\\"\n"
  (cl-format nil "~@C~%" (char 92)) "\\\\\n"

  (cl-format nil "~@C~%" (char 3)) "\\\n"
)

(simple-tests e-tests
  (cl-format nil "*~E*" 0.0) "*0.0E+0*"
  (cl-format nil "*~6E*" 0.0) "*0.0E+0*"
  (cl-format nil "*~6,0E*" 0.0) "* 0.E+0*"
  (cl-format nil "*~7,2E*" 0.0) "*0.00E+0*"
  (cl-format nil "*~5E*" 0.0) "*0.E+0*"
  (cl-format nil "*~10,2,2,,'?E*" 2.8E120) "*??????????*"
  (cl-format nil "*~10,2E*" 9.99999) "*   1.00E+1*"
  (cl-format nil "*~10,2E*" 9.99999E99) "* 1.00E+100*"
  (cl-format nil "*~10,2,2E*" 9.99999E99) "* 1.00E+100*"
  (cl-format nil "*~10,2,2,,'?E*" 9.99999E99) "*??????????*"
)

(simple-tests $-tests
  (cl-format nil "~$" 22.3) "22.30"
  (cl-format nil "~$" 22.375) "22.38"
  (cl-format nil "~3,5$" 22.375) "00022.375"
  (cl-format nil "~3,5,8$" 22.375) "00022.375"
  (cl-format nil "~3,5,10$" 22.375) " 00022.375"
  (cl-format nil "~3,5,14@$" 22.375) "    +00022.375"
  (cl-format nil "~3,5,14@$" 22.375) "    +00022.375"
  (cl-format nil "~3,5,14@:$" 22.375) "+    00022.375"
  (cl-format nil "~3,,14@:$" 0.375) "+        0.375"
  (cl-format nil "~1,1$" -12.0) "-12.0"
  (cl-format nil "~1,1$" 12.0) "12.0"
  (cl-format nil "~1,1$" 12.0) "12.0"
  (cl-format nil "~1,1@$" 12.0) "+12.0"
  (cl-format nil "~1,1,8,' @:$" 12.0) "+   12.0"
  (cl-format nil "~1,1,8,' @$" 12.0) "   +12.0"
  (cl-format nil "~1,1,8,' :$" 12.0) "    12.0"
  (cl-format nil "~1,1,8,' $" 12.0) "    12.0"
  (cl-format nil "~1,1,8,' @:$" -12.0) "-   12.0"
  (cl-format nil "~1,1,8,' @$" -12.0) "   -12.0"
  (cl-format nil "~1,1,8,' :$" -12.0) "-   12.0"
  (cl-format nil "~1,1,8,' $" -12.0) "   -12.0"
  (cl-format nil "~1,1$" 0.001) "0.0"
  (cl-format nil "~2,1$" 0.001) "0.00"
  (cl-format nil "~1,1,6$" 0.001) "   0.0"
  (cl-format nil "~1,1,6$" 0.0015) "   0.0"
  (cl-format nil "~2,1,6$" 0.005) "  0.01"
  (cl-format nil "~2,1,6$" 0.01) "  0.01"
  (cl-format nil "~$" 0.099) "0.10"
  (cl-format nil "~1$" 0.099) "0.1"
  (cl-format nil "~1$" 0.1) "0.1"
  (cl-format nil "~1$" 0.99) "1.0"
  (cl-format nil "~1$" -0.99) "-1.0"
)

(simple-tests f-tests
  (cl-format nil "~,1f" -12.0) "-12.0"
  (cl-format nil "~,0f" 9.4) "9."
  (cl-format nil "~,0f" 9.5) "10."
  (cl-format nil "~,0f" -0.99) "-1."
  (cl-format nil "~,1f" -0.99) "-1.0"
  (cl-format nil "~,2f" -0.99) "-0.99"
  (cl-format nil "~,3f" -0.99) "-0.990"
  (cl-format nil "~,0f" 0.99) "1."
  (cl-format nil "~,1f" 0.99) "1.0"
  (cl-format nil "~,2f" 0.99) "0.99"
  (cl-format nil "~,3f" 0.99) "0.990"
  (cl-format nil "~,3f" -0.099) "-0.099"
  (cl-format nil "~,4f" -0.099) "-0.0990"
  (cl-format nil "~,5f" -0.099) "-0.09900"
  (cl-format nil "~,3f" 0.099) "0.099"
  (cl-format nil "~,4f" 0.099) "0.0990"
  (cl-format nil "~,5f" 0.099) "0.09900"
  (cl-format nil "~f" -1) "-1.0"
  (cl-format nil "~2f" -1) "-1."
  (cl-format nil "~3f" -1) "-1."
  (cl-format nil "~4f" -1) "-1.0"
  (cl-format nil "~8f" -1) "    -1.0"
  (cl-format nil "~2f" -0.0099) "-0."
  (cl-format nil "~3f" -0.0099) "-0."
  (cl-format nil "~4f" -0.0099) "-.01"
  (cl-format nil "~5f" -0.0099) "-0.01"
  (cl-format nil "~6f" -0.0099) "-.0099"
  (cl-format nil "~1f" 0.0099) "0."
  (cl-format nil "~2f" 0.0099) "0."
  (cl-format nil "~3f" 0.0099) ".01"
  (cl-format nil "~4f" 0.0099) "0.01"
  (cl-format nil "~5f" 0.0099) ".0099"
  (cl-format nil "~6f" 0.0099) "0.0099"
  (cl-format nil "~1f" -0.099) "-.1"
  (cl-format nil "~2f" -0.099) "-.1"
  (cl-format nil "~3f" -0.099) "-.1"
  (cl-format nil "~4f" -0.099) "-0.1"
  (cl-format nil "~5f" -0.099) "-.099"
  (cl-format nil "~6f" -0.099) "-0.099"
  (cl-format nil "~1f" 0.099) ".1"
  (cl-format nil "~2f" 0.099) ".1"
  (cl-format nil "~3f" 0.099) "0.1"
  (cl-format nil "~4f" 0.099) ".099"
  (cl-format nil "~5f" 0.099) "0.099"
  (cl-format nil "~1f" -0.99) "-1."
  (cl-format nil "~2f" -0.99) "-1."
  (cl-format nil "~3f" -0.99) "-1."
  (cl-format nil "~4f" -0.99) "-.99"
  (cl-format nil "~5f" -0.99) "-0.99"
  (cl-format nil "~1f" 0.99) "1."
  (cl-format nil "~2f" 0.99) "1."
  (cl-format nil "~3f" 0.99) ".99"
  (cl-format nil "~4f" 0.99) "0.99"
  (cl-format nil "~1f" 111.11111) "111."
  (cl-format nil "~4f" 111.11111) "111."
  (cl-format nil "~5f" 111.11111) "111.1"
  (cl-format nil "~1f" -111.11111) "-111."
  (cl-format nil "~5f" -111.11111) "-111."
  (cl-format nil "~6f" -111.11111) "-111.1"
  (cl-format nil "~1f" 555.55555) "556."
  (cl-format nil "~4f" 555.55555) "556."
  (cl-format nil "~5f" 555.55555) "555.6"
  (cl-format nil "~8f" 555.55555) "555.5556"
  (cl-format nil "~1f" -555.55555) "-556."
  (cl-format nil "~5f" -555.55555) "-556."
  (cl-format nil "~6f" -555.55555) "-555.6"
  (cl-format nil "~8f" -555.55555) "-555.556"
  (cl-format nil "~1f" 999.999) "1000."
  (cl-format nil "~5f" 999.999) "1000."
  (cl-format nil "~6f" 999.999) "1000.0"
  (cl-format nil "~7f" 999.999) "999.999"
  (cl-format nil "~8f" 999.999) " 999.999"
  (cl-format nil "~1f" -999.999) "-1000."
  (cl-format nil "~6f" -999.999) "-1000."
  (cl-format nil "~7f" -999.999) "-1000.0"
  (cl-format nil "~8f" -999.999) "-999.999"
  (cl-format nil "~5,2f" 111.11111) "111.11"
  (cl-format nil "~3,1f" -0.0099) "-.0"
  (cl-format nil "~6,4f" -0.0099) "-.0099"
  (cl-format nil "~6,5f" -0.0099) "-.00990"
  (cl-format nil "~6,6f" -0.0099) "-.009900"
  (cl-format nil "~6,4f" 0.0099) "0.0099"
  (cl-format nil "~6,5f" 0.0099) ".00990"
  (cl-format nil "~6,6f" 0.0099) ".009900"
  (cl-format nil "~2,1f" 0.0099) ".0"
  (cl-format nil "~6,2f" -111.11111) "-111.11"
  (cl-format nil "~6,3f" -111.11111) "-111.111"
  (cl-format nil "~8,5f" -111.11111) "-111.11111"
  (cl-format nil "~12,10f" 1.23456789014) "1.2345678901"
  (cl-format nil "~12,10f" 1.23456789016) "1.2345678902"
  (cl-format nil "~13,10f" -1.23456789014) "-1.2345678901"
  (cl-format nil "~13,10f" -1.23456789016) "-1.2345678902"
  (cl-format nil "~1,1f" 0.1) ".1"
)

(simple-tests ampersand-tests
  (cl-format nil "The quick brown ~a jumped over ~d lazy dogs" 'elephant 5)
  "The quick brown elephant jumped over 5 lazy dogs"
  (cl-format nil "The quick brown ~&~a jumped over ~d lazy dogs" 'elephant 5)
  "The quick brown \nelephant jumped over 5 lazy dogs"
  (cl-format nil "The quick brown ~&~a jumped\n~& over ~d lazy dogs" 'elephant 5)
  "The quick brown \nelephant jumped\n over 5 lazy dogs"
  (cl-format nil "~&The quick brown ~&~a jumped\n~& over ~d lazy dogs" 'elephant 5)
  "The quick brown \nelephant jumped\n over 5 lazy dogs"
  (cl-format nil "~3&The quick brown ~&~a jumped\n~& over ~d lazy dogs" 'elephant 5)
  "\n\nThe quick brown \nelephant jumped\n over 5 lazy dogs"
  (cl-format nil "~@{~&The quick brown ~a jumped over ~d lazy dogs~}" 'elephant 5 'fox 10)
  "The quick brown elephant jumped over 5 lazy dogs\nThe quick brown fox jumped over 10 lazy dogs"
  (cl-format nil "I ~[don't ~:;d~&o ~]have one~%" 0) "I don't have one\n"
  (cl-format nil "I ~[don't ~:;d~&o ~]have one~%" 1) "I d\no have one\n"
)

(simple-tests t-tests
  (cl-format nil "~@{~&~A~8,4T~:*~A~}"
             'a 'aa 'aaa 'aaaa 'aaaaa 'aaaaaa 'aaaaaaa 'aaaaaaaa 'aaaaaaaaa 'aaaaaaaaaa)
  "a       a\naa      aa\naaa     aaa\naaaa    aaaa\naaaaa   aaaaa\naaaaaa  aaaaaa\naaaaaaa aaaaaaa\naaaaaaaa    aaaaaaaa\naaaaaaaaa   aaaaaaaaa\naaaaaaaaaa  aaaaaaaaaa"
  (cl-format nil "~@{~&~A~,4T~:*~A~}"
             'a 'aa 'aaa 'aaaa 'aaaaa 'aaaaaa 'aaaaaaa 'aaaaaaaa 'aaaaaaaaa 'aaaaaaaaaa)
  "a    a\naa   aa\naaa  aaa\naaaa aaaa\naaaaa    aaaaa\naaaaaa   aaaaaa\naaaaaaa  aaaaaaa\naaaaaaaa aaaaaaaa\naaaaaaaaa    aaaaaaaaa\naaaaaaaaaa   aaaaaaaaaa"
  (cl-format nil "~@{~&~A~2,6@T~:*~A~}" 'a 'aa 'aaa 'aaaa 'aaaaa 'aaaaaa 'aaaaaaa 'aaaaaaaa 'aaaaaaaaa 'aaaaaaaaaa)
  "a     a\naa    aa\naaa   aaa\naaaa  aaaa\naaaaa       aaaaa\naaaaaa      aaaaaa\naaaaaaa     aaaaaaa\naaaaaaaa    aaaaaaaa\naaaaaaaaa   aaaaaaaaa\naaaaaaaaaa  aaaaaaaaaa"
)

(simple-tests paren-tests
  (cl-format nil "~(PLEASE SPEAK QUIETLY IN HERE~)") "please speak quietly in here"
  (cl-format nil "~@(PLEASE SPEAK QUIETLY IN HERE~)") "Please speak quietly in here"
  (cl-format nil "~@:(but this Is imporTant~)") "BUT THIS IS IMPORTANT"
  (cl-format nil "~:(the greAt gatsby~)!") "The Great Gatsby!"
  ;; Test cases from CLtL 18.3 - string-upcase, et al.
  (cl-format nil "~@:(~A~)" "Dr. Livingstone, I presume?") "DR. LIVINGSTONE, I PRESUME?"
  (cl-format nil "~(~A~)" "Dr. Livingstone, I presume?") "dr. livingstone, i presume?"
  (cl-format nil "~:(~A~)" " hello ") " Hello "
  (cl-format nil "~:(~A~)" "occlUDeD cASEmenTs FOreSTAll iNADVertent DEFenestraTION")
  "Occluded Casements Forestall Inadvertent Defenestration"
  (cl-format nil "~:(~A~)" 'kludgy-hash-search) "Kludgy-Hash-Search"
  (cl-format nil "~:(~A~)" "DON'T!") "Don'T!"     ;not "Don't!"
  (cl-format nil "~:(~A~)" "pipe 13a, foo16c") "Pipe 13a, Foo16c"
  (cl-format nil "~:(~A~)" nil) "Nil"
  (cl-format nil "~:(~A~)" "") ""
)

(simple-tests square-bracket-tests
  ;; Tests for format without modifiers
  (cl-format nil "I ~[don't ~]have one~%" 0) "I don't have one\n"
  (cl-format nil "I ~[don't ~]have one~%" 1) "I have one\n"
  (cl-format nil "I ~[don't ~;do ~]have one~%" 0) "I don't have one\n"
  (cl-format nil "I ~[don't ~;do ~]have one~%" 1) "I do have one\n"
  (cl-format nil "I ~[don't ~;do ~]have one~%" 2) "I have one\n"
  (cl-format nil "I ~[don't ~:;do ~]have one~%" 0) "I don't have one\n"
  (cl-format nil "I ~[don't ~:;do ~]have one~%" 1) "I do have one\n"
  (cl-format nil "I ~[don't ~:;do ~]have one~%" 2) "I do have one\n"
  (cl-format nil "I ~[don't ~:;do ~]have one~%" 700) "I do have one\n"

  ;; Tests for format with a colon
  (cl-format nil "I ~:[don't ~;do ~]have one~%" true) "I do have one\n"
  (cl-format nil "I ~:[don't ~;do ~]have one~%" 700) "I do have one\n"
  (cl-format nil "I ~:[don't ~;do ~]have one~%" '(a b)) "I do have one\n"
  (cl-format nil "I ~:[don't ~;do ~]have one~%" nil) "I don't have one\n"
  (cl-format nil "I ~:[don't ~;do ~]have one~%" false) "I don't have one\n"

  ;; Tests for format with an at sign
  (cl-format nil "We had ~D wins~@[ (out of ~D tries)~].~%" 15 nil) "We had 15 wins.\n"
  (cl-format nil "We had ~D wins~@[ (out of ~D tries)~].~%" 15 17)
  "We had 15 wins (out of 17 tries).\n"

  ;; Format tests with directives
  (cl-format nil "Max ~D: ~[Blue team ~D~;Red team ~D~:;No team ~A~].~%" 15, 0, 7)
  "Max 15: Blue team 7.\n"
  (cl-format nil "Max ~D: ~[Blue team ~D~;Red team ~D~:;No team ~A~].~%" 15, 1, 12)
  "Max 15: Red team 12.\n"
  (cl-format nil "Max ~D: ~[Blue team ~D~;Red team ~D~:;No team ~A~].~%"
             15, -1, "(system failure)")
  "Max 15: No team (system failure).\n"

  ;; Nested format tests
  (cl-format nil "Max ~D: ~[Blue team ~D~:[~; (complete success)~]~;Red team ~D~:;No team ~].~%"
             15, 0, 7, true)
  "Max 15: Blue team 7 (complete success).\n"
  (cl-format nil "Max ~D: ~[Blue team ~D~:[~; (complete success)~]~;Red team ~D~:;No team ~].~%"
             15, 0, 7, false)
  "Max 15: Blue team 7.\n"

  ;; Test the selector as part of the argument
  (cl-format nil "The answer is ~#[nothing~;~D~;~D out of ~D~:;something crazy~].")
  "The answer is nothing."
  (cl-format nil "The answer is ~#[nothing~;~D~;~D out of ~D~:;something crazy~]." 4)
  "The answer is 4."
  (cl-format nil "The answer is ~#[nothing~;~D~;~D out of ~D~:;something crazy~]." 7 22)
  "The answer is 7 out of 22."
  (cl-format nil "The answer is ~#[nothing~;~D~;~D out of ~D~:;something crazy~]." 1 2 3 4)
  "The answer is something crazy."
)

(simple-tests curly-brace-plain-tests
  ;; Iteration from sublist
  (cl-format nil "Coordinates are~{ [~D,~D]~}~%" [ 0, 1, 1, 0, 3, 5, 2, 1 ])
  "Coordinates are [0,1] [1,0] [3,5] [2,1]\n"

  (cl-format nil "Coordinates are~2{ [~D,~D]~}~%" [ 0, 1, 1, 0, 3, 5, 2, 1 ])
  "Coordinates are [0,1] [1,0]\n"

  (cl-format nil "Coordinates are~{ ~#[none~;<~D>~:;[~D,~D]~]~}~%" [ ])
  "Coordinates are\n"

  (cl-format nil "Coordinates are~{ ~#[none~;<~D>~:;[~D,~D]~]~:}~%" [ ])
  "Coordinates are none\n"

  (cl-format nil "Coordinates are~{ ~#[none~;<~D>~:;[~D,~D]~]~:}~%" [2 3 1])
  "Coordinates are [2,3] <1>\n"

  (cl-format nil "Coordinates are~{~:}~%" "" [])
  "Coordinates are\n"

  (cl-format nil "Coordinates are~{~:}~%" " ~#[none~;<~D>~:;[~D,~D]~]" [2 3 1])
  "Coordinates are [2,3] <1>\n"

  (cl-format nil "Coordinates are~{~:}~%" " ~#[none~;<~D>~:;[~D,~D]~]" [ ])
  "Coordinates are none\n"
)

(simple-tests curly-brace-colon-tests
  ;; Iteration from list of sublists
  (cl-format nil "Coordinates are~:{ [~D,~D]~}~%" [ [0, 1], [1, 0], [3, 5], [2, 1] ])
  "Coordinates are [0,1] [1,0] [3,5] [2,1]\n"

  (cl-format nil "Coordinates are~:{ [~D,~D]~}~%" [ [0, 1, 0], [1, 0, 12], [3, 5], [2, 1] ])
  "Coordinates are [0,1] [1,0] [3,5] [2,1]\n"

  (cl-format nil "Coordinates are~2:{ [~D,~D]~}~%" [ [0, 1], [1, 0], [3, 5], [2, 1] ])
  "Coordinates are [0,1] [1,0]\n"

  (cl-format nil "Coordinates are~:{ ~#[none~;<~D>~:;[~D,~D]~]~}~%" [ ])
  "Coordinates are\n"

  (cl-format nil "Coordinates are~:{ ~#[none~;<~D>~:;[~D,~D]~]~:}~%" [ ])
  "Coordinates are none\n"

  (cl-format nil "Coordinates are~:{ ~#[none~;<~D>~:;[~D,~D]~]~:}~%" [[2 3] [1]])
  "Coordinates are [2,3] <1>\n"

  (cl-format nil "Coordinates are~:{~:}~%" "" [])
  "Coordinates are\n"

  (cl-format nil "Coordinates are~:{~:}~%" " ~#[none~;<~D>~:;[~D,~D]~]" [[2 3] [1]])
  "Coordinates are [2,3] <1>\n"

  (cl-format nil "Coordinates are~:{~:}~%" " ~#[none~;<~D>~:;[~D,~D]~]" [ ])
  "Coordinates are none\n"
)

(simple-tests curly-brace-at-tests
  ;; Iteration from main list
  (cl-format nil "Coordinates are~@{ [~D,~D]~}~%"  0, 1, 1, 0, 3, 5, 2, 1)
  "Coordinates are [0,1] [1,0] [3,5] [2,1]\n"

  (cl-format nil "Coordinates are~2@{ [~D,~D]~}~%" 0, 1, 1, 0, 3, 5, 2, 1)
  "Coordinates are [0,1] [1,0]\n"

  (cl-format nil "Coordinates are~@{ ~#[none~;<~D>~:;[~D,~D]~]~}~%")
  "Coordinates are\n"

  (cl-format nil "Coordinates are~@{ ~#[none~;<~D>~:;[~D,~D]~]~:}~%")
  "Coordinates are none\n"

  (cl-format nil "Coordinates are~@{ ~#[none~;<~D>~:;[~D,~D]~]~:}~%" 2 3 1)
  "Coordinates are [2,3] <1>\n"

  (cl-format nil "Coordinates are~@{~:}~%" "")
  "Coordinates are\n"

  (cl-format nil "Coordinates are~@{~:}~%" " ~#[none~;<~D>~:;[~D,~D]~]" 2 3 1)
  "Coordinates are [2,3] <1>\n"

  (cl-format nil "Coordinates are~@{~:}~%" " ~#[none~;<~D>~:;[~D,~D]~]")
  "Coordinates are none\n"
)

(simple-tests curly-brace-colon-at-tests
  ;; Iteration from sublists on the main arg list
  (cl-format nil "Coordinates are~@:{ [~D,~D]~}~%"  [0, 1], [1, 0], [3, 5], [2, 1] )
  "Coordinates are [0,1] [1,0] [3,5] [2,1]\n"

  (cl-format nil "Coordinates are~@:{ [~D,~D]~}~%" [0, 1, 0], [1, 0, 12], [3, 5], [2, 1] )
  "Coordinates are [0,1] [1,0] [3,5] [2,1]\n"

  (cl-format nil "Coordinates are~2@:{ [~D,~D]~}~%" [0, 1], [1, 0], [3, 5], [2, 1])
  "Coordinates are [0,1] [1,0]\n"

  (cl-format nil "Coordinates are~@:{ ~#[none~;<~D>~:;[~D,~D]~]~}~%")
  "Coordinates are\n"

  (cl-format nil "Coordinates are~@:{ ~#[none~;<~D>~:;[~D,~D]~]~:}~%")
  "Coordinates are none\n"

  (cl-format nil "Coordinates are~@:{ ~#[none~;<~D>~:;[~D,~D]~]~:}~%" [2 3] [1])
  "Coordinates are [2,3] <1>\n"

  (cl-format nil "Coordinates are~@:{~:}~%" "")
  "Coordinates are\n"

  (cl-format nil "Coordinates are~@:{~:}~%" " ~#[none~;<~D>~:;[~D,~D]~]" [2 3] [1])
  "Coordinates are [2,3] <1>\n"

  (cl-format nil "Coordinates are~@:{~:}~%" " ~#[none~;<~D>~:;[~D,~D]~]")
  "Coordinates are none\n"
)

;; TODO tests for ~^ in ~[ constructs and other brackets
;; TODO test ~:^ generates an error when used improperly
;; TODO test ~:^ works in ~@:{...~}
(let [aseq '(a quick brown fox jumped over the lazy dog)
      lseq (mapcat identity (for [x aseq] [x (.-length (name x))]))]
  (simple-tests up-tests
    (cl-format nil "~{~a~^, ~}" aseq) "a, quick, brown, fox, jumped, over, the, lazy, dog"
    (cl-format nil "~{~a~0^, ~}" aseq) "a"
    (cl-format nil "~{~a~#,3^, ~}" aseq) "a, quick, brown, fox, jumped, over"
    (cl-format nil "~{~a~v,3^, ~}" lseq) "a, quick, brown, fox"
    (cl-format nil "~{~a~3,v,4^, ~}" lseq) "a, quick, brown, fox"
  ))

(simple-tests angle-bracket-tests
  (cl-format nil "~<foo~;bar~;baz~>") "foobarbaz"
  (cl-format nil "~20<foo~;bar~;baz~>") "foo      bar     baz"
  (cl-format nil "~,,2<foo~;bar~;baz~>") "foo  bar  baz"
  (cl-format nil "~20<~A~;~A~;~A~>" "foo" "bar" "baz") "foo      bar     baz"
  (cl-format nil "~20:<~A~;~A~;~A~>" "foo" "bar" "baz") "    foo    bar   baz"
  (cl-format nil "~20@<~A~;~A~;~A~>" "foo" "bar" "baz") "foo    bar    baz   "
  (cl-format nil "~20@:<~A~;~A~;~A~>" "foo" "bar" "baz") "   foo   bar   baz  "
  (cl-format nil "~10,,2<~A~;~A~;~A~>" "foo" "bar" "baz") "foo  bar  baz"
  (cl-format nil "~10,10,2<~A~;~A~;~A~>" "foo" "bar" "baz") "foo      bar     baz"
  (cl-format nil "~10,10<~A~;~A~;~A~>" "foo" "bar" "baz") "foo barbaz"
  (cl-format nil "~20<~A~;~^~A~;~^~A~>" "foo" "bar" "baz") "foo      bar     baz"
  (cl-format nil "~20<~A~;~^~A~;~^~A~>" "foo" "bar") "foo              bar"
  (cl-format nil "~20@<~A~;~^~A~;~^~A~>" "foo") "foo                 "
  (cl-format nil "~20:<~A~;~^~A~;~^~A~>" "foo") "                 foo"
)

(simple-tests angle-bracket-max-column-tests
  (cl-format nil "~%;; ~{~<~%;; ~1,50:; ~A~>~}.~%" (into [] (clojure.string/split "This function computes the circular thermodynamic coefficient of the thrombulator angle for use in determining the reaction distance" #"\s")))
  "\n;;  This function computes the circular\n;;  thermodynamic coefficient of the thrombulator\n;;  angle for use in determining the reaction\n;;  distance.\n"
  (cl-format true "~%;; ~{~<~%;; ~:; ~A~>~}.~%" (into [] (clojure.string/split "This function computes the circular thermodynamic coefficient of the thrombulator angle for use in determining the reaction distance." #"\s")))
)

(defn list-to-table-stream [aseq column-width]
  (let [sb (StringBuffer.)]
    (binding [*out* (get-pretty-writer (StringBufferWriter. sb))]
      (doseq [row aseq]
        (doseq [col row]
          (cl-format *out* "~4D~7,vT" col column-width))
        (#'prn)))
    (str sb)
    ;;TODO do we need to extend StringBufferWriter to allow access to underlying StringBuffer?
    #_(str (:base @@(:base @@stream)))))

(defn list-to-table-print [aseq column-width]
  (let [sb (StringBuffer.)]
    (binding [*print-fn* (fn [s] (.append sb (apply str s)))
              *print-newline* true]
      (doseq [row aseq]
        (doseq [col row]
          (cl-format true "~4D~7,vT" col column-width))
        (cljs.core/prn)))
    (str sb)))

(simple-tests column-writer-test
  (list-to-table-stream (map #(vector % (* % %) (* % % %)) (range 1 21)) 8)
  "   1      1       1    \n   2      4       8    \n   3      9      27    \n   4     16      64    \n   5     25     125    \n   6     36     216    \n   7     49     343    \n   8     64     512    \n   9     81     729    \n  10    100    1000    \n  11    121    1331    \n  12    144    1728    \n  13    169    2197    \n  14    196    2744    \n  15    225    3375    \n  16    256    4096    \n  17    289    4913    \n  18    324    5832    \n  19    361    6859    \n  20    400    8000    \n"

  (list-to-table-print (map #(vector % (* % %) (* % % %)) (range 1 21)) 8)
  "   1      1      1   \n   2      4      8   \n   3      9     27   \n   4     16     64   \n   5     25    125   \n   6     36    216   \n   7     49    343   \n   8     64    512   \n   9     81    729   \n  10    100   1000   \n  11    121   1331   \n  12    144   1728   \n  13    169   2197   \n  14    196   2744   \n  15    225   3375   \n  16    256   4096   \n  17    289   4913   \n  18    324   5832   \n  19    361   6859   \n  20    400   8000   \n"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following tests are the various examples from the format
;; documentation in Common Lisp, the Language, 2nd edition, Chapter 22.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expt [base pow] (reduce * (repeat pow base)))

(let [x 5, y "elephant", n 3]
  (simple-tests cltl-intro-tests
    (format nil "foo")  "foo"
    (format nil "The answer is ~D." x)  "The answer is 5."
    (format nil "The answer is ~3D." x)  "The answer is   5."
    (format nil "The answer is ~3,'0D." x)  "The answer is 005."
    (format nil "The answer is ~:D." (expt 47 x)) "The answer is 229,345,007."
    (format nil "Look at the ~A!" y)  "Look at the elephant!"
    (format nil "Type ~:C to ~A." (char 4) "delete all your files")
    "Type Control-D to delete all your files."
    (format nil "~D item~:P found." n)  "3 items found."
    (format nil "~R dog~:[s are~; is~] here." n (= n 1)) "three dogs are here."
    (format nil "~R dog~:*~[s are~; is~:;s are~] here." n) "three dogs are here."
    (format nil "Here ~[are~;is~:;are~] ~:*~R pupp~:@P." n) "Here are three puppies."))

(simple-tests cltl-B-tests
  ;; CLtL didn't have the colons here, but the spec requires them
  (format nil "~,,' ,4:B" 0xFACE) "1111 1010 1100 1110"
  (format nil "~,,' ,4:B" 0x1CE) "1 1100 1110"
  (format nil "~19,,' ,4:B" 0xFACE) "1111 1010 1100 1110"
  ;; This one was a nice idea, but nothing in the spec supports it working this way
  ;; (and SBCL doesn't work this way either)
  ;(format nil "~19,,' ,4:B" 0x1CE) "0000 0001 1100 1110")
)

(simple-tests cltl-P-tests
  (format nil "~D tr~:@P/~D win~:P" 7 1) "7 tries/1 win"
  (format nil "~D tr~:@P/~D win~:P" 1 0) "1 try/0 wins"
  (format nil "~D tr~:@P/~D win~:P" 1 3) "1 try/3 wins"
)

(defn foo [x]
  (format nil "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F"
          x x x x x x))

;; big-pos-ratio is a ratio value that is larger than
;; Double/MAX_VALUE, and has a non-terminating decimal representation
;; if you attempt to represent it exactly.
#_(def big-pos-ratio (/ (* 4 (bigint (. BigDecimal valueOf Double/MAX_VALUE))) 3))
#_(def big-neg-ratio (- big-pos-ratio))
;; tiny-pos-ratio is a ratio between 0 and Double/MIN_VALUE.
#_(def tiny-pos-ratio (/ 1 (bigint (apply str (cons "1" (repeat 340 "0"))))))
#_(def tiny-neg-ratio (- tiny-pos-ratio))

(simple-tests cltl-F-tests
  #_(cl-format false "~10,3f" 4/5)
  #_"     0.800"
  #_(binding [*math-context* java.math.MathContext/DECIMAL128]
    (cl-format false "~10,3f" big-pos-ratio))
  #_"239692417981642093333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.000"
  #_(binding [*math-context* java.math.MathContext/DECIMAL128]
    (cl-format false "~10,3f" big-neg-ratio))
  #_"-239692417981642093333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.000"
  #_(binding [*math-context* java.math.MathContext/DECIMAL128]
    (cl-format false "~10,3f" tiny-pos-ratio))
  #_"     0.000"
  #_(binding [*math-context* java.math.MathContext/DECIMAL128]
    (cl-format false "~10,3f" tiny-neg-ratio))
  #_"    -0.000"
  (foo 3.14159)  "  3.14| 31.42|  3.14|3.1416|3.14|3.14159"
  #_(foo 314159/100000)
  #_"  3.14| 31.42|  3.14|3.1416|3.14|3.14159"
  (foo -3.14159) " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159"
  (foo 100.0)    "100.00|******|100.00| 100.0|100.00|100.0"
  (foo 1234.0)   "1234.00|******|??????|1234.0|1234.00|1234.0"
  (foo 0.006)    "  0.01|  0.06|  0.01| 0.006|0.01|0.006"
)

(defn foo-e [x]
  (format nil
          "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~9,3,2,-2,'%@E|~9,2E"
          x x x x))

;; Clojure doesn't support float/double differences in representation
(simple-tests cltl-E-tests
  #_(cl-format false "~10,3e" 4/5)
  #_"  8.000E-1"
  #_(binding [*math-context* java.math.MathContext/DECIMAL128]
    (cl-format false "~10,3e" big-pos-ratio))
  #_"2.397E+308"
  #_(binding [*math-context* java.math.MathContext/DECIMAL128]
    (cl-format false "~10,3e" big-neg-ratio))
  #_"-2.397E+308"
  #_(binding [*math-context* java.math.MathContext/DECIMAL128]
    (cl-format false "~10,3e" tiny-pos-ratio))
  #_"1.000E-340"
  #_(binding [*math-context* java.math.MathContext/DECIMAL128]
    (cl-format false "~10,3e" tiny-neg-ratio))
  #_"-1.000E-340"
  (foo-e 0.0314159) "  3.14E-2| 31.42$-03|+.003E+01|  3.14E-2"  ; Added this one
  #_(foo-e 314159/10000000)
  #_"  3.14E-2| 31.42$-03|+.003E+01|  3.14E-2"
  (foo-e 3.14159)  "  3.14E+0| 31.42$-01|+.003E+03|  3.14E+0"
  (foo-e -3.14159) " -3.14E+0|-31.42$-01|-.003E+03| -3.14E+0"
  (foo-e 1100.0)   "  1.10E+3| 11.00$+02|+.001E+06|  1.10E+3"
  ; In Clojure, this is identical to the above
  ;  (foo-e 1100.0L0) "  1.10L+3| 11.00$+02|+.001L+06|  1.10L+3"
  (foo-e 1.1E13)   "*********| 11.00$+12|+.001E+16| 1.10E+13"
  (foo-e 1.1E120)  "*********|??????????|%%%%%%%%%|1.10E+120"
  ; Clojure doesn't support real numbers this large
  ;  (foo-e 1.1L1200) "*********|??????????|%%%%%%%%%|1.10L+1200"
)

(simple-tests cltl-E-scale-tests
  (map
    (fn [k] (format nil "Scale factor ~2D~:*: |~13,6,2,VE|"
                    (- k 5) 3.14159))              ;Prints 13 lines
    (range 13))
  '("Scale factor -5: | 0.000003E+06|"
     "Scale factor -4: | 0.000031E+05|"
     "Scale factor -3: | 0.000314E+04|"
     "Scale factor -2: | 0.003142E+03|"
     "Scale factor -1: | 0.031416E+02|"
     "Scale factor  0: | 0.314159E+01|"
     "Scale factor  1: | 3.141590E+00|"
     "Scale factor  2: | 31.41590E-01|"
     "Scale factor  3: | 314.1590E-02|"
     "Scale factor  4: | 3141.590E-03|"
     "Scale factor  5: | 31415.90E-04|"
     "Scale factor  6: | 314159.0E-05|"
     "Scale factor  7: | 3141590.E-06|"))

(defn foo-g [x]
  (format nil
          "~9,2,1,,'*G|~9,3,2,3,'?,,'$G|~9,3,2,0,'%G|~9,2G"
          x x x x))

;; Clojure doesn't support float/double differences in representation
(simple-tests cltl-G-tests
  #_(cl-format false "~10,3g" 4/5)
  #_" 0.800    "
  #_(binding [*math-context* java.math.MathContext/DECIMAL128]
    (cl-format false "~10,3g" big-pos-ratio))
  #_"2.397E+308"
  #_(binding [*math-context* java.math.MathContext/DECIMAL128]
    (cl-format false "~10,3g" big-neg-ratio))
  #_"-2.397E+308"
  #_(binding [*math-context* java.math.MathContext/DECIMAL128]
    (cl-format false "~10,3g" tiny-pos-ratio))
  #_"1.000E-340"
  #_(binding [*math-context* java.math.MathContext/DECIMAL128]
    (cl-format false "~10,3g" tiny-neg-ratio))
  #_"-1.000E-340"
  (foo-g 0.0314159) "  3.14E-2|314.2$-04|0.314E-01|  3.14E-2"
  #_(foo-g 314159/10000000)
  #_"  3.14E-2|314.2$-04|0.314E-01|  3.14E-2"
  (foo-g 0.314159) "  0.31   |0.314    |0.314    | 0.31    "
  (foo-g 3.14159) "   3.1   | 3.14    | 3.14    |  3.1    "
  (foo-g 31.4159) "   31.   | 31.4    | 31.4    |  31.    "
  (foo-g 314.159) "  3.14E+2| 314.    | 314.    |  3.14E+2"
  (foo-g 3141.59) "  3.14E+3|314.2$+01|0.314E+04|  3.14E+3"
  ; In Clojure, this is identical to the above
  ;  (foo-g 3141.59L0) "  3.14L+3|314.2$+01|0.314L+04|  3.14L+3"
  (foo-g 3.14E12) "*********|314.0$+10|0.314E+13| 3.14E+12"
  (foo-g 3.14E120) "*********|?????????|%%%%%%%%%|3.14E+120"
  ; Clojure doesn't support real numbers this large
)

(defn type-clash-error [fun nargs argnum right-type wrong-type]
  (format nil ;; CLtL has this format string slightly wrong
          "~&Function ~S requires its ~:[~:R ~;~*~]~
           argument to be of type ~S,~%but it was called ~
           with an argument of type ~S.~%"
          fun (= nargs 1) argnum right-type wrong-type))

(simple-tests cltl-Newline-tests
  (type-clash-error 'aref nil 2 'integer 'vector)
  "Function aref requires its second argument to be of type integer,\nbut it was called with an argument of type vector.\n"
  (type-clash-error 'car 1 1 'list 'short-float)
  "Function car requires its argument to be of type list,\nbut it was called with an argument of type short-float.\n"
)

(simple-tests cltl-?-tests
  (format nil "~? ~D" "<~A ~D>" '("Foo" 5) 7) "<Foo 5> 7"
  (format nil "~? ~D" "<~A ~D>" '("Foo" 5 14) 7) "<Foo 5> 7"
  (format nil "~@? ~D" "<~A ~D>" "Foo" 5 7) "<Foo 5> 7"
  (format nil "~@? ~D" "<~A ~D>" "Foo" 5 14 7) "<Foo 5> 14"
)

(defn f [n] (format nil "~@(~R~) error~:P detected." n))

(simple-tests cltl-paren-tests
  (format nil "~@R ~(~@R~)" 14 14) "XIV xiv"
  (f 0) "Zero errors detected."
  (f 1) "One error detected."
  (f 23) "Twenty-three errors detected."
)

(let [*print-level* nil *print-length* 5]
  (simple-tests cltl-bracket-tests
    (format nil "~@[ print level = ~D~]~@[ print length = ~D~]"
            *print-level* *print-length*)
    " print length = 5")
)

(let [foo "Items:~#[ none~; ~S~; ~S and ~S~
           ~:;~@{~#[~; and~] ~
           ~S~^,~}~]."]
  (simple-tests cltl-bracket1-tests
    (format nil foo) "Items: none."
    (format nil foo 'foo) "Items: foo."
    (format nil foo 'foo 'bar) "Items: foo and bar."
    (format nil foo 'foo 'bar 'baz) "Items: foo, bar, and baz."
    (format nil foo 'foo 'bar 'baz 'quux) "Items: foo, bar, baz, and quux."
  ))

(simple-tests cltl-curly-bracket-tests
  (format nil
          "The winners are:~{ ~S~}."
          '(fred harry jill))
  "The winners are: fred harry jill."

  (format nil "Pairs:~{ <~S,~S>~}." '(a 1 b 2 c 3))
  "Pairs: <a,1> <b,2> <c,3>."

  (format nil "Pairs:~:{ <~S,~S>~}." '((a 1) (b 2) (c 3)))
  "Pairs: <a,1> <b,2> <c,3>."

  (format nil "Pairs:~@{ <~S,~S>~}." 'a 1 'b 2 'c 3)
  "Pairs: <a,1> <b,2> <c,3>."

  (format nil "Pairs:~:@{ <~S,~S>~}." '(a 1) '(b 2) '(c 3))
  "Pairs: <a,1> <b,2> <c,3>."
)

(simple-tests cltl-angle-bracket-tests
  (format nil "~10<foo~;bar~>")           "foo    bar"
  (format nil "~10:<foo~;bar~>")          "  foo  bar"
  (format nil "~10:@<foo~;bar~>")         "  foo bar "
  (format nil "~10<foobar~>")             "    foobar"
  (format nil "~10:<foobar~>")            "    foobar"
  (format nil "~10@<foobar~>")            "foobar    "
  (format nil "~10:@<foobar~>")           "  foobar  "
)

(let [donestr "Done.~^  ~D warning~:P.~^  ~D error~:P."
      tellstr "~@{~@(~@[~R~^ ~]~A~)~}."] ;; The CLtL example is a little wrong here

  (simple-tests cltl-up-tests
    (format nil donestr) "Done."
    (format nil donestr 3) "Done.  3 warnings."
    (format nil donestr 1 5) "Done.  1 warning.  5 errors."
    (format nil tellstr 23) "Twenty-three."
    (format nil tellstr nil "losers") "Losers."
    (format nil tellstr 23 "losers") "Twenty-three losers."
    (format nil "~15<~S~;~^~S~;~^~S~>" 'foo)
    "            foo"
    (format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar)
    "foo         bar"
    (format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar 'baz)
    "foo   bar   baz"
  ))

(simple-tests cltl-up-x3j13-tests
  (format nil
          "~:{/~S~^ ...~}"
          '((hot dog) (hamburger) (ice cream) (french fries)))
  "/hot .../hamburger/ice .../french ..."
  (format nil
          "~:{/~S~:^ ...~}"
          '((hot dog) (hamburger) (ice cream) (french fries)))
  "/hot .../hamburger .../ice .../french"

  (format nil
          "~:{/~S~#:^ ...~}"  ;; This is wrong in CLtL
          '((hot dog) (hamburger) (ice cream) (french fries)))
  "/hot .../hamburger"
)

(simple-tests pprint-table-tests
  (with-out-str
    (print-table [:b :a]
                 [{:a 1 :b {:a 'is-a} :c ["hi" "there"]}
                  {:b 5 :a 7 :c "dog" :d -700}]))
  "
|        :b | :a |
|-----------+----|
| {:a is-a} |  1 |
|         5 |  7 |
"
  ;; This test is changed a bit due to the way JS prints large numbers, as well as the
  ;; way Nashorn formats floating point output using Java. The number was changed
  ;; (54.7e17 to 54.7e21) to make sure JS prints it in E notation (5.47E22) and Nashorn
  ;; truncates at the desired precision.
  (with-out-str
    (print-table [:a :e :d :c]
                 [{:a 54.7e21 :b {:a 'is-a} :c ["hi" "there"]}
                  {:b 5 :a -23 :c "dog" :d 'panda}]))
  "
|       :a | :e |    :d |             :c |
|----------+----+-------+----------------|
| 5.47e+22 |    |       | [\"hi\" \"there\"] |
|      -23 |    | panda |            dog |
"
  )

(deftest test-cljs-2880
  (are [expected format]
    (= expected (with-out-str (cl-format true format \a)))
    "\\a" "~@c"
    "\\o141" "~'o@c"
    "\\u0061" "~'u@c"))

(deftest test-cljs-2881
  (are [expected ch]
    (= expected (with-out-str (cl-format true "~@c" ch)))
    "\\newline" \newline
    "\\space" \space
    "\\tab" \tab
    "\\backspace" \backspace
    "\\return" \return
    "\\formfeed" \formfeed
    "\\\"" \"
    "\\\\" \\))
