;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.printing-test
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is]]
            [clojure.string :as s]
            [clojure.set :as set]))

(deftest test-print-knobs
  (testing "Testing printing knobs"
    (is (= (binding [*print-length* 0] (str [1 2 3 4 5 6 7 8 9 0]))
          "[...]"))
    (is (= (binding [*print-length* 1] (str [1 2 3 4 5 6 7 8 9 0]))
          "[1 ...]"))
    (is (= (binding [*print-length* 2] (str [1 2 3 4 5 6 7 8 9 0]))
          "[1 2 ...]"))
    (is (= (binding [*print-length* 10] (str [1 2 3 4 5 6 7 8 9 0]))
          "[1 2 3 4 5 6 7 8 9 0]"))
    ;; CLJS-804
    (is (= (binding [*print-length* 10] (str {:foo "bar"}))
          "{:foo \"bar\"}"))
    (is (= (binding [*print-length* 0] (str {:foo "bar" :baz "woz"}))
          "{...}"))
    (is (#{"{:foo \"bar\", ...}" "{:baz \"woz\", ...}"}
          (binding [*print-length* 1] (str {:foo "bar" :baz "woz"}))))
    (is (#{"{:foo \"bar\", :baz \"woz\"}" "{:baz \"woz\", :foo \"bar\"}"}
          (binding [*print-length* 10] (str {:foo "bar" :baz "woz"})))))
  )

(deftest test-print-with-opts
  (testing "Testing printing with opts - :more-marker"
    ; CLJS-1016
    (is (= (pr-str-with-opts [[1 2 3]] {:more-marker "<MORE-MARKER>" :print-length 0})
          "[<MORE-MARKER>]"))
    (is (= (pr-str-with-opts [[1 2 3]] {:more-marker "\u2026" :print-length 1})
          "[1 \u2026]"))
    (is (#{"#{1 2 \u2026}" "#{1 3 \u2026}"
           "#{2 1 \u2026}" "#{2 3 \u2026}"
           "#{3 1 \u2026}" "#{3 2 \u2026}"}
          (pr-str-with-opts [#{1 2 3}] {:more-marker "\u2026" :print-length 2})))
    (is (= (pr-str-with-opts ['(1 2 3)] {:more-marker "\u2026" :print-length 2})
          "(1 2 \u2026)"))
    (is (#{"{:1 1, :2 2, \u2026}" "{:1 1, :3 3, \u2026}"
           "{:2 2, :1 1, \u2026}" "{:2 2, :3 3, \u2026}"
           "{:3 3, :1 1, \u2026}" "{:3 3, :2 2, \u2026}"}
          (pr-str-with-opts [{:1 1 :2 2 :3 3}] {:more-marker "\u2026" :print-length 2}))))

  (testing "Testing printing with opts - :alt-impl"
    ; CLJS-1010
    (is (= (pr-str-with-opts [[1 2 3]] {:alt-impl (fn [obj writer opts] ((:fallback-impl opts) obj writer opts))})
          "[1 2 3]"))
    (is (= (pr-str-with-opts [[1 2 3]] {:alt-impl (fn [obj writer opts] (-write writer (str "<" obj ">")))})
          "<[1 2 3]>"))
    (is (= (pr-str-with-opts [[:start 1 2 [:middle] 3 4 :end] :standalone] {:alt-impl (fn [obj writer opts]
                                                                                        (if (keyword? obj)
                                                                                          (-write writer (str "|" (name obj) "|"))
                                                                                          ((:fallback-impl opts) obj writer opts)))})
          "[|start| 1 2 [|middle|] 3 4 |end|] |standalone|"))
    (is (= (pr-str-with-opts [[1 2 3]] {:alt-impl (fn [obj writer opts])})
          "")))
  )

(defrecord PrintMe [a b])

(deftest test-printing
  (testing "Testing pr-str"
    (is (= (pr-str) ""))
    (is (= (pr-str 1) "1"))
    (is (= (pr-str -1) "-1"))
    (is (= (pr-str -1.5) "-1.5"))
    (is (= (pr-str [3 4]) "[3 4]"))
    (is (= (pr-str "foo") "\"foo\""))
    (is (= (pr-str :hello) ":hello"))
    (is (= (pr-str 'goodbye) "goodbye"))
    ;;(is (= (pr-str #{1 2 3}) "#{1 2 3}"))
    (is (= (pr-str '(7 8 9)) "(7 8 9)"))
    (is (= (pr-str '(deref foo)) "(deref foo)"))
    (is (= (pr-str '(quote bar)) "(quote bar)"))
    (is (= (pr-str 'foo/bar) "foo/bar"))
    (is (= (pr-str \a) "\"a\""))
    (is (= (pr-str :foo/bar) ":foo/bar"))
    (is (= (pr-str nil) "nil"))
    (is (= (pr-str true) "true"))
    (is (= (pr-str false) "false"))
    (is (= (pr-str "string") "\"string\""))
    (is (= (pr-str ["üñîçó∂£" :ทดสอบ/你好 'こんにちは]) "[\"üñîçó∂£\" :ทดสอบ/你好 こんにちは]"))
    (is (= (pr-str "escape chars \t \r \n \\ \" \b \f") "\"escape chars \\t \\r \\n \\\\ \\\" \\b \\f\""))
    (is (= (pr-str (PrintMe. 1 2)) "#cljs.printing-test.PrintMe{:a 1, :b 2}"))
    (is (= (pr-str (js/Date. "2010-11-12T13:14:15.666-05:00"))
          "#inst \"2010-11-12T18:14:15.666-00:00\""))
    (doseq [month (range 1 13)
            day   (range 1 29)
            hour  (range 1 23)]
      (let [pad (fn [n]
                  (if (< n 10)
                    (str "0" n)
                    n))
            inst (str "2010-" (pad month) "-" (pad day) "T" (pad hour) ":14:15.666-00:00")]
        (is (= (pr-str (js/Date. inst)) (str "#inst \"" inst "\"")))))
    (let [uuid-str "550e8400-e29b-41d4-a716-446655440000"
          uuid (cljs.core/uuid uuid-str)]
      (is (= (pr-str uuid) (str "#uuid \"" uuid-str "\""))))
    ;; pr-str PersistentQueueSeq - CLJS-800
    (is (= (pr-str (rest (conj cljs.core.PersistentQueue.EMPTY 1 2 3))) "(2 3)"))
    (is (= "\"asdf\" \"asdf\"" (pr-str "asdf" "asdf")))
    ;; Different hash map order on self-host
    (is (#{"[1 true {:a 2, :b #\"x\\\"y\"} #js [3 4]]"
           "[1 true {:b #\"x\\\"y\", :a 2} #js [3 4]]"}
          (pr-str [1 true {:a 2 :b #"x\"y"} (array 3 4)]))))
  (testing "Testing print-str"
    (is (= (print-str "asdf") "asdf")))
  (testing "Testing println-str"
    (is (= (println-str "asdf") "asdf\n")))
  (testing "Testing prn-str"
    (is (= (prn-str) "\n"))
    (is (= (prn-str "asdf") "\"asdf\"\n"))
    ;; Different hash map order on self-host
    (is (#{"[1 true {:a 2, :b 42} #js [3 4]]\n"
           "[1 true {:b 42, :a 2} #js [3 4]]\n"}
          (prn-str [1 true {:a 2 :b 42} (array 3 4)]))))
  (testing "Testing with-out-str"
    (is (= "12" (with-out-str (print 1) (print 2))))
    (is (= "12" (with-out-str (*print-fn* 1) (*print-fn* 2))))))
