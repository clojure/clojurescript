;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.reader-test
  (:require [cljs.test :refer-macros [deftest testing is] :as test]
            [cljs.reader :as reader]
            [goog.object :as o]))

(deftype T [a b])
(defrecord R [a b])

(deftest test-reader
  (testing "Test basic reading"
    (is (= 1 (reader/read-string "1")))
    (is (= 2 (reader/read-string "#_nope 2")))
    (is (= -1 (reader/read-string "-1")))
    (is (= -1.5 (reader/read-string "-1.5")))
    (is (= [3 4] (reader/read-string "[3 4]")))
    (is (= "foo" (reader/read-string "\"foo\"")))
    (is (= :hello (reader/read-string ":hello")))
    (is (= 'goodbye (reader/read-string "goodbye")))
    (is (= '% (reader/read-string "%")))
    (is (= #{1 2 3} (reader/read-string "#{1 2 3}")))
    (is (= '(7 8 9) (reader/read-string "(7 8 9)")))
    ;; Another bad test found by switching to tools.reader - David
    ;(is (= '(deref foo) (reader/read-string "@foo")))
    ;; Another bad test found by switching to tools.reader - David
    ;;(is (= 'bar (reader/read-string "'bar")))
    (is (= 'foo/bar (reader/read-string "foo/bar")))
    (is (= \a (reader/read-string "\\a")))
    (is (= {:tag 'String} (meta (reader/read-string "^String {:a 1}"))))
    (is (= [:a 'b #{'c {:d [:e :f :g]}}]
              (reader/read-string "[:a b #{c {:d [:e :f :g]}}]")))
    (is (= :foo/bar (reader/read-string ":foo/bar")))
    (is (= nil (reader/read-string "nil")))
    (is (= true (reader/read-string "true")))
    (is (= false (reader/read-string "false")))
    (is (= "string" (reader/read-string "\"string\"")))
    (is (= "escape chars \t \r \n \\ \" \b \f" (reader/read-string "\"escape chars \\t \\r \\n \\\\ \\\" \\b \\f\""))))
  
  (testing "Test reading number literals"
    (is (apply = 0 (map reader/read-string ["0" "+0" "-0" " 0 "])))
    (is (apply = 42 (map reader/read-string ["052" "0x2a" "2r101010" "8R52" "16r2a" "36r16"])))
    (is (apply = 42 (map reader/read-string ["+052" "+0x2a" "+2r101010" "+8r52" "+16R2a" "+36r16"])))
    (is (apply = -42 (map reader/read-string ["-052" "-0X2a" "-2r101010" "-8r52" "-16r2a" "-36R16"]))))

  (testing "Test reading queue literals"
    (is (= cljs.core.PersistentQueue.EMPTY
              (reader/read-string "#queue []")))
    (is (= (-> cljs.core.PersistentQueue.EMPTY (conj 1))
              (reader/read-string "#queue [1]")))
    (is (= (into cljs.core.PersistentQueue.EMPTY [1 2])
              (reader/read-string "#queue [1 2]"))))

  (testing "Test reading comments"
    ;; Another bad test found by switching to tools.reader - David
    ;;(is (nil? (reader/read-string ";foo")))
    (is (= 3 (try
               (reader/read-string ";foo\n3")
               (catch js/Error e :threw))))
    (is (= 3 (try
               (reader/read-string ";foo\n3\n5")
               (catch js/Error e :threw)))))

  (let [est-inst (reader/read-string "#inst \"2010-11-12T13:14:15.666-05:00\"")
        utc-inst (reader/read-string "#inst \"2010-11-12T18:14:15.666-00:00\"")
        pad (fn [n]
              (if (< n 10)
                (str "0" n)
                n))]
    (testing "Testing reading instant literals"
      (is (= (.valueOf (js/Date. "2010-11-12T13:14:15.666-05:00"))
             (.valueOf est-inst)))
      (is (= (.valueOf est-inst)
             (.valueOf (reader/read-string (pr-str est-inst)))))
      (is (= (.valueOf est-inst)
             (.valueOf utc-inst)))
      (doseq [month (range 1 13)
              day   (range 1 29)
              hour  (range 1 23)]
        (let [s (str "#inst \"2010-" (pad month) "-" (pad day) "T" (pad hour) ":14:15.666-06:00\"")]
          (is (= (-> s reader/read-string .valueOf)
                 (-> s reader/read-string pr-str reader/read-string .valueOf)))))))
 
  (let [u (reader/read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")]
    (testing "Testing reading UUID literals"
      (is (= u (reader/read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))
      (is (not (identical? u (reader/read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\""))))
      (is (= u (-> u pr-str reader/read-string)))))
  
  (testing "Testing tag parsers"
    (reader/register-tag-parser! 'foo identity)
    (is (= [1 2] (reader/read-string "#foo [1 2]")))

    ;; tag elements with prefix component
    (reader/register-tag-parser! 'foo.bar/baz identity)
    (is (= [1 2] (reader/read-string "#foo.bar/baz [1 2]")))

    ;; default tag parser
    (reader/register-default-tag-parser! (fn [tag val] val))
    (is (= [1 2] (reader/read-string "#a.b/c [1 2]"))))

  (testing "Character Literals"
    (is (= [\tab \return \newline \space \backspace \formfeed \u1234]
          (reader/read-string "[\\tab \\return \\newline \\space \\backspace \\formfeed \\u1234]"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Unicode Tests

  (testing "Test reading unicode - strings, symbols, keywords"
    (doseq [unicode
            ["اختبار"                  ; arabic
             "ทดสอบ"                   ; thai
             "こんにちは"              ; japanese hiragana
             "你好"                    ; chinese traditional
             "אַ גוט יאָר"               ; yiddish
             "cześć"                   ; polish
             "привет"                  ; russian

             ;; RTL languages skipped below because tricky to insert
             ;;  ' and : at the "start"

             'ทดสอบ
             'こんにちは
             '你好
             'cześć
             'привет

             :ทดสอบ
             :こんにちは
             :你好
             :cześć
             :привет

                                        ;compound data
             {:привет :ru "你好" :cn}
             ]]
      (let [input (pr-str unicode)
            read  (reader/read-string input)]
        (is (= unicode read)))))

  (testing "Testing unicode error cases"
    (doseq [unicode-error
            ["\"abc \\ua\""         ; truncated
             "\"abc \\x0z  ...etc\"" ; incorrect code
             "\"abc \\u0g00 ..etc\"" ; incorrect code
             ]]
      (let [r (try
                (reader/read-string unicode-error)
                :failed-to-throw
                (catch js/Error e :ok))]
        (is (= r :ok)))))

  (testing "Testing non-string input, CLJS-1342"
    (let [r (try
              (reader/read-string :foo)
              :failed-to-throw
              (catch js/Error e :ok))]
      (is (= r :ok))))
)

(deftest test-717
  (testing "Testing reading, CLJS-717"
    (is (array? (reader/read-string "#js [1 2 3]")))
    (is (= (alength (reader/read-string "#js [1 2 3]")) 3))
    (is (= (seq (reader/read-string "#js [1 2 3]")) (seq [1 2 3])))
    (is (= (set (js-keys (reader/read-string "#js {:foo \"bar\" :baz \"woz\"}"))) #{"foo" "baz"}))
    (is (= (o/get (reader/read-string "#js {:foo \"bar\"}") "foo") "bar"))
    (is (= (o/get (reader/read-string "#js {\"foo\" \"bar\"}") "foo") "bar"))
    (is (array? (o/get (reader/read-string "#js {\"foo\" #js [1 2 3]}") "foo")))
    (is (= (seq (o/get (reader/read-string "#js {\"foo\" #js [1 2 3]}") "foo")) '(1 2 3)))))

(deftest test-787
  (testing "Testing reading, CLS-787"
    (is (nil? (reader/read-string "")))))

;; Test doesn't seem relevant now that we rely on tools.reader - David
;(deftest test-819
;  (let [re (reader/read-string  "#\"\s\u00a1\"")
;        m  (re-find re " \u00a1   ")]
;    (testing "Testing reading, CLJS-819"
;      (is (= m " \u00a1")))))

(deftest testing-map-type
  (let [a  (reader/read-string "{:a 1 :b 2 :c 3}")
        b  (reader/read-string "{:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9}")]
    (is (= a {:a 1 :b 2 :c 3}))
    ;; Needs fix to cljs.tools.reader.edn - David
    ;;(is (instance? PersistentArrayMap a))
    (is (= b {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9}))
    (is (instance? PersistentHashMap b))))

;; NOTE: issue uncovered by test.check

(deftest test-slash-reading
  (let [x '({/ 0})]
    (testing "Testing '/ reading"
      (is (= x (reader/read-string (pr-str x))))
      (is (= (reader/read-string (pr-str x)) x)))))

;; This need to be enforced elsewhere not during reading - David
;(deftest testing-cljs-1823
;      (let [;; PersistentArrayMap
;            a (try
;                (reader/read-string "{:a 1 :b 2 :c 3 :a 1}")
;                :failed-to-throw
;                (catch js/Error e (ex-message e)))
;            ;; PersistentHashMap
;            b (try
;                (reader/read-string "{:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :h 7 :i 8 :a 1}")
;                :failed-to-throw
;                (catch js/Error e (ex-message e)))
;            ;; PersistentArrayMap backed PHS
;            c (try
;                (reader/read-string "#{:a :b :c :d :a}")
;                :failed-to-throw
;                (catch js/Error e (ex-message e)))
;            ;; PersistentHashMap backed PHS
;            d (try
;                (reader/read-string "#{:a :b :c :d :e :f :g :h :i :a}")
;                :failed-to-throw
;                (catch js/Error e (ex-message e)))]
;        (is (= "Duplicate key: :a" a))
;        (is (= "Duplicate key: :a" b))
;        (is (= "Duplicate key: :a" c))
;        (is (= "Duplicate key: :a" d))))

;; Not relevant now that we rely on tools.reader and it duplicates Clojure's behavior - David
;(deftest test-error-messages
;  (testing "Leading numbers in keywords"
;    (is (thrown-with-msg? js/Error #"Invalid keyword :0s" (reader/read-string ":0s")))))
