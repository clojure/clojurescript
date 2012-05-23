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

  ;; queue literals
  (assert (= cljs.core.PersistentQueue/EMPTY
             (reader/read-string "#queue []")))

  (assert (= (-> cljs.core.PersistentQueue/EMPTY (conj 1))
             (reader/read-string "#queue [1]")))

  (assert (= (into cljs.core.PersistentQueue/EMPTY [1 2])
             (reader/read-string "#queue [1 2]")))

  ;; new parsers

  (reader/register-tag-parser! "foo" identity)

  (assert (= [1 2] (reader/read-string "#foo [1 2]")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Unicode Tests

  ; sample unicode strings, symbols, keywords
  (doseq [unicode
          ["اختبار"     ; arabic
           "ทดสอบ"      ; thai
               "こんにちは"     ; japanese hiragana
           "你好"        ; chinese traditional
           "אַ גוט יאָר"  ; yiddish
           "cześć"      ; polish
           "привет"     ; russian

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
      (assert (= unicode read)
              (str "Failed to read-string \"" unicode "\" from: " input))))

  ; unicode error cases
  (doseq [unicode-error
          ["\"abc \\ua\""           ; truncated
           "\"abc \\x0z  ...etc\""  ; incorrect code
           "\"abc \\u0g00 ..etc\""  ; incorrect code
           ]]
    (let [r (try
              (reader/read-string unicode-error)
              :failed-to-throw
              (catch js/Error e :ok))]
      (assert (= r :ok) (str "Failed to throw reader error for: " unicode-error))))

  :ok)
