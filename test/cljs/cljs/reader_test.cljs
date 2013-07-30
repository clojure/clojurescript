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
  (assert (= '% (reader/read-string "%")))
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

  ;; inst
  (let [est-inst (reader/read-string "#inst \"2010-11-12T13:14:15.666-05:00\"")
        utc-inst (reader/read-string "#inst \"2010-11-12T18:14:15.666-00:00\"")
        pad (fn [n]
                (if (< n 10)
                  (str "0" n)
                  n))]

    (assert (= (.valueOf (js/Date. "2010-11-12T13:14:15.666-05:00"))
               (.valueOf est-inst)))

    (assert (= (.valueOf est-inst)
               (.valueOf (reader/read-string (pr-str est-inst)))))

    (assert (= (.valueOf est-inst)
               (.valueOf utc-inst)))

    (doseq [month (range 1 13) day (range 1 29) hour (range 1 23)]
      (let [s (str "#inst \"2010-" (pad month) "-" (pad day) "T" (pad hour) ":14:15.666-06:00\"")]
        (assert (= (-> s reader/read-string .valueOf)
                   (-> s reader/read-string pr-str reader/read-string .valueOf))))))

  (let [insts [(reader/read-string "#inst \"2012\"")
               (reader/read-string "#inst \"2012-01\"")
               (reader/read-string "#inst \"2012-01-01\"")
               (reader/read-string "#inst \"2012-01-01T00\"")
               (reader/read-string "#inst \"2012-01-01T00:00:00.000\"")
               (reader/read-string "#inst \"2012-01-01T00:00:00.000123456\"")
               (reader/read-string "#inst \"2012-01-01T00:00:00.000123456789+00:00\"")]]
    (assert (apply = (map #(.valueOf %) insts))))

  ;; uuid literals
  (let [u (reader/read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")]
    (assert (= u (reader/read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))

    (assert (not (identical? u (reader/read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\""))))

    (assert (= u (-> u pr-str reader/read-string))))
  
  ;; new tag parsers

  (reader/register-tag-parser! 'foo identity)

  (assert (= [1 2] (reader/read-string "#foo [1 2]")))

  ;; tag elements with prefix component
  (reader/register-tag-parser! 'foo.bar/baz identity)
  (assert (= [1 2] (reader/read-string "#foo.bar/baz [1 2]")))

  ;; default tag parser
  (reader/register-default-tag-parser! (fn [tag val] val))
  (assert (= [1 2] (reader/read-string "#a.b/c [1 2]")))

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
