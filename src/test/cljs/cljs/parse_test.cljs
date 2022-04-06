(ns cljs.parse-test
  (:require
    [clojure.test :refer [deftest is are]]
    [clojure.test.check :as chk]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]))

(deftest test-parse-long
  (are [s expected]
    (= expected (parse-long s))
    "100" 100
    "+100" 100
    "0" 0
    "+0" 0
    "-0" 0
    "-42" -42
    "9007199254740991" js/Number.MAX_SAFE_INTEGER  ;; largest parsable: 999999999999999934463
    "+9007199254740991" js/Number.MAX_SAFE_INTEGER
    "-9007199254740991" js/Number.MIN_SAFE_INTEGER
    "077" 77) ;; leading 0s are ignored! (not octal)

  (are [s] ;; do not parse
    (nil? (parse-long s))
    "0.3" ;; no float
    "9007199254740992" ;; past max long
    "-9007199254740992" ;; past min long
    "0xA0" ;; no hex
    "2r010")) ;; no radix support

;; generative test - gen long -> str -> parse, compare
(deftest test-gen-parse-long
  (let [res (chk/quick-check
              100000
              (prop/for-all* [gen/large-integer]
                #(= % (-> % str parse-long))))]
    (if (:result res)
      (is true) ;; pass
      (is (:result res) (pr-str res)))))

(deftest test-parse-double
  (are [s expected]
    (= expected (parse-double s))
    "1.234" 1.234
    "+1.234" 1.234
    "-1.234" -1.234
    "+0" +0.0
    "-0.0" -0.0
    "0.0" 0.0
    "5" 5.0
    ".5" 0.5
    "Infinity" ##Inf
    "-Infinity" ##-Inf
    "1.7976931348623157E308" js/Number.MAX_VALUE
    "4.9E-324" js/Number.MIN_VALUE
    "1.7976931348623157E309" js/Number.POSITIVE_INFINITY  ;; past max double
    "2.5e-324" js/Number.MIN_VALUE  ;; past min double, above half minimum
    "2.4e-324" 0.0)  ;; below minimum double
  (is (js/isNaN (parse-double "NaN")))
  (are [s] ;; nil on invalid string
    (nil? (parse-double s))
    "double" ;; invalid string
    "1.7976931348623157G309")) ;; close, but not valid

;; generative test - gen double -> str -> parse, compare
(deftest test-gen-parse-double
  (let [res (chk/quick-check
              100000
              (prop/for-all* [gen/double]
                #(let [parsed (-> % str parse-double)]
                   (if (js/isNaN %)
                     (js/isNaN parsed)
                     (= % parsed)))))]
    (if (:result res)
      (is true) ;; pass
      (is (:result res) (pr-str res)))))

(deftest test-parse-uuid
  (is (parse-uuid (str (random-uuid))))
  (is (nil? (parse-uuid "BOGUS"))) ;; nil on invalid uuid string
  (are [s] ;; throw on invalid type (not string)
    (try (parse-uuid s) (is false) (catch :default _ (is true)))
    123
    nil))

(deftest test-parse-boolean
  (is (identical? true (parse-boolean "true")))
  (is (identical? false (parse-boolean "false")))

  (are [s] ;; nil on invalid string
    (nil? (parse-boolean s))
    "abc"
    "TRUE"
    "FALSE"
    " true ")

  (are [s] ;; throw on invalid type (not string)
    (try (parse-boolean s) (is false) (catch :default _ (is true)))
    nil
    false
    true
    100))
