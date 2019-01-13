(ns clojure.edn-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.edn :as edn]
            [cljs.reader :as reader]
            [cljs.tools.reader.reader-types :as reader-types]))

(defn- test-reader []
  (reader-types/string-push-back-reader "[1 2 3]"))

(deftest test-read
  (testing "Mirrors cljs.reader/read"
    (is (= (edn/read (test-reader))
         (reader/read (test-reader))))
    (is (= (edn/read {} (test-reader))
           (reader/read {} (test-reader))))
    (is (= (edn/read (test-reader) false "EOF" {})
           (reader/read (test-reader) false "EOF" {})))))

(deftest test-read-string
  (testing "Mirrors cljs.reader/read-string"
    (is (= (edn/read-string "(+ 1 2)")
           (reader/read-string "(+ 1 2)")))
    (is (= (edn/read-string "{:a #{[1]}}")
           (reader/read-string "{:a #{[1]}}")))))
