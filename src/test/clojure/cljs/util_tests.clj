;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.util-tests
  (:require [cljs.util :as util]
            [clojure.java.io :as io])
  (:use clojure.test))

(deftest test-levenshtein-distance
  (testing "levenshtein-distance"
    (is (= 0 (util/levenshtein-distance "abc" "abc")))
    (is (= 1 (util/levenshtein-distance "abc" "abcd")))
    (is (= 1 (util/levenshtein-distance "abcd" "abc")))
    (is (= 3 (util/levenshtein-distance "kitten" "sitting")))))

(deftest test-suggestion
  (testing "suggestion"
    (is (= ":optimization" (util/suggestion 3 ":optimization" [":optimization" ":static-fns"])))))

(deftest test-unknown-opts
  (testing "unknown-opts"
    (is (= [[:bogus nil]
            [:optimisations :optimizations]]
          (sort (util/unknown-opts #{:optimisations :bogus} #{:optimizations :static-fns}))))))

(deftest test-relative-name
  (if util/windows?
    (let [initial (System/getProperty "user.dir")]
      (System/setProperty "user.dir" "C:\\Users\\anmonteiro\\Downloads\\clojurescript-master")
      (is (= (util/relative-name (io/file "C:\\Users\\anmonteiro\\Downloads\\clojurescript-master\\out\\index.js")) "out\\index.js"))
      (is (= (util/relative-name (io/as-url (io/file "C:\\Users\\anmonteiro\\Downloads\\clojurescript-master\\node_modules\\lodash\\array.js"))) "node_modules\\lodash\\array.js"))
      ;; Check case-sensitivity:
      (System/setProperty "user.dir" "c:\\users\\anmonteiro\\Downloads\\clojurescript-master")
      (is (= (util/relative-name (io/file "C:\\Users\\anmonteiro\\Downloads\\clojurescript-master\\out\\index.js")) "out\\index.js"))
      (is (= (util/relative-name (io/as-url (io/file "C:\\Users\\anmonteiro\\Downloads\\clojurescript-master\\node_modules\\lodash\\array.js"))) "node_modules\\lodash\\array.js"))
      ;; Check pass-through:
      (is (= (util/relative-name (io/file "C:\\Temp\\clojurescript\\out\\index.js")) "C:\\Temp\\clojurescript\\out\\index.js"))
      (System/setProperty "user.dir" initial))
    ;; Non-windows
    (let [initial (System/getProperty "user.dir")]
      (System/setProperty "user.dir" "/Users/user/clojurescript")
      (is (= (util/relative-name (io/file "/Users/user/clojurescript/out/index.js")) "out/index.js"))
      (is (= (util/relative-name (io/as-url (io/file "/Users/user/clojurescript/out/index.js"))) "out/index.js"))
      ;; Check pass-through:
      (is (= (util/relative-name (io/file "/tmp/clojurescript/out/index.js")) "/tmp/clojurescript/out/index.js"))
      (System/setProperty "user.dir" initial))))

(deftest test-path
  (is (= (.getAbsolutePath (io/file "src/main/clojure/cljs/closure.clj"))
         (util/path (io/as-url (io/file "src/main/clojure/cljs/closure.clj"))))))

(deftest test-bytes-to-hex-str
  (is (= "09616263" (#'util/bytes-to-hex-str (.getBytes "\u0009abc")))))

(deftest test-content-sha
  (is (= "40BD001563085FC35165329EA1FF5C5ECBDBBEEF" (util/content-sha "123")))
  (is (= "40BD0" (util/content-sha "123" 5))))

(deftest test-cljs-3008
  (is (= :compilation (:clojure.error/phase (ex-data (util/compilation-error (Exception.)))))))
