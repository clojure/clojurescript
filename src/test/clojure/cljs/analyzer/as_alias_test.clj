;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.as-alias-test
  (:require [cljs.analyzer :as ana]
            [cljs.analyzer.namespaces :as ana-nses]
            [cljs.env :as env]
            [clojure.test :as test :refer [deftest testing is]]))

;; =============================================================================

(deftest test-check-and-remove-as-alias
  (let [cenv (env/default-compiler-env)]
    (env/with-compiler-env cenv
      (testing "check-and-remove-as-alias basic tests"
        (is (= '{:as-alias {bar bar.core}}
               (ana-nses/check-and-remove-as-alias '[bar.core :as-alias bar])))
        (is (= '{:as-alias {bar bar.core}
                 :libspec  [bar.core :as boo]}
               (ana-nses/check-and-remove-as-alias '[bar.core :as-alias bar :as boo])))
        (is (thrown? Throwable
              (ana-nses/check-and-remove-as-alias '[bar.core :as-alias :bar]))))
      (testing "check-and-remove-as-alias should not elide simple specs"
        (is (= '{:libspec bar.core}
               (ana-nses/check-and-remove-as-alias 'bar.core)))
        (is (= '{:libspec [bar.core]}
               (ana-nses/check-and-remove-as-alias '[bar.core])))))))

(deftest test-eliad-aliases-from-libspecs
  (let [cenv (env/default-compiler-env)]
    (env/with-compiler-env cenv
      (is (= '{:as-aliases {foo foo.core
                            bar bar.core
                            woz woz.core}
               :libspecs   [[woz.core :as wozc]]}
            (ana-nses/elide-aliases-from-libspecs
              '([foo.core :as-alias foo]
                [bar.core :as-alias bar]
                [woz.core :as-alias woz :as wozc]))))
      (is (thrown? Throwable
            (ana-nses/elide-aliases-from-libspecs
              '([foo.core :as-alias foo]
                [bar.core :as-alias bar]
                [woz.core :as-alias woz :as wozc]
                [foo.impl :as-alias foo])))))))

(comment

  (test/run-tests)

  (let [cenv (env/default-compiler-env)]
    (ana-nses/elide-aliases-from-ns-specs
      '((:require-macros [blah.core :as-alias blah])
        (:require [foo.core :as-alias foo]
          [bar.core :as-alias bar]
          [woz.core :as woz]))))
  )
