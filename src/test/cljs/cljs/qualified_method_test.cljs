;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.qualified-method-test
  (:refer-global :only [String])
  (:require [cljs.test :as test :refer-macros [deftest testing is]]))

(deftest qualified-method-return-position-test
  (testing "qualified method returned from function to force it in return position"
    (let [f (fn [] String/.toUpperCase)
          m (f)]
      (is (= "FOO" (m "foo"))))))
