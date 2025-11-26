;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.proxy-test
  (:refer-global :only [Object])
  (:require [cljs.test :as test :refer-macros [deftest testing is are]]
            [cljs.proxy :refer [builder]]
            [goog.object :as gobj]))

(def proxy (builder))

(deftest map-proxy
  (let [proxied (proxy {:foo 1 :bar 2})]
    (is (== 1 (gobj/get proxied "foo")))
    (is (== 2 (gobj/get proxied "bar")))
    (is (= ["foo" "bar"] (seq (Object/keys proxied))))))

(deftest vector-proxy
  (let [proxied (proxy [1 2 3 4])]
    (is (== 4 (alength proxied)))
    (is (== 1 (aget proxied 0)))
    (is (== 4 (aget proxied 3)))))

(comment

  (test/run-tests)

)
