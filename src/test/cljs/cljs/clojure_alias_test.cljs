;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.clojure-alias-test
  "Tests requiring via `clojure.*` instead of `cljs.*`"
  (:refer-clojure :exclude [use-macros])
  (:require [clojure.test :refer [deftest is] :rename {is is?}]
            [clojure.spec.alpha :as s :refer [spec? spec] :rename {spec foo}]))

(deftest normal-test
  (is? (= 1 1)))

(deftest aliases-test
  (is? (= spec? clojure.spec.alpha/spec? cljs.spec.alpha/spec?))
  (is? (foo number?)))

(deftest use-macros
  (s/def ::even? (s/and number? even?))
  (is? (s/valid? ::even? 2)))
