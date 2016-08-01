(ns cljs.clojure-alias-test
  "Tests requiring via `clojure.*` instead of `cljs.*`"
  (:require [clojure.test :refer [deftest is] :rename {is is?}]
            [clojure.spec :as s :refer [spec? spec] :rename {spec foo}]))

(deftest normal-test
  (is? (= 1 1)))

(deftest aliases-test
  (is? (= spec? clojure.spec/spec? cljs.spec/spec?))
  (is? (foo number?)))
