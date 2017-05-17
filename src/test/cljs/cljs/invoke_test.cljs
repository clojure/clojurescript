(ns cljs.invoke-test
  (:require [goog.string :as gstr]))

(defn variadic-fn [& args])

(variadic-fn 1 2 3)

(defn multi-fn
  ([a] a)
  ([a b] a))

(multi-fn 2)

(gstr/urlEncode "foo")

(js/goog.string.urlDecode "bar")
