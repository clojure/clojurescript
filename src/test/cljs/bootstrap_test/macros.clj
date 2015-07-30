(ns bootstrap-test.macros
  (:require [bootstrap-test.helper :refer [bar]]))

(defmacro foo [a b]
  (bar a b))