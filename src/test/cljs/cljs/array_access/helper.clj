(ns cljs.array-access.helper
  (:require [cljs.analyzer :as ana]))

(defmacro unchecked-arrays? []
  (ana/unchecked-arrays?))
