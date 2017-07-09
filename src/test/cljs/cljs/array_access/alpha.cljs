(ns cljs.array-access.alpha
  (:require-macros [cljs.array-access.helper :as helper])
  (:require [cljs.array-access.beta]))

(defn unchecked-arrays? []
  (helper/unchecked-arrays?))
