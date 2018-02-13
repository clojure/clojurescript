(ns code.split.b
  (:require [cljs.loader :as loader]
            [code.split.d :as d]))

(defn hello []
  (println "Hello from code.split.b.")
  (d/hello))

(loader/set-loaded! :b)
