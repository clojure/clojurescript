(ns code.split.c
  (:require [cljs.loader :as loader]
            [code.split.d :as d]))

(enable-console-print!)

(defn hello []
  (println "Hello from code.split.c.")
  (d/hello))

(loader/set-loaded! :c)
