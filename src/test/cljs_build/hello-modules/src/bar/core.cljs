(ns bar.core
  (:require [cljs.loader :as loader]))

(enable-console-print!)

(println "I'm bar!")

(defn woz []
  (println "WOZ!"))

(loader/set-loaded! :bar)
