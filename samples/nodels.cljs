; This one doesn't yet work with :optimizations :advanced
(ns nodels
  (:require [cljs.nodejs :as nodejs]))

(def fs (nodejs/require "fs"))
(def path (nodejs/require "path"))

(defn file-seq [dir]
  (tree-seq
    (fn [f] (.isDirectory (.statSync fs f) ()))
    (fn [d] (map #(.join path d %) (.readdirSync fs d)))
    dir))

(defn -main [& paths] 
  (dorun (map println (mapcat file-seq paths))))

(set! *main-cli-fn* -main)
