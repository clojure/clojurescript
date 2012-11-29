(ns nodecljs
  (:require [cljs.core]
            [bs :as bs]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.reader :as reader]))

(defn ep [text]
  (let [r (reader/push-back-reader text)]
    (loop [str (reader/read r false :eof false)]
      (when (not= str :eof)
        (try
          (let [res (comp/emit-str (ana/analyze js/env str))]
            (js/eval res))
          (catch js/Error e
           (println e)
           #_(set! *e e)))
        (recur (reader/read r false :eof false))))))

(defn -main [file & args]
  ;(set! js/env (assoc js/env :context :expr))
  (let [fs (js/require "fs")
        text (.toString (.readFileSync fs file))]
    (ep text)))

(set! *main-cli-fn* -main)

