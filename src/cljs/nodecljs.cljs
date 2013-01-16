(ns nodecljs
  (:require [cljs.core]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.reader :as reader]))

(defn ep [text]
  (let [r (reader/push-back-reader text)]
    (loop [str (reader/read r false :eof false)]
      (when (not= str :eof)
        (try
          (let [env (ana/empty-env)
                res (comp/emit-str (ana/analyze env str))]
            (js/eval res))
          (catch js/Error e
           (println e)
           #_(set! *e e)))
        (recur (reader/read r false :eof false))))))

(defn -main [file & args]
  ;; Setup the print function
  (set! *print-fn* (.-print (js/require "util")))

  ;; Bootstrap an empty version of the cljs.user namespace
  (swap! cljs.compiler/*emitted-provides* conj (symbol "cljs.user"))
  (.provide js/goog "cljs.user")
  (set! cljs.core/*ns-sym* (symbol "cljs.user"))

  ;(set! js/env (assoc js/env :context :expr))
  (let [fs (js/require "fs")
        text (.toString (.readFileSync fs file))]
    (ep text)))

(set! *main-cli-fn* -main)

