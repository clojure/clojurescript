(ns phantomjs-repl-test.phantomjs
  (:require [clojure.java.io :as io]))
   
(defn run-shell
  "Starts a new OS process and returns an atomized string with it's output."
  [& cmd+args]
  (let [out (atom "")
        proc (.. Runtime getRuntime (exec (into-array String cmd+args)))]
    (future
      (loop []
        (when-let [line (-> proc .getInputStream io/reader .readLine)]
          (swap! out str line)
          (recur))))
    out))

(defn run-script
  [script-file]
  (run-shell "phantomjs" script-file))
