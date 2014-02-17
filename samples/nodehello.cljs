(ns nodehello
  (:require [cljs.nodejs :as nodejs]))

(defn -main [& args]
  (println (apply str (map [\space "world" "hello"] [2 0 1]))))

(nodejs/enable-util-print!)
(set! *main-cli-fn* -main)

(comment
; Compile this using a command line like:

CLOJURESCRIPT_HOME=".../clojurescript/" \
  bin/cljsc samples/nodehello.cljs {:target :nodejs} \
  > out/nodehello.js

; Then run using:
nodejs out/nodehello.js

)
