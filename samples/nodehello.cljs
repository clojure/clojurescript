(ns nodehello)

(defn -main [& args]
  (println (apply str (map [\space "world" "hello"] [2 0 1]))))

(set! *main-cli-fn* -main)

(comment
; Compile this using a command line like:

CLOJURESCRIPT_HOME=".../clojurescript/" \
  bin/cljsc samples/nodehello.cljs \
  {:optimizations :simple :pretty-print true :target :nodejs} \
  > out/nodehello.js

; Then run using:
nodejs out/nodehello.js

)
