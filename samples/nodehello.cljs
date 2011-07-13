(ns nodehello)

(defn -main [& args]
  (println (apply str (map [\ "world" "hello"] [2 0 1]))))

(set! *main-cli-fn* -main)

(comment
; Compile this using a command line like:

CLOJURESCRIPT_HOME=".../clojurescript/" \
  bin/cljsc samples/nodehello.cljs \
  {:optimizations :advanced :pretty-print true :target :cli} \
  > out/nodehello.js

; Then run using:
nodejs out/nodehello.js

)
