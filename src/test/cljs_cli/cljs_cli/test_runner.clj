(ns cljs-cli.test-runner
  (:require
   [cljs-cli.test]
   [cljs-cli.util]))

(defn -main [& args]
  (try
    (binding [cljs-cli.util/*repl-env* (or (first args) "nashorn")
              cljs-cli.util/*repl-opts* (second args)]
      (clojure.test/run-tests 'cljs-cli.test))
    (finally
      (shutdown-agents))))
