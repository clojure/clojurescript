(require '[cljs.closure :as cljsc])
(require '[cljs.repl :as repl])
(require '[cljs.repl.browser :as brepl])

(cljsc/build "src"
  {:output-to "main.js"
   :verbose true})

(repl/repl (brepl/repl-env)
  :repl-verbose true)