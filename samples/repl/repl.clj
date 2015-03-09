(require '[cljs.closure :as cljsc])
(require '[cljs.repl :as repl])
(require '[cljs.repl.browser :as brepl])

(repl/repl
  (brepl/repl-env)
  :init (fn []
          (cljsc/build "src" {:output-to "main.js"}))
  :repl-verbose true)