(require '[cljs.repl :as repl])
(require '[cljs.repl.nashorn :as nashorn])
(repl/repl (nashorn/repl-env))