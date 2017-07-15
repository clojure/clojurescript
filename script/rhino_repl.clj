(require '[cljs.repl :as repl])
(require '[cljs.repl.rhino :as rhino])
(repl/repl (rhino/repl-env))
