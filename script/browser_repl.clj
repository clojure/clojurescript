(require '[cljs.repl :as repl])
(require '[cljs.repl.browser :as browser])
(repl/repl (browser/repl-env))