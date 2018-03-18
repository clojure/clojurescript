(require '[cljs.repl :as r])
(require '[cljs.build.api :as b])
(require '[cljs.repl.browser :as rb])

(def opts
  {:watch "src"
   :output-dir "out"
   :asset-path "/out"
   :optimizations :none
   :modules {:foo {:entries '#{foo.core}
                   :output-to "out/foo.js"}
             :bar {:entries '#{bar.core}
                   :output-to "out/bar.js"}}
   :browser-repl true
   :verbose true})

(b/build "src" opts)
(r/repl* (rb/repl-env) opts)
