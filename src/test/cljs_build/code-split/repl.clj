;; Instructions:

;; ./script/uberjar
;; cd src/test/cljs_build/code-split
;; java -cp ../../../../target/cljs.jar:src clojure.main repl.clj
;; chromium http://localhost:9000/index.html

(require '[cljs.repl :as r])
(require '[cljs.build.api :as b])
(require '[cljs.repl.browser :as rb])

(def opts
  {:output-dir "out"
   :asset-path "/out"
   :optimizations :none
   :modules {:a {:entries '#{code.split.a}
                 :output-to "out/a.js"}
             :b {:entries '#{code.split.b}
                 :output-to "out/b.js"}
             :c {:entries '#{code.split.c}
                 :output-to "out/c.js"}}
   :browser-repl true
   :verbose true})

(b/build "src" opts)
(r/repl* (rb/repl-env) (dissoc opts :modules))
