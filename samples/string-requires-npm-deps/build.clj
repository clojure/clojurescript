(require '[cljs.build.api :as b])

(b/build "src"
  {:output-dir "out"
   :output-to "out/main.js"
   :optimizations :none
   :verbose true
   :target :nodejs
   :compiler-stats true
   :main 'foo.core
   :npm-deps {:react "15.6.1"
              :react-dom "15.6.1"}
   :closure-warnings {:non-standard-jsdoc :off}})
