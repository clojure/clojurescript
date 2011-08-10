(ns repl.test
  (:require [clojure.browser.net :as net]
            [clojure.browser.event :as event]
            [clojure.browser.repl2 :as repl]))

(defn start-repl []
  (repl/start-repl "http://localhost:9000"))

(js/setTimeout start-repl 1000)

(comment

  ;; Compile JavaScript
  (use 'cljs.closure)
  (def opts {:output-to "samples/repl2/out/repl.js" :output-dir "samples/repl2/out"})
  (build "samples/repl2/src" opts)

  ;; Start REPL
  (require '[cljs.repl :as repl])
  (require '[cljs.repl.browser :as browser])
  (def env (browser/repl-env "samples/repl2" 9000))
  (repl/repl env)

  ;; Evaluate some stuff in the browser
  (+ 1 1)
  {:a :b}
  "hello"
  (reduce + [1 2 3 4 5])
  (js/alert "hello world")
  (load-file "clojure/string.cljs")
  (clojure.string/reverse "Hello")

  ;; this is currently not being done automatically as it is in rhino.
  (ns cljs.user)
  (defn sum [coll] (reduce + coll))
  (sum [2 2 2 2])

  )
