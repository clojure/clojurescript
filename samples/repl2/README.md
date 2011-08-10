# Long-Polling browser-connected REPL

This sample application demonstrates how to use the long-polling
browser-connected REPL. This is a work in progress and may not be
the final approach that we take.

# Building

To build the project, launch a Clojure repl and:
   
    (use 'cljs.closure)
    (def opts {:output-to "samples/repl2/out/repl.js" :output-dir "samples/repl2/out"})
    (build "samples/repl2/src" opts)
    
# Connect to REPL

Start the REPL using the browser as the evaluator:

    (require '[cljs.repl :as repl])
    (require '[cljs.repl.browser :as browser])
    (def env (browser/repl-env "samples/repl2" 9000))
    (repl/repl env)

Open a web browser and connect to
http://localhost:9000/index.html. Here are some things that are known
to work:

    ;; Evaluate some basic forms.
    (+ 1 1)
    {:a :b}
    "hello"
    (reduce + [1 2 3 4 5])
    (js/alert "hello world")
    
    ;; Load a file, and use it.
    (load-file "clojure/string.cljs")
    (clojure.string/reverse "Hello")
    
    ;; Define functions and call them.
    (ns cljs.user)
    (defn sum [coll] (reduce + coll))
    (sum [2 2 2 2])
    
    ;; Create dom elements.
    (ns dom.testing (:require [clojure.browser.dom :as dom]))
    (dom/append (dom/get-element "content")
                (dom/element "Hello World!"))

