# Long-Polling browser-connected REPL

This sample applition demonstrates how to use the long-polling
browser-connected REPL. This is a work in progress and may not be
the final approach that we take.

# Building

To build the project, launch a Clojure repl and:
   
    (use 'cljs.closure)
    (def opts {:output-to "samples/repl2/out/repl.js" :output-dir "samples/repl2/out"})
    (build "samples/repl2/src" opts)
    
# Connect to REPL

Start the REPL:

    (require '[cljs.repl :as repl])
    (require '[cljs.repl.browser :as browser])
    (def env (browser/repl-env "samples/repl2" 9000))
    (repl/repl env)

Open a web browser and connect to http://localhost:9000/index.html. In
the REPL do the following.

    ClojureScript:cljs.user> (+ 1 1)
    (+ 1 1)
    2
    ClojureScript:cljs.user> {:a :b}
    {:a :b}
    {:a :b}
    ClojureScript:cljs.user> (reduce + [1 2 3 4 5])
    (reduce + [1 2 3 4 5])
    15
    ClojureScript:cljs.user> (js/alert "hello world")
    (js/alert "hello world")
    nil
    ClojureScript:cljs.user> (load-file "clojure/string.cljs")
    (load-file "clojure/string.cljs")
    
    ClojureScript:cljs.user> (clojure.string/reverse "Hello")
    (clojure.string/reverse "Hello")
    "olleH"

If you try this, expect a lot of things to be broken. This is a prototype.
