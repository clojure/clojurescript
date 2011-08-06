# Building

To build the project, launch a clojure repl and:

    (use cljs.closure)
    (def opts {:output-to "samples/repl/resources/public/javascripts/repl.js" :output-dir "samples/repl/resources/public/javascripts"})
    (build "samples/repl/src/cljs" opts)

# Start up

To start the test endpoint, run:

    lein ring server-headless

and navigate to localhost:3000/index.html
