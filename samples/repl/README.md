# Building

To build the project, launch a clojure repl and:

    (use 'cljs.closure)
    (def opts {:output-to "samples/repl/resources/public/javascripts/repl.js" :output-dir "samples/repl/resources/public/javascripts"})
    (build "samples/repl/src/cljs" opts)

# Start up

To start the test endpoint, run:

    lein ring server-headless

and navigate to localhost:3000/index.html

# Connect to REPL

Build client code as shown above.

Start the REPL:

    (use 'cljs.repl)
    (use 'cljs.eval.browser)
    (def repl-env (create-eval-env 9000))
    (repl repl-env)

Open the index.html page in samples/repl/resources/public/index.html

This repl is not currently compiling anything so you will need to type
JavaScript.

    ClojureScript:> "1 + 1;"
    "1 + 1;"
    "2"
    ClojureScript:> "var a = 3; 1 + a;"
    "var a = 3; 1 + a;"
    "4"
