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

    (require '[cljs.repl :as repl])
    (require '[cljs.eval.browser :as browser])
    (def env (browser/repl-env 9000))
    (repl/repl env)

Open the index.html page in
samples/repl/resources/public/index.html. Compilation is working so
you can now type ClojureScript forms at the REPL and they will be evaluated
in the browser.

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

This currently only works in Safari.

Chrome error is:

XMLHttpRequest cannot load http://localhost:9000/. Origin null is not
allowed by Access-Control-Allow-Origin.

We may need to have the REPL server serve the host page?

