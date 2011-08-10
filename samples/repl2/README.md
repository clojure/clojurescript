# Building

To build the project, launch a clojure repl and:

    (use 'cljs.closure)
    (def opts {:output-to "samples/repl2/out/repl.js" :output-dir "samples/repl2/out"})
    (build "samples/repl2/src" opts)

# Connect to REPL

Build client code as shown above.

Start the REPL:

    (require '[cljs.repl :as repl])
    (require '[cljs.repl.browser :as browser])
    (def env (browser/repl-env 9000))
    (repl/repl env)

With Safari, Open the index.html page at
samples/repl2/index.html. Compilation is working so you can now type
ClojureScript forms at the REPL and they will be evaluated in the
browser.

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

This currently only works in Safari. Other browsers will not allow
connections to another origin. We will need to serve everything
through the REPL server.

