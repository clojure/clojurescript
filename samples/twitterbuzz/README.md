# ClojureScript "TwitterBuzz" Demo

## One-time Setup

See https://github.com/clojure/clojurescript/wiki/Quick-Start

## Run in Development Mode

Compile the demo:

        cljsc src > twitterbuzz.js

After running the above command, open index.html.

## Compile in Development Mode with the REPL (Faster)

* Run `script/repl`
  * To run it from Emacs, `C-x d` and nav to the `clojurescript` directory
  * `M-x set-variable inferior-lisp-program`
    * Set to `"script/repl"`
  * `M-x run-lisp`

* Once the REPL is running, evaluate:

        (use 'cljs.closure)
        (def opts {:output-to "samples/twitterbuzz/twitterbuzz.js" :output-dir "samples/twitterbuzz/out"})

        (build "samples/twitterbuzz/src" opts)

The reason we set the `:output-dir` is because the `index.html` script tag is specifically pointing to that directory.

* See `cljs.closure` source for more compilation examples.

## Run in Production Mode

See https://github.com/clojure/clojurescript/wiki/Quick-Start for instructions on how to compile with 'advanced' mode.
