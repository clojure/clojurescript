*NOTE: this sample is now out of date. Please refer to the Quick Start*

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

## Compile in Advanced Mode

`cljsc` can be run with a Clojure map of compiler options.  To compile using `cljsc` and Closure Compiler's "advanced" optimization setting:

    cljsc src '{:optimizations :advanced}' > twitterbuzz.js
    
Because advanced mode results in only one `.js` file, `twitterbuzz.js`, only one `<script>` include tag is required.  To see the app as compiled in advanced mode, open `index-advanced.html`.

See https://github.com/clojure/clojurescript/wiki/Quick-Start for more information about compiling in advanced mode.
