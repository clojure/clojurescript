## ClojureScript Compiler Compiled with ClojureScript? ##

This is a **patched version of the ClojureScript compiler** that
compiles to ClojureScript. Normally, the ClojureScript compiler is
a pure Clojure program that runs on the JVM. This patched version runs
from the compiled JavaScript. This project was started by
[kanaka](https://github.com/kanaka) but most of the hard problems were
solved by [chouser](https://github.com/chouser).

### Why?

* Why not?
* Compilers are only cool once they are self-hosting (can compile
  their own code). This fork is not self-hosting yet, but that is the
  aim.
* You can use ClojureScript without a JVM.
* You can have a [ClojureScript REPL web
  app](http://kanaka.github.com/clojurescript/web/jsrepl.html) that
  runs locally in your browser (no server involved after loading the
  page).

### Current Caveats

* No compiled macro support yet. This means that you can define new
  macros at the REPL, but the normal "precompiled" macros (like
  defn, fn, let, loop, etc)  are not yet working. So defining
  a function currently looks like this `(def sqr (fn* [x] (* x x)))`.
* The code is not yet compatible with the normal Clojure ClojureScript
  compiler. To make it compatible we really need [Feature Expressions in
  Clojure](http://dev.clojure.org/display/design/Feature+Expressions)
* JavaScript output is not optimized by the Google Closure Compiler
  (which is a Java program).
* The :nodejs compilation target is currently broken. However, the
  `node/run.js` bootstrap script enables compiled CLJS code to be
  invoked that was not compiled with a :target.
* Other miscellaneous broken things that have not been tracked down
  yet.

### Build

You can build the ClojureScript analyzer and compiler with
ClojureScript like this:

```
bin/cljsc src/cljs/cljs/compiler.cljs > compiler.cljs
```

However, that's not all that useful because it is missing the
necessary analyzer `@namespaces` atom built during the analysis phase.
It's also not runnable on it's own because it doesn't have pieces
necessary to run it under a JavaScript engine (e.g. browser or node.js).

You can rebuild the ClojureScript analyzer, compiler, reader and
bootstrap pieces with a web REPL like this:

```
cd web
../bin/cljsc ../src/cljs/webrepl.cljs > webrepl.js
```
Now load the `web/jsrepl.html` file in a browser.

For a REPL in nodejs, build the `src/cljs/noderepl.cljs` code:

```
cd node
../bin/cljsc ../src/cljs/noderepl.cljs > noderepl.js
cp ../src/cljs/goog.js out/
```

Now use the `run.js` bootstrap code to launch the repl:

```
./run.js noderepl.js
```

For direct *.cljs file compilation/evaluation, build the nodecljs.cljs compiler:

```
cd node
../bin/cljsc ../src/cljs/nodecljs.cljs > nodecljs.js
cp ../src/cljs/goog.js out/
```

You can now use a combination of the `run.js` bootstrap code and
`nodecljs.js` to compile/evaluate the `hello.cljs` file:

```
./run.js nodecljs.js hello.cljs
```



--------

## What is ClojureScript? ##

ClojureScript is a new compiler for [Clojure](http://clojure.org) that targets JavaScript. It is designed to emit JavaScript code which is compatible with the advanced compilation mode of the [Google Closure](http://code.google.com/closure/) optimizing compiler.

## Getting Started ##

* [Compare with JavaScript](http://himera.herokuapp.com/synonym.html)
* [Try it online](http://himera.herokuapp.com/index.html)
* Read the [Quick Start](https://github.com/clojure/clojurescript/wiki/Quick-Start) guide.
* Read the [Documentation](https://github.com/clojure/clojurescript/wiki).
* Look at the [Sample Applications](https://github.com/clojure/clojurescript/tree/master/samples).

## Questions, Feedback? ##

Please point all of your questions and feedback [here](http://groups.google.com/group/clojure).

## Developers Welcome ##

ClojureScript operates under the same license as Clojure. All contributors must have a signed CA (Contributor's Agreement) and submit their patch via the appropriate channels. If you're interested in contributing to the project, please see the [contributing](http://clojure.org/contributing) page on [clojure.org](http://clojure.org).

## License ##

    Copyright (c) Rich Hickey. All rights reserved. The use and
    distribution terms for this software are covered by the Eclipse
    Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
    which can be found in the file epl-v10.html at the root of this
    distribution. By using this software in any fashion, you are
    agreeing to be bound by the terms of this license. You must
    not remove this notice, or any other, from this software.
