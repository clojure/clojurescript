# ClojureScript REPL Examples

The ClojureScript REPL has been updated to work with multiple
JavaScript evaluation environments. This readme and the example code
in this project show how to use the new REPL. There are now two
implemented environments: Rhino and the browser.

## Using the new REPL

There are currently four steps in starting a ClojureScript REPL.

1. require cljs.repl
2. require the namespace which implements the desired evaluation environment
3. create a new evaluation environment
4. start the REPL with the created environment

## Evaluating with Rhino

```clj
(require '[cljs.repl :as repl])
(require '[cljs.repl.rhino :as rhino])
(def env (rhino/repl-env))
(repl/repl env)
``` 

## Evaluating in the Browser

A browser-connected REPL works in much the same way as a normal REPL:
forms are read from the console, evaluated and return values are
printed. A major and useful difference form normal REPL usage is that
all side-effects occur in the browser. You can show alerts, manipulate
the dom and interact with running applications.

The main benefit of using the browser as an evaluation environment is
that the REPL is no longer limited to non-browser code. You get all
the benefits of a REPL with all of your code.

The example below shows how to start a browser-connected REPL with an
empty project. The same technique can be used to integrate a REPL into
an existing project. In the future there may be an easier way to start
a browser-connected REPL without a project.

### Building

This sample project contains an HTML file and single ClojureScript
file which establishes the connection to the REPL from the
browser. These are currently the minimum requirements for starting a
browser-connected REPL.

To build the project, launch a Clojure REPL from this folder and evaluate the following
forms:

```clj
(use 'cljs.closure)
(def opts {:output-to "main.js" :output-dir "out"})
(build "src" opts)
```
    
### Starting the REPL and connecting to the browser

Start the REPL using the browser as the evaluator (do it in "samples/repl"):

```clj
(require '[cljs.repl :as repl])
(require '[cljs.repl.browser :as browser])
(def env (browser/repl-env))
(repl/repl env)
```

Open http://localhost:9000/ in a browser. When this page is loaded it will connect
to the REPL. Alternatively you can serve index.html from your own local webserver.

### Try it out

```clj
;; Evaluate some basic forms.
(+ 1 1)
{:a :b}
"hello"
(reduce + [1 2 3 4 5])
(js/alert "Hello World!")
 
;; Load a file, and use it.
(load-file "clojure/string.cljs")
(clojure.string/reverse "Hello")
 
;; Define functions and call them.
(defn sum [coll] (reduce + coll))
(sum [2 2 2 2])
 
;; Create dom elements.
(ns dom.testing (:require [clojure.browser.dom :as dom]))
(dom/append (dom/get-element "content")
            (dom/element "Hello World!"))
```
