# ClojureScript REPL Examples

The ClojureScript REPL has been updated to work with multiple JavaScript evaluation environments. This readme
and the exmaple code show how to use the new REPL. There are now two implemented environments: Rhino and 
the browser.

## Using the new REPL

There a four steps in starting a ClojureScript REPL.

1. require cljs.repl
2. require the namespace which implements the desired evaluation environment
3. create a new evaluation environment
4. start the repl with the created environment

## Evaluating with Rhino

```clj
(require '[cljs.repl :as repl])
(require '[cljs.repl.rhino :as rhino])
(def env (rhino/repl-env))
(repl/repl env)
``` 

## Evaluating in the Browser

A browser-connected REPL works the same as a normal REPL: forms are evaluated and return values are displayed
in the REPL. All side-effects occur in the browser. You can show alerts, manipulate the dom and interact with
running applications. The two main benefits of this are that REPL activity is no longer restricted to 
non-browser code and evaluation time is faster than Rhino.

The example below shows how to start a browser-connected REPL with an empty project. The same technique can be
used to integrate a REPL into an existing project. In the future there may be an easier way to start a
browser-connected REPL without a project.

### Building

This sample project contains an HTML file and single ClojureScript file which establishes are connection to the
REPL from the browser. There are currently the minimum requirements for starting a browser-connected REPL.

To build the project, launch a Clojure repl and:

```clj
(use 'cljs.closure)
(def opts {:output-to "samples/repl2/main.js" :output-dir "samples/repl2/out"})
(build "samples/repl2/src" opts)
```
    
### Starting the REPL and connecting to the browser

Start the REPL using the browser as the evaluator:

```clj
(require '[cljs.repl :as repl])
(require '[cljs.repl.browser :as browser])
(def env (browser/repl-env :root "samples/repl2/"))
(repl/repl env)
```

Open index.html in a browser. When this page is loaded it will connect to the REPL.

### Try it out

```clj
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
```

