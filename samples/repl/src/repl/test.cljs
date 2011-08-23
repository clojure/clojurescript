;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns repl.test
  (:require [clojure.browser.repl :as repl]
            [clojure.browser.dom  :as dom]))

(repl/connect "http://localhost:9000/repl")

(comment

  ;; Note: If you would like for the compiler to be aware of
  ;; everything in a project then delete the 'out' directory before
  ;; calling build. This will force the compiler to compile the whole
  ;; project.

  ;; Compile this project to JavaScript
  (use 'cljs.closure)
  (def opts {:output-to "samples/repl/main.js"
             :output-dir "samples/repl/out"})
  (build "samples/repl/src" opts)

  ;; Start REPL
  (do (require '[cljs.repl :as repl])
      (require '[cljs.repl.browser :as browser])
      (def env (browser/repl-env :root "samples/repl/"))
      (repl/repl env))

  ;; Open the file samples/repl/index.html

  ;; Evaluate some basic forms
  (+ 1 1)
  ;; TODO: Chrome has a problem with keywords
  {:a :b}
  "hello"
  (reduce + [1 2 3 4 5])
  (js/alert "Hello World!")

  ;; Can only load files that depend on what is already avaialble.
  (load-file "clojure/string.cljs")
  (clojure.string/reverse "Hello")

  ;; TODO: This is not being done automatically as it is in rhino.
  (ns cljs.user)
  (defn sum [coll] (reduce + coll))
  (sum [2 2 2 2])

  ;; Create dom elements.
  ;; This require only works because we have already compiled
  ;; 'clojure.browser.dom when the project was built.
  (ns dom.testing (:require [clojure.browser.dom :as dom]))
  (dom/append (dom/get-element "content")
              (dom/element "Hello World!"))

  ;; TODO: This will not work unless you have already required what it
  ;; depends on. You may think that just copying all of goog to 'out'
  ;; will solve the problem but it may not in every case. What if this
  ;; depends on another cljs file which has not been compiled?
  (load-file "clojure/browser/dom.cljs")

  )