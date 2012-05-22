;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns repl.test
  (:require [clojure.browser.repl :as repl]))

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
      (def env (browser/repl-env))
      (repl/repl env))
  
  ;; Open http://localhost:9000/ in a browser. When this page is loaded
  ;; it will connect to the REPL. Alternatively you can serve index.html
  ;; from your own local webserver.
  
  ;; Evaluate some basic forms
  (+ 1 1)
  (string-print "hello")
  (prn "foo")
  (prn {:a :b})
  (println "hello")
  (doseq [next (range 20)] (println next))
  {:a :b}
  "hello"
  (reduce + [1 2 3 4 5])
  (time (reduce + (range 10000)))
  (js/alert "Hello World!")
  
  (load-file "clojure/string.cljs")
  (clojure.string/reverse "Hello")

  (defn sum [coll] (reduce + coll))
  (sum [2 2 2 2])

  ;; Create dom elements.
  (ns dom.testing (:require [clojure.browser.dom :as dom]))
  (dom/append (dom/get-element "content")
              (dom/element "Hello World!"))

  ;; Load something we haven't used yet
  (ns test.crypt
    (:require [goog.crypt :as c]))
  (c/stringToByteArray "ClojureScript")

  (load-namespace 'goog.date.Date)
  (goog.date.Date.)

  )
