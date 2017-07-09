(ns npm-deps-test.string-requires
  (:require [react :refer [createElement]]
            ["react-dom/server" :as ReactDOMServer]
            [npm-deps-test.string-requires-in-classpath]))

(enable-console-print!)

(println "ReactDOMServer exists:" ReactDOMServer
  (.-renderToString ReactDOMServer))
