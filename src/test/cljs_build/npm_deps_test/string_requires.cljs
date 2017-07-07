(ns npm-deps-test.string-requires
  (:require [react :refer [createElement]]
            ["react-dom/server" :as ReactDOMServer]))

(enable-console-print!)

(println "ReactDOMServer exists:" ReactDOMServer
  (.-renderToString ReactDOMServer))
