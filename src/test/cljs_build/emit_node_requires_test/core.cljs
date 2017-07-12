(ns emit-node-requires-test.core
  (:require [react :refer [createElement]]
            ["react-dom/server" :as ReactDOMServer]))

(enable-console-print!)

(println "ReactDOMServer exists:" ReactDOMServer
  (.-renderToString ReactDOMServer))

(println "hi" (ReactDOMServer/renderToString (createElement "div" nil "Hello World!")))
