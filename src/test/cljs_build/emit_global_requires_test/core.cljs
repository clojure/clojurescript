(ns emit-global-requires-test.core
  (:require [react :refer [createElement]]
            ["react-dom/server" :as ReactDOMServer]
            ["@material-ui/core/styles" :as mui-styles]))

(enable-console-print!)

(println "ReactDOMServer exists:" ReactDOMServer
  (.-renderToString ReactDOMServer))

(println "hi" (ReactDOMServer/renderToString (createElement "div" nil "Hello World!")))

(mui-styles/createMuiTheme #js {})
