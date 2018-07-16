(ns emit-global-requires-test.core
  (:require [react :refer [createElement]]
            ["react-dom/server" :as ReactDOMServer]
            ["@material-ui/core/styles" :as mui-styles]
            ["@material-ui/core/styles/a" :as mui-styles-a]))

(enable-console-print!)

(println "ReactDOMServer exists:" ReactDOMServer
  (.-renderToString ReactDOMServer))

(println "hi" (ReactDOMServer/renderToString (createElement "div" nil "Hello World!")))

(mui-styles/createMuiTheme #js {})
(mui-styles-a/foo)
