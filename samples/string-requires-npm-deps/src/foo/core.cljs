(ns foo.core
  (:require [react :refer [createElement]]
            ["react-dom/server" :as rds :refer [renderToString]]
            "create-react-class"))

(enable-console-print!)

(println "resolves single exports" create-react-class)

(println (renderToString (createElement "div" nil "Hello World!")))
