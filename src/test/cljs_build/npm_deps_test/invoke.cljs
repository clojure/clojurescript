(ns npm-deps-test.invoke
  (:require [react :refer [createElement]]
            ["react-dom/server" :as ReactDOMServer]
            ["lodash-es/array" :as array]))

(createElement "div")

(ReactDOMServer/renderToString nil)

(array/findIndex #js [1 2] 2)
