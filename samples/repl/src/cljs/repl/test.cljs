(ns repl.test
  (:require [clojure.browser.net :as net]
            [clojure.browser.event :as event]
            [clojure.browser.repl :as repl]))

(defn start-repl []
  (repl/start-repl "http://localhost:3000"))

(start-repl)
