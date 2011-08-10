(ns repl.test
  (:require [clojure.browser.net :as net]
            [clojure.browser.event :as event]
            [clojure.browser.repl2 :as repl]))

(defn start-repl []
  (repl/start-repl "http://localhost:9000"))

(js/setTimeout start-repl 1000)
