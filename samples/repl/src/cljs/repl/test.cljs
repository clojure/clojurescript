(ns repl.test
  (:require [clojure.browser.net :as net]
            [clojure.browser.event :as event]
            [clojure.browser.repl :as repl]))

(defn log [& args]
  (.log js/console (apply pr-str args)))

(defn log-obj [obj]
  (.log js/console obj))

#_(repl/start-repl "http://localhost:3000/javascript")

(defn start-repl []
  (repl/start-repl "http://localhost:9000"))

(js/setTimeout start-repl 1000)



