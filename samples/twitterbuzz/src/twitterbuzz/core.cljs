(ns twitterbuzz.core
  (:require [goog.net.Jsonp :as jsonp]
            [goog.debug.Console :as console]))

(def console (goog.debug.Console.))
(defn log [msg] (.addLogRecord console msg))

(def twitter-uri (goog.Uri. "http://twitter.com/search.json"))

(defn retrieve [payload callback]
  (.send (goog.net.Jsonp. twitter-uri)
         payload
         callback))

(defn my-callback [json]
  (log json))

(retrieve (.strobj {"q" "clojure"}) my-callback)