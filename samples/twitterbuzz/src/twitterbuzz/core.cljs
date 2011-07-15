(ns twitterbuzz.core
  (:require [goog.net.Jsonp :as jsonp]))

(def twitter-uri (goog.Uri. "http://twitter.com/search.json"))

(defn retrieve [payload callback]
  (.send (goog.net.Jsonp. twitter-uri)
         payload
         callback))

(defn my-callback [json]
  (log json))

(retrieve (.strobj {"q" "clojure"}) my-callback)

(defn register
  "Register fn to be called with new tweets."
  [fn])

(defn poll
  []
  ;; polls twitter periodically using jsonp
  ;; figures out if there are any new tweets
  ;;   drop the n we already have?
  ;;   use the latest known timestamp?
  ;; if there are new tweets
  ;;   keywordize the JSON keys
  ;;   call every registered fn with the vector of maps of new tweets.
  )