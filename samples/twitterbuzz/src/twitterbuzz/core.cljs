(ns twitterbuzz.core
  (:require [goog.net.Jsonp :as jsonp]
            [goog.debug :as debug]
            [goog.debug.FancyWindow :as fancy]
            [goog.debug.Logger :as logger]))

;; Temporary way to see results
(def debug-window
  (let [w (goog.debug.FancyWindow. "Twitterbuzz")
        _ (.setEnabled w true)
        _ (.init w)]
    w))

(def log (logger/getLogger "Twitterbuzz Logger"))

(defn show [x]
  (.info log (debug/expose x)))

(def twitter-uri (goog.Uri. "http://twitter.com/search.json"))

(defn retrieve [payload callback]
  (.send (goog.net.Jsonp. twitter-uri)
         payload
         callback))

(defn my-callback [json]
  (show json))

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
