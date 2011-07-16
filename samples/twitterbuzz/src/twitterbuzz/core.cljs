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
;; remove this logging code once we can see results on the page

(def twitter-uri (goog.Uri. "http://twitter.com/search.json"))

(defn retrieve [payload callback]
  (.send (goog.net.Jsonp. twitter-uri)
         payload
         callback))

;; From Chouser - We need something like this in core.
(defn js-obj-seq [o]
  (for [k (js-keys o)] [(keyword k) (aget o k)]))

(defn obj->map
  "Convert a JavaScript object into a Clojure map."
  [o]
  (into {} (js-obj-seq o)))

(def state (atom {:max-id 1 :functions []}))

(defn my-callback [json]
  (let [result-map (obj->map json)
        max-id (:max_id result-map)
        tweets (filter #(> (:id %) (:max-id @state))
                       (map obj->map (:results result-map)))]
    ;; tweets should always be the newest tweets.
    (do (show (str (count tweets)))
        (swap! state (fn [old] (assoc old :max-id max-id)))
        (show (str (:max-id @state)))
        ;; Call registered functions here
        )))

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
