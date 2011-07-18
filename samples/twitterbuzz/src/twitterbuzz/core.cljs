(ns twitterbuzz.core
  (:require [goog.net.Jsonp :as jsonp]
            [goog.Timer :as timer]
            [goog.events :as events]
            [goog.dom :as dom]))

(def twitter-uri (goog.Uri. "http://twitter.com/search.json"))

(defn retrieve [payload callback]
  (.send (goog.net.Jsonp. twitter-uri)
         payload
         callback))

(def state (atom {:max-id 1 :functions [] :tweet-count 0}))

(defn send-tweets [fns tweets]
  (when (seq fns)
    (do ((first fns) tweets)
        (recur (rest fns) tweets))))

(defn my-callback [json]
  (let [result-map (js->clj json :keywordize-keys true)
        new-max (:max_id result-map)
        old-max (:max-id @state) ;; the filter won't work if you inline this
        tweets (filter #(> (:id %) old-max)
                       (:results result-map))]
    (do  (swap! state (fn [old] (-> old
                                   (assoc :max-id new-max)
                                   (assoc :tweet-count (+ (:tweet-count old) (count tweets)))
                                   ;; this doesn't work
                                   #_(update-in :tweet-count #(+ % (count tweets))))))
         (send-tweets (:functions @state) tweets))))

(defn register
  "Register fn to be called with new tweets."
  [f]
  (swap! state (fn [old] (assoc old :functions (conj (:functions old) f)))))

(defn search-tag
  "Get the current tag value from the page."
  []
  (.value (dom/getElement "twitter-search-tag")))

(defn listener []
  (retrieve (.strobj {"q" (search-tag)}) my-callback))

(defn poll
  "Request new data from twitter once every 24 seconds. This will put
  you at the 150 request/hour rate limit. We can speed it up for the demo."
  []
  (let [timer (goog.Timer. 24000)]
    (do (listener)
        (. timer (start))
        (events/listen timer goog.Timer/TICK listener))))

(poll)
