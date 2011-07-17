(ns twitterbuzz.core
  (:require [goog.net.Jsonp :as jsonp]
            [goog.Timer :as timer]
            [goog.events :as events]
            [goog.dom :as dom]))

;; From Chouser
(defn js-obj-seq [o]
  (for [k (js-keys o)] [(keyword k) (aget o k)]))

(defn obj->map
  "Convert a JavaScript object into a Clojure map. Can we have
  something like this in core?"
  [o]
  (into {} (js-obj-seq o)))

(def twitter-uri (goog.Uri. "http://twitter.com/search.json"))

(defn retrieve [payload callback]
  (.send (goog.net.Jsonp. twitter-uri)
         payload
         callback))

(def state (atom {:max-id 1 :functions []}))

(defn send-tweets [fns tweets]
  (when (seq fns)
    (do ((first fns) tweets)
        (recur (rest fns) tweets))))

(defn my-callback [json]
  (let [result-map (obj->map json)
        new-max (:max_id result-map)
        old-max (:max-id @state) ;; the filter won't work if you inline this
        tweets (filter #(> (:id %) old-max)
                       (map obj->map (:results result-map)))]
    (do  (swap! state (fn [old] (assoc old :max-id new-max)))
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
        (.start timer nil) ;; doesn't work as (.start timer)
        (events/listen timer goog.Timer/TICK listener))))

;; Rendering Stuff - Replace with something cool
;; =============================================

(defn dom-element [element attrs]
  (dom/createDom element
                 (.strobj (reduce (fn [m [k v]]
                                    (assoc m k v))
                                  {}
                                  (map #(vector (name %1) %2) (keys attrs) (vals attrs))))))

(defn html [s]
  (dom/htmlToDocumentFragment s))

(defn add-leaderboard-tweet [tweet]
  (let [parent (dom/getElement "leaderboard")
        child (dom-element "div" {:class "tweet"})
        user (dom-element "div" {:class "user-name"})
        text (dom-element "div" {:class "tweet-text"})
        pic (dom-element "img" {:src (:profile_image_url tweet) :class "profile-pic"})]
    (do (dom/setTextContent text (:text tweet))
        (dom/setTextContent user (:from_user tweet))
        (dom/appendChild child pic)
        (dom/appendChild child user)
        (dom/appendChild child text)
        (dom/appendChild parent child))))

(defn clear-leaderboard []
  (let [parent (dom/getElement "leaderboard")
        title (dom/getFirstElementChild parent)]
    (do (dom/removeChildren parent)
        (dom/appendChild parent title))))

;; Example function - waiting for tweets.
(defn update-view-fn [tweets]
  (let [status (dom/getElement "tweet-status")]
    (do (clear-leaderboard)
        (dom/setTextContent status (str (count tweets) " tweets"))
        (loop [tweets tweets]
          (when (seq tweets)
            (do (add-leaderboard-tweet (first tweets))
                (recur (rest tweets))))))))

(register update-view-fn)

(poll)
