;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns twitterbuzz.core
  (:require [twitterbuzz.dom-helpers :as dom]
            [clojure.string :as string]
            [goog.string :as gstring]
            [goog.net.Jsonp :as jsonp]
            [goog.Timer :as timer]
            [goog.events :as events]
            [goog.events.EventType :as event-type]
            [goog.dom.classes :as classes]))

(def results-per-page 100)
(def max-missing-query 20)

(def initial-state {:max-id 0
                    :graph {}
                    :listeners {}
                    :tweet-count 0
                    :search-tag nil
                    :ignore-mentions #{}})

(def state (atom initial-state))

(defn add-listener
  "Add a listener to the graph."
  [graph k f]
  (let [l (-> graph :listeners k)]
    (assoc-in graph [:listeners k] (conj l f))))

(defn register
  "Register a function to be called when new data arrives specifying
  the event to receive updates for."
  [event f]
  (swap! state add-listener event f))

(def twitter-uri (goog.Uri. "http://search.twitter.com/search.json"))

(defn search-tag
  "Get the current tag value from the page."
  []
  (.-value (dom/get-element :twitter-search-tag)))

(defn retrieve
  "Send request to twitter."
  [payload callback error-callback]
  (.send (goog.net.Jsonp. twitter-uri)
         payload
         callback
         error-callback))

(defn send-event
  "For the given event, call every listener for that event, passing the
   message."
  ([event]
     (send-event event nil))
  ([event message]
     (doseq [f (-> @state :listeners event)]
       (f message))))

(defn parse-mentions
  "Given a map representing a single tweet, return all mentions that
  are found within the tweet text. Twitter usernames are not case
  sensitive so mentioned usernames are always returned in lower case."
  [tweet]
  (map #(string/lower-case (apply str (drop 1 %)))
       (re-seq (re-pattern "@\\w+") (:text tweet))))

(defn add-mentions
  "Add the user to the mentions map for first user she mentions,
  clearing the mentions map of user."
  [graph user mentions]
  (if-let [mention (first mentions)]
    (let [graph (assoc graph mention (get graph mention {:username mention}))
          node (get graph mention)
          mentions-map (get node :mentions {})
          graph (assoc-in graph [mention :mentions user] (inc (get mentions-map user 0)))]
      (assoc-in graph [user :mentions] {}))
    graph))

(defn update-graph
  "Given a graph and a sequence of new tweets in chronological order,
  update the graph."
  [graph tweet-maps]
  (reduce (fn [acc tweet]
            (let [user (string/lower-case (:from_user tweet))
                  mentions (parse-mentions tweet)
                  node (get acc user {:mentions {}})]
              (-> (assoc acc user
                         (assoc node :last-tweet (:text tweet)
                                     :image-url (:profile_image_url tweet)
                                     :username (:from_user tweet)))
                  (add-mentions user mentions))))
          graph
          (map #(select-keys % [:text :from_user :profile_image_url]) tweet-maps)))

(defn num-mentions [user]
  (reduce + (vals (:mentions user))))

(defn update-state
  "Given an old state, maximum id and a new sequence of tweets, return
  an updated state."
  [old-state max-id tweets]
  (-> old-state
      (assoc :max-id max-id)
      (update-in [:tweet-count] #(+ % (count tweets)))
      (assoc :graph (update-graph (:graph old-state) (reverse tweets)))))

(defn new-tweets [max-id tweets]
  (filter #(> (:id %) max-id) tweets))

(defn new-tweets-callback
  "Given a json object, update the state with any new information and
  fire events."
  [json]
  (let [{:keys [max_id results]} (js->clj json :keywordize-keys true)
        tweets (new-tweets (:max-id @state) results)]
    (do (swap! state update-state max_id tweets)
        (send-event :new-tweets tweets)
        (send-event :graph-update (:graph @state)))))

(defn set-tweet-status [css-class message]
  (doto (dom/set-text :tweet-status message)
    (classes/set (name css-class))))

(defn error-callback [error]
  (set-tweet-status :error "Twitter error"))

(defn add-missing-tweets
  "Add missing data to the graph."
  [graph tweets]
  (let [new-tweets (reduce (fn [acc next-tweet]
                             (assoc acc (string/lower-case (:from_user next-tweet))
                                    next-tweet))
                           {}
                           (sort-by :id tweets))]
    (reduce (fn [acc [node-name {:keys [from_user text profile_image_url]}]]
              (if-let [old-tweet (get graph node-name)]
                (if (:last-tweet old-tweet)
                  acc
                  (assoc acc node-name
                         (merge old-tweet {:last-tweet text
                                           :image-url profile_image_url
                                           :username from_user})))
                acc))
            graph
            new-tweets)))

(defn ignored
  "Given a list of the usernames for missing tweets and the tweets
   which are the result of a query for this missing data, return a set of
   twitter usernames which will be ignored moving forward.

   Names may be ignored because the twitter user does not exist or
  just doesn't tweet."
  [missing tweets]
  (when (< (count tweets) results-per-page)
    (let [users (set (map #(string/lower-case (:from_user %)) tweets))
          missing (map string/lower-case missing)]
      (reduce (fn [acc next-missing]
                (if (contains? users next-missing)
                  acc
                  (conj acc next-missing)))
              #{}
              missing))))

(defn add-missing-callback
  "Update the graph and the ignore-mentions list when data is received
  from a missing user query."
  [missing json]
  (let [response (js->clj json :keywordize-keys true)
        tweets (:results response)]
    (if-let [error (:error response)]
      (set-tweet-status :error error)
      (do (swap! state (fn [old-state]
                         (assoc old-state
                           :graph (add-missing-tweets (:graph old-state) tweets)
                           :ignore-mentions (into (:ignore-mentions old-state)
                                                  (ignored missing tweets)))))
          (send-event :new-tweets [])
          (send-event :graph-update (:graph @state))))))

(defn missing-tweets
  "Return a list of usernames with missing tweets in the graph."
  [graph]
  (->> (map second graph)
       (remove :last-tweet)
       (map :username)
       (remove empty?)
       (remove (:ignore-mentions @state))))

(defn fetch-mentioned-tweets
  "Query twitter for usernames which are currently missing data in the
  graph. Limit this query to max-missing-query names."
  [missing]
  (let [q (apply str (interpose " OR " (map #(str "from:" %)
                                            (take max-missing-query missing))))]
    (set-tweet-status :okay "Fetching mentioned tweets")
    (retrieve (doto (js-obj)
                (aset "q" q)
                (aset "rpp" results-per-page))
              #(add-missing-callback missing %)
              error-callback)))

(defn fetch-new-tweets
  "Use the current search tag to fetch new tweets from twitter."
  []
  (when-let [tag (:search-tag @state)]
    (set-tweet-status :okay "Fetching tweets")
    (retrieve (doto (js-obj)
                (aset "q" tag)
                (aset "rpp" results-per-page))
              new-tweets-callback
              error-callback)))

(defn fetch-tweets
  "If there are missing tweets then fetch them, if not fetch new tweets."
  []
  (let [missing (missing-tweets (:graph @state))]
    (if (seq missing)
      (fetch-mentioned-tweets missing)
      (fetch-new-tweets))))

(defn poll
  "Request new data from twitter once every 24 seconds. This will put
  you at the 150 request/hour rate limit. We can speed it up for the demo."
  []
  (let [timer (goog.Timer. 24000)]
    (do (fetch-tweets)
        (. timer (start))
        (events/listen timer goog.Timer/TICK fetch-tweets))))

(defn do-track-button-clicked
  "When the track button is clicked, reset to the initial state
  keeping only the event listeners."
  []
  (do (let [listeners (:listeners @state)]
        (reset! state (assoc initial-state :listeners listeners :search-tag (search-tag))))
      (fetch-tweets)
      (send-event :track-clicked)))

(defn start-app
  "Start polling and listen for UI events."
  []
  (do (poll)
      (events/listen (dom/get-element :twitter-search-button)
                     "click"
                     do-track-button-clicked)
      (events/listen (dom/get-element :twitter-search-tag)
                     event-type/CHANGE
                     do-track-button-clicked)))

(start-app)

(defn link [url s]
  (str "<a href='" url "' target='_twitterbuzz'>" s "</a>"))

(defn markup
  "Add markup to tweet text to activate links."
  [s]
  (let [markup-f (fn [s] (let [w (string/trim s)]
                          (cond (gstring/startsWith w "http://")
                                (link w w)
                                (gstring/startsWith w "@")
                                (link (str "http://twitter.com/#!/" (re-find #"\w+" w)) w)
                                :else s)))]
    (string/join " " (map markup-f (string/split s #"[ ]")))))

(comment

  (parse-mentions {:text "What's up @sue: and @Larry"})
  
  (add-mentions {} "jim" ["sue"])
  (add-mentions {"sue" {}} "jim" ["sue"])
  
  (def tweets [{:profile_image_url "url1"
                :from_user "Jim"
                :text "I like cookies!"}
               {:profile_image_url "url2"
                :from_user "sue"
                :text "Me to @jim."}
               {:profile_image_url "url3"
                :from_user "bob"
                :text "You shouldn't eat so many cookies @sue"}
               {:profile_image_url "url4"
                :from_user "sam"
                :text "@Bob that was a cruel thing to say to @Sue."}
               {:profile_image_url "url5"
                :from_user "ted"
                :text "@foo is awesome!"}])
  
  (def graph (update-graph {} tweets))
  (count graph)

  (num-mentions (get graph "sue"))
  (num-mentions (get graph "bob"))
  (num-mentions (get graph "sam"))
  
  (take 1 (reverse (sort-by #(num-mentions (second %)) (seq graph))))
  
  )
