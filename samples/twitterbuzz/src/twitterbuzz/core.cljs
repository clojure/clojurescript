;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns twitterbuzz.core
  (:require [clojure.string :as string]
            [goog.net.Jsonp :as jsonp]
            [goog.Timer :as timer]
            [goog.events :as events]
            [goog.events.EventType :as event-type]
            [goog.dom.classes :as classes]
            [goog.dom :as dom]))

(def results-per-page 100)
(def missing-limit 20)

(def initial-state {:max-id 1
                    :graph {}
                    :listeners {:new-tweets []
                                :graph-update []
                                :track-clicked []}
                    :tweet-count 0
                    :search-tag nil
                    :ignore-mentions #{}})

(def state (atom initial-state))

(defn add-listener [old-state k f]
  (let [l (-> old-state :listeners k)]
    (assoc-in old-state [:listeners k] (conj l f))))

(defn register
  "Register a function to be called when new data arrives specifying
  the event to receive updates for. Events can be :new-tweets or :graph-update."
  [event f]
  (swap! state add-listener event f))

(def twitter-uri (goog.Uri. "http://search.twitter.com/search.json"))

(defn search-tag
  "Get the current tag value from the page."
  []
  (.value (dom/getElement "twitter-search-tag")))

(defn retrieve [payload callback error-callback]
  (.send (goog.net.Jsonp. twitter-uri)
         payload
         callback
         error-callback))

(defn send-event
  ([event]
     (send-event event nil))
  ([event message]
     (doseq [f (-> @state :listeners event)]
       (f message))))

(defn parse-mentions [tweet]
  (map #(string/lower-case (apply str (drop 1 %)))
       (re-seq (re-pattern "@\\w+") (:text tweet))))

(defn add-mentions
  "Add the user to the mentions map for first user she mentions,
  clearing the mentions map of user"
  [graph user mentions]
  (if-let [mention (first mentions)]
    (let [graph (assoc graph mention (get graph mention {:username mention}))
          node (get graph mention)
          mentions-map (get node :mentions {})
          graph (assoc-in graph [mention :mentions user] (inc (get mentions-map user 0)))]
      (assoc-in graph [user :mentions] {}))
    graph))

(defn update-graph [graph tweet-maps]
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

(defn update-state [old-state max-id tweets]
  (-> old-state
      (assoc :max-id max-id)
      (update-in [:tweet-count] #(+ % (count tweets)))
      (assoc :graph (update-graph (:graph old-state) (reverse tweets)))))

(defn new-tweets-callback [json]
  (let [result-map (js->clj json :keywordize-keys true)
        new-max (:max_id result-map)
        old-max (:max-id @state) ;; the filter won't work if you inline this
        tweets (filter #(> (:id %) old-max)
                       (:results result-map))]
    (do  (swap! state update-state new-max tweets)
         (send-event :new-tweets tweets)
         (send-event :graph-update (:graph @state)))))

(defn set-tweet-status [css-class message]
  (let [ts (dom/getElement "tweet-status")]
    (dom/setTextContent ts message)
    (classes/set ts (name css-class))))

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
   twitter usernames which will be ignored moving forward."
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
  "Update the graph and the ignore-accounts list when data is received
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

(defn do-timer []
  (let [missing (missing-tweets (:graph @state))]
    (if (seq missing)
      (let [q (apply str (interpose " OR " (map #(str "from:" %) (take missing-limit missing))))]
        (set-tweet-status :okay "Fetching mentioned tweets")
        (retrieve (.strobj {"q" q "rpp" results-per-page})
                  #(add-missing-callback missing %) error-callback))
      (when-let [tag (:search-tag @state)]
        (set-tweet-status :okay "Fetching tweets")
        (retrieve (.strobj {"q" tag "rpp" results-per-page})
                  new-tweets-callback error-callback)))))

(defn poll
  "Request new data from twitter once every 24 seconds. This will put
  you at the 150 request/hour rate limit. We can speed it up for the demo."
  []
  (let [timer (goog.Timer. 24000)]
    (do (do-timer)
        (. timer (start))
        (events/listen timer goog.Timer/TICK do-timer))))

(defn do-track-button-clicked []
  (do (let [listeners (:listeners @state)]
        (reset! state (assoc initial-state :listeners listeners :search-tag (search-tag))))
      (do-timer)
      (send-event :track-clicked)))

(defn start-app []
  (do (poll)
      (events/listen (dom/getElement "twitter-search-button")
                     "click"
                     do-track-button-clicked)
      (events/listen (dom/getElement "twitter-search-tag")
                     event-type/CHANGE
                     do-track-button-clicked)))

(start-app)

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

(defn dom-element
  "Create a dom element using a keyword for the element name and a map
  for the attributes."
  [element attrs]
  (dom/createDom (name element)
                 (.strobj (reduce (fn [m [k v]]
                                    (assoc m k v))
                                  {}
                                  (map #(vector (name %1) %2) (keys attrs) (vals attrs))))))

(defn remove-children
  "Remove all children from the element with the passed id."
  [id]
  (let [parent (dom/getElement (name id))]
    (do (dom/removeChildren parent))))
