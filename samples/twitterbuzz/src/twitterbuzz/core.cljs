;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns twitterbuzz.core
  (:require [goog.net.Jsonp :as jsonp]
            [goog.Timer :as timer]
            [goog.events :as events]
            [goog.dom :as dom]))

(def initial-state {:max-id 1
                    :graph {}
                    :listeners {:new-tweets []
                                :graph-update []
                                :track-clicked []
                                :refresh-clicked []}
                    :tweet-count 0
                    :search-tag nil})

(def state (atom initial-state))

(defn add-listener [old-state k f]
  (let [l (-> old-state :listeners k)]
    (assoc-in old-state [:listeners k] (conj l f))))

(defn register
  "Register a function to be called when new data arrives specifying
  the event to receive updates for. Events can be :new-tweets or :graph-update."
  [event f]
  (swap! state add-listener event f))

(def twitter-uri (goog.Uri. "http://twitter.com/search.json"))

(defn search-tag
  "Get the current tag value from the page."
  []
  (.value (dom/getElement "twitter-search-tag")))

(defn retrieve [payload callback]
  (.send (goog.net.Jsonp. twitter-uri)
         payload
         callback))

(defn send-event
  ([event]
     (send-event event nil))
  ([event message]
     (doseq [f (-> @state :listeners event)]
       (f message))))

(defn matches [p s]
  (seq (js* "~{s}.match(new RegExp(~{p}, 'g'))")))

(defn parse-mentions [tweet]
  (map #(apply str (drop 1 %)) (matches "@\\w*" (:text tweet))))

(defn add-mentions
  "Add the user to the mentions map for first user she mentions,
  clearing the mentions map of user"
  [graph user mentions]
  (if-let [mention (first mentions)]
    (let [graph (assoc graph mention (get graph mention {}))
          node (get graph mention)
          mentions-map (get node :mentions {})
          graph (assoc-in graph [mention :mentions user] (inc (get mentions-map user 0)))]
      (assoc-in graph [user :mentions] {}))
    graph))

;;old
#_(defn add-mentions
  "Add the user to the mentions map for each user she mentions."
  [graph user mentions]
  (reduce (fn [acc next-mention]
            (if-let [node (get graph next-mention)]
              (let [mentions-map (get node :mentions {})]
                (assoc-in acc [next-mention :mentions user] (inc (get mentions-map user 0))))
              graph))
          graph
          mentions))

(defn update-graph [graph tweet-maps]
  (reduce (fn [acc tweet]
            (let [user (:from_user tweet)
                  mentions (parse-mentions tweet)
                  node (get acc user {:mentions {}})]
              (-> (assoc acc user
                         (assoc node :last-tweet (:text tweet)
                                     :image-url (:profile_image_url tweet)))
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

(defn my-callback [json]
  (let [result-map (js->clj json :keywordize-keys true)
        new-max (:max_id result-map)
        old-max (:max-id @state) ;; the filter won't work if you inline this
        tweets (filter #(> (:id %) old-max)
                       (:results result-map))]
    (do  (swap! state update-state new-max tweets)
         (send-event :new-tweets tweets)
         (send-event :graph-update (:graph @state)))))

(defn do-timer []
  (if-let [tag (:search-tag @state)]
    (retrieve (.strobj {"q" tag "rpp" 100}) my-callback)))

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

(defn do-refresh-button-clicked []
  (send-event :refresh-clicked))

(defn start-app []
  (do (poll)
      (events/listen (dom/getElement "twitter-search-button")
                     "click"
                     do-track-button-clicked)
      (events/listen (dom/getElement "refresh-button")
                     "click"
                     do-refresh-button-clicked)))

(start-app)

(comment

  (parse-mentions {:text "What's up @sue: and @larry"})
  
  (add-mentions {} "jim" ["sue"])
  (add-mentions {"sue" {}} "jim" ["sue"])
  
  (def tweets [{:profile_image_url "url1"
                :from_user "jim"
                :text "I like cookies!"}
               {:profile_image_url "url2"
                :from_user "sue"
                :text "Me to @jim."}
               {:profile_image_url "url3"
                :from_user "bob"
                :text "You shouldn't eat so many cookies @sue"}
               {:profile_image_url "url4"
                :from_user "sam"
                :text "@bob that was a cruel thing to say to @sue."}])
  
  (def graph (update-graph {} tweets))
  
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
