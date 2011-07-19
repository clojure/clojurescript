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

(def state (atom {:max-id 1
                  :graph {}
                  :new-tweets-listeners []
                  :graph-update-listeners []
                  :tweet-count 0}))

(defn add-listener [k f]
  (swap! state (fn [old] (assoc old k (conj (k old) f)))))

(def twitter-uri (goog.Uri. "http://twitter.com/search.json"))

(defn retrieve [payload callback]
  (.send (goog.net.Jsonp. twitter-uri)
         payload
         callback))

(defn send-tweets [fns tweets]
  (doseq [f fns]
    (f tweets)))

(defn parse-mentions [tweet]
  (map second (re-seq (re-pattern "@(\\w*)") (:text tweet))))

(defn add-mentions
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
                  mentions (parse-mentions tweet)]
              (-> (if-let [existing-node (get acc user)]
                    (assoc acc user
                           (assoc existing-node :last-tweet (:text tweet)))
                    (assoc acc user
                           {:image-url (:profile_image_url tweet)
                            :last-tweet (:text tweet)
                            :mentions {}}))
                  (add-mentions user mentions))))
          graph
          (map #(select-keys % [:text :from_user :profile_image_url]) tweet-maps)))

(defn num-mentions [user]
  (reduce + (vals (:mentions user))))

(defn update-state [old-state max-id tweets]
  (-> old-state
      (assoc :max-id max-id)
      (update-in [:tweet-count] #(+ % (count tweets)))
      (assoc :graph (update-graph (:graph old-state) tweets))))

(defn my-callback [json]
  (let [result-map (js->clj json :keywordize-keys true)
        new-max (:max_id result-map)
        old-max (:max-id @state) ;; the filter won't work if you inline this
        tweets (filter #(> (:id %) old-max)
                       (:results result-map))]
    (do  (swap! state update-state new-max tweets)
         (send-tweets (:new-tweets-listeners @state) tweets)
         (send-tweets (:graph-update-listeners @state) (:graph @state)))))

(defn register
  "Register a function to be called when new data arrives specifying
  the event to receive updates for. Events can be :new-tweets or :graph-update."
  [event f]
  (cond (= event :new-tweets) (add-listener :new-tweets-listeners f)
        (= event :graph-update) (add-listener :graph-update-listeners f)))

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

(comment

  (parse-mentions {:text "What's up @sue and @larry"})
  
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

