(ns twitterbuzz.timeline
  (:require [twitterbuzz.core :as core]
            [goog.dom :as dom]))

;; This is a working example. Please improve in any way to that you
;; can.

;; TODO: Only show a certain number of tweets. Remove tweets from the
;; end of the list when this number is exceeded.

;; may want to put common dom helper functions somewhere else so they
;; can be used by all the views.

(defn dom-element [element attrs]
  (dom/createDom element
                 (.strobj (reduce (fn [m [k v]]
                                    (assoc m k v))
                                  {}
                                  (map #(vector (name %1) %2) (keys attrs) (vals attrs))))))

(defn add-timeline-tweet [tweet]
  (let [parent (dom/getElement "timeline-content")
        child (dom-element "div" {:class "tweet"})
        user (dom-element "div" {:class "user-name"})
        text (dom-element "div" {:class "tweet-text"})
        pic (dom-element "img" {:src (:profile_image_url tweet) :class "profile-pic"})]
    (do (dom/setTextContent text (:text tweet))
        (dom/setTextContent user (:from_user tweet))
        (dom/appendChild child pic)
        (dom/appendChild child user)
        (dom/appendChild child text)
        (dom/insertChildAt parent child 0))))

(defn update-timeline [tweets]
  (let [status (dom/getElement "tweet-status")]
    (do (dom/setTextContent status (str (:tweet-count @core/state) " tweets"))
        (doseq [tweet tweets]
          (add-timeline-tweet tweet)))))

(core/register :new-tweets update-timeline)
