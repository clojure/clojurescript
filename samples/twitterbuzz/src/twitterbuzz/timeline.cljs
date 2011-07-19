;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns twitterbuzz.timeline
  (:require [twitterbuzz.core :as core]
            [goog.dom :as dom]))

(defn dom-element [element attrs]
  (dom/createDom (name element)
                 (.strobj (reduce (fn [m [k v]]
                                    (assoc m k v))
                                  {}
                                  (map #(vector (name %1) %2) (keys attrs) (vals attrs))))))

(defn add-timeline-tweet [tweet]
  (let [parent (dom/getElement "timeline-content")
        child (dom-element :div {:class "tweet"})
        user (dom-element :div {:class "user-name"})
        text (dom-element :div {:class "tweet-text"})
        pic (dom-element :img {:src (:profile_image_url tweet) :class "profile-pic"})]
    (do (dom/insertChildAt text (dom/htmlToDocumentFragment (:text tweet)) 0) ;;(dom/setTextContent text (:text tweet))
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
