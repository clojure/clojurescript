;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns twitterbuzz.timeline
  (:require [twitterbuzz.core :as buzz]
            [goog.dom :as dom]))

(defn add-timeline-tweet [tweet]
  (let [parent (dom/getElement "timeline-content")
        child (buzz/dom-element :div {:class "tweet"})
        user (buzz/dom-element :div {:class "user-name"})
        text (buzz/dom-element :div {:class "tweet-text"})
        pic (buzz/dom-element :img {:src (:profile_image_url tweet) :class "profile-pic"})]
    (do (dom/insertChildAt text (dom/htmlToDocumentFragment (:text tweet)) 0) ;;(dom/setTextContent text (:text tweet))
        (dom/setTextContent user (:from_user tweet))
        (dom/appendChild child pic)
        (dom/appendChild child user)
        (dom/appendChild child text)
        (dom/insertChildAt parent child 0))))

(defn update-timeline [tweets]
  (let [status (dom/getElement "tweet-status")]
    (do (dom/setTextContent status (str (:tweet-count @buzz/state) " tweets"))
        (doseq [tweet tweets]
          (add-timeline-tweet tweet)))))

(buzz/register :track-clicked #(buzz/remove-children "timeline-content"))
(buzz/register :new-tweets update-timeline)
