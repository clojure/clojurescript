;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns twitterbuzz.leaderboard
  (:require [twitterbuzz.core :as buzz]
            [twitterbuzz.dom-helpers :as dom]))

(defn leaderboard-element
  "Create a leaderboard element from a user map."
  [user]
  (dom/build [:div {:class "tweet"}
              [:img {:src (:image-url user) :class "profile-pic"}]
              [:div {:class "tweet-details"}
               [:div {:class "user-name"} (:username user)]
               [:div {:class "tweet-text"} (dom/html (buzz/markup (:last-tweet user)))]
               [:div {} (str (buzz/num-mentions user))]]]))

(defn leaders
  "Given a map of users, return a sequence of users in order of the
  greatest to least number of mentions."
  [nodes]
  (reverse (sort-by #(buzz/num-mentions (second %)) nodes)))

(defn update-leaderboard
  "Put the top 5 mentioned users in the leaderboard."
  [graph]
  (do (dom/remove-children :leaderboard-content)
      (doseq [next-node (take 5 (leaders (seq graph)))]
        (dom/append (dom/get-element :leaderboard-content)
                    (leaderboard-element (second next-node))))))

;; Register event listeners.

(buzz/register :track-clicked #(dom/remove-children :leaderboard-content))
(buzz/register :graph-update update-leaderboard)
