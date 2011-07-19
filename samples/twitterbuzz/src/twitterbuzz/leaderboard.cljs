(ns twitterbuzz.leaderboard
  (:require [twitterbuzz.core :as buzz]
            [goog.dom :as dom]))

(defn dom-element [element attrs]
  (dom/createDom element
                 (.strobj (reduce (fn [m [k v]]
                                    (assoc m k v))
                                  {}
                                  (map #(vector (name %1) %2) (keys attrs) (vals attrs))))))

(defn add-leaderboard-node [node]
  (let [user (first node)
        user-info (second node)
        parent (dom/getElement "leaderboard-content")
        child (dom-element "div" {:class "tweet"})
        user-e (dom-element "div" {:class "user-name"})
        text (dom-element "div" {:class "tweet-text"})
        pic (dom-element "img" {:src (:image-url user-info) :class "profile-pic"})
        num-mentions (dom-element "div")]
    (do (dom/setTextContent text (:last-tweet user-info))
        (dom/setTextContent user-e user)
        (dom/setTextContent num-mentions (str (buzz/num-mentions user-info)))
        (dom/appendChild child pic)
        (dom/appendChild child user-e)
        (dom/appendChild child text)
        (dom/appendChild child num-mentions)
        (dom/appendChild parent child 0))))

(defn clear-leaderboard []
  (let [parent (dom/getElement "leaderboard-content")]
    (do (dom/removeChildren parent))))

(defn top-n [n nodes]
  (reverse (sort-by #(buzz/num-mentions (second %)) nodes)))

(defn update-leaderboard [graph]
  (do (clear-leaderboard)
      (doseq [next-node (top-n 10 (seq graph))]
        (add-leaderboard-node next-node))))

(buzz/register :graph-update update-leaderboard)
