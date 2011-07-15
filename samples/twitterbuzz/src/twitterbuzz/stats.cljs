(ns twitterbuzz.graph)

(defn inc-or-1
  "Returns 1 if x is nil, (inc x) otherwise"
  [x]
  (if (nil? x) 1 (inc x)))

(defn update-retweet-count
  "Updates the total count of retweets by id in m as indicated by the contents of
  the tweet t"
  [m t]
  (let [rt-id (get-in t ["retweeted_status" "id_str"])]
    (when-not (nil? rt-id) (update-in m [rt-id] inc-or-1))))

(defn top-tweets
  "Returns the ids of top n most retweeted tweets out of tweet seq ts"
  [ts n]
  (let [retweet-counts (reduce update-retweet-count {} ts)
        tweet-ids (keys retweet-counts)]
    (take n (sort-by #(get retweet-counts %) tweet-ids))))

(defn graph
  "Returns a map representing the graph of the tweet seq ts"
  [ts]
  {:nodes []
   :edges []})
