(ns twitterbuzz.graph)

;; code review from SDH:
;;
;; 1. I think inc-or-nil could be replaced by fnil
;; 2. update-retweet-count looks broken
;;    `when` instead of `if` in function to be reduced over
;; 3. conversion from string keys to keywords should be
;;    done already before we get here
;; 4. fns like top-tweets should not take an n arg
;;    consumers can always take what they want
;; 5. namespace and file name do not match

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
