(ns twitterbuzz.stats)

;; code review from SDH:
;;
;; 1. I think inc-or-nil could be replaced by fnil - fixed ar
;; 2. update-retweet-count looks broken
;;    `when` instead of `if` in function to be reduced over - fixed ar
;; 3. conversion from string keys to keywords should be
;;    done already before we get here - fixed ar 
;; 4. fns like top-tweets should not take an n arg
;;    consumers can always take what they want - fixed ar
;; 5. namespace and file name do not match - fixed ar

(defn update-retweet-count
  "Updates the total count of retweets by id in m as indicated by the contents of
  the tweet t"
  [m t]
  (let [rt-id (get-in t [:retweeted_status :id_str])]
    (if (nil? rt-id) m (update-in m [rt-id] (fnil inc 0)))))

(defn top-tweets
  "Returns the ids of top n most retweeted tweets out of tweet seq ts"
  [ts]
  (let [retweet-counts (reduce update-retweet-count {} ts)
        tweet-ids (keys retweet-counts)]
    (sort-by #(get retweet-counts %) tweet-ids)))

(defn retweet-of
  "Returns the username of the user who originally authored the tweet
  that t is a retweet of, nil otherwise"
  [t]
  (get-in t [:retweeted_status :user :name]))

(defn mentioned-users
  "Returns the usernames of all the users mentioned in tweet t"
  [t]
  (map #(get % :screen_name) (get-in t [:entities :user_mentions])))

(defn new-edges
  "Filter the seq of sets n to find all sets in n that do not already
  exist in the seq of sets o"
  [n o]
  (filter #(not (contains? o %)) n))

(defn edges
  "Return a lazy seq of pairs of usernames representing the social edges of a tweet
  stream, where a social edge is defined as one user retweeting or
  mentioning another user in a given tweet. Edges are not directional; if
  @alice retweets @bob, and @bob mentions @alice, those two tweets only create
  one edge."
  ([ts]
     (edges ts #{}))
  ([ts edges]
     (let [t (first ts)
           other-users (concat (list (retweet-of t)) (mentioned-users t))
           author (get-in t [:user :name])
           tweet-edges (set (map #(set (list author %)) other-users))
           new-edges (new-edges tweet-edges edges)]
       (when t
         (if (empty? new-edges)
           (recur (rest ts) edges)
           (lazy-seq (concat new-edges (edges (rest ts) (concat new-edges edges)))))))))

(defn graph
  "Returns a map representing the graph of the tweet seq ts"
  [ts]
  {:nodes {}
   :edges []})
