;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns twitterbuzz.radial
  (:require [clojure.set :as set]))

(defn get-mentions
  "Returns the set of mentions for k in data"
  [data k]
  (-> (get-in data [k :mentions])
      keys
      set))

(defn get-descendants
  "Given child-fn (a map of parent to child), and k, return the
   set of all k's descendants. Set includes k."
  [child-fn k]
  (loop [kids #{k}
         check #{k}]
    (let [[c] (seq check)]
      (if c
        (recur (into kids (child-fn c))
               (into (disj check c) (remove kids (child-fn c))))
        kids))))

(defn branch-weights
  "Return a map of node to its weight (number of descendants),
   using child-fn to get the set of children for a node."
  [nodes child-fn]
  (loop [weights {}
         known #{}
         remaining (set nodes)]
    (let [nodes (remove (fn [n] (seq (set/difference (child-fn n) known))) remaining)]
      (if (seq nodes)
        (recur (into weights (map
                              (fn [n] [n (+ 1
                                           (count (child-fn n))
                                           (reduce + (map weights (child-fn n))))])
                              nodes))
               (into known nodes)
               (set/difference remaining nodes))
        weights))))


