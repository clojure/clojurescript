;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns twitterbuzz.radial
  (:require [clojure.set :as set]
            [goog.math :as math]))

(defn get-mentions
  "Returns the set of mentions for k in mentions-data"
  [mentions-data k]
  (-> (get-in mentions-data [k :mentions])
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

(defn weight
  "Weight of noce, given child-fn (mapping of node to set
   of kids)."
  [node child-fn]
  (if-let [kids (seq (child-fn node))]
    (reduce + (map #(weight % child-fn) kids))
    1))

(defn weights
  "Return a map of node to its weight,
   using child-fn to get the set of children for a node."
  [nodes child-fn]
  (reduce
   (fn [m n] (assoc m n (weight n child-fn)))
   {}
   nodes))

(defn layout
  "Returns a map of node => :radius, :slice, :angle.

    weight-fn: one arg fn of node returning weight
    child-fn:  one arg fn of node returning set of nodes"
  ([nodes weight-fn child-fn]
     (layout nodes weight-fn child-fn 1 0 360 #{}))
  ([nodes weight-fn child-fn radius a1 a2 seen]
     (let [slice (- a2 a1)
           total-weight (reduce + (map weight-fn nodes))
           seen (into seen nodes)]
       (loop [m {}
              c1 a1
              [node & more] (seq nodes)]
         (if node
           (let [s (* slice (/ (weight-fn node) total-weight))
                 c2 (+ c1 s)]
             (recur
              (merge
               m
               {node {:radius radius :slice s :angle (/ (+ c1 c2) 2)}}
               (when-let [children (seq (remove seen (child-fn node)))]
                 (layout children weight-fn child-fn (inc radius) c1 c2 seen)))
              c2
              more))
           m)))))

(defn polar->cartesian
  "Convert polar coordinates (from layout) into
   cartesian coordinates on the unit square, assuming the
   square will display max-rings rings."
  [polar-map max-rings]
  (reduce
   (fn [m [k {:keys [radius angle]}]]
     (let [r (/ radius (+ 0.5 max-rings) 2)]
       (assoc m k {:x (+ 0.5 (math/angleDx angle r))
                   :y (+ 0.5 (math/angleDy angle r))})))
   {}
   polar-map))



