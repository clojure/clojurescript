;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns twitterbuzz.anneal)

(defn exp [x]
  (js/Math.exp x))

(defn abs [x]
  (js/Math.abs x))

(defn random []
  (js/Math.random))

(defn standard-prob [e e1 temp]
  (if (< e1 e)
    1
    (exp (/ (- e e1) temp))))

(defn linear-cooling [steps]
  (let [min (- 1 (/ steps (dec steps)))]
    (fn [t]
      (max min (- 1 (/ t steps))))))

(defn anneal
  "Given an energy scoring function, a temperature function
  (calculates temp given iteration t), a permutation function (creates
  a candidate next state given a current state and iteration t), and
  an acceptance probability function (of last next energy and temp),
  yields a lazy seq of (accepted?) states of the form
  {:state s :score :best best :best-score :t t}"

  [energy ;;(energy state) -> score
   temp ;;(temp t) -> 0-1.0
   permute ;;(permute state t) -> new-state
   prob ;;(prob e e1 temp) -> 0-1.0
   state]

  (let [init state
        init-score (energy state)
        step (fn step [{:keys [state score best best-score t]:as ret}]
               (loop [next (permute state) t (inc t)]
                 (let [next-score (energy next)]
                   (if (> (prob score next-score (temp t)) (random))
                     (let [ret {:state next :score next-score :t t
                                :best (if (< next-score best-score) next best)
                                :best-score (min next-score best-score)}]
                       (lazy-seq (cons ret (step ret))))
                     (recur (permute state) (inc t))))))]
    (step {:state init :score init-score :best init :best-score init-score :t 0})))


(comment

(take 10 (take-nth 100
                   (anneal #(abs (- % 42))
                           (linear-cooling 1000)
                           (fn [s _] (+ s (- (random) 0.5)))
                           standard-prob
                           55)))
)
