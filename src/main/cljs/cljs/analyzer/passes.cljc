;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.passes)

(defn apply-passes
  ([ast passes]
   (apply-passes ast passes nil))
  ([ast passes opts]
   (reduce
     (fn [ast pass]
       (pass (:env ast) ast opts))
     ast passes)))

(defn walk
  ([ast passes]
   (walk ast passes nil))
  ([ast passes opts]
   (reduce
     (fn [ast child-k]
       (assoc ast
         child-k
         (let [child (get ast child-k)]
           (if (vector? child)
             (into [] (map #(walk % passes opts)) child)
             (walk child passes opts)))))
     (some-> ast (apply-passes passes opts)) (:children ast))))
