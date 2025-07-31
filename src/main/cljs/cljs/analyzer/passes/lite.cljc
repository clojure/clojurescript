;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.passes.lite)

(defn var? [ast]
  (= :var (:op ast)))

(def replace
  '{cljs.core/vector cljs.core/simple-vector
    cljs.core/vec    cljs.core/simple-vec})

(defn update-var [{:keys [name] :as ast}]
  (update-in ast :name (get replace name)))

(defn replace-var? [ast]
  (and (var? ast)
       (contains? replace (:name ast))))

(defn use-lite-types
  [env ast _]
  (cond-> ast
    (replace-var? ast) update-var))