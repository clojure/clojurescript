;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.passes.lite
  (:refer-clojure :exclude [var?]))

(defn var? [ast]
  (= :var (:op ast)))

(def ctor->simple-ctor
  '{cljs.core/vector cljs.core/simple-vector
    cljs.core/vec    cljs.core/simple-vec})

(defn update-var [{:keys [name] :as ast}]
  (update ast :name ctor->simple-ctor))

(defn replace-var? [ast]
  (and (var? ast)
       (contains? ctor->simple-ctor (:name ast))))

(defn use-lite-types
  [env ast _]
  (cond-> ast
    (replace-var? ast) update-var))