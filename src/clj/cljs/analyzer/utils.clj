;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.utils
  (:require [cljs.analyzer :as ana]))

(defn simplify-env [_ {:keys [op] :as ast}]
  (let [env (:env ast)
        ast (if (= op :fn)
              (assoc ast :methods
                (map #(simplify-env nil %) (:methods ast)))
              ast)]
    (assoc (dissoc ast :env)
      :env {:context (:context env)})))

(defn elide-children [_ ast]
  (dissoc ast :children))

(defn to-ast
  ([form] (to-ast 'cljs.user form))
  ([ns form]
    (let [env (assoc-in (ana/empty-env) [:ns :name] ns)]
      (binding [ana/*passes*
                (or ana/*passes*
                  [elide-children simplify-env ana/infer-type])]
        (ana/analyze env form)))))

(comment
  (require '[clojure.pprint :as pp])
  (pp/pprint (to-ast '(defn foo [a b] (+ a b))))
)
