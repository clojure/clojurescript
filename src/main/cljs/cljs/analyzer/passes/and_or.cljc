;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.passes.and-or
  (:require [cljs.analyzer.passes :as passes]))

(def simple-ops
  #{:var :js-var :local :invoke :const :host-field :host-call :js :quote})

(defn ->expr-env [ast]
  (assoc-in ast [:env :context] :expr))

(defn simple-op? [ast]
  (contains? simple-ops (:op ast)))

(defn simple-test-expr?
  [{:keys [op] :as ast}]
  (boolean
    (and (simple-op? ast)
         ('#{boolean seq}
           (or (:tag ast)
               (when (#{:local :var} op)
                 (-> ast :info :tag)))))))

(defn single-binding-let? [ast]
  (and (= :let (:op ast))
       (= 1 (count (-> ast :bindings)))))

(defn no-statements? [let-ast]
  (= [] (-> let-ast :body :statements)))

(defn returns-if? [let-ast]
  (= :if (-> let-ast :body :ret :op)))

(defn simple-test-binding-let? [ast]
  (and (single-binding-let? ast)
       (no-statements? ast)
       (simple-test-expr? (-> ast :bindings first :init))
       (returns-if? ast)))

(defn test=then? [if-ast]
  ;; remove :env, if same, local will differ only by
  ;; :context (:expr | :statement)
  (= (dissoc (:test if-ast) :env)
     (dissoc (:then if-ast) :env)))

(defn test=else? [if-ast]
  ;; remove :env, if same, local will differ only by
  ;; :context (:expr | :statement)
  (= (dissoc (:test if-ast) :env)
     (dissoc (:else if-ast) :env)))

(defn simple-and? [ast]
  (and (simple-test-binding-let? ast)
       (test=else? (-> ast :body :ret))))

(defn simple-or? [ast]
  (and (simple-test-binding-let? ast)
       (test=then? (-> ast :body :ret))))

(defn optimizable-and? [ast]
  (and (simple-and? ast)
       (simple-test-expr? (-> ast :body :ret :then))))

(defn optimizable-or? [ast]
  (and (simple-or? ast)
       (simple-test-expr? (-> ast :body :ret :else))))

(defn remove-loop-let [fn-ast local]
  (update fn-ast :loop-lets
    (fn [loop-lets]
      (map
        (fn [m]
          (update m :params
            (fn [xs] (remove #(= local (:name %)) xs))))
        loop-lets))))

(defn remove-local-pass [local]
  (fn [env ast opts]
    (cond-> (update-in ast [:env :locals] dissoc local)
      (= :fn (:op ast)) (remove-loop-let local))))

(defn optimize-and [ast]
  (let [{:keys [init name]} (-> ast :bindings first)]
    {:op :js
     :env (:env ast)
     :segs ["((" ") && (" "))"]
     :args [init
            (passes/walk
              (->expr-env (-> ast :body :ret :then))
              [(remove-local-pass name)])]
     :form (:form ast)
     :children [:args]
     :tag 'boolean}))

(defn optimize-or [ast]
  (let [{:keys [init name]} (-> ast :bindings first)]
    {:op :js
     :env (:env ast)
     :segs ["((" ") || (" "))"]
     :args [init
            (passes/walk
              (->expr-env (-> ast :body :ret :else))
              [(remove-local-pass name)])]
     :form (:form ast)
     :children [:args]
     :tag 'boolean}))

(defn optimize [env ast _]
  (cond
    (optimizable-and? ast) (optimize-and ast)
    (optimizable-or? ast)  (optimize-or ast)
    :else ast))
