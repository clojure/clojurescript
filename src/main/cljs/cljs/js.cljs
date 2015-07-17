;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.js
  (:require-macros [cljs.env.macros :as env])
  (:require [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.tools.reader :as r]
            [cljs.tools.reader.reader-types :as rt]
            [cljs.tagged-literals :as tags]))

(js/goog.require "cljs.core$macros")

(defonce
  ^{:doc "Each runtime environment provides a different way to load libraries.
  Whatever function *load-fn* is bound to will be passed a library name
  (a string) and a callback. The callback should be invoked with the source of
  the library (a string)."
    :dynamic true}
  *load-fn*
  (fn [name cb]
    (throw (js/Error. "No *load-fn* set"))))

(defonce
  ^{:doc "Each runtime environment provides various ways to eval JavaScript
  source. Whatever function *eval-fn* is bound to will be passed source to eval
  (a string). The result of eval will be passed back immediately to the caller."
    :dynamic true}
  *eval-fn*
  (fn [js-source]
    (throw (js/Error. "No *eval-fn* set"))))

(defn empty-env []
  (env/default-compiler-env))

;; -----------------------------------------------------------------------------
;; Analyze

(def *loaded* (atom #{}))

(defn require
  ([name cb]
   (when-not (contains? @*loaded* name)
     (*load-fn* name
       (fn [source]
         (*eval-fn* source)
         (cb true)))))
  ([name reload cb]
   (when (= :reload reload)
     (swap! *loaded* disj name))
   (when (= :reload-all reload)
     (reset! *loaded* #{}))
   (when-not (contains? @*loaded* name)
     (*load-fn* name
       (fn [source]
         (*eval-fn* source)
         (cb true))))))

(declare ns-side-effects)

(defn analyze* [env bound-vars source opts cb]
  (let [rdr  (rt/string-push-back-reader source)
        eof  (js-obj)
        aenv (ana/empty-env)]
    (binding [ana/*cljs-ns*    (:*cljs-ns* bound-vars)
              *ns*             (:*ns* bound-vars)
              r/*data-readers* (:*data-readers* bound-vars)
              env/*compiler*   env]
      (loop []
        (let [form (r/read {:eof eof} rdr)]
          (if-not (identical? eof form)
            (let [aenv (assoc aenv :ns (ana/get-namespace ana/*cljs-ns*))
                  ast  (ana/analyze aenv form)]
              (if (= (:op ast) :ns)
                (ns-side-effects env ast opts bound-vars
                  (fn [_] (cb)))
                (recur)))
            (cb)))))))

(defn analyze-deps
  "Given a lib, a namespace, deps, its dependencies, env, an analysis environment
   and opts, compiler options - analyze all of the dependencies. Required to
   correctly analyze usage of other namespaces."
  ([lib deps env bound-vars cb] (analyze-deps lib deps env bound-vars nil cb))
  ([lib deps env bound-vars opts cb]
   (let [compiler @(:*compiler* bound-vars)]
     (binding [ana/*cljs-dep-set* (vary-meta (conj (:*cljs-dep-set* bound-vars) lib)
                                    update-in [:dep-path] conj lib)]
       (assert (every? #(not (contains? (:*cljs-dep-set* bound-vars) %)) deps)
         (str "Circular dependency detected "
           (-> (:*cljs-dep-set* bound-vars) meta :dep-path)))
       (if (seq deps)
         (let [dep (first deps)]
           (when-not (or (not-empty (get-in compiler [::namespaces dep :defs]))
                         (contains? (:js-dependency-index compiler) (name dep)))
             (*load-fn* name
               (fn [source]
                 (if-not (nil? source)
                   (analyze* env bound-vars source opts
                     (fn []
                       (analyze-deps lib (next deps) env bound-vars opts cb)))
                   (throw
                     (ana/error env
                       (ana/error-message :undeclared-ns
                         {:ns-sym dep :js-provide (name dep)}))))))))
         (cb))))))

(defn load-macros [k macros reload reloads bound-vars cb]
  (if (seq macros)
    (let [nsym (first (vals macros))]
      (let [k (or (k reload)
                  (get-in reloads [k nsym])
                  (and (= nsym name) (:*reload-macros* bound-vars) :reload))]
        (if k
          (require nsym k
            (fn []
              (load-macros k (next macros) reload reloads bound-vars cb)))
          (require nsym
            (fn []
              (load-macros k (next macros) reload reloads bound-vars cb))))
        ;(intern-macros nsym k)
        ))
    (cb)))

(defn ns-side-effects
  [env {:keys [op] :as ast} opts bound-vars cb]
  (if (= :ns op)
    (let [{:keys [deps uses require-macros use-macros reload reloads]} ast]
      (letfn [(check-uses-and-load-macros []
                (when (and (:*analyze-deps* bound-vars) (seq uses))
                  (ana/check-uses uses env))
                (when (:*load-macros* bound-vars)
                  (load-macros :use-macros use-macros reload reloads bound-vars
                    (fn []
                      (load-macros :require-macros require-macros reloads reloads bound-vars
                        (fn []
                          (when (seq use-macros)
                            (ana/check-use-macros use-macros env))
                          (cb ast)))))))]
        (if (and (:*analyze-deps* bound-vars) (seq deps))
          (analyze-deps name deps env (dissoc opts :macros-ns)
            check-uses-and-load-macros)
          (check-uses-and-load-macros))))
    (cb ast)))

(defn analyze
  ([env source cb]
   (analyze env source nil cb))
  ([env source opts cb]
   (analyze* env
     {:*cljs-ns*      (or (:ns env) 'cljs.user)
      :*ns*           (create-ns ana/*cljs-ns*)
      :*data-readers* tags/*cljs-data-readers*}
     source opts cb)))

;; -----------------------------------------------------------------------------
;; Emit

(defn emit* [env bound-vars ast cb]
  (cb (with-out-str (comp/emit ast))))

(defn emit [env ast cb]
  (emit* env {} ast cb))

;; -----------------------------------------------------------------------------
;; Eval

(defn eval* [env bound-vars form opts cb]
  (let [ana-env (ana/empty-env)]
    (binding [*eval-fn*        (:*eval-fn* bound-vars)
              ana/*cljs-ns*    (:*cljs-ns* bound-vars)
              *ns*             (:*ns* bound-vars)
              r/*data-readers* (:*data-readers* bound-vars)
              env/*compiler*   env]
      (cb (*eval-fn*
            (with-out-str
              (comp/emit (ana/analyze ana-env form nil opts))))))))

(defn eval
  ([env form cb] (eval env form nil cb))
  ([env form opts cb]
   (env/with-compiler-env env
     (eval* env
       {:*cljs-ns*      (or (:ns env) 'cljs.user)
        :*ns*           (create-ns ana/*cljs-ns*)
        :*data-readers* tags/*cljs-data-readers*
        :*eval-fn*      (or (:js-eval opts) js/eval)}
       form opts cb))))