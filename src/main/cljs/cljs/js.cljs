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
            [cljs.tagged-literals :as tags])
  (:import [goog.string StringBuffer]))

(js/goog.require "cljs.core$macros")

(defn debug-prn
  [& args]
  (binding [*print-fn* *print-err-fn*]
    (apply println args)))

(defn ns->relpath
  "Given a namespace as a symbol return the relative path sans extension"
  [ns-sym]
  (string/replace (ana/munge-path ns-sym) \. \/))

(defonce
  ^{:doc "Each runtime environment provides a different way to load libraries.
  Whatever function *load-fn* is bound to will be passed a munged library name
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

(declare eval-str*)

(def *loaded* (atom #{}))

(defn require
  ([name cb]
    (require name nil cb))
  ([name opts cb]
    (require
      {:*compiler*     (env/default-compiler-env)
       :*cljs-ns*      'cljs.user
       :*ns*           (create-ns 'cljs.user)
       :*data-readers* tags/*cljs-data-readers*
       :*eval-fn*      (or (:js-eval opts) js/eval)}
      name opts cb))
  ([bound-vars name opts cb]
   (require bound-vars name nil opts cb))
  ([bound-vars name reload opts cb]
   (when (= :reload reload)
     (swap! *loaded* disj name))
   (when (= :reload-all reload)
     (reset! *loaded* #{}))
   (when-not (contains? @*loaded* name)
     (let [env (:*env* bound-vars)]
       (*load-fn* (ns->relpath name)
         (fn [source]
           (if source
             (eval-str* bound-vars source opts
               (fn [ret] (cb true)))
             (throw
               (ana/error env
                 (ana/error-message :undeclared-ns
                   {:ns-sym dep :js-provide (name dep)}))))))))))

(declare ns-side-effects)

(defn analyze* [bound-vars source opts cb]
  (let [rdr  (rt/string-push-back-reader source)
        eof  (js-obj)
        aenv (ana/empty-env)]
    ((fn analyze-loop []
       (binding [env/*compiler*   (:*compiler* bound-vars)
                 ana/*cljs-ns*    (:*cljs-ns* bound-vars)
                 *ns*             (:*ns* bound-vars)
                 r/*data-readers* (:*data-readers* bound-vars)]
         (let [form (r/read {:eof eof} rdr)]
           (if-not (identical? eof form)
             (let [aenv (assoc aenv :ns (ana/get-namespace ana/*cljs-ns*))
                   ast  (ana/analyze aenv form)]
               (if (= :ns (:op ast))
                 (ns-side-effects bound-vars aenv ast opts
                   (fn [_] (analyze-loop)))
                 (recur)))
             (cb))))))))

(defn load-deps
  ([bound-vars ana-env lib deps cb] (analyze-deps bound-vars lib deps nil cb))
  ([bound-vars ana-env lib deps opts cb]
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
             (require bound-vars name opts
               (fn [_]
                 (load-deps bound-vars lib (next deps) opts cb)))))
         (cb))))))

(defn analyze-deps
  "Given a lib, a namespace, deps, its dependencies, env, an analysis environment
   and opts, compiler options - analyze all of the dependencies. Required to
   correctly analyze usage of other namespaces."
  ([bound-vars ana-env lib deps cb] (analyze-deps bound-vars lib deps nil cb))
  ([bound-vars ana-env lib deps opts cb]
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
             (*load-fn* (ns->relpath name)
               (fn [source]
                 (if-not (nil? source)
                   (analyze* bound-vars source opts
                     (fn []
                       (analyze-deps bound-vars lib (next deps) opts cb)))
                   (throw
                     (ana/error ana-env
                       (ana/error-message :undeclared-ns
                         {:ns-sym dep :js-provide (name dep)}))))))))
         (cb))))))

(defn load-macros [bound-vars k macros reload reloads opts cb]
  (if (seq macros)
    (let [env  (:*compiler* bound-vars)
          nsym (first (vals macros))]
      (let [k (or (k reload)
                  (get-in reloads [k nsym])
                  (and (= nsym name) (:*reload-macros* bound-vars) :reload))]
        (if k
          (require bound-vars nsym k opts
            (fn []
              (load-macros bound-vars k (next macros) reload reloads opts cb)))
          (require bound-vars nsym opts
            (fn []
              (load-macros bound-vars k (next macros) reload reloads opts cb))))
        ;(intern-macros nsym k)
        ))
    (cb)))

(defn ns-side-effects
  ([bound-vars ana-env ast opts cb]
    (ns-side-effects false bound-vars ana-env ast opts cb))
  ([load bound-vars ana-env {:keys [op] :as ast} opts cb]
   (if (= :ns op)
     (let [{:keys [deps uses requires require-macros use-macros reload reloads]} ast
           env (:*compiler* bound-vars)]
       (letfn [(check-uses-and-load-macros []
                 (when (and (:*analyze-deps* bound-vars) (seq uses))
                   (ana/check-uses uses env))
                 (when (:*load-macros* bound-vars)
                   (load-macros bound-vars :use-macros use-macros reload reloads opts
                     (fn []
                       (load-macros bound-vars :require-macros require-macros reloads reloads opts
                         (fn []
                           (when (seq use-macros)
                             (ana/check-use-macros use-macros env))
                           (cb ast)))))))]
         (cond
           (and load (seq deps))
           (load-deps bound-vars name deps (dissoc opts :macros-ns)
             check-uses-and-load-macros)

           (and (not load) (:*analyze-deps* bound-vars) (seq deps))
           (analyze-deps bound-vars name deps (dissoc opts :macros-ns)
             check-uses-and-load-macros)

           :else
           (check-uses-and-load-macros))))
     (cb ast))))

(defn analyze
  ([env source cb]
   (analyze env source nil cb))
  ([env source opts cb]
   (analyze*
     {:*compiler*     env
      :*cljs-ns*      (or (:ns env) 'cljs.user)
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

(defn eval* [bound-vars form opts cb]
  (binding [env/*compiler*   (:*compiler* bound-vars)
            *eval-fn*        (:*eval-fn* bound-vars)
            ana/*cljs-ns*    (:*cljs-ns* bound-vars)
            *ns*             (:*ns* bound-vars)
            r/*data-readers* (:*data-readers* bound-vars)]
    (let [ana-env (assoc (ana/empty-env)
                    :ns (ana/get-namespace ana/*cljs-ns*))]
      (cb (*eval-fn*
            (with-out-str
              (comp/emit (ana/analyze ana-env form nil opts))))))))

(defn eval
  ([env form cb] (eval env form nil cb))
  ([env form opts cb]
   (eval*
     {:*compiler*     env
      :*cljs-ns*      'cljs.user
      :*ns*           (create-ns 'cljs.user)
      :*data-readers* tags/*cljs-data-readers*
      :*eval-fn*      (or (:js-eval opts) js/eval)}
     form opts cb)))

;; -----------------------------------------------------------------------------
;; Compile

(defn compile* [bound-vars source opts cb]
  (let [rdr  (rt/string-push-back-reader source)
        eof  (js-obj)
        aenv (ana/empty-env)
        sb   (StringBuffer.)]
    ((fn compile-loop []
       (binding [env/*compiler*   (:*compiler* bound-vars)
                 *eval-fn*        (:*eval-fn* bound-vars)
                 ana/*cljs-ns*    (:*cljs-ns* bound-vars)
                 *ns*             (:*ns* bound-vars)
                 r/*data-readers* (:*data-readers* bound-vars)]
         (let [form (r/read {:eof eof} rdr)]
           (if-not (identical? eof form)
             (let [aenv (assoc aenv :ns (ana/get-namespace ana/*cljs-ns*))
                   ast  (ana/analyze aenv form)]
               (.append sb (with-out-str (comp/emit ast)))
               (if (= :ns (:op ast))
                 (ns-side-effects bound-vars aenv ast opts
                   (fn [_] (compile-loop)))
                 (recur)))
             (cb (.toString sb)))))))))

(defn compile
  ([env source cb]
   (compile env source nil cb))
  ([env source opts cb]
    (compile*
      {:*compiler*     env
       :*cljs-ns*      'cljs.user
       :*ns*           (create-ns 'cljs.user)
       :*data-readers* tags/*cljs-data-readers*
       :*eval-fn*      (or (:js-eval opts) js/eval)}
      source opts cb)))

;; -----------------------------------------------------------------------------
;; Evaluate String

(defn eval-str* [bound-vars source opts cb]
  (let [rdr  (rt/string-push-back-reader source)
        eof  (js-obj)
        aenv (ana/empty-env)
        sb   (StringBuffer.)]
    ((fn compile-loop []
       (binding [env/*compiler*   (:*compiler* bound-vars)
                 *eval-fn*        (:*eval-fn* bound-vars)
                 ana/*cljs-ns*    (:*cljs-ns* bound-vars)
                 *ns*             (:*ns* bound-vars)
                 r/*data-readers* (:*data-readers* bound-vars)]
         (let [form (r/read {:eof eof} rdr)]
           (if-not (identical? eof form)
             (let [aenv (assoc aenv :ns (ana/get-namespace ana/*cljs-ns*))
                   ast  (ana/analyze aenv form)]
               (.append sb (with-out-str (comp/emit ast)))
               (if (= :ns (:op ast))
                 (ns-side-effects bound-vars aenv ast opts
                   (fn [_] (compile-loop)))
                 (recur)))
             (let [js-source (.toString sb)]
               (when (:verbose opts)
                 (debug-prn js-source))
               (cb (*eval-fn* js-source))))))))))

(defn eval-str
  ([env source cb]
   (eval-str env source nil cb))
  ([env source opts cb]
   (eval-str*
     {:*compiler*     env
      :*cljs-ns*      'cljs.user
      :*ns*           (create-ns 'cljs.user)
      :*data-readers* tags/*cljs-data-readers*
      :*eval-fn*      (or (:js-eval opts) js/eval)}
     source opts cb)))