;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.js
  (:require-macros [cljs.js]
                   [cljs.env.macros :as env])
  (:require [clojure.string :as string]
            [cljs.env :as env]
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
  ^{:doc "Each runtime environment provides a different way to load a library.
  Whatever function *load-fn* is bound to will be passed two arguments - a
  map and a callback function: The map should have the following keys:

  :name   - the name of the library (a symbol)
  :path   - munged relative library path (a string)

  It is up to the implementor to correctly resolve the corresponding .cljs,
  .cljc, or .js resource (the order must be respected). Upon resolution the
  callback should be invoked with a map containing the following keys:

  :lang   - the language, :clj or :js
  :source - the source of the library (a string)
  :name   - optional, used to uniquely identify the script
  :path   - optional, relative URL style path representing location

  If the resource could not be resolved, the callback should be invoked with
  nil."
    :dynamic true}
  *load-fn*
  (fn [name cb]
    (throw (js/Error. "No *load-fn* set"))))

(defonce
  ^{:doc "Each runtime environment provides various ways to eval JavaScript
  source. Whatever function *eval-fn* is bound to will be passed a map
  containing the following keys:

  :lang   - the language, :clj or :js
  :source - the source of the library (a string)
  :name   - optional, used to unique identify the script
  :path   - optional, relative URL style path representing location

  The result of evaluation should be the return value."
    :dynamic true}
  *eval-fn*
  (fn [js-source]
    (throw (js/Error. "No *eval-fn* set"))))

(defn js-eval [{:keys [source] :as resource}]
  (js/eval source))

(defn empty-state []
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
       :*load-fn*      (or (:load-fn opts) *load-fn*)
       :*eval-fn*      (or (:eval-fn opts) *eval-fn*)}
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
       ((:*load-fn* bound-vars) {:name name :path (ns->relpath name)}
         (fn [resource]
           (assert (or (map? resource) (nil? resource))
             "*load-fn* may only return a map or nil")
           (if resource
             (let [{:keys [lang source]} resource]
               (condp = lang
                 :clj (eval-str* bound-vars source opts
                        (fn [ret] (cb true)))
                 :js  (do
                        ((:*eval-fn* bound-vars) resource)
                        (cb true))
                 (throw
                   (js/Error.
                     (str "Invalid :lang specified " lang ", only :clj or :js allowed")))))
             (throw
               (ana/error env
                 (ana/error-message :undeclared-ns
                   {:ns-sym name :js-provide (cljs.core/name name)}))))))))))

(declare ns-side-effects analyze-deps)

(defn analyze* [bound-vars source opts cb]
  (let [rdr  (rt/string-push-back-reader source)
        eof  (js-obj)
        aenv (ana/empty-env)]
    ((fn analyze-loop []
       (binding [env/*compiler*   (:*compiler* bound-vars)
                 ana/*cljs-ns*    (:*cljs-ns* bound-vars)
                 *ns*             (:*ns* bound-vars)
                 r/*data-readers* (:*data-readers* bound-vars)]
         (let [form (r/read {:eof eof :read-cond :allow :features #{:cljs}} rdr)]
           (if-not (identical? eof form)
             (let [aenv (assoc aenv :ns (ana/get-namespace ana/*cljs-ns*))
                   ast  (ana/analyze aenv form)]
               (if (= :ns (:op ast))
                 (ns-side-effects bound-vars aenv ast opts
                   (fn [_] (analyze-loop)))
                 (recur)))
             (cb))))))))

(defn load-deps
  ([bound-vars ana-env lib deps cb]
   (analyze-deps bound-vars ana-env lib deps nil cb))
  ([bound-vars ana-env lib deps opts cb]
   (when (:verbose opts)
     (debug-prn "Loading dependencies"))
   (let [compiler @(:*compiler* bound-vars)]
     (binding [ana/*cljs-dep-set* (vary-meta (conj (:*cljs-dep-set* bound-vars) lib)
                                    update-in [:dep-path] conj lib)]
       (assert (every? #(not (contains? (:*cljs-dep-set* bound-vars) %)) deps)
         (str "Circular dependency detected "
           (-> (:*cljs-dep-set* bound-vars) meta :dep-path)))
       (if (seq deps)
         (let [dep (first deps)]
           (when (:verbose opts)
             (debug-prn "Loading" dep))
           (require bound-vars dep opts
             (fn [_]
               (load-deps bound-vars ana-env lib (next deps) opts cb))))
         (cb))))))

(defn analyze-deps
  "Given a lib, a namespace, deps, its dependencies, env, an analysis environment
   and opts, compiler options - analyze all of the dependencies. Required to
   correctly analyze usage of other namespaces."
  ([bound-vars ana-env lib deps cb]
   (analyze-deps bound-vars ana-env lib deps nil cb))
  ([bound-vars ana-env lib deps opts cb]
   (let [compiler @(:*compiler* bound-vars)]
     (binding [ana/*cljs-dep-set* (vary-meta (conj (:*cljs-dep-set* bound-vars) lib)
                                    update-in [:dep-path] conj lib)]
       (assert (every? #(not (contains? (:*cljs-dep-set* bound-vars) %)) deps)
         (str "Circular dependency detected "
           (-> (:*cljs-dep-set* bound-vars) meta :dep-path)))
       (if (seq deps)
         (let [dep (first deps)]
           ((:*load-fn* bound-vars) {:name dep :path (ns->relpath dep)}
             (fn [resource]
               (assert (or (map? resource) (nil? resource))
                 "*load-fn* may only return a map or nil")
               (if resource
                 (let [{:keys [lang source]} resource]
                   (condp = lang
                     :clj (analyze* bound-vars source opts
                            (fn []
                              (analyze-deps bound-vars ana-env lib (next deps) opts cb)))
                     :js  (analyze-deps bound-vars ana-env lib (next deps) opts cb)
                     (throw
                       (js/Error.
                         (str "Invalid :lang specified " lang ", only :clj or :js allowed")))))
                 (throw
                   (ana/error ana-env
                     (ana/error-message :undeclared-ns
                       {:ns-sym dep :js-provide (name dep)})))))))
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
   (when (:verbose opts)
     (debug-prn "Namespace side effects for" (:name ast)))
   (if (= :ns op)
     (let [{:keys [deps uses requires require-macros use-macros reload reloads]} ast
           env (:*compiler* bound-vars)]
       (letfn [(check-uses-and-load-macros []
                 (when (and (:*analyze-deps* bound-vars) (seq uses))
                   (when (:verbose opts)
                     (debug-prn "Checking uses"))
                   (ana/check-uses uses env))
                 (if (:*load-macros* bound-vars)
                   (do
                     (when (:verobse opts)
                       (debug-prn "Loading :use-macros"))
                     (load-macros bound-vars :use-macros use-macros reload reloads opts
                       (fn []
                         (when (:verobse opts)
                           (debug-prn "Loading :require-macros"))
                         (load-macros bound-vars :require-macros require-macros reloads reloads opts
                           (fn []
                             (when (seq use-macros)
                               (when (:verobse opts)
                                 (debug-prn "Checking :use-macros"))
                               (ana/check-use-macros use-macros env))
                             (cb ast))))))
                   (cb ast)))]
         (cond
           (and load (seq deps))
           (load-deps bound-vars ana-env (:name ast) deps (dissoc opts :macros-ns)
             check-uses-and-load-macros)

           (and (not load) (:*analyze-deps* bound-vars) (seq deps))
           (analyze-deps bound-vars ana-env (:name ast) deps (dissoc opts :macros-ns)
             check-uses-and-load-macros)

           :else
           (check-uses-and-load-macros))))
     (cb ast))))

(defn analyze
  ([state source cb]
   (analyze state source nil cb))
  ([state source opts cb]
   (analyze*
     {:*compiler*     state
      :*cljs-ns*      'cljs.user
      :*ns*           (create-ns ana/*cljs-ns*)
      :*data-readers* tags/*cljs-data-readers*}
     source opts cb)))

;; -----------------------------------------------------------------------------
;; Emit

(defn emit* [bound-vars ast cb]
  (binding [env/*compiler* (:*compiler* bound-vars)]
    (cb (with-out-str (comp/emit ast)))))

(defn emit [state ast cb]
  (emit* {:*compiler* state} ast cb))

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
            {:lang   :clj
             :source (with-out-str
                       (comp/emit (ana/analyze ana-env form nil opts)))})))))

(defn eval
  ([state form cb]
   (eval state form nil cb))
  ([state form opts cb]
   (eval*
     {:*compiler*     state
      :*cljs-ns*      'cljs.user
      :*ns*           (create-ns 'cljs.user)
      :*data-readers* tags/*cljs-data-readers*
      :*load-fn*      (or (:load-fn opts) *load-fn*)
      :*eval-fn*      (or (:eval-fn opts) *eval-fn*)}
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
         (let [form (r/read {:eof eof :read-cond :allow :features #{:cljs}} rdr)]
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
  ([state source cb]
   (compile state source nil cb))
  ([state source opts cb]
    (compile*
      {:*compiler*     state
       :*cljs-ns*      'cljs.user
       :*ns*           (create-ns 'cljs.user)
       :*data-readers* tags/*cljs-data-readers*
       :*analyze-deps* (or (:analyze-deps opts) true)
       :*load-macros*  (or (:load-macros opts) true)
       :*load-fn*      (or (:load-fn opts) *load-fn*)
       :*eval-fn*      (or (:eval-fn opts) *eval-fn*)}
      source opts cb)))

;; -----------------------------------------------------------------------------
;; Evaluate String

(defn eval-str* [bound-vars source name opts cb]
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
         (let [form (r/read {:eof eof :read-cond :allow :features #{:cljs}} rdr)]
           (if-not (identical? eof form)
             (let [aenv (assoc aenv :ns (ana/get-namespace ana/*cljs-ns*))
                   ast  (ana/analyze aenv form)]
               (if (= :ns (:op ast))
                 (do
                   (.append sb
                     (str "goog.provide(\"" (munge (:name ast)) "\");\n"))
                   (ns-side-effects true bound-vars aenv ast opts
                     (fn [_] (compile-loop))))
                 (do
                   (.append sb (with-out-str (comp/emit ast)))
                   (recur))))
             (let [js-source (.toString sb)]
               (when (:verbose opts)
                 (debug-prn js-source))
               (cb (*eval-fn* {:lang   :clj
                               :name   name
                               :path   (ns->relpath name)
                               :source js-source}))))))))))

(defn eval-str
  ([state source cb]
   (eval-str state source nil cb))
  ([state source name cb]
   (eval-str state source name nil cb))
  ([state source opts name cb]
   (eval-str*
     {:*compiler*     state
      :*cljs-ns*      'cljs.user
      :*ns*           (create-ns 'cljs.user)
      :*data-readers* tags/*cljs-data-readers*
      :*analyze-deps* (or (:analyze-deps opts) true)
      :*load-macros*  (or (:load-macros opts) true)
      :*load-fn*      (or (:load-fn opts) *load-fn*)
      :*eval-fn*      (or (:eval-fn opts) *eval-fn*)}
     source name opts cb)))