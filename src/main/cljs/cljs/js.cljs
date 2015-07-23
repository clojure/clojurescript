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
            [cljs.tagged-literals :as tags]
            [goog.crypt.base64 :as base64]
            [cljs.source-map :as sm])
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

(defn atom? [x]
  (instance? Atom x))

(defn valid-name? [x]
  (or (nil? x) (symbol? x) (string? x)))

(defn valid-opts? [x]
  (or (nil? x) (map? x)))

(defonce
  ^{:doc "Each runtime environment provides a different way to load a library.
  Whatever function *load-fn* is bound to will be passed two arguments - a
  map and a callback function: The map will have the following keys:

  :name   - the name of the library (a symbol)
  :macros - modifier signaling a macros namespace load
  :path   - munged relative library path (a string)

  It is up to the implementor to correctly resolve the corresponding .cljs,
  .cljc, or .js resource (the order must be respected). If :macros is true
  resolution should only consider .clj or .cljc resources (the order must be
  respected). Upon resolution the callback should be invoked with a map
  containing the following keys:

  :lang   - the language, :clj or :js
  :source - the source of the library (a string)

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
  :source - the source of the library (string)
  :name   - used to unique identify the script (symbol)

  The result of evaluation should be the return value."
    :dynamic true}
  *eval-fn*
  (fn [js-source]
    (throw (js/Error. "No *eval-fn* set"))))

(defn js-eval
  "A default JavaScript evaluation function."
  [{:keys [source] :as resource}]
  (js/eval source))

(defn wrap-error [ex]
  {:error ex})

(defn empty-state
  "Construct an empty compiler state. Required to invoke analyze, compile,
   eval and eval-str."
  ([]
   (env/default-compiler-env))
  ([init]
   (doto (empty-state) (swap! init))))

(defn sm-data []
  (atom
    {:source-map (sorted-map)
     :gen-col    0
     :gen-line   0}))

(defn append-source-map [state name source sb sm-data opts]
   (let [t    (.valueOf (js/Date.))
         smn  (if name
                (munge name)
                (str "cljs-" t))
         src  (str smn ".cljs")
         file (str smn ".js")
         json (sm/encode {src (:source-map sm-data)}
                {:lines (+ (:gen-line sm-data) 3)
                 :file file :sources-content [source]})]
     (when (:verbose opts) (debug-prn json))
     (swap! state assoc-in
       [:source-maps name] (:source-map sm-data))
     (.append sb
       (str "\n//# sourceURL=" file
            "\n//# sourceMappingURL=data:application/json;base64,"
            (base64/encodeString json true)))))

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
       :*data-readers* tags/*cljs-data-readers*
       :*load-fn*      (or (:load opts) *load-fn*)
       :*eval-fn*      (or (:eval opts) *eval-fn*)}
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
       (try
         ((:*load-fn* bound-vars)
           {:name   name
            :macros (:macros-ns opts)
            :path   (ns->relpath name)}
          (fn [resource]
            (assert (or (map? resource) (nil? resource))
              "*load-fn* may only return a map or nil")
            (if resource
              (let [{:keys [lang source]} resource]
                (condp = lang
                  :clj (try
                         (eval-str* bound-vars source name opts
                           (fn [ret] (cb {:value true})))
                         (catch :default cause
                           (cb (wrap-error
                                 (ana/error env
                                   (str "Could not require " name) cause)))))
                  :js  (let [res (try
                                   ((:*eval-fn* bound-vars) resource)
                                   (catch :default cause
                                     (wrap-error
                                       (ana/error env
                                         (str "Could not require " name) cause))))]
                         (if (:error res)
                           (cb res)
                           (cb {:value true})))
                  (cb (wrap-error
                        (ana/error env
                          (str "Invalid :lang specified " lang ", only :clj or :js allowed"))))))
              (cb (wrap-error
                    (ana/error env
                      (ana/error-message :undeclared-ns
                        {:ns-sym name :js-provide (cljs.core/name name)})))))))
         (catch :default cause
           (cb (wrap-error
                 (ana/error env
                   (str "Could not require " name) cause)))))))))

(declare ns-side-effects analyze-deps)

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
             (fn [res]
               (if-not (:error res)
                 (load-deps bound-vars ana-env lib (next deps) opts cb)
                 (cb res)))))
         (cb {:value nil}))))))

(declare analyze*)

(defn analyze-deps
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
           (try
             ((:*load-fn* bound-vars) {:name dep :path (ns->relpath dep)}
              (fn [resource]
                (assert (or (map? resource) (nil? resource))
                  "*load-fn* may only return a map or nil")
                (if resource
                  (let [{:keys [name lang source]} resource]
                    (condp = lang
                      :clj (analyze* bound-vars source name opts
                             (fn [res]
                               (if-not (:error res)
                                 (analyze-deps bound-vars ana-env lib (next deps) opts cb)
                                 (cb res))))
                      :js (analyze-deps bound-vars ana-env lib (next deps) opts cb)
                      (wrap-error
                        (ana/error ana-env
                          (str "Invalid :lang specified " lang ", only :clj or :js allowed")))))
                  (cb (wrap-error
                        (ana/error ana-env
                          (ana/error-message :undeclared-ns
                            {:ns-sym dep :js-provide (name dep)})))))))
             (catch :default cause
               (cb (wrap-error
                     (ana/error ana-env
                       (str "Could not analyze dep " dep) cause))))))
         (cb {:value nil}))))))

(defn load-macros [bound-vars k macros reload reloads opts cb]
  (if (seq macros)
    (let [env  (:*compiler* bound-vars)
          nsym (first (vals macros))]
      (let [k (or (k reload)
                  (get-in reloads [k nsym])
                  (and (= nsym name) (:*reload-macros* bound-vars) :reload))]
        (require bound-vars nsym k (assoc opts :macros-ns true)
          (fn [res]
            (if-not (:error res)
              (load-macros bound-vars k (next macros) reload reloads opts cb)
              (cb res))))
        ;(intern-macros nsym k)
        ))
    (cb {:value nil})))

(defn ns-side-effects
  ([bound-vars ana-env ast opts cb]
    (ns-side-effects false bound-vars ana-env ast opts cb))
  ([load bound-vars ana-env {:keys [op] :as ast} opts cb]
   (when (:verbose opts)
     (debug-prn "Namespace side effects for" (:name ast)))
   (if (= :ns op)
     (let [{:keys [deps uses requires require-macros use-macros reload reloads]} ast
           env (:*compiler* bound-vars)]
       (letfn [(check-uses-and-load-macros [res]
                 (if (:error res)
                   (cb res)
                   (let [res (try
                               (when (and (:*analyze-deps* bound-vars) (seq uses))
                                 (when (:verbose opts) (debug-prn "Checking uses"))
                                 (ana/check-uses uses env)
                                 {:value nil})
                               (catch :default cause
                                 (wrap-error
                                   (ana/error ana-env
                                     (str "Could not parse ns form " (:name ast)) cause))))]
                     (if (:error res)
                       (cb res)
                       (if (:*load-macros* bound-vars)
                        (do
                          (when (:verbose opts) (debug-prn "Loading :use-macros"))
                          (load-macros bound-vars :use-macros use-macros reload reloads opts
                            (fn [res]
                              (if (:error res)
                                (cb res)
                                (do
                                  (when (:verbose opts) (debug-prn "Loading :require-macros"))
                                  (load-macros bound-vars :require-macros require-macros reloads reloads opts
                                    (fn [res]
                                      (if (:error res)
                                        (cb res)
                                        (let [res (try
                                                    (when (seq use-macros)
                                                      (when (:verbose opts) (debug-prn "Checking :use-macros"))
                                                      (ana/check-use-macros use-macros env))
                                                    {:value nil}
                                                    (catch :default cause
                                                      (wrap-error
                                                        (ana/error ana-env
                                                          (str "Could not parse ns form " (:name ast)) cause))))]
                                          (if (:error res)
                                            (cb res)
                                            (cb {:value ast})))))))))))
                        (cb {:value ast}))))))]
         (cond
           (and load (seq deps))
           (load-deps bound-vars ana-env (:name ast) deps (dissoc opts :macros-ns)
             check-uses-and-load-macros)

           (and (not load) (:*analyze-deps* bound-vars) (seq deps))
           (analyze-deps bound-vars ana-env (:name ast) deps (dissoc opts :macros-ns)
             check-uses-and-load-macros)

           :else
           (check-uses-and-load-macros {:value nil}))))
     (cb {:value ast}))))

(defn analyze* [bound-vars source name opts cb]
  (let [rdr        (rt/indexing-push-back-reader source 1 name)
        eof        (js-obj)
        aenv       (ana/empty-env)
        bound-vars (cond-> (merge bound-vars
                             {:*cljs-ns* 'cljs.user
                              :*ns* (create-ns ana/*cljs-ns*)})
                     (:source-map opts) (assoc :*sm-data* (sm-data)))]
    ((fn analyze-loop []
       (binding [env/*compiler*         (:*compiler* bound-vars)
                 ana/*cljs-ns*          (:*cljs-ns* bound-vars)
                 *ns*                   (:*ns* bound-vars)
                 r/*data-readers*       (:*data-readers* bound-vars)
                 comp/*source-map-data* (:*sm-data* bound-vars)]
         (let [res (try
                     {:value (r/read {:eof eof :read-cond :allow :features #{:cljs}} rdr)}
                     (catch :default cause
                       (wrap-error
                         (ana/error aenv
                           (str "Could not analyze " name) cause))))]
           (if (:error res)
             (cb res)
             (let [form (:value res)]
               (if-not (identical? eof form)
                 (let [aenv (cond-> (assoc aenv :ns (ana/get-namespace ana/*cljs-ns*))
                              (:context opts) (assoc :context (:context opts))
                              (:def-emits-var opts) (assoc :def-emits-var true))
                       res  (try
                              {:value (ana/analyze aenv form nil opts)}
                              (catch :default cause
                                (wrap-error
                                  (ana/error aenv
                                    (str "Could not analyze " name) cause))))]
                   (if (:error res)
                     (cb res)
                     (let [ast (:value res)]
                       (if (= :ns (:op ast))
                         (ns-side-effects bound-vars aenv ast opts
                           (fn [res]
                             (if (:error res)
                               (cb res)
                               (analyze-loop))))
                         (recur)))))
                 (cb {:value nil}))))))))))

(defn analyze
  "Analyze ClojureScript source. The compiler state will be populated with
   the results of analyzes. The parameters:

   state (atom)
     the compiler state

   source (string)
     the ClojureScript source

   name (symbol)
     optional, the name of the source

   opts (map)
     compilation options.

   :eval - the eval function to invoke, see *eval-fn*
   :load - library resolution function, see *load-fn*

   cb (function)
     callback, will be invoked with a map. If successful the map will contain
     a key :value, the actual value is not meaningful. If unsuccessful the
     map will contain a key :error with an ex-info instance describing the cause
     of failure."
  ([state source cb]
   (analyze state source nil cb))
  ([state source name cb]
   (analyze state source name nil cb))
  ([state source name opts cb]
   {:pre [(atom? state) (string? source)
          (valid-name? name) (valid-opts? opts) (fn? cb)]}
   (analyze*
     {:*compiler*     state
      :*data-readers* tags/*cljs-data-readers*
      :*load-fn*      (or (:load opts) *load-fn*)
      :*eval-fn*      (or (:eval opts) *eval-fn*)}
     source name opts cb)))

;; -----------------------------------------------------------------------------
;; Eval

(defn eval* [bound-vars form opts cb]
  (let [bound-vars (cond-> (merge bound-vars
                             {:*cljs-ns* 'cljs.user
                              :*ns* (create-ns ana/*cljs-ns*)})
                     (:source-map opts) (assoc :*sm-data* (sm-data)))]
    (binding [env/*compiler*         (:*compiler* bound-vars)
              *eval-fn*              (:*eval-fn* bound-vars)
              ana/*cljs-ns*          (:*cljs-ns* bound-vars)
              *ns*                   (:*ns* bound-vars)
              r/*data-readers*       (:*data-readers* bound-vars)
              comp/*source-map-data* (:*sm-data* bound-vars)]
      (let [aenv (ana/empty-env)
            aenv (cond-> (assoc aenv :ns (ana/get-namespace ana/*cljs-ns*))
                   (:context opts) (assoc :context (:context opts))
                   (:def-emits-var opts) (assoc :def-emits-var true))
            res  (try
                   {:value (ana/analyze aenv form nil opts)}
                   (catch :default cause
                     (wrap-error
                       (ana/error aenv
                         (str "Could not eval " form) cause))))]
        (if (:error res)
          (cb res)
          (let [ast (:value res)]
            (if (= :ns (:op ast))
              (ns-side-effects true bound-vars aenv ast opts
                (fn [res]
                  (if (:error res)
                    (cb res)
                    (let [src (str "goog.provide(\"" (munge (:name ast)) "\")")]
                      (cb (*eval-fn* {:lang :clj :source src}))))))
              (let [src (with-out-str (comp/emit ast))]
                (cb (*eval-fn* {:lang :clj :source src}))))))))))

(defn eval
  "Evaluate a single ClojureScript form. The parameters:

   state (atom)
     the compiler state

   form (s-expr)
     the ClojureScript source

   opts (map)
     compilation options.

     :eval - the eval function to invoke, see *eval-fn*
     :load - library resolution function, see *load-fn*

   cb (function)
     callback, will be invoked with a map. If successful the map will contain
     a :value key with the result of evalution. If unsuccessful the map wil
     contain a :error key with an ex-info instance describing the cause of
     failure."
  ([state form cb]
   (eval state form nil cb))
  ([state form opts cb]
   (eval*
     {:*compiler*     state
      :*cljs-ns*      'cljs.user
      :*ns*           (create-ns 'cljs.user)
      :*data-readers* tags/*cljs-data-readers*
      :*analyze-deps* (or (:analyze-deps opts) true)
      :*load-macros*  (or (:load-macros opts) true)
      :*load-fn*      (or (:load opts) *load-fn*)
      :*eval-fn*      (or (:eval opts) *eval-fn*)}
     form opts cb)))

;; -----------------------------------------------------------------------------
;; Compile

(defn compile* [bound-vars source name opts cb]
  (let [rdr        (rt/indexing-push-back-reader source 1 name)
        eof        (js-obj)
        aenv       (ana/empty-env)
        sb         (StringBuffer.)
        bound-vars (cond-> (merge bound-vars
                             {:*cljs-ns* 'cljs.user
                              :*ns* (create-ns ana/*cljs-ns*)})
                     (:source-map opts) (assoc :*sm-data* (sm-data)))]
    ((fn compile-loop []
       (binding [env/*compiler*         (:*compiler* bound-vars)
                 *eval-fn*              (:*eval-fn* bound-vars)
                 ana/*cljs-ns*          (:*cljs-ns* bound-vars)
                 *ns*                   (:*ns* bound-vars)
                 r/*data-readers*       (:*data-readers* bound-vars)
                 comp/*source-map-data* (:*sm-data* bound-vars)]
         (let [res (try
                     {:value (r/read {:eof eof :read-cond :allow :features #{:cljs}} rdr)}
                     (catch :default cause
                       (wrap-error
                         (ana/error aenv
                           (str "Could not compile " name) cause))))]
           (if (:error res)
             (cb res)
             (let [form (:value res)]
               (if-not (identical? eof form)
                 (let [aenv (cond-> (assoc aenv :ns (ana/get-namespace ana/*cljs-ns*))
                              (:context opts) (assoc :context (:context opts))
                              (:def-emits-var opts) (assoc :def-emits-var true))
                       ast  (try
                              (ana/analyze aenv form nil opts)
                              (catch :default cause
                                (wrap-error
                                  (ana/error aenv
                                    (str "Could not compile " name) cause))))]
                   (.append sb (with-out-str (comp/emit ast)))
                   (if (= :ns (:op ast))
                     (ns-side-effects bound-vars aenv ast opts
                       (fn [res]
                         (if (:error res)
                           (cb res)
                           (compile-loop))))
                     (recur)))
                 (do
                   (when (:source-map opts)
                     (append-source-map env/*compiler*
                       name source sb @comp/*source-map-data* opts))
                   (cb {:value (.toString sb)})))))))))))

(defn compile
  "Compile ClojureScript source into JavaScript. The parameters:

   state (atom)
     the compiler state

   source (string)
     the ClojureScript source

   name (symbol)
     optional, the name of the source

   opts (map)
     compilation options.

     :load       - library resolution function, see *load-fn*
     :source-map - set to true to generate inline source map information

   cb (function)
     callback, will be invoked with a map. If successful the map will contain
     a key :value with the compilation result (string). If unsuccessful the map
     will contain a key :error with an ex-info instance describing the cause
     of failure."
  ([state source cb]
   (compile state source nil cb))
  ([state source name cb]
   (compile state source name nil cb))
  ([state source name opts cb]
   {:pre [(atom? state) (string? source)
          (valid-name? name) (valid-opts? opts) (fn? cb)]}
   (compile*
     {:*compiler*     state
      :*data-readers* tags/*cljs-data-readers*
      :*analyze-deps* (or (:analyze-deps opts) true)
      :*load-macros*  (or (:load-macros opts) true)
      :*load-fn*      (or (:load opts) *load-fn*)
      :*eval-fn*      (or (:eval opts) *eval-fn*)
      :*sm-data*      (when (:source-map opts) (sm-data))}
     source name opts cb)))

;; -----------------------------------------------------------------------------
;; Evaluate String

(defn eval-str* [bound-vars source name opts cb]
  (let [rdr        (rt/indexing-push-back-reader source 1 name)
        eof        (js-obj)
        aenv       (ana/empty-env)
        sb         (StringBuffer.)
        bound-vars (cond-> (merge bound-vars
                             {:*cljs-ns* 'cljs.user
                              :*ns* (create-ns ana/*cljs-ns*)})
                     (:source-map opts) (assoc :*sm-data* (sm-data)))]
    ((fn compile-loop [ns]
       (binding [env/*compiler*         (:*compiler* bound-vars)
                 *eval-fn*              (:*eval-fn* bound-vars)
                 ana/*cljs-ns*          ns
                 *ns*                   (create-ns ns)
                 r/*data-readers*       (:*data-readers* bound-vars)
                 comp/*source-map-data* (:*sm-data* bound-vars)]
         (let [res (try
                     {:value (r/read {:eof eof :read-cond :allow :features #{:cljs}} rdr)}
                     (catch :default cause
                       (wrap-error
                         (ana/error aenv
                           (str "Could not eval " name) cause))))]
           (if (:error res)
             (cb res)
             (let [form (:value res)]
               (if-not (identical? eof form)
                 (let [aenv (cond-> (assoc aenv :ns (ana/get-namespace ns))
                              (:context opts) (assoc :context (:context opts))
                              (:def-emits-var opts) (assoc :def-emits-var true))
                       res  (try
                              {:value (ana/analyze aenv form nil opts)}
                              (catch :default cause
                                (wrap-error
                                  (ana/error aenv
                                    (str "Could not eval " name) cause))))]
                   (if (:error res)
                     (cb res)
                     (let [ast (:value res)
                           ns' ana/*cljs-ns*]
                      (if (= :ns (:op ast))
                        (do
                          (.append sb
                            (str "goog.provide(\"" (munge (:name ast)) "\");\n"))
                          (ns-side-effects true bound-vars aenv ast opts
                            (fn [res]
                              (if (:error res)
                                (cb res)
                                (compile-loop ns')))))
                        (do
                          (.append sb (with-out-str (comp/emit ast)))
                          (recur ns'))))))
                 (let [js-source (.toString sb)
                       evalm {:lang :clj
                              :name name
                              :path (ns->relpath name)
                              :source js-source}]
                   (when (:verbose opts)
                     (debug-prn js-source))
                   (when (:source-map opts)
                     (append-source-map env/*compiler*
                       name source sb @comp/*source-map-data* opts))
                   (cb {:ns ns :value (*eval-fn* evalm)}))))))))
      (:*cljs-ns* bound-vars))))

(defn eval-str
  "Evalute ClojureScript source given as a string. The parameters:

  state (atom)
    the compiler state

  source (string)
    the ClojureScript source

  name (symbol)
    optional, the name of the source

  opts (map)
    compilation options.

    :eval       - eval function to invoke, see *eval-fn*
    :load       - library resolution function, see *load-fn*
    :source-map - set to true to generate inline source map information

  cb (function)
    callback, will be invoked with a map. If succesful the map will contain
    a :value key with the result of evaluation. If unsuccessful will contain
    a :error key with an ex-info instance describing the cause of failure."
  ([state source cb]
   (eval-str state source nil cb))
  ([state source name cb]
   (eval-str state source name nil cb))
  ([state source name opts cb]
   {:pre [(atom? state) (string? source)
          (valid-name? name) (valid-opts? opts) (fn? cb)]}
   (eval-str*
     {:*compiler*     state
      :*data-readers* tags/*cljs-data-readers*
      :*analyze-deps* (or (:analyze-deps opts) true)
      :*load-macros*  (or (:load-macros opts) true)
      :*load-fn*      (or (:load opts) *load-fn*)
      :*eval-fn*      (or (:eval opts) *eval-fn*)}
     source name opts cb)))