;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.js
  (:require-macros [cljs.js :refer [dump-core]]
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

(defn- debug-prn
  [& args]
  (binding [*print-fn* *print-err-fn*]
    (apply println args)))

(defn ns->relpath
  "Given a namespace as a symbol return the relative path sans extension"
  [ns-sym]
  (string/replace (ana/munge-path ns-sym) \. \/))

(defn file->ns
  [file]
  (let [lib-name (subs (string/replace file "/" ".")
                   0 (- (count file) 5))]
    (symbol (demunge lib-name))))

(defn- resolve-symbol
  [sym]
  (if (string/starts-with? (str sym) ".")
    sym
    (ana/resolve-symbol sym)))

(defn- atom? [x]
  (instance? Atom x))

(defn- valid-name? [x]
  (or (nil? x) (symbol? x) (string? x)))

(defn- valid-opts? [x]
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

  :lang       - the language, :clj or :js
  :source     - the source of the library (a string)
  :cache      - optional, if a :clj namespace has been precompiled to :js, can
                give an analysis cache for faster loads.
  :source-map - optional, if a :clj namespace has been precompiled to :js, can
                give a V3 source map JSON

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

  :source - the source of the library (string)
  :name   - used to unique identify the script (symbol)
  :cache  - if the source was originally ClojureScript, will be given the
            analysis cache.

  The result of evaluation should be the return value."
    :dynamic true}
  *eval-fn*
  (fn [js-source]
    (throw (js/Error. "No *eval-fn* set"))))

(defn js-eval
  "A default JavaScript evaluation function."
  [{:keys [source] :as resource}]
  (js/eval source))

(defn- wrap-error [ex]
  {:error ex})

(defn empty-state
  "Construct an empty compiler state. Required to invoke analyze, compile,
   eval and eval-str."
  ([]
   (doto (env/default-compiler-env)
     (swap!
       (fn [state]
         (-> state
           (assoc-in [::ana/namespaces 'cljs.core] (dump-core)))))))
  ([init]
   (doto (empty-state) (swap! init))))

(defn load-analysis-cache! [state ns cache]
  (swap! state assoc-in [::ana/namespaces ns] cache))

(defn load-source-map! [state ns sm-json]
  (let [sm (sm/decode (.parse js/JSON sm-json))]
    (swap! state assoc-in [:source-maps ns] sm)))

(defn- sm-data []
  (atom
    {:source-map (sorted-map)
     :gen-col    0
     :gen-line   0}))

(defn- prefix [s pre]
  (str pre s))

(defn- append-source-map
  [state name source sb sm-data {:keys [output-dir asset-path] :as opts}]
   (let [t    (.valueOf (js/Date.))
         smn  (if name
                (string/replace (munge (str name)) "." "/")
                (str "cljs-" t))
         ts   (.valueOf (js/Date.))
         out  (or output-dir asset-path)
         src  (cond-> (str smn ".cljs?rel=" ts)
                out (prefix (str out "/")))
         file (cond-> (str smn ".js?rel=" ts)
                out (prefix (str out "/")))
         json (sm/encode {src (:source-map sm-data)}
                {:lines (+ (:gen-line sm-data) 3)
                 :file  file :sources-content [source]})]
     (when (:verbose opts) (debug-prn json))
     (swap! state assoc-in
       [:source-maps name] (sm/invert-reverse-map (:source-map sm-data)))
     (.append sb
       (str "\n//# sourceURL=" file
            "\n//# sourceMappingURL=data:application/json;base64,"
            (base64/encodeString json)))))

(defn- current-alias-map
  []
  (get-in @env/*compiler* [:cljs.analyzer/namespaces ana/*cljs-ns* :requires]))

;; -----------------------------------------------------------------------------
;; Analyze

(declare eval-str*)

(def *loaded* (atom #{}))

(defn- run-async!
  "Like cljs.core/run!, but for an async procedure, and with the
  ability to break prior to processing the entire collection.

  Chains successive calls to the supplied procedure for items in
  the collection. The procedure should accept an item from the
  collection and a callback of one argument. If the break? predicate,
  when applied to the procedure callback value, yields a truthy
  result, terminates early calling the supplied cb with the callback
  value. Otherwise, when complete, calls cb with nil."
  [proc coll break? cb]
  (if (seq coll)
    (proc (first coll)
      (fn [res]
        (if (break? res)
          (cb res)
          (run-async! proc (rest coll) break? cb))))
    (cb nil)))

(declare require)

(defn- process-deps
  [bound-vars names opts cb]
  (run-async! (fn [name cb]
                (require bound-vars name nil opts cb))
    names
    :error
    cb))

(defn- process-macros-deps
  [bound-vars cache opts cb]
  (process-deps bound-vars
    (distinct (vals (:require-macros cache)))
    (assoc opts :macros-ns true)
    cb))

(defn- process-libs-deps
  [bound-vars cache opts cb]
  (process-deps bound-vars
    (distinct (concat (vals (:requires cache)) (vals (:imports cache))))
    (dissoc opts :macros-ns)
    cb))

(defn require
  ([name cb]
    (require name nil cb))
  ([name opts cb]
    (require nil name opts cb))
  ([bound-vars name opts cb]
   (require bound-vars name nil opts cb))
  ([bound-vars name reload opts cb]
   (let [bound-vars (merge
                      {:*compiler*     (env/default-compiler-env)
                       :*data-readers* tags/*cljs-data-readers*
                       :*load-macros*  (:load-macros opts true)
                       :*analyze-deps* (:analyze-deps opts true)
                       :*load-fn*      (or (:load opts) *load-fn*)
                       :*eval-fn*      (or (:eval opts) *eval-fn*)}
                      bound-vars)
         aname (cond-> name (:macros-ns opts) ana/macro-ns-name)]
     (when (= :reload reload)
       (swap! *loaded* disj aname))
     (when (= :reload-all reload)
       (reset! *loaded* #{}))
     (when (:verbose opts)
       (debug-prn (str "Loading " name (when (:macros-ns opts) " macros") " namespace")))
     (if-not (contains? @*loaded* aname)
       (let [env (:*env* bound-vars)]
         (try
           ((:*load-fn* bound-vars)
             {:name name
              :macros (:macros-ns opts)
              :path (ns->relpath name)}
             (fn [resource]
               (assert (or (map? resource) (nil? resource))
                 "*load-fn* may only return a map or nil")
               (if resource
                 (let [{:keys [lang source cache source-map]} resource]
                   (condp = lang
                     :clj (eval-str* bound-vars source name opts
                            (fn [res]
                              (if (:error res)
                                (cb res)
                                (do
                                  (swap! *loaded* conj aname)
                                  (cb {:value true})))))
                     :js (process-macros-deps bound-vars cache opts
                           (fn [res]
                             (if (:error res)
                               (cb res)
                               (process-libs-deps bound-vars cache opts
                                 (fn [res]
                                   (if (:error res)
                                     (cb res)
                                     (let [res (try
                                                 ((:*eval-fn* bound-vars) resource)
                                                 (when cache
                                                   (load-analysis-cache!
                                                     (:*compiler* bound-vars) aname cache))
                                                 (when source-map
                                                   (load-source-map!
                                                     (:*compiler* bound-vars) aname source-map))
                                                 (catch :default cause
                                                   (wrap-error
                                                     (ana/error env
                                                       (str "Could not require " name) cause))))]
                                       (if (:error res)
                                         (cb res)
                                         (do
                                           (swap! *loaded* conj aname)
                                           (cb {:value true}))))))))))
                     (cb (wrap-error
                           (ana/error env
                             (str "Invalid :lang specified " lang ", only :clj or :js allowed"))))))
                 (cb (wrap-error
                       (ana/error env
                         (ana/error-message (if (:macros-ns opts)
                                              :undeclared-macros-ns
                                              :undeclared-ns)
                           {:ns-sym name :js-provide (cljs.core/name name)})))))))
           (catch :default cause
             (cb (wrap-error
                   (ana/error env
                     (str "Could not require " name) cause))))))
       (cb {:value true})))))

(declare ns-side-effects analyze-deps)

(defn- load-deps
  ([bound-vars ana-env lib deps cb]
   (analyze-deps bound-vars ana-env lib deps nil cb))
  ([bound-vars ana-env lib deps opts cb]
   (when (:verbose opts)
     (debug-prn "Loading dependencies for" lib))
   (binding [ana/*cljs-dep-set* (vary-meta (conj (:*cljs-dep-set* bound-vars) lib)
                                  update-in [:dep-path] conj lib)]
     (assert (every? #(not (contains? (:*cljs-dep-set* bound-vars) %)) deps)
       (str "Circular dependency detected "
         (-> (:*cljs-dep-set* bound-vars) meta :dep-path)))
     (if (seq deps)
       (let [dep (first deps)]
         (require bound-vars dep
           (-> opts
             (dissoc :context)
             (dissoc :ns))
           (fn [res]
             (if-not (:error res)
               (load-deps bound-vars ana-env lib (next deps) opts cb)
               (cb res)))))
       (cb {:value nil})))))

(declare analyze-str*)

(defn- analyze-deps
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
                      :clj (analyze-str* bound-vars source name opts
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

(defn- load-macros [bound-vars k macros reload reloads opts cb]
  (if (seq macros)
    (let [nsym (first (vals macros))
          k    (or (reload k)
                   (get-in reloads [k nsym])
                   (and (= nsym name) (:*reload-macros* bound-vars) :reload)
                   nil)]
      (require bound-vars nsym k
        (-> opts
          (assoc :macros-ns true)
          (dissoc :context)
          (dissoc :ns))
        (fn [res]
          (if-not (:error res)
            (load-macros bound-vars k (next macros) reload reloads opts cb)
            (cb res)))))
    (cb {:value nil})))

(defn- ns-side-effects
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
                           (when (:verbose opts) (debug-prn "Processing :use-macros for" (:name ast)))
                           (load-macros bound-vars :use-macros use-macros reload reloads opts
                             (fn [res]
                               (if (:error res)
                                 (cb res)
                                 (do
                                   (when (:verbose opts) (debug-prn "Processing :require-macros for" (:name ast)))
                                   (load-macros bound-vars :require-macros require-macros reloads reloads opts
                                     (fn [res]
                                       (if (:error res)
                                         (cb res)
                                         (let [res (try
                                                     (when (seq use-macros)
                                                       (when (:verbose opts) (debug-prn "Checking :use-macros for" (:name ast)))
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

(defn- analyze-str* [bound-vars source name opts cb]
  (let [rdr        (rt/indexing-push-back-reader source 1 name)
        eof        (js-obj)
        aenv       (ana/empty-env)
        the-ns     (or (:ns opts) 'cljs.user)
        bound-vars (cond-> (merge bound-vars {:*cljs-ns* the-ns})
                     (:source-map opts) (assoc :*sm-data* (sm-data)))]
    ((fn analyze-loop [last-ast ns]
       (binding [env/*compiler*         (:*compiler* bound-vars)
                 ana/*cljs-ns*          ns
                 ana/*cljs-static-fns*  (:static-fns opts)
                 *ns*                   (create-ns ns)
                 ana/*passes*           (:*passes* bound-vars)
                 r/*alias-map*          (current-alias-map)
                 r/*data-readers*       (:*data-readers* bound-vars)
                 r/resolve-symbol       resolve-symbol
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
                               (analyze-loop ast (:name ast)))))
                         (recur ast ns)))))
                 (cb {:value last-ast}))))))) nil the-ns)))

(defn analyze-str
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
   (analyze-str state source nil cb))
  ([state source name cb]
   (analyze-str state source name nil cb))
  ([state source name opts cb]
   {:pre [(atom? state) (string? source)
          (valid-name? name) (valid-opts? opts) (fn? cb)]}
   (analyze-str*
     {:*compiler*     state
      :*data-readers* tags/*cljs-data-readers*
      :*passes*       (or (:passes opts) ana/*passes*)
      :*analyze-deps* (:analyze-deps opts true)
      :*load-macros*  (:load-macros opts true)
      :*load-fn*      (or (:load opts) *load-fn*)
      :*eval-fn*      (or (:eval opts) *eval-fn*)}
     source name opts cb)))

;; -----------------------------------------------------------------------------
;; Eval

(defn- eval* [bound-vars form opts cb]
  (let [the-ns     (or (:ns opts) 'cljs.user)
        bound-vars (cond-> (merge bound-vars {:*cljs-ns* the-ns})
                     (:source-map opts) (assoc :*sm-data* (sm-data)))]
    (binding [env/*compiler*         (:*compiler* bound-vars)
              *eval-fn*              (:*eval-fn* bound-vars)
              ana/*cljs-ns*          (:*cljs-ns* bound-vars)
              ana/*cljs-static-fns*  (:static-fns opts)
              *ns*                   (create-ns (:*cljs-ns* bound-vars))
              r/*alias-map*          (current-alias-map)
              r/*data-readers*       (:*data-readers* bound-vars)
              r/resolve-symbol       resolve-symbol
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
                      (cb {:value (*eval-fn* {:source src})})))))
              (let [src (with-out-str (comp/emit ast))]
                (cb {:value (*eval-fn* {:source src})})))))))))

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
     a key :value with the result of evalution. If unsuccessful the map will
     contain a key :error with an ex-info instance describing the cause of
     failure."
  ([state form cb]
   (eval state form nil cb))
  ([state form opts cb]
   (eval*
     {:*compiler*     state
      :*data-readers* tags/*cljs-data-readers*
      :*analyze-deps* (:analyze-deps opts true)
      :*load-macros*  (:load-macros opts true)
      :*load-fn*      (or (:load opts) *load-fn*)
      :*eval-fn*      (or (:eval opts) *eval-fn*)}
     form opts cb)))

;; -----------------------------------------------------------------------------
;; Compile

(defn- compile-str* [bound-vars source name opts cb]
  (let [rdr        (rt/indexing-push-back-reader source 1 name)
        eof        (js-obj)
        aenv       (ana/empty-env)
        sb         (StringBuffer.)
        the-ns     (or (:ns opts) 'cljs.user)
        bound-vars (cond-> (merge bound-vars {:*cljs-ns* the-ns})
                     (:source-map opts) (assoc :*sm-data* (sm-data)))]
    ((fn compile-loop [ns]
       (binding [env/*compiler*         (:*compiler* bound-vars)
                 *eval-fn*              (:*eval-fn* bound-vars)
                 ana/*cljs-ns*          ns
                 ana/*cljs-static-fns*  (:static-fns opts)
                 *ns*                   (create-ns ns)
                 r/*alias-map*          (current-alias-map)
                 r/*data-readers*       (:*data-readers* bound-vars)
                 r/resolve-symbol       resolve-symbol
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
                       res  (try
                              {:value (ana/analyze aenv form nil opts)}
                              (catch :default cause
                                (wrap-error
                                  (ana/error aenv
                                    (str "Could not compile " name) cause))))]
                   (if (:error res)
                     (cb res)
                     (let [ast (:value res)]
                       (.append sb (with-out-str (comp/emit ast)))
                       (if (= :ns (:op ast))
                         (ns-side-effects bound-vars aenv ast opts
                           (fn [res]
                             (if (:error res)
                               (cb res)
                               (compile-loop (:name ast)))))
                         (recur ns)))))
                 (do
                   (when (:source-map opts)
                     (append-source-map env/*compiler*
                       name source sb @comp/*source-map-data* opts))
                   (cb {:value (.toString sb)})))))))) the-ns)))

(defn compile-str
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
   (compile-str state source nil cb))
  ([state source name cb]
   (compile-str state source name nil cb))
  ([state source name opts cb]
   {:pre [(atom? state) (string? source)
          (valid-name? name) (valid-opts? opts) (fn? cb)]}
   (compile-str*
     {:*compiler*     state
      :*data-readers* tags/*cljs-data-readers*
      :*analyze-deps* (:analyze-deps opts true)
      :*load-macros*  (:load-macros opts true)
      :*load-fn*      (or (:load opts) *load-fn*)
      :*eval-fn*      (or (:eval opts) *eval-fn*)
      :*sm-data*      (when (:source-map opts) (sm-data))}
     source name opts cb)))

;; -----------------------------------------------------------------------------
;; Evaluate String

(defn- eval-str* [bound-vars source name opts cb]
  (let [rdr        (rt/indexing-push-back-reader source 1 name)
        eof        (js-obj)
        aenv       (ana/empty-env)
        sb         (StringBuffer.)
        the-ns     (or (:ns opts) 'cljs.user)
        bound-vars (cond-> (merge bound-vars {:*cljs-ns* the-ns})
                     (:source-map opts) (assoc :*sm-data* (sm-data)))
        aname      (cond-> name (:macros-ns opts) ana/macro-ns-name)]
    (when (:verbose opts) (debug-prn "Evaluating" name))
    ((fn compile-loop [ns]
       (binding [env/*compiler*         (:*compiler* bound-vars)
                 *eval-fn*              (:*eval-fn* bound-vars)
                 ana/*cljs-ns*          ns
                 ana/*cljs-static-fns*  (:static-fns opts)
                 *ns*                   (create-ns ns)
                 r/*alias-map*          (current-alias-map)
                 r/*data-readers*       (:*data-readers* bound-vars)
                 r/resolve-symbol       resolve-symbol
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
                            (with-out-str (comp/emitln (str "goog.provide(\"" (munge (:name ast)) "\");"))))
                          (ns-side-effects true bound-vars aenv ast opts
                            (fn [res]
                              (if (:error res)
                                (cb res)
                                (compile-loop ns')))))
                        (do
                          (.append sb (with-out-str (comp/emit ast)))
                          (recur ns'))))))
                 (do
                   (when (:source-map opts)
                     (append-source-map env/*compiler*
                       aname source sb @comp/*source-map-data* opts))
                   (let [js-source (.toString sb)
                         evalm     {:lang   :clj
                                    :name   name
                                    :path   (ns->relpath name)
                                    :source js-source
                                    :cache  (get-in @env/*compiler* [::ana/namespaces aname])}
                         complete  (fn [res]
                                     (if (:error res)
                                       (cb res)
                                       (do
                                         (when (:verbose opts)
                                           (debug-prn js-source))
                                         (let [res (try
                                                     {:ns ns :value (*eval-fn* evalm)}
                                                     (catch :default cause
                                                       (wrap-error (ana/error aenv "ERROR" cause))))]
                                           (cb res)))))]
                     (if-let [f (:cache-source opts)]
                       (f evalm complete)
                       (complete {:value nil}))))))))))
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

    :eval         - eval function to invoke, see *eval-fn*
    :load         - library resolution function, see *load-fn*
    :source-map   - set to true to generate inline source map information
    :cache-source - optional, a function to run side-effects with the
                    compilation result prior to actual evalution. This function
                    takes two arguments, the first is the eval map, the source
                    will be under :source. The second argument is a callback of
                    one argument. If an error occurs an :error key should be
                    supplied.

  cb (function)
    callback, will be invoked with a map. If succesful the map will contain
    a :value key with the result of evaluation and :ns the current namespace.
    If unsuccessful will contain a :error key with an ex-info instance describing
    the cause of failure."
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
      :*analyze-deps* (:analyze-deps opts true)
      :*load-macros*  (:load-macros opts true)
      :*load-fn*      (or (:load opts) *load-fn*)
      :*eval-fn*      (or (:eval opts) *eval-fn*)}
     source name opts cb)))

(comment
  (require '[cljs.js :as cljs]
           '[cljs.analyzer :as ana])

  (def vm (js/require "vm"))
  (def fs (js/require "fs"))
  (def st (cljs/empty-state))

  (set! *target* "nodejs")

  (defn node-eval [{:keys [name source]}]
    (.runInThisContext vm source (str (munge name) ".js")))

  (def libs
    {'bootstrap-test.core :cljs
     'bootstrap-test.macros :clj
     'bootstrap-test.helper :clj})

  (defn node-load [{:keys [name macros]} cb]
    (if (contains? libs name)
      (let [path (str "src/test/cljs/" (cljs/ns->relpath name)
                      "." (cljs.core/name (get libs name)))]
        (.readFile fs path "utf-8"
          (fn [err src]
            (cb (if-not err
                  {:lang :clj :source src}
                  (.error js/console err))))))
      (cb nil)))

  (defn elide-env [env ast opts]
    (dissoc ast :env))

  (cljs/analyze-str st "(+ 1 1)" nil
    {:passes [ana/infer-type elide-env]
     :eval node-eval}
    (fn [{:keys [value]}]
      (println value)))

  (cljs/eval st '(defn foo [a b] (+ a b))
    {:eval node-eval}
    (fn [res]
      (println res)))

  (cljs/compile-str st "(defprotocol IFoo (foo [this]))"
    (fn [{:keys [value]}]
      (println "Source:")
      (println value)))

  (cljs/eval-str st
    "(defn foo [a b] (+ a b))
     (defn bar [c d] (+ c d))"
    nil
    {:eval node-eval}
    (fn [res]
      (println res)))

  (cljs/eval-str st "1"
    nil
    {:eval node-eval
     :context :expr}
    (fn [res]
      (println res)))

  (cljs/eval-str st "(def x 1)"
    nil
    {:eval node-eval
     :context :expr
     :def-emits-var true}
    (fn [res]
      (println res)))

  (cljs/eval st '(ns foo.bar)
    {:eval node-eval}
    (fn [res]
      (println res)))

  (cljs/eval st '(def x 1)
    {:eval node-eval
     :context :expr
     :def-emits-var true
     :ns 'foo.bar}
    (fn [res]
      (println res)))

  (cljs/compile-str st "(defn foo\n[a b]\n(+ a b))" 'cljs.foo
    {:verbose true :source-map true}
    (fn [js-source]
      (println "Source:")
      (println js-source)))

  (cljs/eval-str st
    "(ns foo.bar (:require [bootstrap-test.core]))\n(bootstrap-test.core/foo 3 4)"
    'foo.bar
    {:verbose true
     :source-map true
     :eval node-eval
     :load node-load}
    (fn [ret]
      (println ret)))

  (cljs/eval-str st
    "(ns foo.bar (:require-macros [bootstrap-test.macros :refer [foo]]))\n(foo 4 4)"
    'foo.bar
    {:verbose true
     :source-map true
     :eval node-eval
     :load node-load}
    (fn [{:keys [error] :as res}]
      (if error
        (do
          (println "Error:" error)
          (println (.. error -cause -stack)))
        (println "Result:" res))))

  (cljs/compile-str st
    "(ns foo.bar (:require-macros [bootstrap-test.macros :refer [foo]]))\n(foo 4 4)"
    'foo.bar
    {:verbose true
     :source-map true
     :eval node-eval
     :load node-load}
    (fn [{:keys [error] :as res}]
      (if error
        (do
          (println "Error:" error)
          (println (.. error -cause -stack)))
        (println "Result:" res))))

  (cljs/eval-str st
    "(ns foo.bar)\n(first [1 2 3])"
    'foo.bar
    {:verbose true
     :source-map true
     :eval node-eval
     :load node-load}
    (fn [{:keys [error] :as res}]
      (if error
        (do
          (println error)
          (println (.. error -cause -stack)))
        (println res))))

  (cljs/eval-str st
    "(ns foo.bar)\n(map inc [1 2 3])"
    'foo.bar
    {:verbose true
     :source-map true
     :eval node-eval
     :load node-load}
    (fn [{:keys [error] :as res}]
      (if error
        (do
          (println error)
          (println (.. error -cause -stack)))
        (println res))))

  ;; *NOT* source mapped under Node.js
  ;; source-map-support does not yet work, users will need to map
  ;; themselves
  (cljs/eval-str st
    "(ns foo.bar)\n(ffirst [1 2 3])"
    'foo.bar
    {:verbose true
     :source-map true
     :eval node-eval
     :load node-load}
    (fn [{:keys [error] :as res}]
      (if error
        (do
          (println error)
          (println (.. error -cause -stack)))
        (println res))))
  )