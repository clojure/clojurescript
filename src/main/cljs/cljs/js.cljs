;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.js
  (:refer-clojure :exclude [require eval])
  (:require-macros [cljs.js :refer [dump-core]]
                   [cljs.env.macros :as env])
  (:require [clojure.string :as string]
            [clojure.walk :as walk]
            [cljs.env :as env]
            [cljs.spec.alpha]
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

(defn- drop-macros-suffix
  [ns-name]
  (when ns-name
    (if (string/ends-with? ns-name "$macros")
      (subs ns-name 0 (- (count ns-name) 7))
      ns-name)))

(defn- elide-macros-suffix
  [sym]
  (symbol (drop-macros-suffix (namespace sym)) (name sym)))

(defn- resolve-symbol
  [sym]
  (if (string/starts-with? (str sym) ".")
    sym
    (elide-macros-suffix (ana/resolve-symbol sym))))

(defn- read [eof rdr]
  (binding [*ns* (symbol (drop-macros-suffix (str *ns*)))]
    (r/read {:eof eof :read-cond :allow :features #{:cljs}} rdr)))

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
  :file       - optional, the file path, it will be added to AST's :file keyword
                (but not in :meta)
  :cache      - optional, if a :clj namespace has been precompiled to :js, can
                give an analysis cache for faster loads.
  :source-map - optional, if a :clj namespace has been precompiled to :js, can
                give a V3 source map JSON

  If the resource could not be resolved, the callback should be invoked with
  nil."
    :dynamic true}
  *load-fn*
  (fn [m cb]
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
  (fn [m]
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
  [state name source sb sm-data {:keys [output-dir asset-path source-map-timestamp] :as opts}]
   (let [t    (.valueOf (js/Date.))
         mn   (if name
                (munge (str name))
                (str "cljs-" t))
         smn  (cond-> mn
                name (string/replace "." "/"))
         ts   (.valueOf (js/Date.))
         out  (or output-dir asset-path)
         src  (cond-> (str smn ".cljs")
                (true? source-map-timestamp) (str "?rel=" ts)
                out (prefix (str out "/")))
         file (cond-> (str smn ".js")
                (true? source-map-timestamp) (str "?rel=" ts)
                out (prefix (str out "/")))
         json (sm/encode {src (:source-map sm-data)}
                {:lines (+ (:gen-line sm-data) 3)
                 :file  file :sources-content [source]})]
     (when (:verbose opts) (debug-prn json))
     (swap! state assoc-in
       [:source-maps (symbol mn)] (sm/invert-reverse-map (:source-map sm-data)))
     (.append sb
       (str "\n//# sourceURL=" file
            "\n//# sourceMappingURL=data:application/json;base64,"
            (-> (js/encodeURIComponent json)
                (string/replace #"%([0-9A-F]{2})" (fn [[_ match]]
                                                    (.fromCharCode js/String (str "0x" match))))
                (base64/encodeString))))))

(defn- alias-map
  [compiler cljs-ns]
  (->> (merge (get-in compiler [::ana/namespaces cljs-ns :requires])
         (get-in compiler [::ana/namespaces cljs-ns :require-macros]))
    (remove (fn [[k v]] (symbol-identical? k v)))
    (into {})))

;; -----------------------------------------------------------------------------
;; Analyze

(declare ^{:arglists '([bound-vars source name opts cb])} eval-str*)

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

(declare ^{:arglists '([name cb]
                       [name opts cb]
                       [bound-vars name opts cb]
                       [bound-vars name reload opts cb])} require)

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
    (-> opts
      (assoc :macros-ns true)
      (dissoc :emit-constants :optimize-constants))
    cb))

(defn- process-libs-deps
  [bound-vars cache opts cb]
  (process-deps bound-vars
    (distinct (concat (vals (:requires cache)) (vals (:imports cache))))
    (dissoc opts :macros-ns)
    cb))

(defn- pre-file-side-effects
  [st name file opts]
  (when (:verbose opts)
    (debug-prn "Pre-file side-effects" file))
  ;; In case any constants are defined in the namespace, flush any analysis metadata
  ;; so that the constants can be defined wihtout triggering re-defined errors.
  (when (and (get-in @st [::ana/namespaces name :defs])
             (not ('#{cljs.core cljs.core$macros} name)))
    (swap! st update ::ana/namespaces dissoc name)))

(defn- post-file-side-effects
  [file opts]
  (when (:verbose opts)
    (debug-prn "Post-file side-effects" file))
  ;; Note, we don't (set! *unchecked-arrays* false) here, as that would interpreted
  ;; an intrinsic affecting the compilation of this file, emitting a no-op. We bypass this
  ;; and emit our own runtime assignment code.
  (js* "cljs.core._STAR_unchecked_arrays_STAR_ = false;"))

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
                 (let [{:keys [lang source cache source-map file]} resource]
                   (condp keyword-identical? lang
                     :clj (do
                            (pre-file-side-effects (:*compiler* bound-vars) aname file opts)
                            (eval-str* bound-vars source name (assoc opts :cljs-file file)
                              (fn [res]
                                (post-file-side-effects file opts)
                                (if (:error res)
                                  (cb res)
                                  (do
                                    (swap! *loaded* conj aname)
                                    (cb {:value true}))))))
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
                                                     (:*compiler* bound-vars) aname cache)
                                                   (ana/register-specs cache))
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

(defn- patch-alias-map
  [compiler in from to]
  (let [patch (fn [k add-if-present?]
                (swap! compiler update-in [::ana/namespaces in k]
                  (fn [m]
                    (let [replaced (walk/postwalk-replace {from to} m)]
                      (if (and add-if-present?
                               (some #{to} (vals replaced)))
                        (assoc replaced from to)
                        replaced)))))
        patch-renames (fn [k]
                        (swap! compiler update-in [::ana/namespaces in k]
                          (fn [m]
                            (when m
                              (reduce (fn [acc [renamed qualified-sym :as entry]]
                                        (if (= (str from) (namespace qualified-sym))
                                          (assoc acc renamed (symbol (str to) (name qualified-sym)))
                                          (merge acc entry)))
                                {} m)))))]
    (patch :requires true)
    (patch :require-macros true)
    (patch :uses false)
    (patch :use-macros false)
    (patch-renames :renames)
    (patch-renames :rename-macros)))

(defn- self-require? [deps opts]
  (and (true? (:def-emits-var opts)) (some #{ana/*cljs-ns*} deps)))

(defn- load-deps
  ([bound-vars ana-env lib deps cb]
   (load-deps bound-vars ana-env lib deps nil nil cb))
  ([bound-vars ana-env lib deps reload opts cb]
   (when (:verbose opts)
     (debug-prn "Loading dependencies for" lib))
   (binding [ana/*cljs-dep-set* (let [lib (if (self-require? deps opts)
                                            'cljs.user
                                            lib)]
                                  (vary-meta (conj (:*cljs-dep-set* bound-vars) lib)
                                    update-in [:dep-path] conj lib))]
     (let [bound-vars (assoc bound-vars :*cljs-dep-set* ana/*cljs-dep-set*)]
       (if-not (every? #(not (contains? ana/*cljs-dep-set* %)) deps)
         (cb (wrap-error
               (ana/error ana-env
                 (str "Circular dependency detected "
                   (apply str
                     (interpose " -> "
                       (conj (-> ana/*cljs-dep-set* meta :dep-path)
                         (some ana/*cljs-dep-set* deps))))))))
         (if (seq deps)
           (let [dep (first deps)
                 opts' (-> opts
                         (dissoc :context)
                         (dissoc :def-emits-var)
                         (dissoc :ns))]
             (require bound-vars dep reload opts'
               (fn [res]
                 (when (:verbose opts)
                   (debug-prn "Loading result:" res))
                 (if-not (:error res)
                   (load-deps bound-vars ana-env lib (next deps) nil opts cb)
                   (if-let [cljs-dep (let [cljs-ns (ana/clj-ns->cljs-ns dep)]
                                       (get {dep nil} cljs-ns cljs-ns))]
                     (require bound-vars cljs-dep opts'
                       (fn [res]
                         (if (:error res)
                           (cb res)
                           (do
                             (patch-alias-map (:*compiler* bound-vars) lib dep cljs-dep)
                             (load-deps bound-vars ana-env lib (next deps) nil opts
                               (fn [res]
                                 (if (:error res)
                                   (cb res)
                                   (cb (update res :aliased-loads assoc dep cljs-dep)))))))))
                     (cb res))))))
           (cb {:value nil})))))))

(declare ^{:arglists '([bound-vars source name opts cb])} analyze-str*)

(defn- analyze-deps
  ([bound-vars ana-env lib deps cb]
   (analyze-deps bound-vars ana-env lib deps nil cb))
  ([bound-vars ana-env lib deps opts cb]
   (binding [ana/*cljs-dep-set* (vary-meta (conj (:*cljs-dep-set* bound-vars) lib)
                                  update-in [:dep-path] conj lib)]
     (let [compiler @(:*compiler* bound-vars)
           bound-vars (assoc bound-vars :*cljs-dep-set* ana/*cljs-dep-set*)]
       (if-not (every? #(not (contains? ana/*cljs-dep-set* %)) deps)
         (cb (wrap-error
               (ana/error ana-env
                 (str "Circular dependency detected "
                   (apply str
                     (interpose " -> "
                       (conj (-> ana/*cljs-dep-set* meta :dep-path)
                         (some ana/*cljs-dep-set* deps))))))))
         (if (seq deps)
           (let [dep (first deps)]
             (try
               ((:*load-fn* bound-vars) {:name dep :path (ns->relpath dep)}
                (fn [resource]
                  (assert (or (map? resource) (nil? resource))
                    "*load-fn* may only return a map or nil")
                  (if-not resource
                    (if-let [cljs-dep (let [cljs-ns (ana/clj-ns->cljs-ns dep)]
                                        (get {dep nil} cljs-ns cljs-ns))]
                      (do
                        (patch-alias-map (:*compiler* bound-vars) lib dep cljs-dep)
                        (analyze-deps bound-vars ana-env lib (cons cljs-dep (next deps)) opts
                          (fn [res]
                            (if (:error res)
                              (cb res)
                              (cb (update res :aliased-loads assoc dep cljs-dep))))))
                      (cb (wrap-error
                            (ana/error ana-env
                              (ana/error-message :undeclared-ns
                                {:ns-sym dep :js-provide (name dep)})))))
                    (let [{:keys [name lang source file]} resource]
                      (condp keyword-identical? lang
                        :clj (do
                               (pre-file-side-effects (:*compiler* bound-vars) name file opts)
                               (analyze-str* bound-vars source name (assoc opts :cljs-file file)
                                 (fn [res]
                                   (post-file-side-effects file opts)
                                   (if-not (:error res)
                                     (analyze-deps bound-vars ana-env lib (next deps) opts cb)
                                     (cb res)))))
                        :js (analyze-deps bound-vars ana-env lib (next deps) opts cb)
                        (wrap-error
                          (ana/error ana-env
                            (str "Invalid :lang specified " lang ", only :clj or :js allowed"))))))))
               (catch :default cause
                 (cb (wrap-error
                       (ana/error ana-env
                         (str "Could not analyze dep " dep) cause))))))
           (cb {:value nil})))))))

(defn- load-macros [bound-vars k macros lib reload reloads opts cb]
  (if (seq macros)
    (let [nsym (first (vals macros))
          k    (or (reload k)
                   (get-in reloads [k nsym])
                   (and (= nsym name) (:*reload-macros* bound-vars) :reload)
                   nil)
          opts' (-> opts
                  (assoc :macros-ns true)
                  (dissoc :context)
                  (dissoc :def-emits-var)
                  (dissoc :ns)
                  (dissoc :emit-constants :optimize-constants))]
      (require bound-vars nsym k opts'
        (fn [res]
          (if-not (:error res)
            (load-macros bound-vars k (next macros) lib reload reloads opts cb)
            (if-let [cljs-dep (let [cljs-ns (ana/clj-ns->cljs-ns nsym)]
                                (get {nsym nil} cljs-ns cljs-ns))]
              (require bound-vars cljs-dep k opts'
                (fn [res]
                  (if (:error res)
                    (cb res)
                    (do
                      (patch-alias-map (:*compiler* bound-vars) lib nsym cljs-dep)
                      (load-macros bound-vars k (next macros) lib reload reloads opts
                        (fn [res]
                          (if (:error res)
                            (cb res)
                            (cb (update res :aliased-loads assoc nsym cljs-dep)))))))))
              (cb res))))))
    (cb {:value nil})))

(defn- rewrite-ns-ast
  ([ast smap]
   (rewrite-ns-ast ast smap false))
  ([ast smap macros?]
   (let [[uk rk renk] (if macros?
                        [:use-macros :require-macros :rename-macros]
                        [:uses :requires :renames])
         rewrite-renames (fn [m]
                           (when m
                             (reduce (fn [acc [renamed qualified-sym :as entry]]
                                       (let [from (symbol (namespace qualified-sym))
                                             to   (get smap from)]
                                         (if (some? to)
                                           (assoc acc renamed (symbol (str to) (name qualified-sym)))
                                           (merge acc entry))))
                               {} m)))
         rewrite-deps (fn [deps]
                        (into []
                          (map (fn [dep]
                                 (if-let [new-dep (get smap dep)]
                                   new-dep
                                   dep)))
                          deps))]
     (-> ast
       (update uk #(walk/postwalk-replace smap %))
       (update rk #(merge smap (walk/postwalk-replace smap %)))
       (update renk rewrite-renames)
       (update :deps rewrite-deps)))))

(defn- check-macro-autoload-inferring-missing
  [{:keys [requires name] :as ast} cenv]
  (let [namespaces (-> @cenv ::ana/namespaces)
        missing-require-macros (into {}
                                 (filter (fn [[_ full-ns]]
                                           (let [{:keys [use-macros require-macros]} (get namespaces full-ns)]
                                             (or (some #{full-ns} (vals use-macros))
                                                 (some #{full-ns} (vals require-macros))))))
                                 requires)
        ast' (update-in ast [:require-macros] merge missing-require-macros)]
    (swap! cenv update-in [::ana/namespaces name :require-macros] merge missing-require-macros)
    ast'))

(defn- ns-side-effects
  ([bound-vars ana-env ast opts cb]
    (ns-side-effects false bound-vars ana-env ast opts cb))
  ([load bound-vars ana-env {:keys [op] :as ast} opts cb]
   (when (:verbose opts)
     (debug-prn "Namespace side effects for" (:name ast)))
   (if (#{:ns :ns*} op)
     (letfn [(check-uses-and-load-macros [res rewritten-ast]
               (let [env (:*compiler* bound-vars)
                     {:keys [uses use-macros reload reloads name]} rewritten-ast]
                 (if (:error res)
                   (cb res)
                   (if (:*load-macros* bound-vars)
                     (do
                       (when (:verbose opts) (debug-prn "Processing :use-macros for" name))
                       (load-macros bound-vars :use-macros use-macros name reload reloads opts
                         (fn [res]
                           (if (:error res)
                             (cb res)
                             (let [{:keys [require-macros] :as rewritten-ast} (rewrite-ns-ast rewritten-ast (:aliased-loads res) true)]
                               (when (:verbose opts) (debug-prn "Processing :require-macros for" (:name ast)))
                               (load-macros bound-vars :require-macros require-macros name reload reloads opts
                                 (fn [res']
                                   (if (:error res')
                                     (cb res')
                                     (let [{:keys [use-macros] :as rewritten-ast} (rewrite-ns-ast rewritten-ast (:aliased-loads res) true)
                                           res' (try
                                                  (when (seq use-macros)
                                                    (when (:verbose opts) (debug-prn "Checking :use-macros for" (:name ast)))
                                                    (binding [ana/*analyze-deps* (:*analyze-deps* bound-vars)
                                                              env/*compiler* (:*compiler* bound-vars)]
                                                      (ana/check-use-macros use-macros env)))
                                                  {:value nil}
                                                  (catch :default cause
                                                    (wrap-error
                                                      (ana/error ana-env
                                                        (str "Could not parse ns form " (:name ast)) cause))))]
                                       (if (:error res')
                                         (cb res')
                                         (try
                                           (binding [ana/*analyze-deps* (:*analyze-deps* bound-vars)
                                                     env/*compiler* (:*compiler* bound-vars)]
                                             (let [ast' (-> rewritten-ast
                                                          (ana/check-use-macros-inferring-missing env)
                                                          (ana/check-rename-macros-inferring-missing env)
                                                          (check-macro-autoload-inferring-missing env))]
                                               (cb {:value ast'})))
                                           (catch :default cause
                                             (cb (wrap-error
                                                   (ana/error ana-env
                                                     (str "Could not parse ns form " (:name ast)) cause)))))))))))))))
                     (try
                       (when (:verbose opts) (debug-prn "Checking uses"))
                       (ana/check-uses
                         (when (and (:*analyze-deps* bound-vars) (seq uses))
                           (ana/missing-uses uses env))
                         env)
                       (cb {:value ast})
                       (catch :default cause
                         (cb (wrap-error
                               (ana/error ana-env
                                 (str "Could not parse ns form " (:name ast)) cause)))))))))]
       (cond
         (and load (seq (:deps ast)))
         (let [{:keys [reload name deps]} ast]
           (load-deps bound-vars ana-env name deps (or (:require reload) (:use reload)) (dissoc opts :macros-ns)
             #(check-uses-and-load-macros % (rewrite-ns-ast ast (:aliased-loads %)))))

         (and (not load) (:*analyze-deps* bound-vars) (seq (:deps ast)))
         (analyze-deps bound-vars ana-env (:name ast) (:deps ast) (dissoc opts :macros-ns)
           #(check-uses-and-load-macros % (rewrite-ns-ast ast (:aliased-loads %))))

         :else
         (check-uses-and-load-macros {:value nil} ast)))
     (cb {:value ast}))))

(defn- node-side-effects
  [bound-vars sb deps ns-name emit-nil-result?]
  (doseq [dep deps]
    (.append sb
      (with-out-str
        (comp/emitln (munge ns-name) "."
          (ana/munge-node-lib dep)
          " = require('" dep "');"))))
  (when (and (seq deps) emit-nil-result?)
    (.append sb "null;")))

(defn- global-exports-side-effects
  [bound-vars sb deps ns-name emit-nil-result?]
  (let [{:keys [js-dependency-index]} @(:*compiler* bound-vars)]
    (doseq [dep deps]
      (let [{:keys [global-exports]} (get js-dependency-index (name dep))]
        (.append sb
          (with-out-str
            (comp/emit-global-export ns-name global-exports dep)))))
    (when (and (seq deps) emit-nil-result?)
      (.append sb "null;"))))

(defn- trampoline-safe
  "Returns a new function that calls f but discards any return value,
  returning nil instead, thus avoiding any inadvertent trampoline continuation
  if a function happens to be returned."
  [f]
  (comp (constantly nil) f))

(defn- analyze-str* [bound-vars source name opts cb]
  (let [rdr        (rt/indexing-push-back-reader source 1 name)
        cb         (trampoline-safe cb)
        eof        (js-obj)
        aenv       (ana/empty-env)
        the-ns     (or (:ns opts) 'cljs.user)
        bound-vars (cond-> (merge bound-vars {:*cljs-ns* the-ns})
                     (:source-map opts) (assoc :*sm-data* (sm-data)))]
    (trampoline
     (fn analyze-loop [last-ast ns]
       (binding [env/*compiler*         (:*compiler* bound-vars)
                 ana/*cljs-ns*          ns
                 ana/*checked-arrays*   (:checked-arrays opts)
                 ana/*cljs-static-fns*  (:static-fns opts)
                 ana/*fn-invoke-direct* (and (:static-fns opts) (:fn-invoke-direct opts))
                 *ns*                   (create-ns ns)
                 ana/*passes*           (:*passes* bound-vars)
                 r/*alias-map*          (alias-map @(:*compiler* bound-vars) ns)
                 r/*data-readers*       (:*data-readers* bound-vars)
                 r/resolve-symbol       resolve-symbol
                 comp/*source-map-data* (:*sm-data* bound-vars)
                 ana/*cljs-file*        (:cljs-file opts)]
         (let [res (try
                     {:value (read eof rdr)}
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
                       (if (#{:ns :ns*} (:op ast))
                         ((trampoline-safe ns-side-effects) bound-vars aenv ast opts
                           (fn [res]
                             (if (:error res)
                               (cb res)
                               (trampoline analyze-loop ast (:name ast)))))
                         #(analyze-loop ast ns)))))
                 (cb {:value last-ast}))))))) nil the-ns)))

(defn analyze-str
  "Analyze ClojureScript source. The compiler state will be populated with
   the results of analyzes. The parameters:

   state (atom)
     the compiler state

   source (string)
     the ClojureScript source

   name (symbol or string)
     optional, the name of the source

   opts (map)
     compilation options.

      :eval             - eval function to invoke, see *eval-fn*
      :load             - library resolution function, see *load-fn*
      :source-map       - set to true to generate inline source map information
      :def-emits-var    - sets whether def (and derived) forms return either a Var
                          (if set to true) or the def init value (if false).
                          Defaults to false.
      :checked-arrays   - if :warn or :error, checks inferred types and values passed
                          to aget/aset. Logs for incorrect values if :warn, throws if
                          :error. Defaults to false.
      :static-fns       - employ static dispatch to specific function arities in
                          emitted JavaScript, as opposed to making use of the
                          `call` construct. Defaults to false.
      :fn-invoke-direct - if `true`, does not generate `.call(null...)` calls for
                          unknown functions, but instead direct invokes via
                          `f(a0,a1...)`. Defaults to `false`.
      :target           - use `:nodejs` if targeting Node.js. Takes no other options
                          at the moment.
      :ns               - optional, the namespace in which to evaluate the source.
      :verbose          - optional, emit details from compiler activity. Defaults to
                          false.
      :context          - optional, sets the context for the source. Possible values
                          are `:expr`, `:statement` and `:return`. Defaults to
                          `:expr`.

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
      :*cljs-dep-set* ana/*cljs-dep-set*
      :*load-macros*  (:load-macros opts true)
      :*load-fn*      (or (:load opts) *load-fn*)
      :*eval-fn*      (or (:eval opts) *eval-fn*)}
     source name opts cb)))

;; -----------------------------------------------------------------------------
;; Eval

(declare ^{:arglists '([])} clear-fns!)

(defn- eval* [bound-vars form opts cb]
  (let [the-ns     (or (:ns opts) 'cljs.user)
        bound-vars (cond-> (merge bound-vars {:*cljs-ns* the-ns})
                     (:source-map opts) (assoc :*sm-data* (sm-data)))]
    (clear-fns!)
    (binding [env/*compiler*         (:*compiler* bound-vars)
              *eval-fn*              (:*eval-fn* bound-vars)
              ana/*cljs-ns*          (:*cljs-ns* bound-vars)
              ana/*checked-arrays*   (:checked-arrays opts)
              ana/*cljs-static-fns*  (:static-fns opts)
              ana/*fn-invoke-direct* (and (:static-fns opts) (:fn-invoke-direct opts))
              *ns*                   (create-ns (:*cljs-ns* bound-vars))
              r/*alias-map*          (alias-map @(:*compiler* bound-vars) (:*cljs-ns* bound-vars))
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
          (let [ast (:value res)
                [node-deps ast] (if (keyword-identical? (:target opts) :nodejs)
                                  (let [{node-libs true libs-to-load false} (group-by ana/node-module-dep? (:deps ast))]
                                    [node-libs (assoc ast :deps libs-to-load)])
                                  [nil ast])]
            (if (#{:ns :ns*} (:op ast))
              (ns-side-effects true bound-vars aenv ast opts
                (fn [res]
                  (if (:error res)
                    (cb res)
                    (let [ns-name (:name ast)
                          sb (StringBuffer.)]
                      (.append sb
                        (with-out-str (comp/emitln (str "goog.provide(\"" (comp/munge ns-name) "\");"))))
                      (when-not (nil? node-deps)
                        (node-side-effects bound-vars sb node-deps ns-name (:def-emits-var opts)))
                      (global-exports-side-effects bound-vars sb
                        (filter ana/dep-has-global-exports? (:deps ast))
                        ns-name
                        (:def-emits-var opts))
                      (cb {:value (*eval-fn* {:source (.toString sb)})})))))
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

      :eval             - eval function to invoke, see *eval-fn*
      :load             - library resolution function, see *load-fn*
      :source-map       - set to true to generate inline source map information
      :def-emits-var    - sets whether def (and derived) forms return either a Var
                          (if set to true) or the def init value (if false). Default
                          is false.
      :checked-arrays   - if :warn or :error, checks inferred types and values passed
                          to aget/aset. Logs for incorrect values if :warn, throws if
                          :error. Defaults to false.
      :static-fns       - employ static dispatch to specific function arities in
                          emitted JavaScript, as opposed to making use of the
                          `call` construct. Defaults to false.
      :fn-invoke-direct - if `true`, does not generate `.call(null...)` calls for
                          unknown functions, but instead direct invokes via
                          `f(a0,a1...)`. Defaults to `false`.
      :target           - use `:nodejs` if targeting Node.js. Takes no other options
                          at the moment.
      :ns               - optional, the namespace in which to evaluate the source.
      :verbose          - optional, emit details from compiler activity. Defaults to
                          false.
      :context          - optional, sets the context for the source. Possible values
                          are `:expr`, `:statement` and `:return`. Defaults to
                          `:expr`.

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
      :*cljs-dep-set* ana/*cljs-dep-set*
      :*load-macros*  (:load-macros opts true)
      :*load-fn*      (or (:load opts) *load-fn*)
      :*eval-fn*      (or (:eval opts) *eval-fn*)}
     form opts cb)))

;; -----------------------------------------------------------------------------
;; Compile

(defn- compile-str* [bound-vars source name opts cb]
  (let [rdr        (rt/indexing-push-back-reader source 1 name)
        cb         (trampoline-safe cb)
        eof        (js-obj)
        aenv       (ana/empty-env)
        sb         (StringBuffer.)
        the-ns     (or (:ns opts) 'cljs.user)
        bound-vars (cond-> (merge bound-vars {:*cljs-ns* the-ns})
                     (:source-map opts) (assoc :*sm-data* (sm-data)))]
    (trampoline
     (fn compile-loop [ns]
       (binding [env/*compiler*         (:*compiler* bound-vars)
                 *eval-fn*              (:*eval-fn* bound-vars)
                 ana/*cljs-ns*          ns
                 ana/*checked-arrays*   (:checked-arrays opts)
                 ana/*cljs-static-fns*  (:static-fns opts)
                 ana/*fn-invoke-direct* (and (:static-fns opts) (:fn-invoke-direct opts))
                 *ns*                   (create-ns ns)
                 r/*alias-map*          (alias-map @(:*compiler* bound-vars) ns)
                 r/*data-readers*       (:*data-readers* bound-vars)
                 r/resolve-symbol       resolve-symbol
                 comp/*source-map-data* (:*sm-data* bound-vars)]
         (let [res (try
                     {:value (read eof rdr)}
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
                     (let [ast (:value res)
                           [node-deps ast] (if (keyword-identical? (:target opts) :nodejs)
                                             (let [{node-libs true libs-to-load false} (group-by ana/node-module-dep? (:deps ast))]
                                               [node-libs (assoc ast :deps libs-to-load)])
                                             [nil ast])]
                       (if (#{:ns :ns*} (:op ast))
                         ((trampoline-safe ns-side-effects) bound-vars aenv ast opts
                           (fn [res]
                             (if (:error res)
                               (cb res)
                               (let [ns-name (:name ast)]
                                 (.append sb (with-out-str (comp/emit (:value res))))
                                 (when-not (nil? node-deps)
                                   (node-side-effects bound-vars sb node-deps ns-name (:def-emits-var opts)))
                                 (trampoline compile-loop (:name ast))))))
                         (do
                           (.append sb (with-out-str (comp/emit ast)))
                           #(compile-loop ns))))))
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

   name (symbol or string)
     optional, the name of the source - used as key in :source-maps

   opts (map)
     compilation options.

      :eval             - eval function to invoke, see *eval-fn*
      :load             - library resolution function, see *load-fn*
      :source-map       - set to true to generate inline source map information
      :def-emits-var    - sets whether def (and derived) forms return either a Var
                          (if set to true) or the def init value (if false). Default
                          is false.
      :checked-arrays   - if :warn or :error, checks inferred types and values passed
                          to aget/aset. Logs for incorrect values if :warn, throws if
                          :error. Defaults to false.
      :static-fns       - employ static dispatch to specific function arities in
                          emitted JavaScript, as opposed to making use of the
                          `call` construct. Defaults to false.
      :fn-invoke-direct - if `true`, does not generate `.call(null...)` calls for
                          unknown functions, but instead direct invokes via
                          `f(a0,a1...)`. Defaults to `false`.
      :target           - use `:nodejs` if targeting Node.js. Takes no other options
                          at the moment.
      :ns               - optional, the namespace in which to evaluate the source.
      :verbose          - optional, emit details from compiler activity. Defaults to
                          false.
      :context          - optional, sets the context for the source. Possible values
                          are `:expr`, `:statement` and `:return`. Defaults to
                          `:expr`.

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
      :*cljs-dep-set* ana/*cljs-dep-set*
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
        cb         (trampoline-safe cb)
        eof        (js-obj)
        aenv       (ana/empty-env)
        sb         (StringBuffer.)
        the-ns     (or (:ns opts) 'cljs.user)
        bound-vars (cond-> (merge bound-vars {:*cljs-ns* the-ns})
                     (:source-map opts) (assoc :*sm-data* (sm-data)))
        aname      (cond-> name (:macros-ns opts) ana/macro-ns-name)]
    (when (:verbose opts) (debug-prn "Evaluating" name))
    (clear-fns!)
    (trampoline
     (fn compile-loop [ns]
       (binding [env/*compiler*         (:*compiler* bound-vars)
                 *eval-fn*              (:*eval-fn* bound-vars)
                 ana/*cljs-ns*          ns
                 ana/*checked-arrays*   (:checked-arrays opts)
                 ana/*cljs-static-fns*  (:static-fns opts)
                 ana/*fn-invoke-direct* (and (:static-fns opts) (:fn-invoke-direct opts))
                 *ns*                   (create-ns ns)
                 r/*alias-map*          (alias-map @(:*compiler* bound-vars) ns)
                 r/*data-readers*       (:*data-readers* bound-vars)
                 r/resolve-symbol       resolve-symbol
                 comp/*source-map-data* (:*sm-data* bound-vars)
                 ana/*cljs-file*        (:cljs-file opts)]
         (let [res (try
                     {:value (read eof rdr)}
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
                           ns' ana/*cljs-ns*
                           [node-deps ast] (if (keyword-identical? (:target opts) :nodejs)
                                             (let [{node-libs true libs-to-load false} (group-by ana/node-module-dep? (:deps ast))]
                                               [node-libs (assoc ast :deps libs-to-load)])
                                             [nil ast])]
                      (if (#{:ns :ns*} (:op ast))
                        (do
                          (.append sb
                            (with-out-str (comp/emitln (str "goog.provide(\"" (comp/munge (:name ast)) "\");"))))
                          ((trampoline-safe ns-side-effects) true bound-vars aenv ast opts
                            (fn [res]
                              (if (:error res)
                                (cb res)
                                (let [ns-name (:name ast)]
                                  (when-not (nil? node-deps)
                                    (node-side-effects bound-vars sb node-deps ns-name (:def-emits-var opts)))
                                  (global-exports-side-effects bound-vars sb
                                    (filter ana/dep-has-global-exports? (:deps ast))
                                    ns-name
                                    (:def-emits-var opts))
                                  (trampoline compile-loop ns'))))))
                        (do
                          (env/with-compiler-env (assoc @(:*compiler* bound-vars) :options opts)
                            (.append sb (with-out-str (comp/emit ast))))
                          #(compile-loop ns'))))))
                 (do
                   (when (:source-map opts)
                     (append-source-map env/*compiler*
                       aname source sb @comp/*source-map-data* opts))
                   (when (symbol? aname)
                     (ana/dump-specs aname))
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
                       ((trampoline-safe f) evalm complete)
                       (complete {:value nil}))))))))))
      (:*cljs-ns* bound-vars))))

(defn eval-str
  "Evalute ClojureScript source given as a string. The parameters:

  state (atom)
    the compiler state

  source (string)
    the ClojureScript source

  name (symbol or string)
    optional, the name of the source - used as key in :source-maps

  opts (map)
    compilation options.

    :eval             - eval function to invoke, see *eval-fn*
    :load             - library resolution function, see *load-fn*
    :source-map       - set to true to generate inline source map information
    :cache-source     - optional, a function to run side-effects with the
                        compilation result prior to actual evalution. This function
                        takes two arguments, the first is the eval map, the source
                        will be under :source. The second argument is a callback of
                        one argument. If an error occurs an :error key should be
                        supplied.
    :def-emits-var    - sets whether def (and derived) forms return either a Var
                        (if set to true) or the def init value (if false). Default
                        is false.
    :checked-arrays   - if :warn or :error, checks inferred types and values passed
                        to aget/aset. Logs for incorrect values if :warn, throws if
                        :error. Defaults to false.
    :static-fns       - employ static dispatch to specific function arities in
                        emitted JavaScript, as opposed to making use of the
                        `call` construct. Defaults to false.
    :fn-invoke-direct - if `true`, does not generate `.call(null...)` calls for
                        unknown functions, but instead direct invokes via
                        `f(a0,a1...)`. Defaults to `false`.
    :target           - use `:nodejs` if targeting Node.js. Takes no other options
                        at the moment.
    :ns               - optional, the namespace in which to evaluate the source.
    :verbose          - optional, emit details from compiler activity. Defaults to
                        false.
    :context          - optional, sets the context for the source. Possible values
                     are `:expr`, `:statement` and `:return`. Defaults to
                      `:expr`.

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
      :*cljs-dep-set* ana/*cljs-dep-set*
      :*load-macros*  (:load-macros opts true)
      :*load-fn*      (or (:load opts) *load-fn*)
      :*eval-fn*      (or (:eval opts) *eval-fn*)}
     source name opts cb)))

;;; Support for cljs.core/eval

;; The following volatiles and fns set up a scheme to
;; emit function values into JavaScript as numeric
;; references that are looked up. Needed to implement eval.

(defonce ^:private fn-index (volatile! 0))
(defonce ^:private fn-refs (volatile! {}))

(defn- clear-fns!
  "Clears saved functions."
  []
  (vreset! fn-refs {}))

(defn- put-fn
  "Saves a function, returning a numeric representation."
  [f]
  (let [n (vswap! fn-index inc)]
    (vswap! fn-refs assoc n f)
    n))

(defn- get-fn
  "Gets a function, given its numeric representation."
  [n]
  (get @fn-refs n))

(defn- emit-fn [f]
  (print "cljs.js.get_fn(" (put-fn f) ")"))

(defmethod comp/emit-constant* js/Function
  [f]
  (emit-fn f))

(defmethod comp/emit-constant* cljs.core/Var
  [f]
  (emit-fn f))

(defn- eval-impl
  ([form]
   (eval-impl form (.-name *ns*)))
  ([form ns]
   (let [result (atom nil)]
     (let [st env/*compiler*]
       (eval st form
         {:ns            ns
          :context       :expr
          :def-emits-var true}
         (fn [{:keys [value error]}]
           (if error
             (throw error)
             (reset! result value)))))
     @result)))

(set! *eval* eval-impl)

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
