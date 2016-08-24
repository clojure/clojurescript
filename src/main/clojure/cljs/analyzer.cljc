;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.analyzer
  #?(:clj  (:refer-clojure :exclude [macroexpand-1 ensure])
     :cljs (:refer-clojure :exclude [macroexpand-1 ns-interns ensure js-reserved]))
  #?(:cljs (:require-macros
             [cljs.analyzer.macros
              :refer [no-warn wrapping-errors
                      disallowing-recur allowing-redef]]
             [cljs.env.macros :refer [ensure]]))
  #?(:clj (:require [cljs.util :as util :refer [ns->relpath topo-sort]]
                    [clojure.java.io :as io]
                    [clojure.string :as string]
                    [clojure.set :as set]
                    [cljs.env :as env :refer [ensure]]
                    [cljs.js-deps :as deps]
                    [cljs.tagged-literals :as tags]
                    [clojure.tools.reader :as reader]
                    [clojure.tools.reader.reader-types :as readers]
                    [clojure.edn :as edn])
     :cljs (:require [goog.string :as gstring]
                     [clojure.string :as string]
                     [clojure.set :as set]
                     [cljs.env :as env]
                     [cljs.tagged-literals :as tags]
                     [cljs.tools.reader :as reader]
                     [cljs.tools.reader.reader-types :as readers]
                     [cljs.reader :as edn]))
  #?(:clj (:import [java.io File Reader PushbackReader FileOutputStream FileInputStream]
                   [java.util.regex Pattern]
                   [java.net URL]
                   [java.lang Throwable]
                   [clojure.lang Namespace Var LazySeq ArityException]
                   [cljs.tagged_literals JSValue])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:dynamic *cljs-ns* 'cljs.user)
(def ^:dynamic *cljs-file* nil)
#?(:clj (def ^:dynamic *unchecked-if* false))
(def ^:dynamic *cljs-static-fns* false)
(def ^:dynamic *cljs-macros-path* "/cljs/core")
(def ^:dynamic *cljs-macros-is-classpath* true)
(def ^:dynamic *cljs-dep-set* (with-meta #{} {:dep-path []}))
(def ^:dynamic *analyze-deps* true)
(def ^:dynamic *load-tests* true)
(def ^:dynamic *load-macros* true)
(def ^:dynamic *reload-macros* false)
(def ^:dynamic *macro-infer* true)

(def ^:dynamic *file-defs* nil)

#?(:clj
   (def transit-read-opts
     (util/compile-if (import '[com.cognitect.transit ReadHandler])
       {:handlers
        {"cljs/js"
         (reify com.cognitect.transit.ReadHandler
           (fromRep [_ v] (JSValue. v)))
         "cljs/regex"
          (reify com.cognitect.transit.ReadHandler
           (fromRep [_ v] (Pattern/compile v)))}})))

#?(:clj
   (def transit-write-opts
     (util/compile-if (import '[com.cognitect.transit WriteHandler])
       {:handlers
        {JSValue
         (reify com.cognitect.transit.WriteHandler
           (tag [_ _] "cljs/js")
           (rep [_ js] (.val ^JSValue js))
           (stringRep [_ _] nil))
        Pattern
         (reify com.cognitect.transit.WriteHandler
           (tag [_ _] "cljs/regex")
           (rep [_ pat] (.pattern ^Pattern pat))
           (stringRep [_ _] nil))}})))

#?(:clj
   (def transit
     (delay
       (try
         (require '[cognitect.transit])
         (let [ns (find-ns 'cognitect.transit)]
           (when ns
             {:writer @(ns-resolve ns 'writer)
              :reader @(ns-resolve ns 'reader)
              :write  @(ns-resolve ns 'write)
              :read   @(ns-resolve ns 'read)}))
         (catch Throwable t
           nil)))))

;; log compiler activities
(def ^:dynamic *verbose* false)

(def -cljs-macros-loaded (atom false))

(def ^:dynamic *cljs-warnings*
  {:preamble-missing true
   :unprovided true
   :undeclared-var true
   :undeclared-ns true
   :undeclared-ns-form true
   :redef true
   :redef-in-file true
   :dynamic true
   :fn-var true
   :fn-arity true
   :fn-deprecated true
   :protocol-deprecated true
   :undeclared-protocol-symbol true
   :invalid-protocol-symbol true
   :multiple-variadic-overloads true
   :variadic-max-arity true
   :overload-arity true
   :extending-base-js-type true
   :invoke-ctor true
   :invalid-arithmetic true
   :protocol-invalid-method true
   :protocol-duped-method true
   :protocol-multiple-impls true
   :protocol-with-variadic-method true
   :single-segment-namespace true
   :munged-namespace true
   :ns-var-clash true
   :extend-type-invalid-method-shape true
   :unsupported-js-module-type true
   :unsupported-preprocess-value true})

(def js-reserved
  #{"arguments" "abstract" "boolean" "break" "byte" "case"
    "catch" "char" "class" "const" "continue"
    "debugger" "default" "delete" "do" "double"
    "else" "enum" "export" "extends" "final"
    "finally" "float" "for" "function" "goto" "if"
    "implements" "import" "in" "instanceof" "int"
    "interface" "let" "long" "native" "new"
    "package" "private" "protected" "public"
    "return" "short" "static" "super" "switch"
    "synchronized" "this" "throw" "throws"
    "transient" "try" "typeof" "var" "void"
    "volatile" "while" "with" "yield" "methods"
    "null" "constructor"})

#?(:clj (def SENTINEL (Object.))
   :cljs (def SENTINEL (js-obj)))

(defn gets
  ([m k0 k1]
    (let [m (get m k0 SENTINEL)]
      (when-not (identical? m SENTINEL)
        (get m k1))))
  ([m k0 k1 k2]
   (let [m (get m k0 SENTINEL)]
     (when-not (identical? m SENTINEL)
       (let [m (get m k1 SENTINEL)]
         (when-not (identical? m SENTINEL)
           (get m k2))))))
  ([m k0 k1 k2 k3]
   (let [m (get m k0 SENTINEL)]
     (when-not (identical? m SENTINEL)
       (let [m (get m k1 SENTINEL)]
         (when-not (identical? m SENTINEL)
           (let [m (get m k2 SENTINEL)]
             (when-not (identical? m SENTINEL)
               (get m k3)))))))))

#?(:cljs
   (def CLJ_NIL_SYM 'clj-nil))

#?(:cljs
   (def NUMBER_SYM 'number))

#?(:cljs
   (def STRING_SYM 'string))

(def BOOLEAN_SYM 'boolean)

#?(:cljs
   (def JS_STAR_SYM 'js*))

#?(:cljs
   (def DOT_SYM '.))

#?(:cljs
   (def NEW_SYM 'new))

#?(:cljs
   (def CLJS_CORE_SYM 'cljs.core))

#?(:cljs
   (def CLJS_CORE_MACROS_SYM 'cljs.core$macros))

(def IGNORE_SYM 'ignore)

(def ANY_SYM 'any)

#?(:cljs
   (defn ^boolean cljs-seq? [x]
     (implements? ISeq x)))

#?(:cljs
   (defn ^boolean cljs-map? [x]
     (implements? IMap x)))

#?(:cljs
   (defn ^boolean cljs-vector? [x]
     (implements? IVector x)))

#?(:cljs
   (defn ^boolean cljs-set? [x]
     (implements? ISet x)))

#?(:cljs
   (defn munge-path [ss]
     (munge (str ss))))

#?(:cljs
   (defn ns->relpath
     "Given a namespace as a symbol return the relative path. May optionally
     provide the file extension, defaults to :cljs."
     ([ns] (ns->relpath ns :cljs))
     ([ns ext]
      (str (string/replace (munge-path ns) \. \/) "." (name ext)))))

#?(:cljs
   (defn topo-sort
     ([x get-deps]
      (topo-sort x 0 (atom (sorted-map)) (memoize get-deps)))
     ([x depth state memo-get-deps]
      (let [deps (memo-get-deps x)]
        (swap! state update-in [depth] (fnil into #{}) deps)
        (doseq [dep deps]
          (topo-sort dep (inc depth) state memo-get-deps))
        (doseq [[<depth _] (subseq @state < depth)]
          (swap! state update-in [<depth] set/difference deps))
        (when (= depth 0)
          (distinct (apply concat (vals @state))))))))

(declare message namespaces)

(defn ast?
  #?(:cljs {:tag boolean})
  [x]
  (and (map? x) (contains? x :op)))

(defmulti error-message (fn [warning-type & _] warning-type))

(defmethod error-message :preamble-missing
  [warning-type info]
  (str "Preamble resource file not found: " (string/join " " (:missing info))))

(defmethod error-message :unprovided
  [warning-type info]
  (str "Required namespace not provided for " (string/join " " (:unprovided info))))

(defmethod error-message :undeclared-var
  [warning-type info]
  (str (if (:macro-present? info)
         "Can't take value of macro "
         "Use of undeclared Var ")
    (:prefix info) "/" (:suffix info)))

(defmethod error-message :undeclared-ns
  [warning-type {:keys [ns-sym js-provide] :as info}]
  (str "No such namespace: " ns-sym
       ", could not locate " (ns->relpath ns-sym :cljs)
       ", " (ns->relpath ns-sym :cljc)
       ", or Closure namespace \"" js-provide "\""))

(defmethod error-message :undeclared-macros-ns
  [warning-type {:keys [ns-sym js-provide] :as info}]
  (str "No such macros namespace: " ns-sym
       ", could not locate " (ns->relpath ns-sym :clj)
       " or " (ns->relpath ns-sym :cljc)))

(defmethod error-message :dynamic
  [warning-type info]
  (str (:name info) " not declared ^:dynamic"))

(defmethod error-message :redef
  [warning-type info]
  (str (:sym info) " already refers to: " (symbol (str (:ns info)) (str (:sym info)))
    " being replaced by: " (symbol (str (:ns-name info)) (str (:sym info)))))

(defmethod error-message :redef-in-file
  [warning-type info]
  (str (:sym info) " at line " (:line info) " is being replaced"))

(defmethod error-message :fn-var
  [warning-type info]
  (str (symbol (str (:ns-name info)) (str (:sym info)))
    " no longer fn, references are stale"))

(defmethod error-message :fn-arity
  [warning-type info]
  (str "Wrong number of args (" (:argc info) ") passed to "
    (or (:ctor info)
      (:name info))))

(defmethod error-message :fn-deprecated
  [warning-type info]
  (str (-> info :fexpr :info :name) " is deprecated."))

(defmethod error-message :undeclared-ns-form
  [warning-type info]
  (str "Invalid :refer, " (:type info) " " (:lib info) "/" (:sym info) " does not exist"))

(defmethod error-message :protocol-deprecated
  [warning-type info]
  (str "Protocol " (:protocol info) " is deprecated"))

(defmethod error-message :undeclared-protocol-symbol
  [warning-type info]
  (str "Can't resolve protocol symbol " (:protocol info)))

(defmethod error-message :invalid-protocol-symbol
  [warning-type info]
  (str "Symbol " (:protocol info) " is not a protocol"))

(defmethod error-message :protocol-invalid-method
  [warning-type info]
  (if (:no-such-method info)
    (str "Bad method signature in protocol implementation, "
      (:protocol info) " does not declare method called " (:fname info))
    (str "Bad method signature in protocol implementation, "
      (:protocol info) " " (:fname info) " does not declare arity " (:invalid-arity info))))

(defmethod error-message :protocol-duped-method
  [warning-type info]
  (str "Duplicated methods in protocol implementation " (:protocol info) " " (:fname info)))

(defmethod error-message :protocol-multiple-impls
  [warning-type info]
  (str "Protocol " (:protocol info) " implemented multiple times"))

(defmethod error-message :protocol-with-variadic-method
  [warning-type info]
  (str "Protocol " (:protocol info) " declares method "
       (:name info) " with variadic signature (&)"))

(defmethod error-message :multiple-variadic-overloads
  [warning-type info]
  (str (:name info) ": Can't have more than 1 variadic overload"))

(defmethod error-message :variadic-max-arity
  [warning-type info]
  (str (:name info) ": Can't have fixed arity function with more params than variadic function"))

(defmethod error-message :overload-arity
  [warning-type info]
  (str (:name info) ": Can't have 2 overloads with same arity"))

(defmethod error-message :extending-base-js-type
  [warning-type info]
  (str "Extending an existing JavaScript type - use a different symbol name "
       "instead of " (:current-symbol info) " e.g " (:suggested-symbol info)))

(defmethod error-message :invalid-arithmetic
  [warning-type info]
  (str (:js-op info) ", all arguments must be numbers, got " (:types info) " instead."))

(defmethod error-message :invoke-ctor
  [warning-type info]
  (str "Cannot invoke type constructor " (-> info :fexpr :info :name) " as function "))

(defmethod error-message :single-segment-namespace
  [warning-type info]
  (str (:name info) " is a single segment namespace"))

(defmethod error-message :munged-namespace
  [warning-type {:keys [name] :as info}]
  (let [munged (->> (string/split (clojure.core/name name) #"\.")
                 (map #(if (js-reserved %) (str % "$") %))
                 (string/join ".")
                 (munge))]
    (str "Namespace " name " contains a reserved JavaScript keyword,"
         " the corresponding Google Closure namespace will be munged to " munged)))

(defmethod error-message :ns-var-clash
  [warning-type {:keys [ns var] :as info}]
  (str "Namespace " ns " clashes with var " var))

(defmethod error-message :extend-type-invalid-method-shape
  [warning-type {:keys [protocol method] :as info}]
  (str "Bad extend-type method shape for protocol " protocol " method " method
       ", method arities must be grouped together"))

(defmethod error-message :unsupported-js-module-type
  [warning-type {:keys [module-type file] :as info}]
  (str "Unsupported JavaScript module type " module-type " for foreign library "
       file "."))

(defmethod error-message :unsupported-preprocess-value
  [warning-type {:keys [preprocess file]}]
  (str "Unsupported preprocess value " preprocess " for foreign library "
       file "."))

(defn default-warning-handler [warning-type env extra]
  (when (warning-type *cljs-warnings*)
    (when-let [s (error-message warning-type extra)]
      #?(:clj  (binding [*out* *err*]
                 (println (message env (str "WARNING: " s))))
         :cljs (binding [*print-fn* *print-err-fn*]
                 (println (message env (str "WARNING: " s))))))))

(def ^:dynamic *cljs-warning-handlers*
  [default-warning-handler])

#?(:clj
   (defmacro with-warning-handlers [handlers & body]
     `(binding [*cljs-warning-handlers* ~handlers]
        ~@body)))

(defn- repeat-char [c n]
  (loop [ret c n n]
    (if (pos? n)
      (recur (str ret c) (dec n))
      ret)))

(defn- hex-format [s pad]
  #?(:clj  (str "_u" (format (str "%0" pad "x") (int (first s))) "_")
     :cljs (let [hex (.toString (.charCodeAt s 0) 16)
                 len (. hex -length)
                 hex (if (< len pad)
                       (str (repeat-char "0" (- pad len)) hex)
                       hex)]
             (str "_u" hex "_"))))

(defn gen-constant-id [value]
  (let [prefix (cond
                 (keyword? value) "cst$kw$"
                 (symbol? value)  "cst$sym$"
                 :else
                 (throw
                   #?(:clj (Exception. (str "constant type " (type value) " not supported"))
                      :cljs (js/Error. (str "constant type " (type value) " not supported")))))
        name   (if (keyword? value)
                 (subs (str value) 1)
                 (str value))
        name   (if (= "." name)
                 "_DOT_"
                 (-> name
                     (string/replace "-" "_DASH_")
                     (munge)
                     (string/replace "." "$")
                     (string/replace #"(?i)[^a-z0-9$_]" #(hex-format % 4))))]
    (symbol (str prefix name))))

(defn- register-constant!
  ([val] (register-constant! nil val))
  ([env val]
   (swap! env/*compiler*
     (fn [cenv]
       (cond->
         (-> cenv
           (update-in [::constant-table]
             (fn [table]
               (if (get table val)
                 table
                 (assoc table val (gen-constant-id val))))))
         env (update-in [::namespaces (-> env :ns :name) ::constants]
               (fn [{:keys [seen order] :or {seen #{} order []} :as constants}]
                 (cond-> constants
                   (not (contains? seen val))
                   (assoc
                     :seen (conj seen val)
                     :order (conj order val))))))))))

(def default-namespaces '{cljs.core {:name cljs.core}
                          cljs.user {:name cljs.user}})

;; this exists solely to support read-only namespace access from macros.
;; External tools should look at the authoritative ::namespaces slot in the
;; compiler-env atoms/maps they're using already; this value will yield only
;; `default-namespaces` when accessed outside the scope of a
;; compilation/analysis call
(def namespaces
  #?(:clj
     (reify clojure.lang.IDeref
       (deref [_]
         (if-not (nil? env/*compiler*)
           (::namespaces @env/*compiler*)
           default-namespaces)))
     :cljs
     (reify IDeref
       (-deref [_]
         (if-not (nil? env/*compiler*)
           (::namespaces @env/*compiler*)
           default-namespaces)))))

(defn get-namespace
  ([key]
    (get-namespace env/*compiler* key))
  ([cenv key]
   (let [ns (get-in @cenv [::namespaces key])]
     (if-not (nil? ns)
       ns
       (when (= 'cljs.user key)
         {:name 'cljs.user})))))

#?(:clj
   (defmacro no-warn [& body]
     (let [no-warnings (zipmap (keys *cljs-warnings*) (repeat false))]
       `(binding [*cljs-warnings* ~no-warnings]
          ~@body))))

#?(:clj
   (defmacro all-warn [& body]
     (let [all-warnings (zipmap (keys *cljs-warnings*) (repeat true))]
       `(binding [*cljs-warnings* ~all-warnings]
          ~@body))))

(defn get-line [x env]
  (or (-> x meta :line) (:line env)))

(defn get-col [x env]
  (or (-> x meta :column) (:column env)))

(defn intern-macros
  "Given a Clojure namespace intern all macros into the ambient ClojureScript
   analysis environment."
  ([ns] (intern-macros ns false))
  ([ns reload]
    (when (or (nil? (get-in @env/*compiler* [::namespaces ns :macros]))
              reload)
      (swap! env/*compiler* assoc-in [::namespaces ns :macros]
        (->> #?(:clj (ns-interns ns) :cljs (ns-interns* ns))
             (filter (fn [[_ ^Var v]] (.isMacro v)))
             (map (fn [[k v]]
                    [k (as-> (meta v) vm
                         (let [ns (.getName ^Namespace (:ns vm))]
                           (assoc vm
                             :ns ns
                             :name (symbol (str ns) (str k))
                             :macro true)))]))
             (into {}))))))

#?(:clj
   (def load-mutex (Object.)))

#?(:clj
   (defn load-core []
     (when (not @-cljs-macros-loaded)
       (reset! -cljs-macros-loaded true)
       (if *cljs-macros-is-classpath*
         (locking load-mutex
           (load *cljs-macros-path*))
         (locking load-mutex
           (load-file *cljs-macros-path*))))
     (intern-macros 'cljs.core)))

#?(:clj
   (defmacro with-core-macros
     [path & body]
     `(do
        (when (not= *cljs-macros-path* ~path)
          (reset! -cljs-macros-loaded false))
        (binding [*cljs-macros-path* ~path]
          ~@body))))

#?(:clj
   (defmacro with-core-macros-file
     [path & body]
     `(do
        (when (not= *cljs-macros-path* ~path)
          (reset! -cljs-macros-loaded false))
        (binding [*cljs-macros-path* ~path
                  *cljs-macros-is-classpath* false]
          ~@body))))

(defn empty-env
  "Construct an empty analysis environment. Required to analyze forms."
  []
  (ensure
    {:ns (get-namespace *cljs-ns*)
     :context :statement
     :locals {}
     :fn-scope []
     :js-globals (into {}
                   (map #(vector % {:name %})
                     '(alert window document console escape unescape
                       screen location navigator history location
                       global process require module exports)))}))

(defn source-info
  ([env]
   (when (:line env)
     (source-info nil env)))
  ([name env]
   (cond-> {:file (if (= (-> env :ns :name) 'cljs.core)
                    "cljs/core.cljs"
                    *cljs-file*)
            :line (get-line name env)
            :column (get-col name env)}
     (:root-source-info env)
     (merge (select-keys env [:root-source-info])))))

(defn message [env s]
  (str s
    (if (:line env)
      (str " at line " (:line env) " " *cljs-file*)
      (when *cljs-file*
        (str " in file " *cljs-file*)))))

(defn warning [warning-type env extra]
  (doseq [handler *cljs-warning-handlers*]
    (handler warning-type env extra)))

(defn error
  ([env msg]
   (error env msg nil))
  ([env msg cause]
   (ex-info (message env msg)
     (assoc (source-info env) :tag :cljs/analysis-error)
     cause)))

(defn analysis-error?
  #?(:cljs {:tag boolean})
  [ex]
  (= :cljs/analysis-error (:tag (ex-data ex))))

#?(:clj
   (defmacro wrapping-errors [env & body]
     `(try
        ~@body
        (catch Throwable err#
          (if (analysis-error? err#)
            (throw err#)
            (throw (error ~env (.getMessage err#) err#)))))))

;; namespaces implicit to the inclusion of cljs.core
(def implicit-nses '#{goog goog.object goog.string goog.array Math})

(defn implicit-import?
  #?(:cljs {:tag boolean})
  [env prefix suffix]
  (contains? implicit-nses prefix))

(declare get-expander)

(defn confirm-var-exist-warning [env prefix suffix]
  (fn [env prefix suffix]
    (warning :undeclared-var env
      {:prefix         prefix
       :suffix         suffix
       :macro-present? (not (nil? (get-expander (symbol (str prefix) (str suffix)) env)))})))

(defn loaded-js-ns?
  "Check if a JavaScript namespace has been loaded. JavaScript vars are
  not currently checked."
  #?(:cljs {:tag boolean})
  [env prefix]
  (when-not (gets @env/*compiler* ::namespaces prefix)
    (let [ns (:ns env)]
      (if-not (nil? (get (:requires ns) prefix))
        true
        (if-not (nil? (get (:imports ns) prefix))
          true
          false)))))

(defn confirm-var-exists
  ([env prefix suffix]
   (let [warn (confirm-var-exist-warning env prefix suffix)]
     (confirm-var-exists env prefix suffix warn)))
  ([env prefix suffix missing-fn]
   (let [sufstr     (str suffix)
         suffix-str (if (and #?(:clj  (not= ".." sufstr)
                                :cljs (not (identical? ".." sufstr))) ;; leave cljs.core$macros/.. alone
                          #?(:clj  (re-find #"\." sufstr)
                             :cljs ^boolean (.test #"\." sufstr)))
                      (first (string/split sufstr #"\."))
                      suffix)
         suffix     (symbol suffix-str)]
     (when (and (not (implicit-import? env prefix suffix))
                (not (loaded-js-ns? env prefix))
                (not (and (= 'cljs.core prefix) (= 'unquote suffix)))
                (nil? (gets @env/*compiler* ::namespaces prefix :defs suffix)))
       (missing-fn env prefix suffix)))))

(defn confirm-var-exists-throw []
  (fn [env prefix suffix]
    (confirm-var-exists env prefix suffix
      (fn [env prefix suffix]
        (throw (error env (str "Unable to resolve var: " suffix " in this context")))))))

(defn resolve-ns-alias
  ([env name]
   (resolve-ns-alias env name (symbol name)))
  ([env name default]
   (let [sym (symbol name)]
     (get (:requires (:ns env)) sym default))))

(defn resolve-macro-ns-alias
  ([env name]
   (resolve-macro-ns-alias env name (symbol name)))
  ([env name default]
   (let [sym (symbol name)]
     (get (:require-macros (:ns env)) sym default))))

(defn confirm-ns
  "Given env, an analysis environment, and ns-sym, a symbol identifying a
   namespace, confirm that the namespace exists. Warn if not found."
  [env ns-sym]
  (when (and (not= 'cljs.core ns-sym)
             (nil? (get implicit-nses ns-sym))
             (nil? (get (-> env :ns :requires) ns-sym))
             ;; something else may have loaded the namespace, i.e. load-file
             (nil? (gets @env/*compiler* ::namespaces ns-sym))
             ;; macros may refer to namespaces never explicitly required
             ;; confirm that the library at least exists
             #?(:clj (nil? (util/ns->source ns-sym))))
    (warning :undeclared-ns env {:ns-sym ns-sym})))

(defn core-name?
  "Is sym visible from core in the current compilation namespace?"
  #?(:cljs {:tag boolean})
  [env sym]
  (and (or (gets @env/*compiler* ::namespaces 'cljs.core :defs sym)
           (when-let [mac (get-expander sym env)]
             (let [^Namespace ns (-> mac meta :ns)]
               (= (.getName ns) #?(:clj 'cljs.core :cljs 'cljs.core$macros)))))
       (not (contains? (-> env :ns :excludes) sym))))

(defn resolve-var
  "Resolve a var. Accepts a side-effecting confirm fn for producing
   warnings about unresolved vars."
  ([env sym] (resolve-var env sym nil))
  ([env sym confirm]
     (if #?(:clj  (= "js" (namespace sym))
            :cljs (identical? "js" (namespace sym)))
       {:name sym :ns 'js}
       (let [s    (str sym)
             lcls (:locals env)
             lb   (get lcls sym)]
         (cond
           (not (nil? lb)) lb

           (not (nil? (namespace sym)))
           (let [ns      (namespace sym)
                 ns      (if #?(:clj  (= "clojure.core" ns)
                                :cljs (identical? "clojure.core" ns))
                           "cljs.core"
                           ns)
                 full-ns (resolve-ns-alias env ns)]
             (when-not (nil? confirm)
               (when (not= (-> env :ns :name) full-ns)
                 (confirm-ns env full-ns))
               (confirm env full-ns (symbol (name sym))))
             (merge (gets @env/*compiler* ::namespaces full-ns :defs (symbol (name sym)))
               {:name (symbol (str full-ns) (str (name sym)))
                :ns full-ns}))

           #?(:clj  (and (.contains s ".")
                         (not (.contains s "..")))
              :cljs (and ^boolean (goog.string/contains s ".")
                         (not ^boolean (goog.string/contains s ".."))))
           (let [idx    (.indexOf s ".")
                 prefix (symbol (subs s 0 idx))
                 suffix (subs s (inc idx))
                 lb     (get lcls prefix)]
             (if-not (nil? lb)
               {:name (symbol (str (:name lb)) suffix)}
               (let [cur-ns  (-> env :ns :name)
                     full-ns (gets @env/*compiler* ::namespaces cur-ns :imports prefix)]
                 (if-not (nil? full-ns)
                   {:name (symbol (str full-ns) suffix)}
                   (let [info (gets @env/*compiler* ::namespaces cur-ns :defs prefix)]
                     (if-not (nil? info)
                       (merge info
                         {:name (symbol (str cur-ns) (str sym))
                          :ns cur-ns})
                       (merge (gets @env/*compiler* ::namespaces prefix :defs (symbol suffix))
                         {:name (if (= "" prefix) (symbol suffix) (symbol (str prefix) suffix))
                          :ns prefix})))))))

           (not (nil? (gets @env/*compiler* ::namespaces (-> env :ns :name) :uses sym)))
           (let [full-ns (gets @env/*compiler* ::namespaces (-> env :ns :name) :uses sym)]
             (merge
               (gets @env/*compiler* ::namespaces full-ns :defs sym)
               {:name (symbol (str full-ns) (str sym))
                :ns full-ns}))

           (not (nil? (gets @env/*compiler* ::namespaces (-> env :ns :name) :renames sym)))
           (let [qualified-symbol (gets @env/*compiler* ::namespaces (-> env :ns :name) :renames sym)
                 full-ns (symbol (namespace qualified-symbol))
                 sym     (symbol (name qualified-symbol))]
             (merge
               (gets @env/*compiler* ::namespaces full-ns :defs sym)
               {:name qualified-symbol
                :ns full-ns}))

           (not (nil? (gets @env/*compiler* ::namespaces (-> env :ns :name) :imports sym)))
           (recur env (gets @env/*compiler* ::namespaces (-> env :ns :name) :imports sym) confirm)

           :else
           (let [cur-ns (-> env :ns :name)
                 full-ns (cond
                           (not (nil? (gets @env/*compiler* ::namespaces cur-ns :defs sym))) cur-ns
                           (core-name? env sym) 'cljs.core
                           :else cur-ns)]
             (when-not (nil? confirm)
               (confirm env full-ns sym))
             (merge (gets @env/*compiler* ::namespaces full-ns :defs sym)
               {:name (symbol (str full-ns) (str sym))
                :ns full-ns})))))))

(defn resolve-existing-var
  "Given env, an analysis environment, and sym, a symbol, resolve an existing var.
   Emits a warning if no such var exists."
  [env sym]
  (if-not (-> sym meta ::no-resolve)
    (resolve-var env sym confirm-var-exists)
    (resolve-var env sym)))

(defn confirm-bindings
  "Given env, an analysis environment env, and names, a list of symbols, confirm
   that all correspond to declared dynamic vars."
  [env names]
  (doseq [name names]
    (let [env (assoc env :ns (get-namespace *cljs-ns*))
          ev (resolve-existing-var env name)]
      (when (and ev (not (-> ev :dynamic)))
        (warning :dynamic env {:ev ev :name (:name ev)})))))

(defn resolve-macro-var
  "Given env, an analysis environment, and sym, a symbol, resolve a macro."
  [env sym]
  (let [ns (-> env :ns :name)
        namespaces (get @env/*compiler* ::namespaces)]
    (cond
      (namespace sym)
      (let [ns (namespace sym)
            ns (if (= "clojure.core" ns) "cljs.core" ns)
            full-ns (resolve-macro-ns-alias env ns)]
        (get-in namespaces [full-ns :macros (symbol (name sym))]))

      (get-in namespaces [ns :use-macros sym])
      (let [full-ns (get-in namespaces [ns :use-macros sym])]
        (get-in namespaces [full-ns :macros sym]))

      (get-in namespaces [ns :rename-macros sym])
      (let [qualified-symbol (get-in namespaces [ns :rename-macros sym])
            full-ns (symbol (namespace qualified-symbol))
            sym     (symbol (name qualified-symbol))]
        (get-in namespaces [full-ns :macros sym]))

      :else
      (let [ns (cond
                 (get-in namespaces [ns :macros sym]) ns
                 (core-name? env sym) 'cljs.core)]
        (when ns
          (get-in namespaces [ns :macros sym]))))))

(declare analyze analyze-symbol analyze-seq)

(def specials '#{if def fn* do let* loop* letfn* throw try recur new set!
                 ns deftype* defrecord* . js* & quote case* var})

(def ^:dynamic *recur-frames* nil)
(def ^:dynamic *loop-lets* ())
(def ^:dynamic *allow-redef* false)

#?(:clj
   (defmacro disallowing-recur [& body]
     `(binding [*recur-frames* (cons nil *recur-frames*)] ~@body)))

#?(:clj
   (defmacro allowing-redef [& body]
     `(binding [*allow-redef* true] ~@body)))

;; TODO: move this logic out - David
(defn analyze-keyword
  [env sym]
  (register-constant! env sym)
  {:op :constant :env env :form sym :tag 'cljs.core/Keyword})

(defn get-tag [e]
  (let [tag (-> e :tag)]
    (if-not (nil? tag)
      tag
      (let [tag (-> e :info :tag)]
        (if-not (nil? tag)
          tag
          (-> e :form meta :tag))))))

(defn find-matching-method [f params]
  ;; if local fn, need to look in :info
  (let [methods (or (:methods f) (-> f :info :methods))
        c       (count params)]
    (some
      (fn [m]
        (and (or (== (:max-fixed-arity m) c)
                 (:variadic m))
             m))
      methods)))

(defn type?
  #?(:cljs {:tag boolean})
  [env t]
  ;; don't use resolve-existing-var to avoid warnings
  (when (and (not (nil? t)) (symbol? t))
    (let [var (resolve-var env t)
          type (:type var)]
      (if-not (nil? type)
        type
        (let [type (-> var :info :type)]
          (if-not (nil? type)
            type
            (let [proto (:protocol-symbol var)]
              (if-not (nil? proto)
                proto
                (get '#{cljs.core/PersistentHashMap cljs.core/List} t)))))))))

(declare infer-tag)

(def NOT_NATIVE '#{clj not-native})

(def BOOLEAN_OR_SEQ '#{boolean seq})

(defn infer-if [env e]
  (let [{{:keys [op form]} :test} e
        then-tag (infer-tag env (:then e))]
    (if (and #?(:clj (= op :constant)
                :cljs (keyword-identical? op :constant))
             (not (nil? form))
             (not (false? form)))
      then-tag
      (let [else-tag (infer-tag env (:else e))]
        (cond
          (or #?(:clj (= then-tag else-tag)
                 :cljs (symbol-identical? then-tag else-tag))
              #?(:clj (= else-tag IGNORE_SYM)
                 :cljs (symbol-identical? else-tag IGNORE_SYM))) then-tag
          #?(:clj (= then-tag IGNORE_SYM)
             :cljs (symbol-identical? then-tag IGNORE_SYM)) else-tag
          ;; TODO: temporary until we move not-native -> clj - David
          (and (or (not (nil? (get NOT_NATIVE then-tag))) (type? env then-tag))
               (or (not (nil? (get NOT_NATIVE else-tag))) (type? env else-tag)))
          'clj
          :else
          (if (and (not (nil? (get BOOLEAN_OR_SEQ then-tag)))
                   (not (nil? (get BOOLEAN_OR_SEQ else-tag))))
            'seq
            (let [then-tag (if #?(:clj (set? then-tag)
                                  :cljs (cljs-set? then-tag))
                             then-tag #{then-tag})
                  else-tag (if #?(:clj (set? else-tag)
                                  :cljs (cljs-set? else-tag))
                             else-tag #{else-tag})]
              (into then-tag else-tag))))))))

(defn infer-invoke [env e]
  (let [{info :info :as f} (:f e)
        ret-tag (when-not (nil? (:fn-var info)) (:ret-tag info))]
    (if-not (nil? ret-tag)
      ret-tag
      (let [args (:args e)
            me (assoc (find-matching-method f args) :op :method)
            ret-tag (infer-tag env me)]
        (if-not (nil? ret-tag)
          ret-tag
          ANY_SYM)))))

(defn infer-tag
  "Given env, an analysis environment, and e, an AST node, return the inferred
   type of the node"
  [env e]
  (let [tag (get-tag e)]
    (if-not (nil? tag)
      tag
      (case (:op e)
        :recur    IGNORE_SYM
        :throw    IGNORE_SYM
        :let      (infer-tag env (:expr e))
        :loop     (infer-tag env (:expr e))
        :do       (infer-tag env (:ret e))
        :method   (infer-tag env (:expr e))
        :def      (infer-tag env (:init e))
        :invoke   (infer-invoke env e)
        :if       (infer-if env e)
        :constant (case (:form e)
                    true BOOLEAN_SYM
                    false BOOLEAN_SYM
                    ANY_SYM)
        :var      (if-not (nil? (:init e))
                    (infer-tag env (:init e))
                    (infer-tag env (:info e)))
        :dot      ANY_SYM
        :js       ANY_SYM
        nil))))

(defmulti parse (fn [op & rest] op))

(defn- var-ast
  [env sym]
  (let [var (resolve-var env sym (confirm-var-exists-throw))
        expr-env (assoc env :context :expr)]
    (if-let [var-ns (:ns var)]
      {:var (analyze expr-env sym)
       :sym (analyze expr-env `(quote ~(symbol (name var-ns) (name (:name var)))))
       :meta (let [ks [:ns :doc :file :line :column]
                   m (merge
                       (let [user-meta (:meta var)
                             uks (keys user-meta)]
                         (zipmap uks
                           (map #(list 'quote (get user-meta %)) uks)))
                       (assoc (zipmap ks (map #(list 'quote (get var %)) ks))
                         :name `(quote ~(symbol (name (:name var))))
                         :test `(when ~sym (.-cljs$lang$test ~sym))
                         :arglists (let [arglists (:arglists var)
                                         arglists' (if (= 'quote (first arglists))
                                                     (second arglists)
                                                     arglists)]
                                    (list 'quote
                                      (doall (map with-meta arglists'
                                               (:arglists-meta var)))))))]
              (analyze expr-env m))})))

(defmethod parse 'var
  [op env [_ sym :as form] _ _]
  (merge
    {:env env
     :op :var-special
     :form form}
    (var-ast env sym)))

(defmethod parse 'if
  [op env [_ test then else :as form] name _]
  (when (< (count form) 3)
    (throw (error env "Too few arguments to if")))
  (when (> (count form) 4)
   (throw (error env "Too many arguments to if")))
  (let [test-expr (disallowing-recur (analyze (assoc env :context :expr) test))
        then-expr (allowing-redef (analyze env then))
        else-expr (allowing-redef (analyze env else))]
    {:env env :op :if :form form
     :test test-expr :then then-expr :else else-expr
     :unchecked *unchecked-if*
     :children [test-expr then-expr else-expr]}))

(defmethod parse 'case*
  [op env [_ sym tests thens default :as form] name _]
  (assert (symbol? sym) "case* must switch on symbol")
  (assert (every? vector? tests) "case* tests must be grouped in vectors")
  (let [expr-env (assoc env :context :expr)
        v        (disallowing-recur (analyze expr-env sym))
        tests    (mapv #(mapv (fn [t] (analyze expr-env t)) %) tests)
        thens    (mapv #(analyze env %) thens)
        default  (analyze env default)]
    (assert (every? (fn [t]
                      (or
                        (-> t :info :const)
                        (and (= :constant (:op t))
                             ((some-fn number? string? char?) (:form t)))))
              (apply concat tests))
      "case* tests must be numbers, strings, or constants")
    {:env env :op :case* :form form
     :v v :tests tests :thens thens :default default
     :children (vec (concat [v] tests thens (if default [default])))}))

(defmethod parse 'throw
  [op env [_ throw :as form] name _]
  (let [throw-expr (disallowing-recur (analyze (assoc env :context :expr) throw))]
    {:env env :op :throw :form form
     :throw throw-expr
     :children [throw-expr]}))

(defmethod parse 'try
  [op env [_ & body :as form] name _]
  (let [catchenv (update-in env [:context] #(if (= :expr %) :return %))
        catch? (every-pred seq? #(= (first %) 'catch))
        default? (every-pred catch? #(= (second %) :default))
        finally? (every-pred seq? #(= (first %) 'finally))

        {:keys [body cblocks dblock fblock]}
        (loop [parser {:state :start :forms body
                       :body [] :cblocks [] :dblock nil :fblock nil}]
          (if (seq? (:forms parser))
            (let [[form & forms*] (:forms parser)
                  parser* (assoc parser :forms forms*)]
              (case (:state parser)
                :start (cond
                         (catch? form) (recur (assoc parser :state :catches))
                         (finally? form) (recur (assoc parser :state :finally))
                         :else (recur (update-in parser* [:body] conj form)))
                :catches (cond
                           (default? form) (recur (assoc parser* :dblock form :state :finally))
                           (catch? form) (recur (update-in parser* [:cblocks] conj form))
                           (finally? form) (recur (assoc parser :state :finally))
                           :else (throw (error env "Invalid try form")))
                :finally (recur (assoc parser* :fblock form :state :done))
                :done (throw (error env "Unexpected form after finally"))))
            parser))

        finally (when (seq fblock)
                  (analyze (assoc env :context :statement) `(do ~@(rest fblock))))
        e (when (or (seq cblocks) dblock) (gensym "e"))
        default (if-let [[_ _ name & cb] dblock]
                  `(cljs.core/let [~name ~e] ~@cb)
                  `(throw ~e))
        cblock (if (seq cblocks)
                 `(cljs.core/cond
                   ~@(mapcat
                      (fn [[_ type name & cb]]
                        (when name (assert (not (namespace name)) "Can't qualify symbol in catch"))
                        `[(cljs.core/instance? ~type ~e)
                          (cljs.core/let [~name ~e] ~@cb)])
                      cblocks)
                   :else ~default)
                 default)
        locals (:locals catchenv)
        locals (if e
                 (assoc locals e
                        {:name e
                         :line (get-line e env)
                         :column (get-col e env)})
                 locals)
        catch (when cblock
                (analyze (assoc catchenv :locals locals) cblock))
        try (analyze (if (or e finally) catchenv env) `(do ~@body))]

    {:env env :op :try :form form
     :try try
     :finally finally
     :name e
     :catch catch
     :children [try catch finally]}))

(defn valid-proto [x]
  (when (symbol? x) x))

(defmethod parse 'def
  [op env form _ _]
  (let [pfn (fn
              ([_ sym] {:sym sym})
              ([_ sym init] {:sym sym :init init})
              ([_ sym doc init] {:sym sym :doc doc :init init}))
        args (apply pfn form)
        sym (:sym args)
        sym-meta (meta sym)
        tag (-> sym meta :tag)
        protocol (-> sym meta :protocol valid-proto)
        dynamic (-> sym meta :dynamic)
        ns-name (-> env :ns :name)
        locals (:locals env)
        clash-ns (symbol (str ns-name "." sym))
        sym-ns   (namespace sym)
        sym      (cond
                   (and sym-ns (not #?(:clj  (= (symbol sym-ns) ns-name)
                                       :cljs (symbol-identical? (symbol sym-ns) ns-name))))
                   (throw (error env (str "Can't def ns-qualified name in namespace " sym-ns)))

                   (some? sym-ns)
                   (symbol (name sym))

                   :else sym)]
    (when (get-in @env/*compiler* [::namespaces clash-ns])
      (warning :ns-var-clash env
        {:ns (symbol (str ns-name "." sym))
         :var (symbol (str ns-name) (str sym))}))
    (when (:const (resolve-var (dissoc env :locals) sym))
      (throw (error env "Can't redefine a constant")))
    (when-let [doc (:doc args)]
      (when-not (string? doc)
        (throw (error env "Too many arguments to def"))))
    (when-let [v (get-in @env/*compiler* [::namespaces ns-name :defs sym])]
      (when (and (not *allow-redef*)
                 (not (:declared v))
                 (not (:declared sym-meta))
                 *file-defs*
                 (get @*file-defs* sym))
        (warning :redef-in-file env {:sym sym :line (:line v)})))
    (when *file-defs*
      (swap! *file-defs* conj sym))
    (let [env (if (or (and (not= ns-name 'cljs.core)
                           (core-name? env sym))
                      (get-in @env/*compiler* [::namespaces ns-name :uses sym]))
                (let [ev (resolve-existing-var (dissoc env :locals) sym)]
                  (warning :redef env {:sym sym :ns (:ns ev) :ns-name ns-name})
                  (swap! env/*compiler* update-in [::namespaces ns-name :excludes] conj sym)
                  (update-in env [:ns :excludes] conj sym))
                env)
          var-name (:name (resolve-var (dissoc env :locals) sym))
          init-expr (when (contains? args :init)
                      (swap! env/*compiler* assoc-in [::namespaces ns-name :defs sym]
                        (merge
                          {:name var-name}
                          sym-meta
                          (when dynamic {:dynamic true})
                          (source-info var-name env)))
                      (disallowing-recur
                        (analyze (assoc env :context :expr) (:init args) sym)))
          fn-var? (and init-expr (= (:op init-expr) :fn))
          tag (if fn-var?
                (or (:ret-tag init-expr) tag)
                tag)
          export-as (when-let [export-val (-> sym meta :export)]
                      (if (= true export-val) var-name export-val))
          doc (or (:doc args) (-> sym meta :doc))]
      (when-let [v (get-in @env/*compiler* [::namespaces ns-name :defs sym])]
        (when (and (not (-> sym meta :declared))
                   (and (:fn-var v) (not fn-var?)))
          (warning :fn-var env {:ns-name ns-name :sym sym})))
      (swap! env/*compiler* assoc-in [::namespaces ns-name :defs sym]
        (merge
          {:name var-name}
          ;; remove actual test metadata, as it includes non-valid EDN and
          ;; cannot be present in analysis cached to disk - David
          (cond-> sym-meta
            (:test sym-meta) (assoc :test true))
          {:meta (-> sym-meta
                   (dissoc :test)
                   (update-in [:file]
                     (fn [f]
                       (if (= (-> env :ns :name) 'cljs.core)
                         "cljs/core.cljs"
                         f))))}
          (when doc {:doc doc})
          (when dynamic {:dynamic true})
          (source-info var-name env)
          ;; the protocol a protocol fn belongs to
          (when protocol
            {:protocol protocol})
          ;; symbol for reified protocol
          (when-let [protocol-symbol (-> sym meta :protocol-symbol)]
            {:protocol-symbol protocol-symbol
             :info (-> protocol-symbol meta :protocol-info)
             :impls #{}})
          (when fn-var?
            (let [params (map #(vec (map :name (:params %))) (:methods init-expr))]
              (merge
                {:fn-var true
                 ;; protocol implementation context
                 :protocol-impl (:protocol-impl init-expr)
                 ;; inline protocol implementation context
                 :protocol-inline (:protocol-inline init-expr)}
                (if-let [top-fn-meta (:top-fn sym-meta)]
                  top-fn-meta
                  {:variadic (:variadic init-expr)
                   :max-fixed-arity (:max-fixed-arity init-expr)
                   :method-params params
                   :arglists (:arglists sym-meta)
                   :arglists-meta (doall (map meta (:arglists sym-meta)))}))) )
          (when (and fn-var? tag)
            {:ret-tag tag})))
      (merge
        {:env env
         :op :def
         :form form
         :name var-name
         :var (assoc
                (analyze
                  (-> env (dissoc :locals)
                    (assoc :context :expr)
                    (assoc :def-var true))
                  sym)
                :op :var)
         :doc doc
         :jsdoc (:jsdoc sym-meta)
         :init init-expr}
        (when (:def-emits-var env)
          {:var-ast (var-ast env sym)})
        (when-let [test (:test sym-meta)]
          {:test (analyze (assoc env :context :expr) test)})
        (when tag
          (if fn-var?
            {:ret-tag tag}
            {:tag tag}))
        (when dynamic {:dynamic true})
        (when export-as {:export export-as})
        (when init-expr {:children [init-expr]})))))

(defn analyze-fn-method-param [env]
  (fn [[locals params] name]
    (when (namespace name)
      (throw (error env (str "Can't use qualified name as parameter: " name))))
    (let [line   (get-line name env)
          column (get-col name env)
          nmeta  (meta name)
          tag    (:tag nmeta)
          shadow (when-not (nil? locals)
                   (locals name))
          env    (merge (select-keys env [:context])
                   {:line line :column column})
          param  {:op :var
                  :name name
                  :line line
                  :column column
                  :tag tag
                  :shadow shadow
                  ;; Give the fn params the same shape
                  ;; as a :var, so it gets routed
                  ;; correctly in the compiler
                  :env env
                  :info {:name name :shadow shadow}
                  :binding-form? true}]
     [(assoc locals name param) (conj params param)])))

(defn analyze-fn-method-body [env form recur-frames]
  (binding [*recur-frames* recur-frames]
    (analyze env form)))

(defn- analyze-fn-method [env locals form type]
  (let [param-names     (first form)
        variadic        (boolean (some '#{&} param-names))
        param-names     (vec (remove '#{&} param-names))
        body            (next form)
        step            (analyze-fn-method-param env)
        step-init       [locals []]
        [locals params] (reduce step step-init param-names)
        params'         (if (true? variadic)
                          (butlast params)
                          params)
        fixed-arity     (count params')
        recur-frame     {:params params :flag (atom nil)}
        recur-frames    (cons recur-frame *recur-frames*)
        body-env        (assoc env :context :return :locals locals)
        body-form       `(do ~@body)
        expr            (analyze-fn-method-body body-env body-form recur-frames)
        recurs          @(:flag recur-frame)]
    {:env env
     :variadic variadic
     :params params
     :max-fixed-arity fixed-arity
     :type type
     :form form
     :expr expr
     :recurs recurs}))

(declare analyze-wrap-meta)

(defn fn-name-var [env locals name]
  (when-not (nil? name)
    (let [ns       (-> env :ns :name)
          shadow   (get locals name)
          shadow   (when (nil? shadow)
                     (get-in env [:js-globals name]))
          fn-scope (:fn-scope env)
          name-var {:name name
                    :info {:fn-self-name true
                           :fn-scope fn-scope
                           :ns ns
                           :shadow shadow}}
          tag      (-> name meta :tag)
          ret-tag  (when-not (nil? tag)
                     {:ret-tag tag})]
      (merge name-var ret-tag))))

(defn analyze-fn-methods-pass2* [menv locals type meths]
  (doall (map #(analyze-fn-method menv locals % type) meths)))

(defn analyze-fn-methods-pass2 [menv locals type meths]
  (no-warn (analyze-fn-methods-pass2* menv locals type meths)))

(defmethod parse 'fn*
  [op env [_ & args :as form] name _]
  (let [[name meths] (if (symbol? (first args))
                       [(first args) (next args)]
                       [name (seq args)])
        ;; turn (fn [] ...) into (fn ([]...))
        meths        (if (vector? (first meths))
                       (list meths)
                       meths)
        locals       (:locals env)
        name-var     (fn-name-var env locals name)
        env          (if-not (nil? name)
                       (update-in env [:fn-scope] conj name-var)
                       env)
        locals       (if (and (not (nil? locals))
                              (not (nil? name)))
                       (assoc locals name name-var)
                       locals)
        form-meta    (meta form)
        type         (::type form-meta)
        proto-impl   (::protocol-impl form-meta)
        proto-inline (::protocol-inline form-meta)
        menv         (if (> (count meths) 1)
                       (assoc env :context :expr)
                       env)
        menv         (merge menv
                       {:protocol-impl proto-impl
                        :protocol-inline proto-inline})
        methods      (map #(analyze-fn-method menv locals % type) meths)
        mfa          (apply max (map :max-fixed-arity methods))
        variadic     (boolean (some :variadic methods))
        locals       (if-not (nil? name)
                       (update-in locals [name] assoc
                         ;; TODO: can we simplify? - David
                         :fn-var true
                         :variadic variadic
                         :max-fixed-arity mfa
                         :method-params (map :params methods))
                       locals)
        methods      (if-not (nil? name)
                       ;; a second pass with knowledge of our function-ness/arity
                       ;; lets us optimize self calls
                       (analyze-fn-methods-pass2 menv locals type meths)
                       methods)
        form         (vary-meta form dissoc ::protocol-impl ::protocol-inline ::type)
        js-doc       (when (true? variadic)
                       "@param {...*} var_args")
        children     (mapv :expr methods)
        ast          {:op :fn
                      :env env
                      :form form
                      :name name-var
                      :methods methods
                      :variadic variadic
                      :tag 'function
                      :recur-frames *recur-frames*
                      :loop-lets *loop-lets*
                      :jsdoc [js-doc]
                      :max-fixed-arity mfa
                      :protocol-impl proto-impl
                      :protocol-inline proto-inline
                      :children children}]
    (let [variadic-methods (filter :variadic methods)
          variadic-params  (count (:params (first variadic-methods)))
          param-counts     (map (comp count :params) methods)]
      (when (< 1 (count variadic-methods))
        (warning :multiple-variadic-overloads env {:name name-var}))
      (when (not (or (zero? variadic-params) (== variadic-params (+ 1 mfa))))
        (warning :variadic-max-arity env {:name name-var}))
      (when (not= (distinct param-counts) param-counts)
        (warning :overload-arity env {:name name-var})))
    (analyze-wrap-meta ast)))

(defmethod parse 'letfn*
  [op env [_ bindings & exprs :as form] name _]
  (when-not (and (vector? bindings) (even? (count bindings)))
    (throw (error env "bindings must be vector of even number of elements")))
  (let [n->fexpr (into {} (map (juxt first second) (partition 2 bindings)))
        names    (keys n->fexpr)
        context  (:context env)
        ;; first pass to collect information for recursive references
        [meth-env bes]
        (reduce (fn [[{:keys [locals] :as env} bes] n]
                  (let [ret-tag (-> n meta :tag)
                        fexpr (no-warn (analyze env (n->fexpr n)))
                        be (cond->
                             {:name n
                              :fn-var true
                              :line (get-line n env)
                              :column (get-col n env)
                              :local true
                              :shadow (locals n)
                              :variadic (:variadic fexpr)
                              :max-fixed-arity (:max-fixed-arity fexpr)
                              :method-params (map :params (:methods fexpr))}
                             ret-tag (assoc :ret-tag ret-tag))]
                    [(assoc-in env [:locals n] be)
                     (conj bes be)]))
                [env []] names)
        meth-env (assoc meth-env :context :expr)
        ;; the real pass
        [meth-env bes]
        (reduce (fn [[meth-env bes] {:keys [name shadow] :as be}]
                  (let [env (assoc-in meth-env [:locals name] shadow)
                        fexpr (analyze env (n->fexpr name))
                        be' (assoc be
                              :init fexpr
                              :variadic (:variadic fexpr)
                              :max-fixed-arity (:max-fixed-arity fexpr)
                              :method-params (map :params (:methods fexpr)))]
                    [(assoc-in env [:locals name] be')
                     (conj bes be')]))
          [meth-env []] bes)
        expr (analyze (assoc meth-env :context (if (= :expr context) :return context)) `(do ~@exprs))]
    {:env env :op :letfn :bindings bes :expr expr :form form
     :children (conj (vec (map :init bes)) expr)}))

(defn analyze-do-statements* [env exprs]
  (seq (map #(analyze (assoc env :context :statement) %) (butlast exprs))))

(defn analyze-do-statements [env exprs]
  (disallowing-recur (analyze-do-statements* env exprs)))

(defmethod parse 'do
  [op env [_ & exprs :as form] _ _]
  (let [statements (analyze-do-statements env exprs)]
    (if (<= (count exprs) 1)
      (let [ret      (analyze env (first exprs))
            children (conj (vec statements) ret)]
        {:op :do
         :env env
         :form form
         :statements statements :ret ret
         :children children})
      (let [ret-env  (if (= :statement (:context env))
                       (assoc env :context :statement)
                       (assoc env :context :return))
            ret      (analyze ret-env (last exprs))
            children (conj (vec statements) ret)]
        {:op :do
         :env env
         :form form
         :statements statements
         :ret ret
         :children children}))))

(defn analyze-let-binding-init [env init loop-lets]
  (binding [*loop-lets* loop-lets]
    (analyze env init)))

(defn get-let-tag [name init-expr]
  (let [tag (-> name meta :tag)]
    (if-not (nil? tag)
      tag
      (let [tag (-> init-expr :tag)]
        (if-not (nil? tag)
          tag
          (-> init-expr :info :tag))))))

(defn analyze-let-bindings* [encl-env bindings]
  (loop [bes []
         env (assoc encl-env :context :expr)
         bindings (seq (partition 2 bindings))]
    (let [binding (first bindings)]
      (if-not (nil? binding)
        (let [[name init] binding]
          (when (or (not (nil? (namespace name)))
                  #?(:clj  (.contains (str name) ".")
                     :cljs ^boolean (goog.string/contains (str name) ".")))
            (throw (error encl-env (str "Invalid local name: " name))))
          (let [init-expr (analyze-let-binding-init env init (cons {:params bes} *loop-lets*))
                line (get-line name env)
                col (get-col name env)
                be {:name name
                    :line line
                    :column col
                    :init init-expr
                    :tag (get-let-tag name init-expr)
                    :local true
                    :shadow (-> env :locals name)
                    ;; Give let* bindings same shape as var so
                    ;; they get routed correctly in the compiler
                    :op :var
                    :env {:line line :column col}
                    :info {:name name
                           :shadow (-> env :locals name)}
                    :binding-form? true}
                be (if (= :fn (:op init-expr))
                     ;; TODO: can we simplify - David
                     (merge be
                       {:fn-var true
                        :variadic (:variadic init-expr)
                        :max-fixed-arity (:max-fixed-arity init-expr)
                        :method-params (map :params (:methods init-expr))})
                     be)]
            (recur (conj bes be)
              (assoc-in env [:locals name] be)
              (next bindings))))
        [bes env]))))

(defn analyze-let-bindings [encl-env bindings]
  (disallowing-recur (analyze-let-bindings* encl-env bindings)))

(defn analyze-let-body* [env context exprs]
  (analyze (assoc env :context (if (= :expr context) :return context)) `(do ~@exprs)))

(defn analyze-let-body [env context exprs recur-frames loop-lets]
  (binding [*recur-frames* recur-frames
            *loop-lets* loop-lets]
    (analyze-let-body* env context exprs)))

(defn analyze-let
  [encl-env [_ bindings & exprs :as form] is-loop]
  (when-not (and (vector? bindings) (even? (count bindings)))
    (throw (error encl-env "bindings must be vector of even number of elements")))
  (let [context      (:context encl-env)
        [bes env]    (analyze-let-bindings encl-env bindings)
        recur-frame  (when (true? is-loop)
                       {:params bes :flag (atom nil)})
        recur-frames (if recur-frame
                       (cons recur-frame *recur-frames*)
                       *recur-frames*)
        loop-lets    (cond
                       (true? is-loop) *loop-lets*
                       (not (nil? *loop-lets*)) (cons {:params bes} *loop-lets*))
        expr         (analyze-let-body env context exprs recur-frames loop-lets)
        op           (if (true? is-loop) :loop :let)
        children     (conj (vec (map :init bes)) expr)]
    {:op op
     :env encl-env
     :bindings bes
     :expr expr
     :form form
     :children children}))

(defmethod parse 'let*
  [op encl-env form _ _]
  (analyze-let encl-env form false))

(defmethod parse 'loop*
  [op encl-env form _ _]
  (analyze-let encl-env form true))

(defmethod parse 'recur
  [op env [_ & exprs :as form] _ _]
  (let [context (:context env)
        frame (first *recur-frames*)
        exprs (disallowing-recur (vec (map #(analyze (assoc env :context :expr) %) exprs)))]
    (when-not frame
      (throw (error env "Can't recur here")))
    (when-not (= (count exprs) (count (:params frame)))
      (throw (error env "recur argument count mismatch")))
    (reset! (:flag frame) true)
    (assoc {:env env :op :recur :form form}
      :frame frame
      :exprs exprs
      :children exprs)))

(defmethod parse 'quote
  [_ env [_ x] _ _]
  (analyze (assoc env :quoted? true) x))

(defmethod parse 'new
  [_ env [_ ctor & args :as form] _ _]
  (when-not (symbol? ctor)
    (throw (error env "First arg to new must be a symbol")))
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         ctorexpr (analyze enve ctor)
         ctor-var (resolve-existing-var env ctor)
         record-args
         (when (and (:record ctor-var) (not (-> ctor meta :internal-ctor)))
           (repeat 3 (analyze enve nil)))
         argexprs (into (vec (map #(analyze enve %) args)) record-args)
         known-num-fields (:num-fields ctor-var)
         argc (count args)]
     (when (and (not (-> ctor meta :internal-ctor))
                known-num-fields (not= known-num-fields argc))
       (warning :fn-arity env {:argc argc :ctor ctor}))
     {:env env :op :new :form form :ctor ctorexpr :args argexprs
      :children (into [ctorexpr] argexprs)
      :tag (let [name (-> ctorexpr :info :name)]
             (or ('{js/Object object
                    js/String string
                    js/Array  array
                    js/Number number
                    js/Function function
                    js/Boolean boolean} name)
                 name))})))

(defmethod parse 'set!
  [_ env [_ target val alt :as form] _ _]
  (let [[target val] (if alt
                       ;; (set! o -prop val)
                       [`(. ~target ~val) alt]
                       [target val])]
    (disallowing-recur
     (let [enve (assoc env :context :expr)
           targetexpr (cond
                       (and
                         (= target '*unchecked-if*) ;; TODO: proper resolve
                         (or (true? val) (false? val)))
                       (do
                         (set! *unchecked-if* val)
                         ::set-unchecked-if)

                       (symbol? target)
                       (do
                         (when (:const (resolve-var (dissoc env :locals) target))
                           (throw (error env "Can't set! a constant")))
                         (let [local (-> env :locals target)]
                           (when-not (or (nil? local)
                                         (and (:field local)
                                              (or (:mutable local)
                                                  (:unsynchronized-mutable local)
                                                  (:volatile-mutable local))))
                             (throw (error env "Can't set! local var or non-mutable field"))))
                         (analyze-symbol enve target))

                       :else
                       (when (seq? target)
                         (let [targetexpr (analyze-seq enve target nil)]
                           (when (:field targetexpr)
                             targetexpr))))
           valexpr (analyze enve val)]
       (when-not targetexpr
         (throw (error env "set! target must be a field or a symbol naming a var")))
       (cond
        (= targetexpr ::set-unchecked-if) {:env env :op :no-op}
        :else {:env env :op :set! :form form :target targetexpr :val valexpr
               :children [targetexpr valexpr]})))))

(declare analyze-file)

#?(:clj
   (defn locate-src
     "Given a namespace return the corresponding ClojureScript (.cljs or .cljc)
     resource on the classpath or file from the root of the build."
     [ns]
     (or (util/ns->source ns)
       ;; Find sources available in inputs given to cljs.closure/build - Juho Teperi
       (some (fn [source]
               (if (= ns (:ns source))
                 (:source-file source)))
             (:sources @env/*compiler*))
       ;; Find sources in directory given to cljs.compiler/compile-root - Juho Teperi
       (let [rootp (when-let [root (:root @env/*compiler*)]
                     (.getPath ^File root))
             cljsf (io/file rootp (ns->relpath ns :cljs))
             cljcf (io/file rootp (ns->relpath ns :cljc))]
         (if (and (.exists cljsf) (.isFile cljsf))
           cljsf
           (if (and (.exists cljcf) (.isFile cljcf))
             cljcf))))))

(defn foreign-dep?
  #?(:cljs {:tag boolean})
  [dep]
  {:pre [(symbol? dep)]}
  (let [js-index (:js-dependency-index @env/*compiler*)]
    (if-let [[_ {:keys [foreign]}] (find js-index (name dep))]
      foreign
      false)))

(defn analyze-deps
  "Given a lib, a namespace, deps, its dependencies, env, an analysis environment
   and opts, compiler options - analyze all of the dependencies. Required to
   correctly analyze usage of other namespaces."
  ([lib deps env] (analyze-deps lib deps env nil))
  ([lib deps env opts]
   (let [compiler @env/*compiler*]
     (binding [*cljs-dep-set* (vary-meta (conj *cljs-dep-set* lib) update-in [:dep-path] conj lib)]
       (assert (every? #(not (contains? *cljs-dep-set* %)) deps)
         (str "Circular dependency detected, "
           (apply str
             (interpose " -> "
               (conj (-> *cljs-dep-set* meta :dep-path)
                 (some *cljs-dep-set* deps))))))
       (doseq [dep deps]
         (when-not (or (not-empty (get-in compiler [::namespaces dep :defs]))
                       (contains? (:js-dependency-index compiler) (name dep))
                       #?(:clj (deps/find-classpath-lib dep)))
           #?(:clj (if-let [src (locate-src dep)]
                     (analyze-file src opts)
                     (throw
                       (error env
                         (error-message :undeclared-ns {:ns-sym dep :js-provide (name dep)}))))
              :cljs (throw
                      (error env
                        (error-message :undeclared-ns {:ns-sym dep :js-provide (name dep)}))))))))))

(defn missing-use? [lib sym cenv]
  (let [js-lib (get-in cenv [:js-dependency-index (name lib)])]
    (and (= (get-in cenv [::namespaces lib :defs sym] ::not-found) ::not-found)
         (not (= (get js-lib :group) :goog))
         (not (get js-lib :closure-lib)))))

(defn missing-rename? [sym cenv]
  (let [lib (symbol (namespace sym))
        sym (symbol (name sym))]
    (missing-use? lib sym cenv)))

(defn missing-use-macro? [lib sym]
  (let [the-ns #?(:clj (find-ns lib) :cljs (find-macros-ns lib))]
    (or (nil? the-ns) (nil? (.findInternedVar ^clojure.lang.Namespace the-ns sym)))))

(defn missing-rename-macro? [sym]
  (let [lib (symbol (namespace sym))
        sym (symbol (name sym))
        the-ns #?(:clj (find-ns lib) :cljs (find-macros-ns lib))]
    (or (nil? the-ns) (nil? (.findInternedVar ^clojure.lang.Namespace the-ns sym)))))

;; returns (s/map-of symbol? symbol?)
(defn missing-uses
  [uses env]
  (let [cenv @env/*compiler*]
    (into {} (filter (fn [[sym lib]] (missing-use? lib sym cenv)) uses))))

;; returns (s/map-of symbol? qualified-symbol?)
(defn missing-renames [renames env]
  (let [cenv @env/*compiler*]
    (into {} (filter (fn [[_ qualified-sym]] (missing-rename? qualified-sym cenv)) renames))))

;; returns (s/map-of symbol? symbol?)
(defn missing-use-macros [use-macros env]
  (let [cenv @env/*compiler*]
    (into {} (filter (fn [[sym lib]] (missing-use-macro? lib sym)) use-macros))))

;; returns (s/map-of symbol? symbol?)
(defn inferred-use-macros [use-macros env]
  (let [cenv @env/*compiler*]
    (into {} (filter (fn [[sym lib]] (not (missing-use-macro? lib sym))) use-macros))))

;; returns (s/map-of symbol? symbol?)
(defn inferred-rename-macros [rename-macros env]
  (into {} (filter (fn [[_ qualified-sym]] (not (missing-rename-macro? qualified-sym))) rename-macros)))

(defn check-uses [uses env]
  (let [cenv @env/*compiler*]
    (doseq [[sym lib] uses]
      (when (missing-use? lib sym cenv)
        (throw
          (error env
            (error-message :undeclared-ns-form {:type "var" :lib lib :sym sym})))))))

(defn check-use-macros
  ([use-macros env]
    (check-use-macros use-macros nil env))
  ([use-macros missing-uses env]
   (let [cenv @env/*compiler*]
     (doseq [[sym lib] use-macros]
       (when (missing-use-macro? lib sym)
         (throw
           (error env
             (error-message :undeclared-ns-form {:type "macro" :lib lib :sym sym})))))
     (check-uses (missing-use-macros missing-uses env) env)
     (inferred-use-macros missing-uses env))))

(defn check-use-macros-inferring-missing
  [{:keys [name uses use-macros] :as ast} env]
  (let [missing-uses        (when (and *analyze-deps* (seq uses))
                              (missing-uses uses env))
        maybe-macros        (apply dissoc uses (keys missing-uses))
        remove-missing-uses #(apply dissoc % (keys missing-uses))
        ast' (-> ast
               (update-in [:use-macros]
                 #(-> %
                   (merge (check-use-macros use-macros missing-uses env))
                   (merge (inferred-use-macros maybe-macros env))))
               (update-in [:uses] remove-missing-uses))]
    (swap! env/*compiler*
      #(-> %
        (update-in [::namespaces name :use-macros] merge (:use-macros ast'))
        (update-in [::namespaces name :uses] remove-missing-uses)))
    ast'))

(defn check-rename-macros-inferring-missing
  [{:keys [name renames] :as ast} env]
  (let [missing-renames        (when (and *analyze-deps* (seq renames))
                                 (missing-renames renames env))
        maybe-macros           (apply dissoc renames (keys missing-renames))
        missing-rename-macros  (inferred-rename-macros missing-renames env)
        remove-missing-renames #(apply dissoc % (keys missing-renames))
        ast' (-> ast
               (update-in [:rename-macros]
                 #(-> %
                   (merge missing-rename-macros)
                   (merge (inferred-rename-macros maybe-macros env))))
               (update-in [:renames] remove-missing-renames))]
    (swap! env/*compiler*
      #(-> %
        (update-in [::namespaces name :rename-macros] merge (:rename-macros ast'))
        (update-in [::namespaces name :renames] remove-missing-renames)))
    ast'))

(defn parse-ns-error-msg [spec msg]
  (str msg "; offending spec: " (pr-str spec)))

(defn basic-validate-ns-spec [env macros? spec]
  (when-not (or (symbol? spec) (sequential? spec))
    (throw
      (error env
        (parse-ns-error-msg spec
          "Only [lib.ns & options] and lib.ns specs supported in :require / :require-macros"))))
  (when (sequential? spec)
    (when-not (symbol? (first spec))
      (throw
        (error env
          (parse-ns-error-msg spec
            "Library name must be specified as a symbol in :require / :require-macros"))))
    (when-not (odd? (count spec))
      (throw
        (error env
          (parse-ns-error-msg spec
            "Only :as alias, :refer (names) and :rename {from to} options supported in :require"))))
    (when-not (every? #{:as :refer :rename} (map first (partition 2 (next spec))))
      (throw
        (error env
          (parse-ns-error-msg spec
            "Only :as, :refer and :rename options supported in :require / :require-macros"))))
    (when-not (let [fs (frequencies (next spec))]
                (and (<= (fs :as 0) 1)
                     (<= (fs :refer 0) 1)))
      (throw
        (error env
          (parse-ns-error-msg spec
            "Each of :as and :refer options may only be specified once in :require / :require-macros"))))))

(defn parse-ns-excludes [env args]
  (reduce
    (fn [s [k & filters]]
      (if (= k :refer-clojure)
        (do
          (when (seq (:excludes s))
            (throw (error env "Only one :refer-clojure form is allowed per namespace definition")))
          (let [valid-kws #{:exclude :rename}
                xs
                (loop [fs (seq filters)
                       ret {:excludes #{}
                            :renames {}}
                       err (not (even? (count filters)))]
                  (cond
                    (true? err)
                    (throw
                      (error env "Only [:refer-clojure :exclude (names)] and optionally `:rename {from to}` specs supported"))

                    (not (nil? fs))
                    (let [kw (first fs)]
                      (if (valid-kws kw)
                        (let [refs (second fs)]
                          (cond
                            (not (or (and (= kw :exclude) (sequential? refs) (every? symbol? refs))
                                   (and (= kw :rename) (map? refs) (every? #(every? symbol? %) refs))))
                            (recur fs ret true)

                            (= kw :exclude)
                            (recur (nnext fs) (update-in ret [:excludes] into refs) false)

                            (= kw :rename)
                            (recur (nnext fs) (update-in ret [:renames] merge refs) false)))
                        (recur fs ret true )))

                    :else ret))]
            (merge-with into s xs)))
        s))
    {:excludes #{}
     :renames {}} args))

(defn use->require [env [lib & filters :as spec]]
  (when-not (and (symbol? lib) (odd? (count spec)))
    (throw
      (error env
        (parse-ns-error-msg spec
          "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros"))))
  (loop [fs (seq filters) ret [lib] err false]
    (cond
      (true? err)
      (throw
        (error env
          (parse-ns-error-msg spec
            "Only [lib.ns :only (names)] and optionally `:rename {from to}` specs supported in :use / :use-macros")))

      (not (nil? fs))
      (let [kw (first fs)
            only? (= kw :only)]
        (if (or only? (= kw :rename))
          (if (some #{(if only? :refer kw)} ret)
            (throw
              (error env
                (parse-ns-error-msg spec
                  "Each of :only and :rename options may only be specified once in :use / :use-macros")))
            (let [refs (second fs)]
              (if-not (or (and only? (sequential? refs) (every? symbol? refs))
                          (and (= kw :rename) (map? refs) (every? #(every? symbol? %) refs)))
                (recur fs ret true)
                (recur (nnext fs) (into ret [(if only? :refer kw) refs]) false))))
          (recur fs ret true )))

      :else (if (some #{:refer} ret)
              ret
              (recur fs ret true)))))

(defn parse-require-spec [env macros? deps aliases spec]
  (if (symbol? spec)
    (recur env macros? deps aliases [spec])
    (do
      (basic-validate-ns-spec env macros? spec)
      (let [[lib & opts] spec
            lib (if-let [js-module-name (get-in @env/*compiler* [:js-module-index (name lib)])]
                  (symbol js-module-name)
                  lib)
            {alias :as referred :refer renamed :rename :or {alias lib}} (apply hash-map opts)
            referred-without-renamed (seq (remove (set (keys renamed)) referred))
            [rk uk renk] (if macros? [:require-macros :use-macros :rename-macros] [:require :use :rename])]
        (when-not (or (symbol? alias) (nil? alias))
          (throw
            (error env
              (parse-ns-error-msg spec
                ":as must be followed by a symbol in :require / :require-macros"))))
        (when alias
          (let [alias-type (if macros? :macros :fns)
                lib'       ((alias-type @aliases) alias)]
            (when (and (not (nil? lib')) (not= lib lib'))
              (throw (error env (parse-ns-error-msg spec ":as alias must be unique"))))
            (swap! aliases
              update-in [alias-type]
              conj [alias lib])))
        (when-not (or (and (sequential? referred)
                           (every? symbol? referred))
                      (nil? referred))
          (throw
            (error env
              (parse-ns-error-msg spec
                ":refer must be followed by a sequence of symbols in :require / :require-macros"))))
        (when-not macros?
          (swap! deps conj lib))
        (merge
          (when alias
            {rk (merge {alias lib} {lib lib})})
          (when referred-without-renamed {uk (apply hash-map (interleave referred-without-renamed (repeat lib)))})
          (when renamed
            {renk (reduce (fn [m [original renamed]]
                            (when-not (some #{original} referred)
                              (throw (error env
                                       (str "Renamed symbol " original " not referred"))))
                            (assoc m renamed (symbol (str lib) (str original))))
                    {} renamed)}))))))

(defn parse-import-spec [env deps spec]
  (when-not (or (and (sequential? spec)
                     (every? symbol? spec))
                (and (symbol? spec) (nil? (namespace spec))))
    (throw (error env (parse-ns-error-msg spec "Only lib.ns.Ctor or [lib.ns Ctor*] spec supported in :import"))))
  (let [import-map (if (sequential? spec)
                     (->> (rest spec)
                       (map #(vector % (symbol (str (first spec) "." %))))
                       (into {}))
                     {(symbol (last (string/split (str spec) #"\."))) spec})]
    (doseq [[_ spec] import-map]
      (swap! deps conj spec))
    {:import  import-map
     :require import-map}))

(declare parse-ns)

(defn macro-autoload-ns?
  "Given a spec form check whether the spec namespace requires a macro file
   of the same name. If so return true."
  #?(:cljs {:tag boolean})
  [form]
  (when *macro-infer*
    (let [ns (if (sequential? form) (first form) form)
         {:keys [use-macros require-macros]}
         (or (get-in @env/*compiler* [::namespaces ns])
             #?(:clj
                (when-let [res (util/ns->source ns)]
                  (:ast (parse-ns res)))))]
      (or (some #{ns} (vals use-macros))
          (some #{ns} (vals require-macros))))))

(defn clj-ns->cljs-ns
  "Given a symbol that starts with clojure as the first segment return the
   same symbol with the first segment replaced with cljs"
  [sym]
  (let [segs (string/split (clojure.core/name sym) #"\.")]
    (if (= "clojure" (first segs))
      (symbol (string/join "." (cons "cljs" (next segs))))
      sym)))

#?(:clj
   (defn aliasable-clj-ns?
     "Predicate for testing with a symbol represents an aliasable clojure namespace."
     [sym]
     (when-not (util/ns->source sym)
       (let [[seg1 :as segs] (string/split (clojure.core/name sym) #"\.")]
         (when (= "clojure" seg1)
           (let [sym' (clj-ns->cljs-ns sym)]
             (util/ns->source sym')))))))

#?(:clj
   (defn rewrite-cljs-aliases
     "Alias non-existing clojure.* namespaces to existing cljs.* namespaces if
      possible."
     [args]
     (letfn [(process-spec [maybe-spec]
               (let [[lib & xs] (if (sequential? maybe-spec)
                                  maybe-spec
                                  [maybe-spec])]
                 (if (and (symbol? lib) (aliasable-clj-ns? lib))
                   (let [lib' (clj-ns->cljs-ns lib)
                         spec (cons lib' xs)]
                     (into (if xs [spec] []) [(list lib' :as lib)]))
                   [maybe-spec])))
             (process-form [[k & specs :as form]]
               (if (#{:use :require} k)
                 (cons k (mapcat process-spec specs))
                 form))]
       (map process-form args))))

(defn desugar-ns-specs
  "Given an original set of ns specs desugar :include-macros and :refer-macros
   usage into only primitive spec forms - :use, :require, :use-macros,
   :require-macros. If a library includes a macro file of with the same name
   as the namespace will also be desugared."
  [args]
  (let [{:keys [require] :as indexed}
        (->> args
          (map (fn [[k & specs]] [k (into [] specs)]))
          (into {}))
        sugar-keys #{:include-macros :refer-macros}
        ;; drop spec k and value from spec for generated :require-macros
        remove-from-spec
        (fn [pred spec]
          (if-not (and (sequential? spec) (some pred spec))
            spec
            (let [[l r] (split-with (complement pred) spec)]
              (recur pred (concat l (drop 2 r))))))
        ;; rewrite :refer-macros to :refer for generated :require-macros
        replace-refer-macros
        (fn [spec]
          (if-not (sequential? spec)
            spec
            (map (fn [x] (if (= x :refer-macros) :refer x)) spec)))
        reload-spec? #(#{:reload :reload-all} %)
        to-macro-specs
        (fn [specs]
          (->> specs
            (filter
              (fn [x]
                (or (and (sequential? x)
                         (some sugar-keys x))
                    (reload-spec? x)
                    (macro-autoload-ns? x))))
            (map (fn [x]
                   (if-not (reload-spec? x)
                     (->> x (remove-from-spec #{:include-macros})
                            (remove-from-spec #{:refer})
                            (remove-from-spec #{:rename})
                            (replace-refer-macros))
                     x)))))
        remove-sugar (partial remove-from-spec sugar-keys)]
    (if-let [require-specs (seq (to-macro-specs require))]
      (map (fn [x]
             (if-not (reload-spec? x)
               (let [[k v] x]
                 (cons k (map remove-sugar v)))
               x))
        (update-in indexed [:require-macros] (fnil into []) require-specs))
      args)))

(defn find-def-clash [env ns segments]
  (let [to-check (map (fn [xs]
                        [(symbol (string/join "." (butlast xs)))
                         (symbol (last xs))])
                   (drop 2 (reductions conj [] segments)))]
    (doseq [[clash-ns name] to-check]
      (when (get-in @env/*compiler* [::namespaces clash-ns :defs name])
        (warning :ns-var-clash env
          {:ns ns
           :var (symbol (str clash-ns) (str name))})))))

(defn macro-ns-name [name]
  (let [name-str (str name)]
    (if-not #?(:clj  (.endsWith name-str "$macros")
               :cljs (gstring/endsWith name-str "$macros"))
      (symbol (str name-str "$macros"))
      name)))

(defmethod parse 'ns
  [_ env [_ name & args :as form] _ opts]
  (when-not (symbol? name)
    (throw (error env "Namespaces must be named by a symbol.")))
  (let [name (cond-> name (:macros-ns opts) macro-ns-name)]
    (let [segments (string/split (clojure.core/name name) #"\.")]
      (when (= 1 (count segments))
        (warning :single-segment-namespace env {:name name}))
      (when (some js-reserved segments)
        (warning :munged-namespace env {:name name}))
      (find-def-clash env name segments)
      #?(:clj
         (when (some (complement util/valid-js-id-start?) segments)
           (throw
             (AssertionError.
               (str "Namespace " name " has a segment starting with an invaild "
                    "JavaScript identifier"))))))
    (let [docstring    (if (string? (first args)) (first args))
          mdocstr      (-> name meta :doc)
          args         (if docstring (next args) args)
          metadata     (if (map? (first args)) (first args))
          form-meta    (meta form)
          args         (desugar-ns-specs
                         #?(:clj  (rewrite-cljs-aliases
                                    (if metadata (next args) args))
                            :cljs (if metadata (next args) args)))
          name         (vary-meta name merge metadata)
          {excludes :excludes core-renames :renames} (parse-ns-excludes env args)
          core-renames (reduce (fn [m [original renamed]]
                                 (assoc m renamed (symbol "cljs.core" (str original))))
                         {} core-renames)
          deps         (atom #{})
          aliases      (atom {:fns {} :macros {}})
          spec-parsers {:require        (partial parse-require-spec env false deps aliases)
                        :require-macros (partial parse-require-spec env true deps aliases)
                        :use            (comp (partial parse-require-spec env false deps aliases)
                                          (partial use->require env))
                        :use-macros     (comp (partial parse-require-spec env true deps aliases)
                                          (partial use->require env))
                        :import         (partial parse-import-spec env deps)}
          valid-forms  (atom #{:use :use-macros :require :require-macros :import})
          reload       (atom {:use nil :require nil :use-macros nil :require-macros nil})
          reloads      (atom {})
          {uses :use requires :require renames :rename
           use-macros :use-macros require-macros :require-macros
           rename-macros :rename-macros imports :import :as params}
          (reduce
            (fn [m [k & libs]]
              (when-not (#{:use :use-macros :require :require-macros :import} k)
                (throw (error env "Only :refer-clojure, :require, :require-macros, :use, :use-macros, and :import libspecs supported")))
              (when-not (@valid-forms k)
                (throw (error env (str "Only one " k " form is allowed per namespace definition"))))
              (swap! valid-forms disj k)
              ;; check for spec type reloads
              (when-not (= :import k)
                (when (some #{:reload} libs)
                  (swap! reload assoc k :reload))
                (when (some #{:reload-all} libs)
                  (swap! reload assoc k :reload-all)))
              ;; check for individual ns reloads from REPL interactions
              (when-let [xs (seq (filter #(-> % meta :reload) libs))]
                (swap! reloads assoc k
                  (zipmap (map first xs) (map #(-> % meta :reload) xs))))
              (apply merge-with merge m
                (map (spec-parsers k)
                  (remove #{:reload :reload-all} libs))))
            {} (remove (fn [[r]] (= r :refer-clojure)) args))]
      (set! *cljs-ns* name)
      (let [ns-info
            {:name           name
             :doc            (or docstring mdocstr)
             :excludes       excludes
             :use-macros     use-macros
             :require-macros require-macros
             :rename-macros  rename-macros
             :uses           uses
             :requires       requires
             :renames        (merge renames core-renames)
             :imports        imports}
            ns-info
            (if (:merge form-meta)
              ;; for merging information in via require usage in REPLs
              (let [ns-info' (get-in @env/*compiler* [::namespaces name])]
                (if (pos? (count ns-info'))
                  (let [merge-keys
                        [:use-macros :require-macros :rename-macros
                         :uses :requires :renames :imports]]
                    (merge
                      ns-info'
                      (merge-with merge
                        (select-keys ns-info' merge-keys)
                        (select-keys ns-info merge-keys))))
                  ns-info))
              ns-info)]
        (swap! env/*compiler* update-in [::namespaces name] merge ns-info)
        (merge {:op      :ns
                :env     env
                :form    form
                :deps    @deps
                :reload  @reload
                :reloads @reloads}
          (cond-> ns-info
            (@reload :use)
            (update-in [:uses]
              (fn [m] (with-meta m {(@reload :use) true})))
            (@reload :require)
            (update-in [:requires]
              (fn [m] (with-meta m {(@reload :require) true})))))))))

(defn parse-type
  [op env [_ tsym fields pmasks body :as form]]
  (let [t (:name (resolve-var (dissoc env :locals) tsym))
        locals (reduce (fn [m fld]
                         (assoc m fld
                                {:name fld
                                 :line (get-line fld env)
                                 :column (get-col fld env)
                                 :field true
                                 :mutable (-> fld meta :mutable)
                                 :unsynchronized-mutable (-> fld meta :unsynchronized-mutable)
                                 :volatile-mutable (-> fld meta :volatile-mutable)
                                 :tag (-> fld meta :tag)
                                 :shadow (m fld)}))
                       {} (if (= :defrecord* op)
                            (concat fields '[__meta __extmap ^:mutable __hash])
                            fields))
        protocols (-> tsym meta :protocols)]
    (swap! env/*compiler* update-in [::namespaces (-> env :ns :name) :defs tsym]
           (fn [m]
             (let [m (assoc (or m {})
                       :name t
                       :type true
                       :num-fields (count fields)
                       :record (= :defrecord* op))]
               (merge m
                      (dissoc (meta tsym) :protocols)
                      {:protocols protocols}
                      (source-info tsym env)))))
    {:op op :env env :form form :t t :fields fields :pmasks pmasks
     :protocols (disj protocols 'cljs.core/Object)
     :body (analyze (assoc env :locals locals) body)}))

(defmethod parse 'deftype*
  [_ env form _ _]
  (parse-type :deftype* env form))

(defmethod parse 'defrecord*
  [_ env form _ _]
  (parse-type :defrecord* env form) )

;; dot accessor code

(def ^:private property-symbol? #(boolean (and (symbol? %) (re-matches #"^-.*" (name %)))))

(defn- classify-dot-form
  [[target member args]]
  [(cond (nil? target) ::error
         :default      ::expr)
   (cond (property-symbol? member) ::property
         (symbol? member)          ::symbol
         (seq? member)             ::list
         :default                  ::error)
   (cond (nil? args) ()
         :default    ::expr)])

(defmulti build-dot-form #(classify-dot-form %))

;; (. o -p)
;; (. (...) -p)
(defmethod build-dot-form [::expr ::property ()]
  [[target prop _]]
  {:dot-action ::access :target target :field (-> prop name (.substring 1) symbol)})

;; (. o -p <args>)
(defmethod build-dot-form [::expr ::property ::list]
  [[target prop args]]
  #?(:clj  (throw (Error. (str "Cannot provide arguments " args " on property access " prop)))
     :cljs (throw (js/Error. (str "Cannot provide arguments " args " on property access " prop)))))

(defn- build-method-call
  "Builds the intermediate method call map used to reason about the parsed form during
  compilation."
  [target meth args]
  (if (symbol? meth)
    {:dot-action ::call :target target :method meth :args args}
    {:dot-action ::call :target target :method (first meth) :args args}))

;; (. o m 1 2)
(defmethod build-dot-form [::expr ::symbol ::expr]
  [[target meth args]]
  (build-method-call target meth args))

;; (. o m)
(defmethod build-dot-form [::expr ::symbol ()]
  [[target meth args]]
  (build-method-call target meth args))

;; (. o (m))
;; (. o (m 1 2))
(defmethod build-dot-form [::expr ::list ()]
  [[target meth-expr _]]
  (build-method-call target (first meth-expr) (rest meth-expr)))

(defmethod build-dot-form :default
  [dot-form]
  #?(:clj  (throw
             (Error.
               (str "Unknown dot form of "
                 (list* '. dot-form) " with classification "
                 (classify-dot-form dot-form))))
     :cljs (throw
             (js/Error.
               (str "Unknown dot form of "
                 (list* '. dot-form) " with classification "
                 (classify-dot-form dot-form))))))

(defn analyze-dot [env target field member+ form]
  (let [v [target field member+]
        {:keys [dot-action target method field args]} (build-dot-form v)
        enve       (assoc env :context :expr)
        targetexpr (analyze enve target)
        form-meta  (meta form)
        tag        (:tag form-meta)]
    (case dot-action
      ::access (let [children [targetexpr]]
                 {:op :dot
                  :env env
                  :form form
                  :target targetexpr
                  :field field
                  :children children
                  :tag tag})
      ::call   (let [argexprs (map #(analyze enve %) args)
                     children (into [targetexpr] argexprs)]
                 {:op :dot
                  :env env
                  :form form
                  :target targetexpr
                  :method method
                  :args argexprs
                  :children children
                  :tag tag}))))

(defmethod parse '.
  [_ env [_ target & [field & member+] :as form] _ _]
  (disallowing-recur (analyze-dot env target field member+ form)))

(defn get-js-tag [form]
  (let [form-meta (meta form)
        tag       (:tag form-meta)]
    (if-not (nil? tag)
      tag
      (when (true? (:numeric form-meta))
        'number))))

(defn js-star-interp
  [env ^String s]
  (let [idx (.indexOf s "~{")]
    (if (== -1 idx)
      (list s)
      (let [end (.indexOf s "}" idx)
            inner (:name (resolve-existing-var env (symbol (subs s (+ 2 idx) end))))]
        (lazy-seq
          (cons (subs s 0 idx)
            (cons inner
              (js-star-interp env (subs s (inc end))))))))))

(defn js-star-seg
  [^String s]
  (let [idx (.indexOf s "~{")]
    (if (== -1 idx)
      (list s)
      (let [end (.indexOf s "}" idx)]
        (lazy-seq
          (cons (subs s 0 idx)
            (js-star-seg (subs s (inc end)))))))))

(def NUMERIC_SET '#{any number long double})

(defn numeric-type?
  #?(:cljs {:tag boolean})
  [t]
  ;; TODO: type inference is not strong enough to detect that
  ;; when functions like first won't return nil, so variadic
  ;; numeric functions like cljs.core/< would produce a spurious
  ;; warning without this - David
  (if (nil? t)
    true
    (if (and (symbol? t) (not (nil? (get NUMERIC_SET t))))
      true
      (when #?(:clj  (set? t)
               :cljs (cljs-set? t))
        (or (contains? t 'number)
            (contains? t 'long)
            (contains? t 'double)
            (contains? t 'any))))))

(defn analyze-js-star* [env jsform args form]
  (let [enve      (assoc env :context :expr)
        argexprs  (vec (map #(analyze enve %) args))
        form-meta (meta form)
        segs      (js-star-seg jsform)
        tag       (get-js-tag form)
        js-op     (:js-op form-meta)
        numeric   (:numeric form-meta)]
    (when (true? numeric)
      (let [types (map #(infer-tag env %) argexprs)]
        (when-not (every? numeric-type? types)
          (warning :invalid-arithmetic env
            {:js-op js-op
             :types (into [] types)}))))
    {:op :js
     :env env
     :segs segs
     :args argexprs
     :tag tag
     :form form
     :children argexprs
     :js-op js-op
     :numeric numeric}))

(defn analyze-js-star [env jsform args form]
  (disallowing-recur (analyze-js-star* env jsform args form)))

(defmethod parse 'js*
  [op env [_ jsform & args :as form] _ _]
  (when-not (string? jsform)
    (throw (error env "Invalid js* form")))
  (if-not (nil? args)
    (analyze-js-star env jsform args form)
    (let [code      (apply str (js-star-interp env jsform))
          tag       (get-js-tag form)
          form-meta (meta form)
          js-op     (:js-op form-meta)
          numeric   (:numeric form-meta)]
      {:op :js
       :env env
       :form form
       :code code
       :tag tag
       :js-op js-op
       :numeric numeric})))

(defn- analyzed?
  #?(:cljs {:tag boolean})
  [f]
  (contains? (meta f) ::analyzed))

(defn- all-values?
  #?(:cljs {:tag boolean})
  [exprs]
  (every? #(or (nil? %) (symbol? %) (string? %) (number? %) (true? %) (false? %)) exprs))

(defn- valid-arity?
  #?(:cljs {:tag boolean})
  [argc method-params]
  (boolean (some #{argc} (map count method-params))))

(defn parse-invoke*
  [env [f & args :as form]]
  (let [enve    (assoc env :context :expr)
        fexpr   (analyze enve f)
        argc    (count args)
        fn-var? (-> fexpr :info :fn-var)
        kw?     (= 'cljs.core/Keyword (:tag fexpr))]
    (when ^boolean fn-var?
      (let [{:keys [^boolean variadic max-fixed-arity method-params name]} (:info fexpr)]
        (when (and (not (valid-arity? argc method-params))
                   (or (not variadic)
                       (and variadic (< argc max-fixed-arity))))
          (warning :fn-arity env {:name name :argc argc}))))
    (when (and kw? (not (or (== 1 argc) (== 2 argc))))
      (warning :fn-arity env {:name (first form) :argc argc}))
    (let [deprecated? (-> fexpr :info :deprecated)
          no-warn? (-> form meta :deprecation-nowarn)]
      (when (and (boolean deprecated?)
                 (not (boolean no-warn?)))
        (warning :fn-deprecated env {:fexpr fexpr})))
    (when-not (nil? (-> fexpr :info :type))
      (warning :invoke-ctor env {:fexpr fexpr}))
    (if (or (not (boolean *cljs-static-fns*))
            (not (symbol? f))
            fn-var?
            (analyzed? f)
            (all-values? args))
      (let [ana-expr #(analyze enve %)
            argexprs (map ana-expr args)]
        {:env env :op :invoke :form form :f fexpr :args (vec argexprs)
         :children (into [fexpr] argexprs)})
      (let [arg-syms (take argc (repeatedly gensym))]
        (analyze env
                 `(let [~@(vec (interleave arg-syms args))]
                    (~(vary-meta f assoc ::analyzed true) ~@arg-syms)))))))

(defn parse-invoke
  [env form]
  (disallowing-recur (parse-invoke* env form)))

(defn analyze-symbol
  "Finds the var associated with sym"
  [env sym]
  (if ^boolean (:quoted? env)
    (do
      (register-constant! env sym)
      (analyze-wrap-meta {:op :constant :env env :form sym :tag 'cljs.core/Symbol}))
    (let [{:keys [line column]} (meta sym)
          env  (if-not (nil? line)
                 (assoc env :line line)
                 env)
          env  (if-not (nil? column)
                 (assoc env :column column)
                 env)
          ret  {:env env :form sym}
          lcls (:locals env)
          lb   (get lcls sym)]
      (if-not (nil? lb)
        (assoc ret :op :var :info lb)
        (if-not (true? (:def-var env))
          (let [sym-meta (meta sym)
                info     (if-not (contains? sym-meta ::analyzed)
                           (resolve-existing-var env sym)
                           (resolve-var env sym))]
            (assoc ret :op :var :info info))
          (let [info (resolve-var env sym)]
            (assoc ret :op :var :info info)))))))

(defn excluded?
  #?(:cljs {:tag boolean})
  [env sym]
  (if-not (nil? (gets env :ns :excludes sym))
    true
    (not (nil? (gets @env/*compiler* ::namespaces (gets env :ns :name) :excludes sym)))))

(defn used?
  #?(:cljs {:tag boolean})
  [env sym]
  (if-not (nil? (gets env :ns :use-macros sym))
    true
    (not (nil? (gets @env/*compiler* ::namespaces (gets env :ns :name) :use-macros sym)))))

(defn get-expander-ns [env ^String nstr]
  ;; first check for clojure.* -> cljs.* cases
  (let [res  (or (resolve-macro-ns-alias env nstr nil)
                 (resolve-ns-alias env nstr nil))
        nstr (if res (str res) nstr)]
    (cond
     #?@(:clj  [(= "clojure.core" nstr) (find-ns 'cljs.core)]
         :cljs [(identical? "clojure.core" nstr) (find-macros-ns CLJS_CORE_MACROS_SYM)])
     #?@(:clj  [(= "clojure.repl" nstr) (find-ns 'cljs.repl)]
         :cljs [(identical? "clojure.repl" nstr) (find-macros-ns 'cljs.repl)])
     #?@(:clj  [(.contains nstr ".") (find-ns (symbol nstr))]
         :cljs [(goog.string/contains nstr ".") (find-macros-ns (symbol nstr))])
     :else (some-> env :ns :require-macros (get (symbol nstr)) #?(:clj  find-ns
                                                                  :cljs find-macros-ns)))))

(defn get-expander* [sym env]
  (when-not (or (not (nil? (gets env :locals sym))) ; locals hide macros
                (and (excluded? env sym) (not (used? env sym))))
    (let [nstr (namespace sym)]
      (cond
        (not (nil? nstr))
        (let [ns (get-expander-ns env nstr)]
          (when-not (nil? ns)
            (.findInternedVar ^clojure.lang.Namespace ns (symbol (name sym)))))

        (not (nil? (gets env :ns :rename-macros sym)))
        (let [qualified-symbol (gets env :ns :rename-macros sym)
              nsym (symbol (namespace qualified-symbol))
              sym  (symbol (name qualified-symbol))]
          (.findInternedVar ^clojure.lang.Namespace
            #?(:clj (find-ns nsym) :cljs (find-macros-ns nsym)) sym))

        :else
        (let [nsym (gets env :ns :use-macros sym)]
          (if-not (nil? nsym)
            (.findInternedVar ^clojure.lang.Namespace
            #?(:clj (find-ns nsym) :cljs (find-macros-ns nsym)) sym)
            (.findInternedVar ^clojure.lang.Namespace
            #?(:clj (find-ns 'cljs.core) :cljs (find-macros-ns CLJS_CORE_MACROS_SYM)) sym)))))))

(defn get-expander
  "Given a sym, a symbol identifying a macro, and env, an analysis environment
   return the corresponding Clojure macroexpander."
  [sym env]
  (let [mvar (get-expander* sym env)]
    (when (and (not (nil? mvar))
            #?(:clj  (.isMacro ^clojure.lang.Var mvar)
               :cljs ^boolean (.isMacro mvar)))
      mvar)))

(defn macroexpand-1*
  [env form]
  (let [op (first form)]
    (if-not (nil? (get specials op))
      form
      (let [mac-var (when (symbol? op) (get-expander op env))]
        (if-not (nil? mac-var)
          (#?@(:clj [binding [*ns* (create-ns *cljs-ns*)]]
               :cljs [do])
            (let [mchk  #?(:clj  (some-> (find-ns 'clojure.spec)
                                   (ns-resolve 'macroexpand-check))
                           :cljs (and ^::no-resolve cljs.spec
                                      ^::no-resolve cljs.spec/macroexpand-check))
                  _     (when mchk
                          (mchk mac-var (next form)))
                  form' (try
                          (apply @mac-var form env (rest form))
                          #?(:clj (catch ArityException e
                                    (throw (ArityException. (- (.actual e) 2) (.name e))))))]
              (if #?(:clj (seq? form') :cljs (cljs-seq? form'))
                (let [sym' (first form')
                      sym  (first form)]
                  (if #?(:clj  (= sym' 'js*)
                         :cljs (symbol-identical? sym' JS_STAR_SYM))
                    (let [sym   (if (namespace sym)
                                  sym
                                  (symbol "cljs.core" (str sym)))
                          js-op {:js-op sym}
                          numeric #?(:clj  (-> mac-var meta ::numeric)
                                     :cljs (let [mac-var-ns   (symbol (namespace (.-sym mac-var)))
                                                 mac-var-name (symbol (name (.-sym mac-var)))]
                                             (get-in @env/*compiler*
                                               [::namespaces mac-var-ns :defs mac-var-name :meta ::numeric])))
                          js-op (if (true? numeric)
                                  (assoc js-op :numeric true)
                                  js-op)]
                      (vary-meta form' merge js-op))
                    form'))
                form')))
          (if (symbol? op)
            (let [opname (str op)]
              (cond
                (identical? \.
                  #?(:clj  (first opname)
                     :cljs (.charAt opname 0)))
                (let [[target & args] (next form)]
                  (with-meta (list* #?(:clj '. :cljs DOT_SYM) target (symbol (subs opname 1)) args)
                    (meta form)))

                (identical? \.
                  #?(:clj  (last opname)
                     :cljs (.charAt opname (dec (. opname -length)))))
                (with-meta
                  (list* #?(:clj 'new :cljs NEW_SYM) (symbol (subs opname 0 (dec (count opname)))) (next form))
                  (meta form))

                :else form))
            form))))))

(defn macroexpand-1
  "Given a env, an analysis environment, and form, a ClojureScript form,
   macroexpand the form once."
  [env form]
  (ensure (wrapping-errors env (macroexpand-1* env form))))

(declare analyze-list)

(defn analyze-seq* [op env form name opts]
  (if-not (nil? (get specials op))
    (parse op env form name opts)
    (parse-invoke env form)))

(defn analyze-seq*-wrap [op env form name opts]
  (wrapping-errors env
    (analyze-seq* op env form name opts)))

(defn analyze-seq
  ([env form name] (analyze-seq env form name nil))
  ([env form name opts]
   (if ^boolean (:quoted? env)
     (analyze-list env form)
     (let [line (-> form meta :line)
           line (if (nil? line)
                  (:line env)
                  line)
           col  (-> form meta :column)
           col  (if (nil? col)
                  (:column env)
                  col)
           env  (assoc env :line line :column col)]
       (let [op (first form)]
         (when (nil? op)
           (throw (error env "Can't call nil")))
         (let [mform (macroexpand-1 env form)]
           (if (identical? form mform)
             (analyze-seq*-wrap op env form name opts)
             (analyze env mform name opts))))))))

(defn analyze-map
  [env form]
  (let [expr-env (assoc env :context :expr)
        ks (disallowing-recur (vec (map #(analyze expr-env %) (keys form))))
        vs (disallowing-recur (vec (map #(analyze expr-env %) (vals form))))]
    (analyze-wrap-meta {:op :map :env env :form form
                        :keys ks :vals vs
                        :children (vec (interleave ks vs))
                        :tag 'cljs.core/IMap})))

(defn analyze-list
  [env form]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (doall (map #(analyze expr-env %) form)))]
    (analyze-wrap-meta {:op :list :env env :form form :items items :children items :tag 'cljs.core/IList})))

(defn analyze-vector
  [env form]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (vec (map #(analyze expr-env %) form)))]
    (analyze-wrap-meta {:op :vector :env env :form form :items items :children items :tag 'cljs.core/IVector})))

(defn analyze-set
  [env form ]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (vec (map #(analyze expr-env %) form)))]
    (analyze-wrap-meta {:op :set :env env :form form :items items :children items :tag 'cljs.core/ISet})))

(defn analyze-js-value
  [env ^JSValue form]
  (let [val (.-val form)
        expr-env (assoc env :context :expr)
        items (if (map? val)
                (zipmap (keys val)
                        (disallowing-recur (doall (map #(analyze expr-env %) (vals val)))))
                (disallowing-recur (doall (map #(analyze expr-env %) val))))]
    {:op :js-value
     :js-type (if (map? val) :object :array)
     :env env
     :form form
     :items items
     :children items
     :tag (if (map? val) 'object 'array)}))

(defn elide-reader-meta [m]
  (dissoc m :file :line :column :end-column :end-line :source))

(defn analyze-wrap-meta [expr]
  (let [form (:form expr)
        m    (elide-reader-meta (meta form))]
    (if (seq m)
      (let [env (:env expr) ; take on expr's context ourselves
            expr (assoc-in expr [:env :context] :expr) ; change expr to :expr
            meta-expr (analyze-map (:env expr) m)]
        {:op :meta :env env :form form
         :meta meta-expr :expr expr :children [meta-expr expr]})
      expr)))

(defn infer-type [env ast _]
  (let [tag (:tag ast)]
    (if (nil? tag)
      (let [tag (infer-tag env ast)]
        (if-not (nil? tag)
          (assoc ast :tag tag)
          ast))
      ast)))

#?(:clj
   (defn ns-side-effects
     [env {:keys [op] :as ast} opts]
     (if (= :ns op)
       (let [{:keys [name deps uses require-macros use-macros reload reloads]} ast]
         (when (and *analyze-deps* (seq deps))
           (analyze-deps name deps env (dissoc opts :macros-ns)))
         (if *load-macros*
           (do
             (load-core)
             (doseq [nsym (vals use-macros)]
               (let [k (or (:use-macros reload)
                         (get-in reloads [:use-macros nsym])
                         (and (= nsym name) *reload-macros* :reload))]
                 (if k
                   (locking load-mutex
                     (clojure.core/require nsym k))
                   (locking load-mutex
                     (clojure.core/require nsym)))
                 (intern-macros nsym k)))
             (doseq [nsym (vals require-macros)]
               (let [k (or (:require-macros reload)
                         (get-in reloads [:require-macros nsym])
                         (and (= nsym name) *reload-macros* :reload))]
                 (if k
                   (locking load-mutex
                     (clojure.core/require nsym k))
                   (locking load-mutex
                     (clojure.core/require nsym)))
                 (intern-macros nsym k)))
             (-> ast
               (check-use-macros-inferring-missing env)
               (check-rename-macros-inferring-missing env)))
           (do
             (check-uses
               (when (and *analyze-deps* (seq uses))
                 (missing-uses uses env))
               env)
             ast)))
       ast)))

(def ^:dynamic *passes* nil)

#?(:clj
   (defn analyze-form [env form name opts]
     (load-core)
     (cond
       (symbol? form) (analyze-symbol env form)
       (and (seq? form) (seq form)) (analyze-seq env form name opts)
       (map? form) (analyze-map env form)
       (vector? form) (analyze-vector env form)
       (set? form) (analyze-set env form)
       (keyword? form) (analyze-keyword env form)
       (instance? JSValue form) (analyze-js-value env form)
       (= () form) (analyze-list env form)
       :else
       (let [tag (cond
                   (nil? form) 'clj-nil
                   (number? form) 'number
                   (string? form) 'string
                   (true? form) 'boolean
                   (false? form) 'boolean)]
         (cond-> {:op :constant :env env :form form}
           tag (assoc :tag tag))))))

#?(:cljs
   (defn analyze-form [env form name opts]
     (cond
       (symbol? form) (analyze-symbol env form)
       (and (cljs-seq? form) (seq form)) (analyze-seq env form name opts)
       (cljs-map? form) (analyze-map env form)
       (cljs-vector? form) (analyze-vector env form)
       (cljs-set? form) (analyze-set env form)
       (keyword? form) (analyze-keyword env form)
       (instance? cljs.tagged-literals/JSValue form) (analyze-js-value env form)
       (= () form) (analyze-list env form)
       :else
       (let [tag (cond
                   (nil? form) CLJ_NIL_SYM
                   (number? form) NUMBER_SYM
                   (string? form) STRING_SYM
                   (true? form) BOOLEAN_SYM
                   (false? form) BOOLEAN_SYM)]
         (cond-> {:op :constant :env env :form form}
           tag (assoc :tag tag))))))

(defn analyze* [env form name opts]
  (let [passes *passes*
        passes (if (nil? passes)
                 #?(:clj  [infer-type ns-side-effects]
                    :cljs [infer-type])
                 passes)
        form   (if (instance? LazySeq form)
                 (if (seq form) form ())
                 form)
        ast    (analyze-form env form name opts)]
    (reduce (fn [ast pass] (pass env ast opts)) ast passes)))

(defn analyze
  "Given an environment, a map containing {:locals (mapping of names to bindings), :context
  (one of :statement, :expr, :return), :ns (a symbol naming the
  compilation ns)}, and form, returns an expression object (a map
  containing at least :form, :op and :env keys). If expr has any (immediately)
  nested exprs, must have :children [exprs...] entry. This will
  facilitate code walking without knowing the details of the op set."
  ([env form] (analyze env form nil))
  ([env form name] (analyze env form name nil))
  ([env form name opts]
   (ensure
     (wrapping-errors env
       (binding [reader/*alias-map* (or reader/*alias-map* {})]
         (analyze* env form name opts))))))

#?(:clj
   (defn- source-path
     "Returns a path suitable for providing to tools.reader as a 'filename'."
     [x]
     (cond
       (instance? File x) (.getAbsolutePath ^File x)
       :default (str x))))

(defn resolve-symbol [s]
  (:name (resolve-var (assoc @env/*compiler* :ns (get-namespace *cljs-ns*)) s)))

#?(:clj
   (defn forms-seq*
     "Seq of Clojure/ClojureScript forms from rdr, a java.io.Reader. Optionally
     accepts a filename argument which will be used in any emitted errors."
     ([^Reader rdr] (forms-seq* rdr nil))
     ([^Reader rdr filename]
      {:pre [(instance? Reader rdr)]}
      (let [eof-sentinel (Object.)
            opts (merge
                   {:eof eof-sentinel}
                   (if (and filename (= (util/ext filename) "cljc"))
                     {:read-cond :allow :features #{:cljs}}))
            pbr (readers/indexing-push-back-reader
                  (PushbackReader. rdr) 1 filename)
            data-readers tags/*cljs-data-readers*
            forms-seq_
            (fn forms-seq_ []
              (lazy-seq
                (let [form (binding [*ns* (create-ns *cljs-ns*)
                                     reader/*data-readers* data-readers
                                     reader/*alias-map*
                                     (apply merge
                                       ((juxt :requires :require-macros)
                                         (get-namespace *cljs-ns*)))
                                     reader/resolve-symbol resolve-symbol]
                             (reader/read opts pbr))]
                  (if (identical? form eof-sentinel)
                    (.close rdr)
                    (cons form (forms-seq_))))))]
        (forms-seq_)))))

#?(:clj
   (defn forms-seq
     "DEPRECATED: Seq of Clojure/ClojureScript forms from [f], which can be anything
     for which `clojure.java.io/reader` can produce a `java.io.Reader`. Optionally
     accepts a [filename] argument, which the reader will use in any emitted errors."
     ([f] (forms-seq f (source-path f)))
     ([f filename] (forms-seq f filename false))
     ([f filename return-reader?]
      (let [rdr (io/reader f)
            pbr (readers/indexing-push-back-reader
                  (PushbackReader. rdr) 1 filename)
            data-readers tags/*cljs-data-readers*
            forms-seq*
            (fn forms-seq* []
              (lazy-seq
                (let [eof-sentinel (Object.)
                      form (binding [*ns* (create-ns *cljs-ns*)
                                     reader/*data-readers* data-readers
                                     reader/*alias-map*
                                     (apply merge
                                       ((juxt :requires :require-macros)
                                         (get-namespace *cljs-ns*)))]
                             (reader/read pbr nil eof-sentinel))]
                  (if (identical? form eof-sentinel)
                    (.close rdr)
                    (cons form (forms-seq*))))))]
        (if (true? return-reader?)
          [(forms-seq*) rdr]
          (forms-seq*))))))

#?(:clj
   (defn parse-ns
     "Helper for parsing only the essential namespace information from a
      ClojureScript source file and returning a cljs.closure/IJavaScript compatible
      map _not_ a namespace AST node.

      By default does not load macros or perform any analysis of dependencies. If
      opts parameter provided :analyze-deps and :load-macros keys their values will
      be used for *analyze-deps* and *load-macros* bindings respectively. This
      function does _not_ side-effect the ambient compilation environment unless
      requested via opts where :restore is false."
     ([src] (parse-ns src nil nil))
     ([src opts] (parse-ns src nil opts))
     ([src dest opts]
      (ensure
        (let [src (if (symbol? src)
                    (util/ns->source src)
                    src)
              ijs
              (binding [env/*compiler* (if (false? (:restore opts))
                                         env/*compiler*
                                         (atom @env/*compiler*))
                        *cljs-ns* 'cljs.user
                        *cljs-file* src
                        *macro-infer*
                        (or (when (contains? opts :macro-infer)
                              (:macro-infer opts))
                          false)
                        *analyze-deps*
                        (or (when (contains? opts :analyze-deps)
                              (:analyze-deps opts))
                          false)
                        *load-macros*
                        (or (when (contains? opts :load-macros)
                              (:load-macros opts))
                          false)]
                (let [rdr (when-not (sequential? src) (io/reader src))]
                  (try
                    (loop [forms (if rdr
                                   (forms-seq* rdr (source-path src))
                                   src)]
                      (if (seq forms)
                        (let [env (empty-env)
                              ast (no-warn (analyze env (first forms) nil opts))]
                          (if (= :ns (:op ast))
                            (let [ns-name (:name ast)
                                  ns-name (if (and (= 'cljs.core ns-name)
                                                   (= "cljc" (util/ext src)))
                                            'cljs.core$macros
                                            ns-name)
                                  deps (merge (:uses ast) (:requires ast))]
                              (merge
                                {:ns           (or ns-name 'cljs.user)
                                 :provides     [ns-name]
                                 :requires     (if (= 'cljs.core ns-name)
                                                 (set (vals deps))
                                                 (cond-> (conj (set (vals deps)) 'cljs.core)
                                                   (get-in @env/*compiler* [:options :emit-constants])
                                                   (conj 'constants-table)))
                                 :file         dest
                                 :source-file  (when rdr src)
                                 :source-forms (when-not rdr src)
                                 :ast          ast
                                 :macros-ns    (or (:macros-ns opts)
                                                   (= 'cljs.core$macros ns-name))}
                                (when (and dest (.exists ^File dest))
                                  {:lines (with-open [reader (io/reader dest)]
                                            (-> reader line-seq count))})))
                            (recur (rest forms))))
                        (throw (AssertionError. (str "No ns form found in " src)))))
                    (finally
                      (when rdr
                        (.close ^Reader rdr))))))]
          ijs)))))

#?(:clj
   (defn cache-file
     "Given a ClojureScript source file returns the read/write path to the analysis
      cache file. Defaults to the read path which is usually also the write path."
     ([src] (cache-file src "out"))
     ([src output-dir] (cache-file src (parse-ns src) output-dir))
     ([src ns-info output-dir] (cache-file src (parse-ns src) output-dir :read))
     ([src ns-info output-dir mode]
      {:pre [(map? ns-info)]}
      (if-let [core-cache
               (and (= mode :read)
                    (= (:ns ns-info) 'cljs.core)
                    (or (and @transit (io/resource "cljs/core.cljs.cache.aot.json"))
                        (io/resource "cljs/core.cljs.cache.aot.edn")))]
        core-cache
        (let [target-file (util/to-target-file output-dir ns-info
                            (util/ext (:source-file ns-info)))]
          (if @transit
            (io/file (str target-file ".cache.json"))
            (io/file (str target-file ".cache.edn"))))))))

#?(:clj
   (defn requires-analysis?
     "Given a src, a resource, and output-dir, a compilation output directory
      return true or false depending on whether src needs to be (re-)analyzed.
      Can optionally pass cache, the analysis cache file."
     ([src] (requires-analysis? src "out"))
     ([src output-dir]
      (let [cache (cache-file src output-dir)]
        (requires-analysis? src cache output-dir)))
     ([src cache output-dir]
      (cond
        (util/url? cache)
        (let [path (.getPath ^URL cache)]
          (if (or (.endsWith path "cljs/core.cljs.cache.aot.edn")
                  (.endsWith path "cljs/core.cljs.cache.aot.json"))
            false
            (throw (Exception. (str "Invalid anlaysis cache, must be file not URL " cache)))))

        (and (util/file? cache)
             (not (.exists ^File cache)))
        true

        :else
        (let [out-src (util/to-target-file output-dir (parse-ns src))]
          (if (not (.exists out-src))
            true
            (util/changed? src cache)))))))

#?(:clj
   (def transit-write-mutex (Object.)))

#?(:clj
   (defn write-analysis-cache
     ([ns cache-file]
       (write-analysis-cache ns cache-file nil))
     ([ns ^File cache-file src]
      (util/mkdirs cache-file)
      (let [ext (util/ext cache-file)
            analysis (dissoc (get-in @env/*compiler* [::namespaces ns]) :macros)]
        (case ext
          "edn"  (spit cache-file
                   (str (when
                     (str ";; Analyzed by ClojureScript " (util/clojurescript-version) "\n"))
                       (pr-str analysis)))
          "json" (when-let [{:keys [writer write]} @transit]
                   (locking transit-write-mutex
                     (write
                       (writer (FileOutputStream. cache-file) :json
                         transit-write-opts)
                       analysis)))))
      (when src
        (.setLastModified ^File cache-file (util/last-modified src))))))

#?(:clj
   (defn analyze-file
     "Given a java.io.File, java.net.URL or a string identifying a resource on the
      classpath attempt to analyze it.

      This function side-effects the ambient compilation environment
      `cljs.env/*compiler*` to aggregate analysis information. opts argument is
      compiler options, if :cache-analysis true will cache analysis to
      \":output-dir/some/ns/foo.cljs.cache.edn\". This function does not return a
      meaningful value."
     ([f]
      (analyze-file f nil))
     ([f opts]
      (analyze-file f false opts))
     ([f skip-cache opts]
      (binding [*file-defs*    (atom #{})
                *unchecked-if* false]
        (let [output-dir (util/output-directory opts)
              res        (cond
                           (instance? File f) f
                           (instance? URL f) f
                           (re-find #"^file://" f) (URL. f)
                           :else (io/resource f))]
          (assert res (str "Can't find " f " in classpath"))
          (ensure
            (let [ns-info (parse-ns res)
                  path    (if (instance? File res)
                            (.getPath ^File res)
                            (.getPath ^URL res))
                  cache   (when (:cache-analysis opts)
                            (cache-file res ns-info output-dir))]
              (when-not (get-in @env/*compiler* [::namespaces (:ns ns-info) :defs])
                (if (or skip-cache (not cache) (requires-analysis? res output-dir))
                  (binding [*cljs-ns* 'cljs.user
                            *cljs-file* path
                            reader/*alias-map* (or reader/*alias-map* {})]
                    (when (or *verbose* (:verbose opts))
                      (util/debug-prn "Analyzing" (str res)))
                    (let [env (assoc (empty-env) :build-options opts)
                          ns  (with-open [rdr (io/reader res)]
                                (loop [ns nil forms (seq (forms-seq* rdr (util/path res)))]
                                  (if forms
                                    (let [form (first forms)
                                          env (assoc env :ns (get-namespace *cljs-ns*))
                                          ast (analyze env form nil opts)]
                                      (if (= (:op ast) :ns)
                                        (recur (:name ast) (next forms))
                                        (recur ns (next forms))))
                                    ns)))]
                      (when (and cache (true? (:cache-analysis opts)))
                        (write-analysis-cache ns cache res))))
                  (try
                    ;; we want want to keep dependency analysis information
                    ;; don't revert the environment - David
                    (let [{:keys [ns]} (parse-ns res
                                         (merge opts
                                           {:restore false
                                            :analyze-deps true
                                            :load-macros true}))
                          ext          (util/ext cache)
                          cached-ns    (case ext
                                         "edn"  (edn/read-string (slurp cache))
                                         "json" (let [{:keys [reader read]} @transit]
                                                  (read (reader (io/input-stream cache) :json
                                                          transit-read-opts))))]
                     (when (or *verbose* (:verbose opts))
                       (util/debug-prn "Reading analysis cache for" (str res)))
                     (swap! env/*compiler*
                       (fn [cenv]
                         (let []
                           (doseq [x (get-in cached-ns [::constants :order])]
                             (register-constant! x))
                           (-> cenv
                             (assoc-in [::namespaces ns] cached-ns))))))
                    (catch Throwable e
                      (analyze-file f true opts))))))))))))
