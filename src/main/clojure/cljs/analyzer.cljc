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
              :refer [no-warn wrapping-errors with-warning-handlers
                      disallowing-recur allowing-redef disallowing-ns*]]
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

;; User file-local compiler flags
#?(:clj (def ^:dynamic *unchecked-if* false))
#?(:clj (def ^:dynamic *unchecked-arrays* false))

;; Compiler dynamic vars
(def ^:dynamic *cljs-ns* 'cljs.user)
(def ^:dynamic *cljs-file* nil)
(def ^:dynamic *checked-arrays* false)
(def ^:dynamic *check-alias-dupes* true)
(def ^:dynamic *cljs-static-fns* false)
(def ^:dynamic *fn-invoke-direct* false)
(def ^:dynamic *cljs-macros-path* "/cljs/core")
(def ^:dynamic *cljs-macros-is-classpath* true)
(def ^:dynamic *cljs-dep-set* (with-meta #{} {:dep-path []}))
(def ^:dynamic *analyze-deps* true)
(def ^:dynamic *load-tests* true)
(def ^:dynamic *load-macros* true)
(def ^:dynamic *reload-macros* false)
(def ^:dynamic *macro-infer* true)
(def ^:dynamic *passes* nil)
(def ^:dynamic *file-defs* nil)
(def ^:dynamic *private-var-access-nowarn* false)

(def constants-ns-sym
  "The namespace of the constants table as a symbol."
  'cljs.core.constants)

(def ^:private identity-counter (atom 0))

(defn- add-identity [m]
  (assoc m :identity (swap! identity-counter inc)))

#?(:clj
   (def transit-read-opts
     (try
       (require '[cognitect.transit])
       (when-some [ns (find-ns 'cognitect.transit)]
         (let [read-handler     @(ns-resolve ns 'read-handler)
               read-handler-map @(ns-resolve ns 'read-handler-map)]
           {:handlers
             (read-handler-map
               {"cljs/js"    (read-handler (fn [v] (JSValue. v)))
                "cljs/regex" (read-handler (fn [v] (Pattern/compile v)))})}))
       (catch Throwable t
         nil))))

#?(:clj
   (def transit-write-opts
     (try
       (require '[cognitect.transit])
       (when-some [ns (find-ns 'cognitect.transit)]
         (let [write-handler     @(ns-resolve ns 'write-handler)
               write-handler-map @(ns-resolve ns 'write-handler-map)]
           {:handlers
             (write-handler-map
               {JSValue
                (write-handler
                  (fn [_] "cljs/js")
                  (fn [js] (.val ^JSValue js)))
                Pattern
                (write-handler
                  (fn [_] "cljs/regex")
                  (fn [pat] (.pattern ^Pattern pat)))})}))
       (catch Throwable t
         nil))))

#?(:clj
   (def transit
     (delay
       (try
         (require '[cognitect.transit])
         (when-some [ns (find-ns 'cognitect.transit)]
           {:writer @(ns-resolve ns 'writer)
            :reader @(ns-resolve ns 'reader)
            :write  @(ns-resolve ns 'write)
            :read   @(ns-resolve ns 'read)})
         (catch Throwable t
           nil)))))

;; log compiler activities
(def ^:dynamic *verbose* false)

(def -cljs-macros-loaded (atom false))

(def ^:dynamic *cljs-warnings*
  {:preamble-missing true
   :unprovided true
   :undeclared-var true
   :private-var-access true
   :undeclared-ns true
   :undeclared-ns-form true
   :redef true
   :redef-in-file true
   :dynamic true
   :fn-var true
   :fn-arity true
   :fn-deprecated true
   :declared-arglists-mismatch true
   :protocol-deprecated true
   :undeclared-protocol-symbol true
   :invalid-protocol-symbol true
   :multiple-variadic-overloads true
   :variadic-max-arity true
   :overload-arity true
   :extending-base-js-type true
   :invoke-ctor true
   :invalid-arithmetic true
   :invalid-array-access true
   :protocol-invalid-method true
   :protocol-duped-method true
   :protocol-multiple-impls true
   :protocol-with-variadic-method true
   :protocol-impl-with-variadic-method true
   :protocol-impl-recur-with-target true
   :single-segment-namespace true
   :munged-namespace true
   :ns-var-clash true
   :non-dynamic-earmuffed-var true
   :extend-type-invalid-method-shape true
   :unsupported-js-module-type true
   :unsupported-preprocess-value true
   :js-shadowed-by-local true
   :infer-warning false})

(defn unchecked-arrays? []
  *unchecked-arrays*)

(defn checked-arrays
  "Returns false-y, :warn, or :error based on configuration and the
   current value of *unchecked-arrays*."
  []
  (when (and (not (-> @env/*compiler* :options :advanced))
             (not *unchecked-arrays*))
    *checked-arrays*))

(def js-reserved
  #{"arguments" "abstract" "await" "boolean" "break" "byte" "case"
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

(def es5-allowed
  #{"default"})

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

(defmethod error-message :private-var-access
  [warning-type info]
  (str "var: " (:sym info) " is not public"))

(defmethod error-message :undeclared-ns
  [warning-type {:keys [ns-sym js-provide] :as info}]
  (str "No such namespace: " ns-sym
       ", could not locate " (ns->relpath ns-sym :cljs)
       ", " (ns->relpath ns-sym :cljc)
       ", or JavaScript source providing \"" js-provide "\""
    (when (string/includes? (ns->relpath ns-sym) "_")
      " (Please check that namespaces with dashes use underscores in the ClojureScript file name)")))

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
  (str (-> info :fexpr :info :name) " is deprecated"))

(defmethod error-message :declared-arglists-mismatch
  [warning-type info]
  (str (symbol (str (:ns-name info)) (str (:sym info)))
    " declared arglists " (:declared info)
    " mismatch defined arglists " (:defined info)))

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

(defmethod error-message :protocol-impl-with-variadic-method
  [warning-type info]
  (str "Protocol " (:protocol info) " implements method "
    (:name info) " with variadic signature (&)"))

(defmethod error-message :protocol-impl-recur-with-target
  [warning-type info]
  (str "Ignoring target object \"" (pr-str (:form info)) "\" passed in recur to protocol method head"))

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
  (str (:js-op info) ", all arguments must be numbers, got " (:types info) " instead"))

(defmethod error-message :invalid-array-access
  [warning-type {:keys [name types]}]
  (case name
    (cljs.core/checked-aget cljs.core/checked-aget')
    (str "cljs.core/aget, arguments must be an array followed by numeric indices, got " types " instead"
      (when (or (= 'object (first types))
                (every? #{'string} (rest types)))
        (str " (consider "
          (if (== 2 (count types))
            "goog.object/get"
            "goog.object/getValueByKeys")
          " for object access)")))

    (cljs.core/checked-aset cljs.core/checked-aset')
    (str "cljs.core/aset, arguments must be an array, followed by numeric indices, followed by a value, got " types " instead"
      (when (or (= 'object (first types))
                (every? #{'string} (butlast (rest types))))
        " (consider goog.object/set for object access)"))))

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

(defmethod error-message :non-dynamic-earmuffed-var
  [warning-type {:keys [var] :as info}]
  (str var " not declared dynamic and thus is not dynamically rebindable, but its name "
    "suggests otherwise. Please either indicate ^:dynamic " var " or change the name"))

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

(defmethod error-message :js-shadowed-by-local
  [warning-type {:keys [name]}]
  (str name " is shadowed by a local"))

(defmethod error-message :infer-warning
  [warning-type {:keys [warn-type form type property]}]
  (case warn-type
    :target   (str "Cannot infer target type in expression " form "")
    :property (str "Cannot resolve property " property
                   " for inferred type " type  " in expression " form)
    :object   (str "Adding extern to Object for property " property " due to "
                   "ambiguous expression " form)))

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
         (if (some? env/*compiler*)
           (::namespaces @env/*compiler*)
           default-namespaces)))
     :cljs
     (reify IDeref
       (-deref [_]
         (if (some? env/*compiler*)
           (::namespaces @env/*compiler*)
           default-namespaces)))))

(defn get-namespace
  ([key]
    (get-namespace env/*compiler* key))
  ([cenv key]
   (if-some [ns (get-in @cenv [::namespaces key])]
       ns
       (when (= 'cljs.user key)
         {:name 'cljs.user}))))

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
    (when (or (nil? (gets @env/*compiler* ::namespaces ns :macros))
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
                   (map #(vector % {:op :js-var :name % :ns 'js})
                     '(alert window document console escape unescape
                       screen location navigator history location
                       global process require module exports)))}))

(defn- source-info->error-data
  [{:keys [file line column]}]
  {:clojure.error/source file
   :clojure.error/line   line
   :clojure.error/column column})

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

(defn- accumulating-warning-handler [warn-acc]
  (fn [warning-type env extra]
    (when (warning-type *cljs-warnings*)
      (swap! warn-acc conj [warning-type env extra]))))

(defn- replay-accumulated-warnings [warn-acc]
  (run! #(apply warning %) @warn-acc))

(defn- error-data
  ([env phase]
   (error-data env phase nil))
  ([env phase symbol]
   (merge (-> (source-info env) source-info->error-data)
     {:clojure.error/phase phase}
     (when symbol
       {:clojure.error/symbol symbol}))))

(defn- compile-syntax-error
  [env msg symbol]
  (ex-info nil (error-data env :compile-syntax-check symbol)
    #?(:clj (RuntimeException. ^String msg) :cljs (js/Error. msg))))

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

(defn has-error-data?
  #?(:cljs {:tag boolean})
  [ex]
  (contains? (ex-data ex) :clojure.error/phase))

#?(:clj
   (defmacro wrapping-errors [env & body]
     `(try
        ~@body
        (catch Throwable err#
          (cond
            (has-error-data? err#) (throw err#)
            (analysis-error? err#) (throw (ex-info nil (error-data ~env :compilation) err#))
            :else (throw (ex-info nil (error-data ~env :compilation) (error ~env (.getMessage err#) err#))))))))

;; namespaces implicit to the inclusion of cljs.core
(def implicit-nses '#{goog goog.object goog.string goog.array Math String})

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
      (or (some? (get (:requires ns) prefix))
          (some? (get (:imports ns) prefix))))))

(defn- internal-js-module-exists?
  [js-module-index module]
  ;; we need to check both keys and values of the JS module index, because
  ;; macroexpansion will be looking for the provided name - António Monteiro
  (contains?
    (into #{}
      (mapcat (fn [[k v]]
                [k (:name v)]))
      js-module-index)
    (str module)))

(def js-module-exists?* (memoize internal-js-module-exists?))

(defn js-module-exists?
  [module]
  (js-module-exists?* (get-in @env/*compiler* [:js-module-index]) module))

(defn node-module-dep?
  #?(:cljs {:tag boolean})
  [module]
  #?(:clj (contains?
            (get-in @env/*compiler* [:node-module-index])
            (str module))
     :cljs (try
             (and (= *target* "nodejs")
                  (boolean (js/require.resolve (str module))))
             (catch :default _
               false))))

(defn dep-has-global-exports?
  [module]
  (let [global-exports (get-in @env/*compiler* [:js-dependency-index (str module) :global-exports])]
    (or (contains? global-exports (symbol module))
        (contains? global-exports (name module)))))

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
                (nil? (gets @env/*compiler* ::namespaces prefix :defs suffix))
                (not (js-module-exists? prefix)))
       (missing-fn env prefix suffix)))))

(defn confirm-var-exists-throw []
  (fn [env prefix suffix]
    (confirm-var-exists env prefix suffix
      (fn [env prefix suffix]
        (throw (error env (str "Unable to resolve var: " suffix " in this context")))))))

(defn resolve-ns-alias
  ([env name]
   (resolve-ns-alias env name (symbol name)))
  ([env name not-found]
   (let [sym (symbol name)]
     (get (:requires (:ns env)) sym not-found))))

(defn resolve-macro-ns-alias
  ([env name]
   (resolve-macro-ns-alias env name (symbol name)))
  ([env name not-found]
   (let [sym (symbol name)]
     (get (:require-macros (:ns env)) sym not-found))))

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
             #?(:clj (nil? (util/ns->source ns-sym)))
             (not (js-module-exists? ns-sym)))
    (warning :undeclared-ns env {:ns-sym ns-sym :js-provide ns-sym})))

(defn core-name?
  "Is sym visible from core in the current compilation namespace?"
  #?(:cljs {:tag boolean})
  [env sym]
  (and (or (some? (gets @env/*compiler* ::namespaces 'cljs.core :defs sym))
           (if-some [mac (get-expander sym env)]
             (let [^Namespace ns (-> mac meta :ns)]
               (= (.getName ns) #?(:clj 'cljs.core :cljs 'cljs.core$macros)))
             false))
       (not (contains? (-> env :ns :excludes) sym))))

(defn public-name?
  "Is sym public?"
  #?(:cljs {:tag boolean})
  [ns sym]
  (let [var-ast (or (gets @env/*compiler* ::namespaces ns :defs sym)
                    #?(:clj  (gets @env/*compiler* ::namespaces ns :macros sym)
                       :cljs (gets @env/*compiler* ::namespaces (symbol (str (name ns) "$macros")) :defs sym)))]
    (and (some? var-ast)
         (not (or (:private var-ast)
                  (:anonymous var-ast))))))

(defn js-tag? [x]
  (and (symbol? x)
       (or (= 'js x)
           (= "js" (namespace x)))))

(defn normalize-js-tag [x]
  ;; if not 'js, assume constructor
  (if-not (= 'js x)
    (with-meta 'js
      {:prefix (conj (->> (string/split (name x) #"\.")
                       (map symbol) vec)
                 'prototype)})
    x))

(defn ->type-set
  "Ensures that a type tag is a set."
  [t]
  (if #?(:clj  (set? t)
         :cljs (cljs-set? t))
    t
    #{t}))

(defn canonicalize-type [t]
  "Ensures that a type tag is either nil, a type symbol, or a non-singleton
  set of type symbols, absorbing clj-nil into seq and all types into any."
  (cond
    (symbol? t) t
    (empty? t) nil
    (== 1 (count t)) (first t)
    (contains? t 'any) 'any
    (contains? t 'seq) (let [res (disj t 'clj-nil)]
                         (if (== 1 (count res))
                           'seq
                           res))
    :else t))

(defn add-types
  "Produces a union of types."
  ([] 'any)
  ([t1] t1)
  ([t1 t2]
   (if (or (nil? t1)
           (nil? t2))
     'any
     (-> (set/union (->type-set t1) (->type-set t2))
       canonicalize-type)))
  ([t1 t2 & ts]
   (apply add-types (add-types t1 t2) ts)))

(def alias->type
  '{object   Object
    string   String
    number   Number
    array    Array
    function Function
    boolean  Boolean
    symbol   Symbol})

(defn has-extern?*
  ([pre externs]
   (let [pre (if-some [me (find
                            (get-in externs '[Window prototype])
                            (first pre))]
               (if-some [tag (-> me first meta :tag)]
                 (into [tag 'prototype] (next pre))
                 pre)
               pre)]
     (has-extern?* pre externs externs)))
  ([pre externs top]
   (cond
     (empty? pre) true
     :else
     (let [x  (first pre)
           me (find externs x)]
       (cond
         (not me) false
         :else
         (let [[x' externs'] me
               xmeta (meta x')]
           (if (and (= 'Function (:tag xmeta)) (:ctor xmeta))
             (or (has-extern?* (into '[prototype] (next pre)) externs' top)
                 (has-extern?* (next pre) externs' top))
             (recur (next pre) externs' top))))))))

(defn has-extern?
  ([pre]
    (has-extern? pre (get @env/*compiler* ::externs)))
  ([pre externs]
   (or (has-extern?* pre externs)
       (when (= 1 (count pre))
         (let [x (first pre)]
           (or (get-in externs (conj '[Window prototype] x))
               (get-in externs (conj '[Number] x)))))
       (-> (last pre) str (string/starts-with? "cljs$")))))

(defn js-tag
  ([pre]
   (js-tag pre :tag))
  ([pre tag-type]
   (js-tag pre tag-type (get @env/*compiler* ::externs)))
  ([pre tag-type externs]
   (js-tag pre tag-type externs externs))
  ([pre tag-type externs top]
   (when-let [[p externs' :as me] (find externs (first pre))]
     (let [tag (-> p meta tag-type)]
       (if (= (count pre) 1)
         (when tag (symbol "js" (str (alias->type tag tag))))
         (or (js-tag (next pre) tag-type externs' top)
             (js-tag (into '[prototype] (next pre)) tag-type (get top tag) top)))))))

(defn dotted-symbol? [sym]
  (let [s (str sym)]
    #?(:clj  (and (.contains s ".")
                  (not (.contains s "..")))
       :cljs (and ^boolean (goog.string/contains s ".")
                  (not ^boolean (goog.string/contains s ".."))))))

(defn munge-node-lib [name]
  (str "node$module$" (munge (string/replace (str name) #"[.\/]" #?(:clj "\\$"
                                                                    :cljs "$$")))))

(defn munge-global-export [name]
  (str "global$module$" (munge (string/replace (str name) #"[.\/]" #?(:clj "\\$"
                                                                      :cljs "$$")))))

(defn resolve-alias
  "Takes a namespace and an unqualified symbol and potentially returns a new
  symbol to be used in lieu of the original."
  [ns sym]
  ;; Conditionally alias aget/aset fns to checked variants
  (if (and (= 'cljs.core ns)
           ('#{aget aset} sym)
           (checked-arrays))
    (get-in '{:warn  {aget checked-aget
                      aset checked-aset}
              :error {aget checked-aget'
                      aset checked-aset'}}
      [(checked-arrays) sym])
    sym))

(defn ns->module-type [ns]
  (cond
    (js-module-exists? ns) :js
    (node-module-dep? ns) :node
    (dep-has-global-exports? ns) :global))

(defmulti resolve* (fn [env sym full-ns current-ns] (ns->module-type full-ns)))

(defmethod resolve* :js
  [env sym full-ns current-ns]
  {:name (symbol (str full-ns) (str (name sym)))
   :op :js-var
   :ns full-ns})

(defmethod resolve* :node
  [env sym full-ns current-ns]
  {:name (symbol (str current-ns) (str (munge-node-lib full-ns) "." (name sym)))
   :op :js-var
   :ns current-ns})

(defmethod resolve* :global
  [env sym full-ns current-ns]
  (let [pre (into '[Object] (->> (string/split (name sym) #"\.") (map symbol) vec))]
    (when-not (has-extern? pre)
      (swap! env/*compiler* update-in
        (into [::namespaces current-ns :externs] pre) merge {}))
    {:name (symbol (str current-ns) (str (munge-global-export full-ns) "." (name sym)))
     :op :js-var
     :ns current-ns
     :tag (with-meta 'js {:prefix pre})}))

(def ^:private private-var-access-exceptions
  "Specially-treated symbols for which we don't trigger :private-var-access warnings."
  '#{cljs.core/checked-aget
     cljs.core/checked-aset
     cljs.core/checked-aget'
     cljs.core/checked-aset'})

(defmethod resolve* :default
  [env sym full-ns current-ns]
  (let [sym-ast (gets @env/*compiler* ::namespaces full-ns :defs (symbol (name sym)))
        sym-name (symbol (str full-ns) (str (name sym)))]
    (when (and (not= current-ns full-ns)
               (:private sym-ast)
               (not *private-var-access-nowarn*)
               (not (contains? private-var-access-exceptions sym-name)))
      (warning :private-var-access env
        {:sym sym-name}))
    (merge sym-ast
      {:name sym-name
       :op :var
       :ns   full-ns})))

(defn required? [ns env]
  (or (contains? (set (vals (gets env :ns :requires))) ns)
      (contains? (set (vals (gets env :ns :uses))) ns)))

(defn invokeable-ns?
  "Returns true if ns is a required namespace and a JavaScript module that
   might be invokeable as a function."
  [ns env]
  (let [ns (resolve-ns-alias env ns)]
    (and (required? ns env)
         (or (js-module-exists? ns)
             (node-module-dep? ns)
             (dep-has-global-exports? ns)))))

(defn resolve-invokeable-ns [ns current-ns env]
  (let [ns (resolve-ns-alias env ns)
        module-type (ns->module-type ns)]
    (case module-type
      :js     {:name (symbol
                       (or (gets @env/*compiler* :js-module-index ns :name)
                           (resolve-ns-alias env ns)))
               :op :js-var
               :ns 'js}
      :node   {:name (symbol (str current-ns)
                       (munge-node-lib (resolve-ns-alias env ns)))
               :op :js-var
               :ns current-ns}
      :global {:name (symbol (str current-ns)
                       (munge-global-export (resolve-ns-alias env ns)))
               :op :js-var
               :ns current-ns})))

;; core.async calls `macroexpand-1` manually with an ill-formed
;; :locals map. Normally :locals maps symbols maps, but
;; core.async adds entries mapping symbols to symbols. We work
;; around that specific case here. This is called defensively
;; every time we lookup the :locals map.
(defn handle-symbol-local [sym lb]
  (if (symbol? lb)
    {:name sym}
    lb))

(defn resolve-var
  "Resolve a var. Accepts a side-effecting confirm fn for producing
   warnings about unresolved vars."
  ([env sym] (resolve-var env sym nil))
  ([env sym confirm]
   (let [locals (:locals env)]
     (if #?(:clj  (= "js" (namespace sym))
            :cljs (identical? "js" (namespace sym)))
       (let [symn (-> sym name symbol)
             shadowed-by-local (handle-symbol-local symn (get locals symn))]
         (cond
           (some? shadowed-by-local)
           (do (warning :js-shadowed-by-local env {:name sym})
               (assoc shadowed-by-local :op :local))

           :else
           (let [pre (->> (string/split (name sym) #"\.") (map symbol) vec)]
             (when (and (not (has-extern? pre))
                        ;; ignore exists? usage
                        (not (-> sym meta ::no-resolve)))
               (swap! env/*compiler* update-in
                 (into [::namespaces (-> env :ns :name) :externs] pre) merge {}))
             (merge
               {:name sym
                :op :js-var
                :ns   'js
                :tag  (with-meta (or (js-tag pre) (:tag (meta sym)) 'js) {:prefix pre})}
               (when-let [ret-tag (js-tag pre :ret-tag)]
                 {:js-fn-var true
                  :ret-tag ret-tag})))))
       (let [s  (str sym)
             lb (handle-symbol-local sym (get locals sym))
             current-ns (-> env :ns :name)]
         (cond
           (some? lb) (assoc lb :op :local)

           (some? (namespace sym))
           (let [ns      (namespace sym)
                 ns      (if #?(:clj  (= "clojure.core" ns)
                                :cljs (identical? "clojure.core" ns))
                           "cljs.core"
                           ns)
                 full-ns (resolve-ns-alias env ns
                           (or (and (js-module-exists? ns)
                                    (gets @env/*compiler* :js-module-index ns :name))
                             (symbol ns)))]
             (when (some? confirm)
               (when (not= current-ns full-ns)
                 (confirm-ns env full-ns))
               (confirm env full-ns (symbol (name sym))))
             (resolve* env sym full-ns current-ns))

           (dotted-symbol? sym)
           (let [idx    (.indexOf s ".")
                 prefix (symbol (subs s 0 idx))
                 suffix (subs s (inc idx))]
             (if-some [lb (handle-symbol-local prefix (get locals prefix))]
               {:op :local
                :name (symbol (str (:name lb) "." suffix))}
               (if-some [full-ns (gets @env/*compiler* ::namespaces current-ns :imports prefix)]
                 {:op :js-var
                  :name (symbol (str full-ns) suffix)}
                 (if-some [info (gets @env/*compiler* ::namespaces current-ns :defs prefix)]
                   (merge info
                     {:name (symbol (str current-ns) (str sym))
                      :op :var
                      :ns current-ns})
                   (merge (gets @env/*compiler* ::namespaces prefix :defs (symbol suffix))
                     {:name (if (= "" prefix) (symbol suffix) (symbol (str prefix) suffix))
                      :op :var
                      :ns prefix})))))

           (some? (gets @env/*compiler* ::namespaces current-ns :uses sym))
           (let [full-ns (gets @env/*compiler* ::namespaces current-ns :uses sym)]
             (resolve* env sym full-ns current-ns))

           (some? (gets @env/*compiler* ::namespaces current-ns :renames sym))
           (let [qualified-symbol (gets @env/*compiler* ::namespaces current-ns :renames sym)
                 full-ns (symbol (namespace qualified-symbol))
                 sym     (symbol (name qualified-symbol))]
             (resolve* env sym full-ns current-ns))

           (some? (gets @env/*compiler* ::namespaces current-ns :imports sym))
           (recur env (gets @env/*compiler* ::namespaces current-ns :imports sym) confirm)

           (some? (gets @env/*compiler* ::namespaces current-ns :defs sym))
           (do
             (when (some? confirm)
               (confirm env current-ns sym))
             (merge (gets @env/*compiler* ::namespaces current-ns :defs sym)
               {:name (symbol (str current-ns) (str sym))
                :op :var
                :ns current-ns}))

           (core-name? env sym)
           (do
             (when (some? confirm)
               (confirm env 'cljs.core sym))
             (merge (gets @env/*compiler* ::namespaces 'cljs.core :defs sym)
               {:name (symbol "cljs.core" (str sym))
                :op :var
                :ns 'cljs.core}))

           (invokeable-ns? s env)
           (resolve-invokeable-ns s current-ns env)

           :else
           (do
             (when (some? confirm)
               (confirm env current-ns sym))
             (merge (gets @env/*compiler* ::namespaces current-ns :defs sym)
               {:name (symbol (str current-ns) (str sym))
                :op :var
                :ns current-ns}))))))))

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
      (some? (namespace sym))
      (let [ns (namespace sym)
            ns (if (= "clojure.core" ns) "cljs.core" ns)
            full-ns (resolve-macro-ns-alias env ns)
            #?@(:cljs [full-ns (if-not (string/ends-with? (str full-ns) "$macros")
                                 (symbol (str full-ns "$macros"))
                                 full-ns)])]
        #?(:clj (get-in namespaces [full-ns :macros (symbol (name sym))])
           :cljs (get-in namespaces [full-ns :defs (symbol (name sym))])))

      (some? (get-in namespaces [ns :use-macros sym]))
      (let [full-ns (get-in namespaces [ns :use-macros sym])]
        (get-in namespaces [full-ns :macros sym]))

      (some? (get-in namespaces [ns :rename-macros sym]))
      (let [qualified-symbol (get-in namespaces [ns :rename-macros sym])
            full-ns (symbol (namespace qualified-symbol))
            sym     (symbol (name qualified-symbol))]
        (get-in namespaces [full-ns :macros sym]))

      :else
      (let [ns (cond
                 (some? (get-in namespaces [ns :macros sym])) ns
                 (core-name? env sym) #?(:clj  'cljs.core
                                         :cljs CLJS_CORE_MACROS_SYM))]
        (when (some? ns)
          #?(:clj  (get-in namespaces [ns :macros sym])
             :cljs (get-in namespaces [ns :defs sym])))))))

(declare analyze analyze-symbol analyze-seq)

;; Note: This is the set of parse multimethod dispatch values,
;; along with '&, and differs from cljs.core/special-symbol?
(def specials '#{if def fn* do let* loop* letfn* throw try recur new set!
                 ns deftype* defrecord* . js* & quote case* var ns*})

(def ^:dynamic *recur-frames* nil)
(def ^:dynamic *loop-lets* ())
(def ^:dynamic *allow-redef* false)
(def ^:dynamic *allow-ns* true)

#?(:clj
   (defmacro disallowing-recur [& body]
     `(binding [*recur-frames* (cons nil *recur-frames*)] ~@body)))

#?(:clj
   (defmacro allowing-redef [& body]
     `(binding [*allow-redef* true] ~@body)))

#?(:clj
   (defmacro disallowing-ns* [& body]
     `(binding [*allow-ns* false] ~@body)))

;; TODO: move this logic out - David
(defn analyze-keyword
  [env sym]
  (register-constant! env sym)
  {:op :const :val sym :env env :form sym :tag 'cljs.core/Keyword})

(defn get-tag [e]
  (if-some [tag (-> e :form meta :tag)]
    tag
    (if-some [tag (-> e :tag)]
      tag
      (-> e :info :tag))))

(defn find-matching-method [f params]
  ;; if local fn, need to look in :info
  (let [methods (or (:methods f) (-> f :info :methods))
        c       (count params)]
    (some
      (fn [m]
        (and (or (== (:fixed-arity m) c)
                 (:variadic? m))
             m))
      methods)))

(defn type?
  #?(:cljs {:tag boolean})
  [env t]
  ;; don't use resolve-existing-var to avoid warnings
  (when (and (some? t) (symbol? t))
    (let [var (resolve-var env t)]
      (if-some [type (:type var)]
        type
          (if-some [type (-> var :info :type)]
            type
              (if-some [proto (:protocol-symbol var)]
                proto
                (get '#{cljs.core/PersistentHashMap cljs.core/List} t)))))))

(declare infer-tag)

(def NOT_NATIVE '#{clj not-native})

(def BOOLEAN_OR_SEQ '#{boolean seq})

(defn unwrap-quote [{:keys [op] :as expr}]
  (if #?(:clj (= op :quote)
         :cljs (keyword-identical? op :quote))
    (:expr expr)
    expr))

(defn infer-if [env e]
  (let [{:keys [op form]} (unwrap-quote (:test e))
        then-tag (infer-tag env (:then e))]
    (if (and #?(:clj (= op :const)
                :cljs (keyword-identical? op :const))
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
          (and (or (some? (get NOT_NATIVE then-tag)) (type? env then-tag))
               (or (some? (get NOT_NATIVE else-tag)) (type? env else-tag)))
          'clj
          :else
          (if (and (some? (get BOOLEAN_OR_SEQ then-tag))
                   (some? (get BOOLEAN_OR_SEQ else-tag)))
            'seq
            (let [then-tag (if #?(:clj (set? then-tag)
                                  :cljs (cljs-set? then-tag))
                             then-tag #{then-tag})
                  else-tag (if #?(:clj (set? else-tag)
                                  :cljs (cljs-set? else-tag))
                             else-tag #{else-tag})]
              (into then-tag else-tag))))))))

(defn infer-invoke [env {f :fn :keys [args] :as e}]
  (let [me (assoc (find-matching-method f args) :op :fn-method)]
    (if-some [ret-tag (infer-tag env me)]
      ret-tag
      (let [{:keys [info]} f]
        (if-some [ret-tag (if (or (true? (:fn-var info))
                                  (true? (:js-fn-var info)))
                            (:ret-tag info)
                            (when (= 'js (:ns info)) 'js))]
          ret-tag
          ANY_SYM)))))

(defn infer-tag
  "Given env, an analysis environment, and e, an AST node, return the inferred
   type of the node"
  [env e]
    (if-some [tag (get-tag e)]
      tag
      (case (:op e)
        :recur    IGNORE_SYM
        :throw    IGNORE_SYM
        :let      (infer-tag env (:body e))
        :loop     (infer-tag env (:body e))
        :do       (infer-tag env (:ret e))
        :fn-method (infer-tag env (:body e))
        :def      (infer-tag env (:init e))
        :invoke   (infer-invoke env e)
        :if       (infer-if env e)
        :const    (case (:form e)
                    true BOOLEAN_SYM
                    false BOOLEAN_SYM
                    ANY_SYM)
        :quote    (infer-tag env (:expr e))
        (:var :local :js-var :binding)
                  (if-some [init (:init e)]
                    (infer-tag env init)
                    (infer-tag env (:info e)))
        (:host-field :host-call)      ANY_SYM
        :js       ANY_SYM
        nil)))

(defmulti parse (fn [op & rest] op))

(defn var-meta
  ([var]
    (var-meta var nil))
  ([var expr-env]
   (let [sym (:name var)
         ks [:ns :doc :file :line :column]
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
     (if expr-env
       (analyze expr-env m)
       m))))

(defn var-ast
  [env sym]
  ;; we need to dissoc locals for the `(let [x 1] (def x x))` case, because we
  ;; want the var's AST and `resolve-var` will check locals first. - António Monteiro
  (binding [*private-var-access-nowarn* true]
    (let [env      (dissoc env :locals)
          var      (resolve-var env sym (confirm-var-exists-throw))
          expr-env (assoc env :context :expr)]
      (when-some [var-ns (:ns var)]
        {:var  (analyze expr-env sym)
         :sym  (analyze expr-env `(quote ~(symbol (name var-ns) (name (:name var)))))
         :meta (var-meta var expr-env)}))))

(defmethod parse 'var
  [op env [_ sym :as form] _ _]
  (when (not= 2 (count form))
    (throw (error env "Wrong number of args to var")))
  (when-not (symbol? sym)
    (throw (error env "Argument to var must be symbol")))
  (merge
    {:env env
     :op :the-var
     :children [:var :sym :meta]
     :form form}
    (var-ast env sym)))

(def ^:private predicate->tag
  '{
    ;; Base values
    cljs.core/nil?            clj-nil
    cljs.core/undefined?      clj-nil
    cljs.core/false?          boolean
    cljs.core/true?           boolean
    cljs.core/zero?           number
    cljs.core/infinite?       number

    ;; Base types
    cljs.core/boolean?        boolean
    cljs.core/string?         string
    cljs.core/char?           string
    cljs.core/number?         number
    cljs.core/integer?        number
    cljs.core/float?          number
    cljs.core/double?         number
    cljs.core/array?          array
    cljs.core/seq?            seq

    ;; JavaScript types
    cljs.core/regexp?         js/RegExp

    ;; Types
    cljs.core/keyword?        cljs.core/Keyword
    cljs.core/var?            cljs.core/Var
    cljs.core/symbol?         cljs.core/Symbol
    cljs.core/volatile?       cljs.core/Volatile
    cljs.core/delay?          cljs.core/Delay
    cljs.core/reduced?        cljs.core/Reduced

    ;;; Note: For non-marker protocol entries below, we
    ;;; omit predicates that are based on satisfies? because
    ;;; we cannot safely apply the fast-path optimization
    ;;; which is enabled when the protocol type is inferred.
    ;;; If adding a non-marker entry here, also add a test to
    ;;; cljs.extend-to-native-test/test-extend-to-protocols.

    ;; Protocols
    cljs.core/map-entry?      cljs.core/IMapEntry
    cljs.core/uuid?           cljs.core/IUUID
    cljs.core/tagged-literal? cljs.core/ITaggedLiteral
    cljs.core/inst?           cljs.core/Inst
    cljs.core/sequential?     cljs.core/ISequential
    cljs.core/list?           cljs.core/IList
    cljs.core/record?         cljs.core/IRecord
    cljs.core/chunked-seq?    cljs.core/IChunkedSeq

    ;; Composites
    cljs.core/seqable?        #{cljs.core/ISeqable array string}
    cljs.core/ident?          #{cljs.core/Keyword cljs.core/Symbol}
    })

(defn- simple-predicate-induced-tag
  "Look for a predicate-induced tag when the test expression is a simple
   application of a predicate to a local, as in (string? x)."
  [env test]
  (when (and (list? test)
             (== 2 (count test))
             (every? symbol? test))
    (let [analyzed-fn (no-warn (analyze (assoc env :context :expr) (first test)))]
      (when (= :var (:op analyzed-fn))
        (when-let [tag (predicate->tag (:name analyzed-fn))]
          (let [sym (last test)]
            (when (and (nil? (namespace sym))
                       (get-in env [:locals sym]))
              [sym tag])))))))

(defn- type-check-induced-tag
  "Look for a type-check-induced tag when the test expression is the use of
   instance? on a local, as in (instance? ICounted x)."
  [env test]
  (when (and (list? test)
             (== 3 (count test))
             (every? symbol? test))
    (let [analyzed-fn (no-warn (analyze (assoc env :context :expr) (first test)))]
      (when (= :var (:op analyzed-fn))
        (when ('#{cljs.core/instance?} (:name analyzed-fn))
          (let [analyzed-type (no-warn (analyze (assoc env :context :expr) (second test)))
                tag (:name analyzed-type)
                sym (last test)]
            (when (and (= :var (:op analyzed-type))
                       (nil? (namespace sym))
                       (get-in env [:locals sym]))
              [sym tag])))))))

(defn- truth-induced-tag
  "Refine a tag to exclude clj-nil if the test is a simple symbol."
  [env test]
  (when (and (symbol? test)
             (nil? (namespace test)))
    (let [analyzed-symbol (no-warn (analyze (assoc env :context :expr) test))]
      (when-let [tag (:tag analyzed-symbol)]
        (when (and (set? tag)
                   (contains? tag 'clj-nil))
          [test (canonicalize-type (disj tag 'clj-nil))])))))

(defn- set-test-induced-tags
  "Looks at the test and sets any tags which are induced by virtue
  of the test being truthy. For example in (if (string? x) x :bar)
  the local x in the then branch must be of string type."
  [env test]
  (let [[local tag] (or (simple-predicate-induced-tag env test)
                        (type-check-induced-tag env test)
                        (truth-induced-tag env test))]
    (cond-> env
      local (assoc-in [:locals local :tag] tag))))

(defmethod parse 'if
  [op env [_ test then else :as form] name _]
  (when (< (count form) 3)
    (throw (compile-syntax-error env "Too few arguments to if" 'if)))
  (when (> (count form) 4)
    (throw (compile-syntax-error env "Too many arguments to if" 'if)))
  (let [test-expr (disallowing-recur (analyze (assoc env :context :expr) test))
        then-expr (allowing-redef (analyze (set-test-induced-tags env test) then))
        else-expr (allowing-redef (analyze env else))]
    {:env env :op :if :form form
     :test test-expr :then then-expr :else else-expr
     :unchecked *unchecked-if*
     :children [:test :then :else]}))

(defmethod parse 'case*
  [op env [_ sym tests thens default :as form] name _]
  (assert (symbol? sym) "case* must switch on symbol")
  (assert (every? vector? tests) "case* tests must be grouped in vectors")
  (let [expr-env (assoc env :context :expr)
        v        (disallowing-recur (analyze expr-env sym))
        tests    (mapv #(mapv (fn [t] (analyze expr-env t)) %) tests)
        thens    (mapv #(analyze env %) thens)
        nodes    (mapv (fn [tests then]
                         {:op :case-node
                          ;synthetic node, no :form
                          :env env
                          :tests (mapv (fn [test]
                                         {:op :case-test
                                          :form (:form test)
                                          :env expr-env
                                          :test test
                                          :children [:test]})
                                       tests)
                          :then {:op :case-then
                                 :form (:form then)
                                 :env env
                                 :then then
                                 :children [:then]}
                          :children [:tests :then]})
                       tests
                       thens)
        default  (analyze env default)]
    (assert (every? (fn [t]
                      (or
                        (-> t :info :const)
                        (and (= :const (:op t))
                             ((some-fn number? string? char?) (:form t)))))
              (apply concat tests))
      "case* tests must be numbers, strings, or constants")
    {:env env :op :case :form form
     :test v :nodes nodes :default default
     :children [:test :nodes :default]}))

(defmethod parse 'throw
  [op env [_ throw-form :as form] name _]
  (cond
    (= 1 (count form))
    (throw
      (error env "Too few arguments to throw, throw expects a single Error instance"))
    (< 2 (count form))
    (throw
      (error env "Too many arguments to throw, throw expects a single Error instance")))
  (let [throw-expr (disallowing-recur (analyze (assoc env :context :expr) throw-form))]
    {:env env :op :throw :form form
     :exception throw-expr
     :children [:exception]}))

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
                  (-> (disallowing-recur (analyze (assoc env :context :statement) `(do ~@(rest fblock))))
                      (assoc :body? true)))
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
                (disallowing-recur (analyze (assoc catchenv :locals locals) cblock)))
        try (disallowing-recur (analyze (if (or e finally) catchenv env) `(do ~@body)))]

    {:env env :op :try :form form
     :body (assoc try :body? true)
     :finally finally
     :name e
     :catch catch
     :children (vec
                 (concat [:body]
                         (when catch
                           [:catch])
                         (when finally
                           [:finally])))}))

(defn valid-proto [x]
  (when (symbol? x) x))

(defn elide-env [env ast opts]
  (dissoc ast :env))

(defn replace-env-pass [new-env]
  (fn [env ast opts]
    (assoc ast :env new-env)))

(defn ast-children [ast]
  (mapcat (fn [c]
            (let [g (get ast c)]
              (cond
                (vector? g) g
                g [g])))
          (:children ast)))

(defn constant-value?
  [{:keys [op] :as ast}]
  (or (#{:quote :const} op)
      (and (#{:map :set :vector} op)
           (every? constant-value? (ast-children ast)))))

(defn const-expr->constant-value [{:keys [op] :as e}]
  (case op
    :quote  (const-expr->constant-value (:expr e))
    :const  (:val e)
    :map    (zipmap (map const-expr->constant-value (:keys e))
                    (map const-expr->constant-value (:vals e)))
    :set    (into #{} (map const-expr->constant-value (:items e)))
    :vector (into [] (map const-expr->constant-value (:items e)))))

(defn- earmuffed? [sym]
  (let [s (name sym)]
    (and (> (count s) 2)
         (string/starts-with? s "*")
         (string/ends-with? s "*"))))

(defn- core-ns? [ns-sym]
  (let [s (name ns-sym)]
    (and (not= 'cljs.user ns-sym)
         (or (string/starts-with? s "cljs.")
             (string/starts-with? s "clojure.")))))

(defmethod parse 'def
  [op env form _ _]
  (when (> (count form) 4)
    (throw (error env "Too many arguments to def")))
  (let [pfn (fn
              ([_ sym] {:sym sym})
              ([_ sym init] {:sym sym :init init})
              ([_ sym doc init] {:sym sym :doc doc :init init}))
        args (apply pfn form)
        sym (:sym args)
        const? (-> sym meta :const)
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
    (when (some? (get-in @env/*compiler* [::namespaces clash-ns]))
      (warning :ns-var-clash env
        {:ns (symbol (str ns-name "." sym))
         :var (symbol (str ns-name) (str sym))}))
    (when (some? (:const (resolve-var (dissoc env :locals) sym)))
      (throw (error env "Can't redefine a constant")))
    (when-some [doc (:doc args)]
      (when-not (string? doc)
        (throw (error env "Too many arguments to def"))))
    (when (and (not dynamic)
               (earmuffed? sym)
               (not (core-ns? ns-name)))
      (warning :non-dynamic-earmuffed-var env
        {:var (str sym)}))
    (when-some [v (get-in @env/*compiler* [::namespaces ns-name :defs sym])]
      (when (and (not *allow-redef*)
                 (not (:declared v))
                 (not (:declared sym-meta))
                 *file-defs*
                 (get @*file-defs* sym))
        (warning :redef-in-file env {:sym sym :line (:line v)}))
      (when (and (:declared v)
                 (:arglists v)
                 (not= (:arglists v) (:arglists sym-meta)))
        (warning :declared-arglists-mismatch env {:ns-name  ns-name :sym sym
                                                  :declared (second (:arglists v))
                                                  :defined  (second (:arglists sym-meta))})))
    (let [env (if (or (and (not= ns-name 'cljs.core)
                           (core-name? env sym))
                      (some? (get-in @env/*compiler* [::namespaces ns-name :uses sym])))
                (let [ev (resolve-existing-var (dissoc env :locals)
                           ;; ::no-resolve true is to suppress "can't take value
                           ;; of macro warning" when sym resolves to a macro
                           (with-meta sym {::no-resolve true}))
                      conj-to-set (fnil conj #{})]
                  (when (public-name? (:ns ev) sym)
                    (warning :redef env {:sym sym :ns (:ns ev) :ns-name ns-name}))
                  (swap! env/*compiler* update-in [::namespaces ns-name :excludes]
                     conj-to-set sym)
                  (update-in env [:ns :excludes] conj-to-set sym))
                env)
          var-name (:name (resolve-var (dissoc env :locals) sym))
          init-expr (when (contains? args :init)
                      (swap! env/*compiler* assoc-in [::namespaces ns-name :defs sym]
                        (merge
                          {:name var-name}
                          sym-meta
                          (when (true? dynamic) {:dynamic true})
                          (source-info var-name env)))
                      (disallowing-recur
                        (disallowing-ns*
                          (analyze (assoc env :context :expr) (:init args) sym))))
          fn-var? (and (some? init-expr) (= (:op init-expr) :fn))
          tag (if fn-var?
                (or (:ret-tag init-expr) tag (:inferred-ret-tag init-expr))
                (or tag (:tag init-expr)))
          export-as (when-let [export-val (-> sym meta :export)]
                      (if (= true export-val) var-name export-val))
          doc (or (:doc args) (-> sym meta :doc))]
      (when-some [v (get-in @env/*compiler* [::namespaces ns-name :defs sym])]
        (when (and (not (-> sym meta :declared))
                   (and (true? (:fn-var v)) (not fn-var?)))
          (warning :fn-var env {:ns-name ns-name :sym sym})))

      ;; declare must not replace any analyzer data of an already def'd sym
      (when (or (nil? (get-in @env/*compiler* [::namespaces ns-name :defs sym]))
                (not (:declared sym-meta)))
        (when *file-defs*
          (swap! *file-defs* conj sym))

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
            (when const?
              (let [const-expr
                    (binding [*passes* (conj *passes* (replace-env-pass {:context :expr}))]
                      (analyze env (:init args)))]
                (when (constant-value? const-expr)
                  {:const-expr const-expr})))
            (when (true? dynamic) {:dynamic true})
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
                  {:fn-var (not (:macro sym-meta))
                   ;; protocol implementation context
                   :protocol-impl (:protocol-impl init-expr)
                   ;; inline protocol implementation context
                   :protocol-inline (:protocol-inline init-expr)}
                  (if-some [top-fn-meta (:top-fn sym-meta)]
                    top-fn-meta
                    {:variadic? (:variadic? init-expr)
                     :max-fixed-arity (:max-fixed-arity init-expr)
                     :method-params params
                     :arglists (:arglists sym-meta)
                     :arglists-meta (doall (map meta (:arglists sym-meta)))}))))
            (when (and (:declared sym-meta)
                       (:arglists sym-meta))
              {:declared true
               :fn-var true
               :method-params (second (:arglists sym-meta))})
            (if (and fn-var? (some? tag))
              {:ret-tag tag}
              (when tag {:tag tag})))))
      (merge
        {:env env
         :op :def
         :form form
         :ns ns-name
         :name var-name
         :var (assoc
                (analyze
                  (-> env (dissoc :locals)
                    (assoc :context :expr)
                    (assoc :def-var true))
                  sym)
                :op :var)
         :doc doc
         :jsdoc (:jsdoc sym-meta)}
        (when (true? (:def-emits-var env))
          {:var-ast (var-ast env sym)})
        (when-some [test (:test sym-meta)]
          {:test (analyze (assoc env :context :expr) test)})
        (when (some? tag)
          (if fn-var?
            {:ret-tag tag}
            {:tag tag}))
        (when (true? dynamic) {:dynamic true})
        (when (some? export-as) {:export export-as})
        (if (some? init-expr)
          {:init init-expr
           :children [:var :init]}
          {:children [:var]})))))

(defn analyze-fn-method-param [env]
  (fn [[locals params] [arg-id name]]
    (when (namespace name)
      (throw (error env (str "Can't use qualified name as parameter: " name))))
    (let [line   (get-line name env)
          column (get-col name env)
          nmeta  (meta name)
          tag    (:tag nmeta)
          shadow (when (some? locals)
                   (handle-symbol-local name (locals name)))
          env    (merge (select-keys env [:context])
                   {:line line :column column})
          param  {:op :binding
                  :name name
                  :line line
                  :column column
                  :tag tag
                  :shadow shadow
                  :local :arg
                  :arg-id arg-id
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

(defn- analyze-fn-method [env locals form type analyze-body?]
  (let [param-names     (first form)
        variadic        (boolean (some '#{&} param-names))
        param-names     (vec (remove '#{&} param-names))
        body            (next form)
        step            (analyze-fn-method-param env)
        step-init       [locals []]
        [locals params] (reduce step step-init (map-indexed vector param-names))
        params'         (if (true? variadic)
                          (butlast params)
                          params)
        fixed-arity     (count params')
        recur-frame     {:protocol-impl (:protocol-impl env)
                         :params        params
                         :flag          (atom nil)
                         :tags          (atom [])}
        recur-frames    (cons recur-frame *recur-frames*)
        body-env        (assoc env :context :return :locals locals)
        body-form       `(do ~@body)
        expr            (when analyze-body?
                          (analyze-fn-method-body body-env body-form recur-frames))
        recurs          @(:flag recur-frame)]
    (merge
      {:env env
       :op :fn-method
       :variadic? variadic
       :params params
       :fixed-arity fixed-arity
       :type type
       :form form
       :recurs recurs}
      (if (some? expr)
        {:body (assoc expr :body? true)
         :children [:params :body]}
        {:children [:params]}))))

(declare analyze-wrap-meta)

(defn fn-name-var [env locals name]
  (when (some? name)
    (let [ns       (-> env :ns :name)
          shadow   (handle-symbol-local name (get locals name))
          shadow   (when (nil? shadow)
                     (get-in env [:js-globals name]))
          fn-scope (:fn-scope env)
          name-var {:name name
                    :op :binding
                    :local :fn
                    :info {:fn-self-name true
                           :fn-scope fn-scope
                           :ns ns
                           :shadow shadow}}
          tag      (-> name meta :tag)
          ret-tag  (when (some? tag)
                     {:ret-tag tag})]
      (merge name-var ret-tag))))

(defn analyze-fn-methods-pass2* [menv locals type meths]
  (mapv #(analyze-fn-method menv locals % type true) meths))

(defn analyze-fn-methods-pass2 [menv locals type meths]
  (analyze-fn-methods-pass2* menv locals type meths))

(defmethod parse 'fn*
  [op env [_ & args :as form] name _]
  (let [named-fn?    (symbol? (first args))
        [name meths] (if named-fn?
                         [(first args) (next args)]
                         [name (seq args)])
        ;; turn (fn [] ...) into (fn ([]...))
        meths        (if (vector? (first meths))
                       (list meths)
                       meths)
        locals       (:locals env)
        name-var     (fn-name-var env locals name)
        env          (if (some? name)
                       (update-in env [:fn-scope] conj name-var)
                       env)
        locals       (if (and (some? locals)
                              named-fn?)
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
        methods      (map #(disallowing-ns* (analyze-fn-method menv locals % type (nil? name))) meths)
        mfa          (transduce (map :fixed-arity) max 0 methods)
        variadic     (boolean (some :variadic? methods))
        locals       (if named-fn?
                       (update-in locals [name] assoc
                         ;; TODO: can we simplify? - David
                         :fn-var true
                         :variadic? variadic
                         :max-fixed-arity mfa
                         :method-params (map :params methods))
                       locals)
        methods      (if (some? name)
                       ;; a second pass with knowledge of our function-ness/arity
                       ;; lets us optimize self calls
                       (disallowing-ns* (analyze-fn-methods-pass2 menv locals type meths))
                       (vec methods))
        form         (vary-meta form dissoc ::protocol-impl ::protocol-inline ::type)
        js-doc       (when (true? variadic)
                       "@param {...*} var_args")
        children     (if (some? name-var)
                       [:local :methods]
                       [:methods])
        inferred-ret-tag (let [inferred-tags (map (partial infer-tag env) (map :body methods))]
                           (when (apply = inferred-tags)
                             (first inferred-tags)))
        ast   (merge {:op :fn
                      :env env
                      :form form
                      :name name-var
                      :methods methods
                      :variadic? variadic
                      :tag 'function
                      :inferred-ret-tag inferred-ret-tag
                      :recur-frames *recur-frames*
                      :loop-lets *loop-lets*
                      :jsdoc [js-doc]
                      :max-fixed-arity mfa
                      :protocol-impl proto-impl
                      :protocol-inline proto-inline
                      :children children}
                     (when (some? name-var)
                       {:local name-var}))]
    (let [variadic-methods (into []
                             (comp (filter :variadic?) (take 1))
                             methods)
          variadic-params  (if (pos? (count variadic-methods))
                             (count (:params (nth variadic-methods 0)))
                             0)
          param-counts     (into [] (map (comp count :params)) methods)]
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
                              :op :binding
                              :fn-var true
                              :line (get-line n env)
                              :column (get-col n env)
                              :local :letfn
                              :shadow (handle-symbol-local n (locals n))
                              :variadic? (:variadic? fexpr)
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
                              :variadic? (:variadic? fexpr)
                              :max-fixed-arity (:max-fixed-arity fexpr)
                              :method-params (map :params (:methods fexpr)))]
                    [(assoc-in env [:locals name] be')
                     (conj bes be')]))
          [meth-env []] bes)
        expr (-> (analyze (assoc meth-env :context (if (= :expr context) :return context)) `(do ~@exprs))
                 (assoc :body? true))]
    {:env env :op :letfn :bindings bes :body expr :form form
     :children [:bindings :body]}))

(defn analyze-do-statements* [env exprs]
  (mapv #(analyze (assoc env :context :statement) %) (butlast exprs)))

(defn analyze-do-statements [env exprs]
  (disallowing-recur (analyze-do-statements* env exprs)))

(defmethod parse 'do
  [op env [_ & exprs :as form] _ _]
  (let [statements (analyze-do-statements env exprs)]
    (if (<= (count exprs) 1)
      (let [ret      (analyze env (first exprs))
            children [:statements :ret]]
        {:op :do
         :env env
         :form form
         :statements statements :ret ret
         :children children})
      (let [ret-env  (if (= :statement (:context env))
                       (assoc env :context :statement)
                       (assoc env :context :return))
            ret      (analyze ret-env (last exprs))
            children [:statements :ret]]
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
  (if-some [tag (-> name meta :tag)]
      tag
        (if-some [tag (-> init-expr :tag)]
          tag
          (-> init-expr :info :tag))))

(defn analyze-let-bindings* [encl-env bindings op]
  (loop [bes []
         env (assoc encl-env :context :expr)
         bindings (seq (partition 2 bindings))]

      (if-some [[name init] (first bindings)]
        (let []
          (when (or (some? (namespace name))
                  #?(:clj  (.contains (str name) ".")
                     :cljs ^boolean (goog.string/contains (str name) ".")))
            (throw (error encl-env (str "Invalid local name: " name))))
          (let [init-expr (analyze-let-binding-init env init (cons {:params bes} *loop-lets*))
                line (get-line name env)
                col (get-col name env)
                shadow (handle-symbol-local name (get-in env [:locals name]))
                be {:name name
                    :line line
                    :column col
                    :init init-expr
                    :tag (get-let-tag name init-expr)
                    :local op
                    :shadow shadow
                    ;; Give let* bindings same shape as var so
                    ;; they get routed correctly in the compiler
                    :op :binding
                    :env {:line line :column col}
                    :info {:name name
                           :shadow shadow}
                    :binding-form? true}
                be (if (= :fn (:op init-expr))
                     ;; TODO: can we simplify - David
                     (merge be
                       {:fn-var true
                        ;; copy over the :fn-method information we need for invoke type inference
                        :methods (into [] (map #(select-keys % [:tag :fixed-arity :variadic?]) (:methods init-expr)))
                        :variadic? (:variadic? init-expr)
                        :max-fixed-arity (:max-fixed-arity init-expr)
                        :method-params (map :params (:methods init-expr))})
                     be)
                be (add-identity be)]
            (recur (conj bes be)
              (assoc-in env [:locals name] be)
              (next bindings))))
        [bes env])))

(defn analyze-let-bindings [encl-env bindings op]
  (disallowing-recur (analyze-let-bindings* encl-env bindings op)))

(defn analyze-let-body* [env context exprs]
  (analyze (assoc env :context (if (= :expr context) :return context)) `(do ~@exprs)))

(defn analyze-let-body [env context exprs recur-frames loop-lets]
  (binding [*recur-frames* recur-frames
            *loop-lets* loop-lets]
    (analyze-let-body* env context exprs)))

(defn analyze-let
  [encl-env [_ bindings & exprs :as form] is-loop widened-tags]
  (when-not (and (vector? bindings) (even? (count bindings)))
    (throw (error encl-env "bindings must be vector of even number of elements")))
  (let [context      (:context encl-env)
        op           (if (true? is-loop) :loop :let)
        bindings     (if widened-tags
                       (vec (mapcat
                              (fn [[name init] widened-tag]
                                [(vary-meta name assoc :tag widened-tag) init])
                              (partition 2 bindings)
                              widened-tags))
                       bindings)
        [bes env]    (analyze-let-bindings encl-env bindings op)
        recur-frame  (when (true? is-loop)
                       {:params bes
                        :flag (atom nil)
                        :tags (atom (mapv :tag bes))})
        recur-frames (if recur-frame
                       (cons recur-frame *recur-frames*)
                       *recur-frames*)
        loop-lets    (cond
                       (true? is-loop) *loop-lets*
                       (some? *loop-lets*) (cons {:params bes} *loop-lets*))
        ;; Accumulate warnings for deferred replay iff there's a possibility of re-analyzing
        warn-acc     (when (and is-loop
                                (not widened-tags))
                       (atom []))
        expr         (if warn-acc
                       (with-warning-handlers [(accumulating-warning-handler warn-acc)]
                         (analyze-let-body env context exprs recur-frames loop-lets))
                       (analyze-let-body env context exprs recur-frames loop-lets))
        children     [:bindings :body]
        nil->any     (fnil identity 'any)]
    (if (and is-loop
             (not widened-tags)
             (not= (mapv nil->any @(:tags recur-frame))
                   (mapv (comp nil->any :tag) bes)))
      (recur encl-env form is-loop @(:tags recur-frame))
      (do
        (when warn-acc
          (replay-accumulated-warnings warn-acc))
        {:op       op
         :env      encl-env
         :bindings bes
         :body     (assoc expr :body? true)
         :form     form
         :children children}))))

(defmethod parse 'let*
  [op encl-env form _ _]
  (analyze-let encl-env form false nil))

(defmethod parse 'loop*
  [op encl-env form _ _]
  (analyze-let encl-env form true nil))

(defmethod parse 'recur
  [op env [_ & exprs :as form] _ _]
  (let [context (:context env)
        frame (first *recur-frames*)
        ;; Add dummy implicit target object if recuring to proto impl method head
        add-implicit-target-object? (and (:protocol-impl frame)
                                         (= (count exprs) (dec (count (:params frame)))))
        exprs (cond->> exprs add-implicit-target-object? (cons nil))
        exprs (disallowing-recur (vec (map #(analyze (assoc env :context :expr) %) exprs)))]
    (when-not frame
      (throw (error env "Can't recur here")))
    (when-not (= (count exprs) (count (:params frame)))
      (throw (error env (str "recur argument count mismatch, expected: "
                          (count (:params frame)) " args, got: " (count exprs)))))
    (when (and (:protocol-impl frame)
               (not add-implicit-target-object?))
      (warning :protocol-impl-recur-with-target env {:form (:form (first exprs))}))
    (reset! (:flag frame) true)
    (swap! (:tags frame) (fn [tags]
                           (mapv (fn [tag expr]
                                   (add-types tag (:tag expr)))
                             tags exprs)))
    (assoc {:env env :op :recur :form form}
      :frame frame
      :exprs exprs
      :children [:exprs])))

(defn analyze-const
  [env form]
  (let [;; register constants
        {:keys [tag]} (analyze (assoc env :quoted? true) form)]
    {:op       :const
     :env      env
     :literal? true
     :val      form
     :tag      tag
     :form     form}))

(defmethod parse 'quote
  [_ env [_ x :as form] _ _]
  (when (not= 2 (count form))
    (throw (error env "Wrong number of args to quote")))
  (let [expr (analyze-const env x)]
    {:op :quote
     :expr expr
     :env env
     :form form
     :tag (:tag expr)
     :children [:expr]}))

(defmethod parse 'new
  [_ env [_ ctor & args :as form] _ _]
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         ctorexpr (analyze enve ctor)
         ctor-var (when (#{:var :local :js-var} (:op ctorexpr))
                    (resolve-existing-var env ctor))
         record-args
         (when (and (:record ctor-var) (not (-> ctor meta :internal-ctor)))
           (repeat 3 (analyze enve nil)))
         argexprs (into (vec (map #(analyze enve %) args)) record-args)
         known-num-fields (:num-fields ctor-var)
         argc (count args)]
     (when (and (not (-> ctor meta :internal-ctor))
                (some? known-num-fields) (not= known-num-fields argc))
       (warning :fn-arity env {:argc argc :ctor ctor}))
     {:env env :op :new :form form :class ctorexpr :args argexprs
      :children [:class :args]
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
      (binding [*private-var-access-nowarn* true]
        (let [enve  (assoc env :context :expr)
              texpr (cond
                      (symbol? target)
                      (do
                        (cond
                          (and (= target '*unchecked-if*)   ;; TODO: proper resolve
                               (or (true? val) (false? val)))
                          (set! *unchecked-if* val)

                          (and (= target '*unchecked-arrays*) ;; TODO: proper resolve
                               (or (true? val) (false? val)))
                          (set! *unchecked-arrays* val)

                          (and (= target '*warn-on-infer*)
                               (or (true? val) (false? val)))
                          (set! *cljs-warnings* (assoc *cljs-warnings* :infer-warning val)))
                        (when (some? (:const (resolve-var (dissoc env :locals) target)))
                          (throw (error env "Can't set! a constant")))
                        (let [local (handle-symbol-local target (-> env :locals target))]
                          (when-not (or (nil? local)
                                        (and (:field local)
                                             (or (:mutable local)
                                                 (:unsynchronized-mutable local)
                                                 (:volatile-mutable local))))
                            (throw (error env "Can't set! local var or non-mutable field"))))
                        (analyze-symbol enve target))

                      :else
                      (when (seq? target)
                        (let [texpr (analyze-seq enve target nil)]
                          (when (:field texpr)
                            texpr))))
              vexpr (analyze enve val)]
          ;; as top level fns are decomposed for Closure cross-module code motion, we need to
          ;; restore their :methods information
          (when (seq? target)
            (let [sym  (some-> target second)
                  meta (meta sym)]
              (when-let [info (and (= :fn (:op vexpr)) (:top-fn meta))]
                (swap! env/*compiler* update-in
                  [::namespaces (-> env :ns :name) :defs sym :methods]
                  (fnil conj [])
                  ;; just use original fn meta, as the fn method is already desugared
                  ;; only get tag from analysis
                  (merge
                    (select-keys info [:fixed-arity :variadic?])
                    (select-keys (-> vexpr :methods first) [:tag]))))))
          (when-not texpr
            (throw (error env "set! target must be a field or a symbol naming a var")))
          (cond
            (and (not (:def-emits-var env))                 ;; non-REPL context
                 (some? ('#{*unchecked-if* *unchecked-arrays* *warn-on-infer*} target)))
            {:env env :op :no-op}

            :else
            {:env env :op :set! :form form :target texpr :val vexpr
             :children [:target :val]}))))))

#?(:clj (declare analyze-file))

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
  (let [js-index (:js-dependency-index @env/*compiler*)]
    (if-some [[_ {:keys [foreign]}] (find js-index (name dep))]
      foreign
      false)))

(defn analyze-deps
  "Given a lib, a namespace, deps, its dependencies, env, an analysis environment
   and opts, compiler options - analyze all of the dependencies. Required to
   correctly analyze usage of other namespaces."
  ([lib deps env]
   (analyze-deps lib deps env
     (when env/*compiler*
       (:options @env/*compiler*))))
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
         (when-not (or (some? (get-in compiler [::namespaces dep :defs]))
                       (contains? (:js-dependency-index compiler) (name dep))
                       (node-module-dep? dep)
                       (js-module-exists? (name dep))
                       #?(:clj (deps/find-classpath-lib dep)))
           #?(:clj (if-some [src (locate-src dep)]
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
         (not (get js-lib :closure-lib))
         (not (node-module-dep? lib))
         (not (dep-has-global-exports? lib)))))

(defn missing-rename? [sym cenv]
  (let [lib (symbol (namespace sym))
        sym (symbol (name sym))]
    (missing-use? lib sym cenv)))

(defn missing-use-macro? [lib sym]
  ;; guard against string requires
  (when (symbol? lib)
    (let [the-ns #?(:clj (find-ns lib) :cljs (find-macros-ns lib))]
      (or (nil? the-ns) (nil? (.findInternedVar ^clojure.lang.Namespace the-ns sym))))))

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
  (when-not (or (symbol? spec) (string? spec) (sequential? spec))
    (throw
      (error env
        (parse-ns-error-msg spec
          "Only [lib.ns & options] and lib.ns specs supported in :require / :require-macros"))))
  (when (sequential? spec)
    (when-not (or (symbol? (first spec)) (string? (first spec)))
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

                    (some? fs)
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
                        (recur fs ret true)))

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

      (some? fs)
      (let [kw (first fs)
            only? (= kw :only)]
        (if (or only? (= kw :rename))
          (if (some? (some #{(if only? :refer kw)} ret))
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

      :else (if (some? (some #{:refer} ret))
              ret
              (recur fs ret true)))))

(defn parse-require-spec [env macros? deps aliases spec]
  (if (or (symbol? spec) (string? spec))
    (recur env macros? deps aliases [spec])
    (do
      (basic-validate-ns-spec env macros? spec)
      (let [[lib & opts] spec
            ;; We need to load JS modules by the name that has been created by the
            ;; Google Closure compiler, e.g. module$resources$libs$calculator.
            ;; This means that we need to create an alias from the module name
            ;; given with :provides to the new name.
            [lib js-module-provides] (if-some [js-module-name (gets @env/*compiler* :js-module-index (str lib) :name)]
                                       [(symbol js-module-name) lib]
                                       [lib nil])
            {alias :as referred :refer renamed :rename
             :or {alias (if (string? lib)
                          (symbol (munge lib))
                          lib)}}
            (apply hash-map opts)
            referred-without-renamed (seq (remove (set (keys renamed)) referred))
            [rk uk renk] (if macros? [:require-macros :use-macros :rename-macros] [:require :use :rename])]
        (when-not (or (symbol? alias) (nil? alias))
          (throw
            (error env
              (parse-ns-error-msg spec
                ":as must be followed by a symbol in :require / :require-macros"))))
        (when (some? alias)
          (let [alias-type (if macros? :macros :fns)
                lib'       ((alias-type @aliases) alias)]
            (when (and (some? lib') (not= lib lib'))
              (throw (error env (parse-ns-error-msg spec ":as alias must be unique"))))
            (swap! aliases
              update-in [alias-type]
              conj [alias lib] (when js-module-provides [js-module-provides lib]))))
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
          (when (some? alias)
            {rk (merge {alias lib} {lib lib}
                  (when js-module-provides {js-module-provides lib}))})
          (when (some? referred-without-renamed)
            {uk (apply hash-map (interleave referred-without-renamed (repeat lib)))})
          (when (some? renamed)
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

#?(:clj (declare parse-ns))

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
   (defn process-rewrite-form [[k & specs :as form]]
     (letfn [(process-spec [maybe-spec]
               (let [[lib & xs] (if (sequential? maybe-spec)
                                  maybe-spec
                                  [maybe-spec])]
                 (if (and (symbol? lib) (aliasable-clj-ns? lib))
                   (let [lib' (clj-ns->cljs-ns lib)
                         spec (cons lib' xs)]
                     (into (if xs [spec] []) [(list lib' :as lib)]))
                   [maybe-spec])))]
       (if (#{:use :require} k)
         (cons k (mapcat process-spec specs))
         form))))

#?(:clj
   (defn rewrite-cljs-aliases
     "Alias non-existing clojure.* namespaces to existing cljs.* namespaces if
      possible."
     [args]
     (map process-rewrite-form args)))

(defn canonicalize-specs [specs]
  (letfn [(canonicalize [quoted-spec-or-kw]
            (if (keyword? quoted-spec-or-kw)
              quoted-spec-or-kw
              (as-> (second quoted-spec-or-kw) spec
                (if (or (vector? spec) (map? spec)) spec [spec]))))]
    (map canonicalize specs)))

(defn canonicalize-import-specs [specs]
  (letfn [(canonicalize [quoted-spec-or-kw]
            (if (keyword? quoted-spec-or-kw)
              quoted-spec-or-kw
              (second quoted-spec-or-kw)))]
    (map canonicalize specs)))

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
    (if-some [require-specs (seq (to-macro-specs require))]
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
  (when-not *allow-ns*
    (throw (error env "Namespace declarations must appear at the top-level.")))
  (when-not (symbol? name)
    (throw (error env "Namespaces must be named by a symbol.")))
  (let [name (cond-> name (:macros-ns opts) macro-ns-name)]
    (let [segments (string/split (clojure.core/name name) #"\.")]
      (when (= 1 (count segments))
        (warning :single-segment-namespace env {:name name}))
      (let [segment (some js-reserved segments)]
        (when (some? segment)
          (warning :munged-namespace env {:name name})))
      (find-def-clash env name segments)
      #?(:clj
         (when (some (complement util/valid-js-id-start?) segments)
           (throw
             (AssertionError.
               (str "Namespace " name " has a segment starting with an invaild "
                    "JavaScript identifier"))))))
    (let [docstring    (when (string? (first args)) (first args))
          mdocstr      (-> name meta :doc)
          args         (if (some? docstring) (next args) args)
          metadata     (when (map? (first args)) (first args))
          args         (desugar-ns-specs
                         #?(:clj  (rewrite-cljs-aliases
                                    (if metadata (next args) args))
                            :cljs (if (some? metadata) (next args) args)))
          name         (vary-meta name merge metadata)
          {excludes :excludes core-renames :renames} (parse-ns-excludes env args)
          core-renames (reduce (fn [m [original renamed]]
                                 (assoc m renamed (symbol "cljs.core" (str original))))
                         {} core-renames)
          deps         (atom [])
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
            (fn [m [k & libs :as libspec]]
              (when-not (#{:use :use-macros :require :require-macros :import} k)
                (throw (error env (str "Only :refer-clojure, :require, :require-macros, :use, :use-macros, and :import libspecs supported. Got " libspec " instead."))))
              (when-not (@valid-forms k)
                (throw (error env (str "Only one " k " form is allowed per namespace definition"))))
              (swap! valid-forms disj k)
              ;; check for spec type reloads
              (when-not (= :import k)
                (when (some? (some #{:reload} libs))
                  (swap! reload assoc k :reload))
                (when (some? (some #{:reload-all} libs))
                  (swap! reload assoc k :reload-all)))
              ;; check for individual ns reloads from REPL interactions
              (when-let [xs (seq (filter #(-> % meta :reload) libs))]
                (swap! reloads assoc k
                  (zipmap (map first xs) (map #(-> % meta :reload) xs))))
              (apply merge-with merge m
                (map (spec-parsers k)
                  (remove #{:reload :reload-all} libs))))
            {} (remove (fn [[r]] (= r :refer-clojure)) args))
          ;; patch `require-macros` and `use-macros` in Bootstrap for namespaces
          ;; that require their own macros
          #?@(:cljs [[require-macros use-macros]
                     (map (fn [spec-map]
                            (if (:macros-ns opts)
                              (let [ns (symbol (subs (str name) 0 (- (count (str name)) 7)))]
                                (reduce (fn [m [k v]]
                                          (cond-> m
                                            (not (symbol-identical? v ns))
                                            (assoc k v)))
                                  {} spec-map))
                              spec-map)) [require-macros use-macros])])]
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
             :imports        imports}]
        (swap! env/*compiler* update-in [::namespaces name] merge ns-info)
        (merge {:op      :ns
                :env     env
                :form    form
                :deps    (into [] (distinct @deps))
                :reload  @reload
                :reloads @reloads}
          (cond-> ns-info
            (@reload :use)
            (update-in [:uses]
              (fn [m] (with-meta m {(@reload :use) true})))
            (@reload :require)
            (update-in [:requires]
              (fn [m] (with-meta m {(@reload :require) true})))))))))

(defn- check-duplicate-aliases
  [env old new]
  (let [ns-name (:name old)]
    (doseq [k [:requires :require-macros]]
      (let [old-aliases (get old k)
            new-aliases (get new k)]
        (when-some [alias (some (set (keys new-aliases))
                            (->> old-aliases
                              (remove (fn [[k v :as entry]]
                                        (or (= k v)
                                            (= entry (find new-aliases k)))))
                              keys))]
          (throw (error env
                   (str "Alias " alias " already exists in namespace " ns-name
                     ", aliasing " (get old-aliases alias)))))))))

(defmethod parse 'ns*
  [_ env [_ quoted-specs :as form] _ opts]
  (when-let [not-quoted (->> (remove keyword? quoted-specs)
                          (remove #(and (seq? %) (= 'quote (first %))) )
                          first)]
    (throw (error env (str "Arguments to " (name (first quoted-specs))
                        " must be quoted. Offending spec: " not-quoted))))
  (when-not *allow-ns*
    (throw (error env (str "Calls to `" (name (first quoted-specs))
                        "` must appear at the top-level."))))
  (let [specs        (if (= :import (first quoted-specs))
                       (canonicalize-import-specs quoted-specs)
                       (canonicalize-specs quoted-specs))
        name         (-> env :ns :name)
        args         (desugar-ns-specs
                       #?(:clj  (list (process-rewrite-form
                                        specs))
                          :cljs (list specs)))
        {excludes :excludes core-renames :renames} (parse-ns-excludes env args)
        core-renames (reduce (fn [m [original renamed]]
                               (assoc m renamed (symbol "cljs.core" (str original))))
                       {} core-renames)
        deps         (atom [])
        aliases      (atom {:fns {} :macros {}})
        spec-parsers {:require        (partial parse-require-spec env false deps aliases)
                      :require-macros (partial parse-require-spec env true deps aliases)
                      :use            (comp (partial parse-require-spec env false deps aliases)
                                        (partial use->require env))
                      :use-macros     (comp (partial parse-require-spec env true deps aliases)
                                        (partial use->require env))
                      :import         (partial parse-import-spec env deps)}
        reload       (atom {:use nil :require nil :use-macros nil :require-macros nil})
        reloads      (atom {})
        {uses :use requires :require renames :rename
         use-macros :use-macros require-macros :require-macros
         rename-macros :rename-macros imports :import :as params}
        (reduce
          (fn [m [k & libs]]
            ;; check for spec type reloads
            (when-not (= :import k)
              (when (some? (some #{:reload} libs))
                (swap! reload assoc k :reload))
              (when (some? (some #{:reload-all} libs))
                (swap! reload assoc k :reload-all)))
            ;; check for individual ns reloads from REPL interactions
            (when-some [xs (seq (filter #(-> % meta :reload) libs))]
              (swap! reloads assoc k
                (zipmap (map first xs) (map #(-> % meta :reload) xs))))
            (apply merge-with merge m
              (map (spec-parsers k)
                (remove #{:reload :reload-all} libs))))
          {} (remove (fn [[r]] (= r :refer-clojure)) args))]
    (set! *cljs-ns* name)
    (let [require-info
          {:name           name
           :excludes       excludes
           :use-macros     use-macros
           :require-macros require-macros
           :rename-macros  rename-macros
           :uses           uses
           :requires       requires
           :renames        (merge renames core-renames)
           :imports        imports}
          ns-info
          (let [ns-info' (get-in @env/*compiler* [::namespaces name])]
            (if (pos? (count ns-info'))
              (let [merge-keys
                    [:use-macros :require-macros :rename-macros
                     :uses :requires :renames :imports]]
                #?(:clj
                   (when *check-alias-dupes*
                     (check-duplicate-aliases env ns-info' require-info)))
                (merge
                  ns-info'
                  {:excludes excludes}
                  (merge-with merge
                    (select-keys ns-info' merge-keys)
                    (select-keys require-info merge-keys))))
              require-info))]
      (swap! env/*compiler* update-in [::namespaces name] merge ns-info)
      (merge {:op      :ns*
              :env     env
              :form    form
              :deps    (into [] (distinct @deps))
              :reload  @reload
              :reloads @reloads}
        (cond-> require-info
          (@reload :use)
          (update-in [:uses]
            (fn [m] (with-meta m {(@reload :use) true})))
          (@reload :require)
          (update-in [:requires]
            (fn [m] (with-meta m {(@reload :require) true}))))))))

(defn parse-type
  [op env [_ tsym fields pmasks body :as form]]
  (let [t (:name (resolve-var (dissoc env :locals) tsym))
        locals (reduce (fn [m fld]
                         (assoc m fld
                                {:name fld
                                 :line (get-line fld env)
                                 :column (get-col fld env)
                                 :local :field
                                 :field true
                                 :mutable (-> fld meta :mutable)
                                 :unsynchronized-mutable (-> fld meta :unsynchronized-mutable)
                                 :volatile-mutable (-> fld meta :volatile-mutable)
                                 :tag (-> fld meta :tag)
                                 :shadow (m fld)}))
                       {} (if (= :defrecord op)
                            (concat fields '[__meta __extmap ^:mutable __hash])
                            fields))
        protocols (-> tsym meta :protocols)]
    (swap! env/*compiler* update-in [::namespaces (-> env :ns :name) :defs tsym]
           (fn [m]
             (let [m (assoc (or m {})
                       :name t
                       :tag 'function
                       :type true
                       :num-fields (count fields)
                       :record (= :defrecord op))]
               (merge m
                      (dissoc (meta tsym) :protocols)
                      {:protocols protocols}
                      (source-info tsym env)))))
    {:op op :env env :form form :t t :fields fields :pmasks pmasks
     :tag 'function
     :protocols (disj protocols 'cljs.core/Object)
     :children [#_:fields :body]
     :body (analyze (assoc env :locals locals) body)}))

(defmethod parse 'deftype*
  [_ env form _ _]
  (parse-type :deftype env form))

(defmethod parse 'defrecord*
  [_ env form _ _]
  (parse-type :defrecord env form) )

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
        target-tag (:tag targetexpr)
        prop       (or field method)
        tag        (or (:tag form-meta)
                       (and (js-tag? target-tag)
                            (vary-meta (normalize-js-tag target-tag)
                              update-in [:prefix] (fnil conj '[Object]) prop))
                       nil)]
    (when (and (not= 'constructor prop)
               (not (string/starts-with? (str prop) "cljs$")))
      ;; Adding to Object
      (when (= 'Object (first (-> tag meta :prefix)))
        (warning :infer-warning env
          {:warn-type :object :form form :property prop}))
      (when (not= 'js target-tag)
        ;; Cannot determine type of the target
        (when (or (nil? target-tag) ('#{any} target-tag))
          (warning :infer-warning env
            {:warn-type :target :form form}))
        ;; Unresolveable property on existing extern
        (let [[pre' pre] ((juxt butlast identity) (-> tag meta :prefix))]
          (when (and (has-extern? pre') (not (has-extern? pre)))
            (warning :infer-warning env
              {:warn-type :property :form form
               :type (symbol "js"
                       (string/join "."
                         (cond-> pre' (= 'prototype (last pre')) butlast)))
               :property prop})))))
    (when (js-tag? tag)
      (let [pre (-> tag meta :prefix)]
        (when-not (has-extern? pre)
          (swap! env/*compiler* update-in
            (into [::namespaces (-> env :ns :name) :externs] pre) merge {}))))
    (case dot-action
      ::access (let [children [:target]]
                 {:op :host-field
                  :env env
                  :form form
                  :target targetexpr
                  :field field
                  :children children
                  :tag (if (js-tag? tag)
                         (or (js-tag (-> tag meta :prefix) :tag) tag)
                         tag)})
      ::call   (let [argexprs (mapv #(analyze enve %) args)
                     children [:target :args]]
                 {:op :host-call
                  :env env
                  :form form
                  :target targetexpr
                  :method method
                  :args argexprs
                  :children children
                  :tag (if (js-tag? tag)
                         (or (js-tag (-> tag meta :prefix) :ret-tag) 'js)
                         tag)}))))

(defmethod parse '.
  [_ env [_ target & [field & member+] :as form] _ _]
  (disallowing-recur (analyze-dot env target field member+ form)))

(defn get-js-tag [form]
  (let [form-meta (meta form)]
    (if-some [tag (:tag form-meta)]
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
  (cond
    (nil? t) true
    (= 'clj-nil t) true
    (js-tag? t) true ;; TODO: revisit
    :else
    (if (and (symbol? t) (some? (get NUMERIC_SET t)))
      true
      (when #?(:clj  (set? t)
               :cljs (cljs-set? t))
        (or (contains? t 'number)
            (contains? t 'long)
            (contains? t 'double)
            (contains? t 'any)
            (contains? t 'js))))))

(def array-types
  '#{array objects ints longs floats doubles chars shorts bytes boolean})

(defn array-type?
  #?(:cljs {:tag boolean})
  [t]
  ;; TODO same inference caveats as the numeric-type? fn above
  (cond
    (nil? t) true
    (= 'clj-nil t) true
    (js-tag? t) true ;; TODO: revisit
    (= 'any t) true
    (contains? array-types t) true
    :else
    (boolean
      (when #?(:clj  (set? t)
               :cljs (cljs-set? t))
        (or (contains? t 'any)
            (contains? t 'js)
            (some array-types t))))))

(defn analyze-js-star* [env jsform args form]
  (let [enve      (assoc env :context :expr)
        argexprs  (vec (map #(analyze enve %) args))
        form-meta (meta form)
        segs      (js-star-seg jsform)
        tag       (get-js-tag form)
        js-op     (:js-op form-meta)
        numeric   (:numeric form-meta)
        validate  (fn [warning-type valid-types?]
                    (let [types (map #(infer-tag env %) argexprs)]
                      (when-not (valid-types? types)
                        (warning warning-type env
                          {:js-op js-op
                           :types (into [] types)}))))
        op-match? (fn [sym]
                    #?(:clj  (= sym (:js-op form-meta))
                       :cljs (symbol-identical? sym (:js-op form-meta))))]
    (when (true? numeric)
      (validate :invalid-arithmetic #(every? numeric-type? %)))
    {:op :js
     :env env
     :segs segs
     :args argexprs
     :tag tag
     :form form
     :children [:args]
     :js-op js-op
     :numeric numeric}))

(defn analyze-js-star [env jsform args form]
  (disallowing-recur (analyze-js-star* env jsform args form)))

(defmethod parse 'js*
  [op env [_ jsform & args :as form] _ _]
  (when-not (string? jsform)
    (throw (error env "Invalid js* form")))
  (if (some? args)
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

;; TODO: analyzed analyzed? should take pass name as qualified keyword arg
;; then compiler passes can mark/check individually - David

(defn- unsorted-map? [x]
  (and (map? x)
       (not (sorted? x))))

(defn analyzed
  "Mark a form as being analyzed. Assumes x satisfies IMeta. Useful to suppress
  warnings that will have been caught by a first compiler pass."
  [x]
  (cond
    (unsorted-map? x) (assoc x ::analyzed true)
    :else (vary-meta x assoc ::analyzed true)))

(defn analyzed?
  "Returns boolean if the form has already been marked as analyzed."
  #?(:cljs {:tag boolean})
  [x]
  (boolean
    (cond
      (unsorted-map? x) (::analyzed x)
      :else (::analyzed (meta x)))))

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
        kw?     (= 'cljs.core/Keyword (:tag fexpr))
        cur-ns  (-> env :ns :name)
        HO-invoke? (and (boolean *cljs-static-fns*)
                        (not fn-var?)
                        (not (js-tag? f))
                        (not kw?)
                        (not (analyzed? f)))
        ;; function expressions, eg: ((deref m) x) or ((:x m) :a)
        bind-f-expr? (and HO-invoke?
                          (not (symbol? f)))
        ;; Higher order invokes with (some) argument expressions. Bind the arguments
        ;; to avoid exponential complexity that is created by the IFn arity check branch.
        bind-args? (and HO-invoke?
                        (not (all-values? args)))]
    (when ^boolean fn-var?
      (let [{^boolean variadic :variadic? :keys [max-fixed-arity method-params name ns macro]} (:info fexpr)]
        ;; don't warn about invalid arity when when compiling a macros namespace
        ;; that requires itself, as that code is not meant to be executed in the
        ;; `$macros` ns - António Monteiro
        (when (and #?(:cljs (not (and (gstring/endsWith (str cur-ns) "$macros")
                                      (symbol-identical? cur-ns ns)
                                      (true? macro))))
                   (not (valid-arity? argc method-params))
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
    (when (some? (-> fexpr :info :type))
      (warning :invoke-ctor env {:fexpr fexpr}))
    (if (or bind-args? bind-f-expr?)
      (let [arg-syms (when bind-args? (take argc (repeatedly gensym)))
            f-sym (when bind-f-expr? (gensym "fexpr__"))
            bindings (cond-> []
                       bind-args? (into (interleave arg-syms args))
                       bind-f-expr? (conj f-sym (analyzed f)))]
        (analyze env
          `(let [~@bindings]
             (~(analyzed (if bind-f-expr? f-sym f))
               ~@(if bind-args? arg-syms args)))))
      (let [ana-expr #(analyze enve %)
            argexprs (mapv ana-expr args)]
        {:env env :op :invoke :form form :fn fexpr :args argexprs
         :children [:fn :args]}))))

(defn parse-invoke
  [env form]
  (disallowing-recur (parse-invoke* env form)))

(defn desugar-dotted-expr [{:keys [op] :as expr}]
  (case op
    (:var :local) (if (dotted-symbol? (symbol (name (:name expr))))
                    (let [s      (name (:name expr))
                          idx    (.lastIndexOf s ".")
                          _ (assert (not= (inc idx) (count s)))
                          prefix (with-meta (symbol (namespace (:name expr)) (subs s 0 idx))
                                            (meta (:form expr)))
                          field (symbol (subs s (inc idx)))]
                      (assert (not (:const-expr expr)))
                      {:op :host-field
                       :env (:env expr)
                       :form (list '. prefix field)
                       :target (desugar-dotted-expr (-> expr
                                                        (assoc :name prefix
                                                               :form prefix)
                                                        (dissoc :tag)
                                                        (assoc-in [:info :name] prefix)
                                                        (assoc-in [:env :context] :expr)))
                       :field field
                       :tag (:tag expr)
                       :children [:target]})
                    expr)
    ;:var
    expr))


(defn analyze-symbol
  "Finds the var associated with sym"
  [env sym]
  (if ^boolean (:quoted? env)
    (do
      (register-constant! env sym)
      (analyze-wrap-meta {:op :const :val sym :env env :form sym :tag 'cljs.core/Symbol}))
    (let [{:keys [line column]} (meta sym)
          env  (if-not (nil? line)
                 (assoc env :line line)
                 env)
          env  (if-not (nil? column)
                 (assoc env :column column)
                 env)
          ret  {:env env :form sym}
          lcls (:locals env)]
      (if-some [lb (handle-symbol-local sym (get lcls sym))]
        (merge
          (assoc ret :op :local :info lb)
          ;; this is a temporary workaround for core.async see CLJS-3030 - David
          (when (map? lb)
            (select-keys lb [:name :local :arg-id :variadic? :init])))
        (let [sym-meta (meta sym)
              sym-ns (namespace sym)
              cur-ns (str (-> env :ns :name))
              ;; when compiling a macros namespace that requires itself, we need
              ;; to resolve calls to `my-ns.core/foo` to `my-ns.core$macros/foo`
              ;; to avoid undeclared variable warnings - António Monteiro
              #?@(:cljs [sym (if (and sym-ns
                                   (not= sym-ns "cljs.core")
                                   (gstring/endsWith cur-ns "$macros")
                                   (not (gstring/endsWith sym-ns "$macros"))
                                   (= sym-ns (subs cur-ns 0 (- (count cur-ns) 7))))
                               (symbol (str sym-ns "$macros") (name sym))
                               sym)])
              info     (if-not (contains? sym-meta ::analyzed)
                         (resolve-existing-var env sym)
                         (resolve-var env sym))]
          (assert (:op info) (:op info))
          (desugar-dotted-expr
            (if-not (true? (:def-var env))
              (merge
                (assoc ret :info info)
                (select-keys info [:op :name :ns :tag])
                (when-let [const-expr (:const-expr info)]
                  {:const-expr const-expr}))
              (let [info (resolve-var env sym)]
                (merge (assoc ret :op :var :info info)
                       (select-keys info [:op :name :ns :tag]))))))))))

(defn excluded?
  #?(:cljs {:tag boolean})
  [env sym]
  (or (some? (gets env :ns :excludes sym))
      (some? (gets @env/*compiler* ::namespaces (gets env :ns :name) :excludes sym))))

(defn used?
  #?(:cljs {:tag boolean})
  [env sym]
  (or (some? (gets env :ns :use-macros sym))
      (some? (gets @env/*compiler* ::namespaces (gets env :ns :name) :use-macros sym))))

(defn get-expander-ns [env ^String nstr]
  ;; first check for clojure.* -> cljs.* cases
  (let [res  (or (resolve-macro-ns-alias env nstr nil)
                 (resolve-ns-alias env nstr nil))
        nstr (if (some? res) (str res) nstr)]
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
  (when-not (or (some? (gets env :locals sym)) ; locals hide macros
                (and (excluded? env sym) (not (used? env sym))))
    (let [nstr (namespace sym)]
      (cond
        (some? nstr)
        (let [ns (get-expander-ns env nstr)]
          (when (some? ns)
            (.findInternedVar ^clojure.lang.Namespace ns (symbol (name sym)))))

        (some? (gets env :ns :rename-macros sym))
        (let [qualified-symbol (gets env :ns :rename-macros sym)
              nsym (symbol (namespace qualified-symbol))
              sym  (symbol (name qualified-symbol))]
          (.findInternedVar ^clojure.lang.Namespace
            #?(:clj (find-ns nsym) :cljs (find-macros-ns nsym)) sym))

        :else
        (let [nsym (gets env :ns :use-macros sym)]
          (if (and (some? nsym) (symbol? nsym))
            (.findInternedVar ^clojure.lang.Namespace
              #?(:clj (find-ns nsym) :cljs (find-macros-ns nsym)) sym)
            (.findInternedVar ^clojure.lang.Namespace
              #?(:clj (find-ns 'cljs.core) :cljs (find-macros-ns CLJS_CORE_MACROS_SYM)) sym)))))))

(defn get-expander
  "Given a sym, a symbol identifying a macro, and env, an analysis environment
   return the corresponding Clojure macroexpander."
  [sym env]
  (let [mvar (get-expander* sym env)]
    (when (and (some? mvar)
            #?(:clj  (.isMacro ^clojure.lang.Var mvar)
               :cljs ^boolean (.isMacro mvar)))
      mvar)))

#?(:cljs
   (let [cached-var (delay (get (ns-interns* 'cljs.spec.alpha) 'macroexpand-check))]
     (defn get-macroexpand-check-var []
       (when (some? (find-ns-obj 'cljs.spec.alpha))
         @cached-var))))

(defn- var->sym [var]
  #?(:clj  (symbol (str (.-ns ^clojure.lang.Var var)) (str (.-sym ^clojure.lang.Var var)))
     :cljs (.-sym var)))

(defn- do-macroexpand-check
  [env form mac-var]
  (when (not (-> @env/*compiler* :options :spec-skip-macros))
    (let [mchk #?(:clj (some-> (find-ns 'clojure.spec.alpha)
                       (ns-resolve 'macroexpand-check))
                :cljs (get-macroexpand-check-var))]
    (when (some? mchk)
      (try
        (mchk mac-var (next form))
        (catch #?(:clj Throwable :cljs :default) e
          (throw (ex-info nil (error-data env :macro-syntax-check (var->sym mac-var)) e))))))))

(defn macroexpand-1*
  [env form]
  (let [op (first form)]
    (if (contains? specials op)
      (do
        (when (= 'ns op)
          (do-macroexpand-check env form (get-expander 'cljs.core/ns-special-form env)))
        form)
      ;else
        (if-some [mac-var (when (symbol? op) (get-expander op env))]
          (#?@(:clj [binding [*ns* (create-ns *cljs-ns*)]]
               :cljs [do])
            (do-macroexpand-check env form mac-var)
            (let [form' (try
                          (apply @mac-var form env (rest form))
                          #?(:clj (catch ArityException e
                                    (throw (ArityException. (- (.actual e) 2) (.name e)))))
                          (catch #?(:clj Throwable :cljs :default) e
                            (throw (ex-info nil (error-data env :macroexpansion (var->sym mac-var)) e))))]
              (if #?(:clj (seq? form') :cljs (cljs-seq? form'))
                (let [sym' (first form')
                      sym  (first form)]
                  (if #?(:clj  (= sym' 'js*)
                         :cljs (symbol-identical? sym' JS_STAR_SYM))
                    (let [sym   (if (some? (namespace sym))
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
            form)))))

(defn macroexpand-1
  "Given a env, an analysis environment, and form, a ClojureScript form,
   macroexpand the form once."
  [env form]
  (wrapping-errors env (macroexpand-1* env form)))

(declare analyze-list)

(defn analyze-seq* [op env form name opts]
  (if (contains? specials op)
    (parse op env form name opts)
    (parse-invoke env form)))

(defn analyze-seq*-wrap [op env form name opts]
  (wrapping-errors env
    (analyze-seq* op env form name opts)))

(defn analyze-seq
  ([env form name]
   (analyze-seq env form name
     (when env/*compiler*
       (:options @env/*compiler*))))
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
        ks (disallowing-recur (mapv #(analyze expr-env %) (keys form)))
        vs (disallowing-recur (mapv #(analyze expr-env %) (vals form)))]
    (analyze-wrap-meta {:op :map :env env :form form
                        :keys ks :vals vs
                        :children [:keys :vals]
                        :tag 'cljs.core/IMap})))

;; :list is not used in the emitter any more, but analyze-list is called from analyze-const
;; to hit the `register-constant!` cases for symbols and keywords.
(defn analyze-list
  [env form]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (mapv #(analyze expr-env %) form))]
    (analyze-wrap-meta {:op :list :env env :form form :items items :children [:items] :tag 'cljs.core/IList})))

(defn analyze-vector
  [env form]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (mapv #(analyze expr-env %) form))]
    (analyze-wrap-meta {:op :vector :env env :form form :items items :children [:items] :tag 'cljs.core/IVector})))

(defn analyze-set
  [env form]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (mapv #(analyze expr-env %) form))]
    (analyze-wrap-meta {:op :set :env env :form form :items items :children [:items] :tag 'cljs.core/ISet})))

(defn analyze-js-value
  [env ^JSValue form]
  (let [val (.-val form)
        expr-env (assoc env :context :expr)]
    (if (map? val)
      (let [keys (vec (keys val))
            vals (disallowing-recur
                   (mapv #(analyze expr-env %) (vals val)))]
        {:op :js-object
         :env env
         :form form
         :keys keys
         :vals vals
         :children [:vals]
         :tag 'object})
      (let [items (disallowing-recur
                    (mapv #(analyze expr-env %) val))]
        {:op :js-array
         :env env
         :form form
         :items items
         :children [:items]
         :tag 'array}))))

(defn record-ns+name [x]
  (map symbol
       #?(:clj
          ((juxt (comp #(string/join "." %) butlast) last)
           (string/split (.getName ^Class (type x)) #"\."))
          :cljs
          (string/split (pr-str (type x)) #"/"))))

(defn analyze-record
  [env x]
  (let [;; register constansts
        _items_   (disallowing-recur
                    (analyze (assoc env :context :expr) (into {} x)))
        [ns name] (record-ns+name x)]
    {:op :const
     :val x
     :env env
     :form x
     :tag (symbol (str ns) (str name))}))

(defn elide-reader-meta [m]
  (dissoc m :file :line :column :end-column :end-line :source))

(defn elide-analyzer-meta [m]
  (dissoc m ::analyzed))

(defn elide-irrelevant-meta [m]
  (-> m elide-reader-meta elide-analyzer-meta))

(defn analyze-wrap-meta [expr]
  (let [form (:form expr)
        m    (elide-irrelevant-meta (meta form))]
    (if (some? (seq m))
      (let [env (:env expr) ; take on expr's context ourselves
            expr (assoc-in expr [:env :context] :expr) ; change expr to :expr
            meta-expr (analyze-map (:env expr) m)]
        {:op :with-meta :env env :form form
         :meta meta-expr :expr expr :children [:meta :expr]})
      expr)))

(defn infer-type [env {:keys [tag] :as ast} _]
  (if (or (nil? tag) (= 'function tag))
    ;; infer-type won't get a chance to process :methods
    ;; so treat :fn as a special case for now, could probably
    ;; fix up to use :children to walk child nodes
    (if (= :fn (:op ast))
      (update ast :methods
        (fn [ms] (into [] (map #(infer-type env % _)) ms)))
      (if-some [tag (infer-tag env ast)]
        (assoc ast :tag tag)
        ast))
    ast))

(defn- repl-self-require? [env deps]
  (and (:repl-env env) (some #{*cljs-ns*} deps)))

#?(:clj
   (defn ns-side-effects
     [env {:keys [op] :as ast} opts]
     (if (#{:ns :ns*} op)
       (let [{:keys [name deps uses require-macros use-macros reload reloads]} ast]
         (when (and *analyze-deps* (seq deps))
           (analyze-deps
             (if (repl-self-require? env deps) 'cljs.user name)
             deps env (dissoc opts :macros-ns)))
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

;; A set of validators that can be used to do static type
;; checking of runtime fns based on inferred argument types.
(def invoke-arg-type-validators
  (let [aget-validator {:valid?       #(and (array-type? (first %))
                                            (every? numeric-type? (rest %)))
                        :warning-type :invalid-array-access}
        aset-validator {:valid?       #(and (array-type? (first %))
                                            (every? numeric-type? (butlast (rest %))))
                        :warning-type :invalid-array-access}]
    {'cljs.core/checked-aget  aget-validator
     'cljs.core/checked-aset  aset-validator
     'cljs.core/checked-aget' aget-validator
     'cljs.core/checked-aset' aset-validator}))

(defn check-invoke-arg-types
  [env {:keys [op] :as ast} opts]
  (when (and (not (analyzed? ast))
             #?(:clj  (= :invoke op)
                :cljs (keyword-identical? :invoke op)))
    (when-some [[name {:keys [valid? warning-type]}] (find invoke-arg-type-validators (-> ast :fn :info :name))]
      (let [types (mapv :tag (:args ast))]
        (when-not (valid? types)
          (warning warning-type env
            {:name  name
             :types types})))))
  (analyzed ast))

#?(:clj
   (defn analyze-form [env form name opts]
     (cond
       (symbol? form) (analyze-symbol env form)
       (and (seq? form) (seq form)) (analyze-seq env form name opts)
       (record? form) (analyze-record env form)
       (map? form) (analyze-map env form)
       (vector? form) (analyze-vector env form)
       (set? form) (analyze-set env form)
       (keyword? form) (analyze-keyword env form)
       (instance? JSValue form) (analyze-js-value env form)
       :else
       (let [tag (cond
                   (nil? form) 'clj-nil
                   (number? form) 'number
                   (string? form) 'string
                   (instance? Character form) 'string
                   (true? form) 'boolean
                   (false? form) 'boolean
                   (= () form) 'cljs.core/IList)]
         (cond-> {:op :const :val form :env env :form form}
           tag (assoc :tag tag))))))

#?(:cljs
   (defn analyze-form [env form name opts]
     (cond
       (symbol? form) (analyze-symbol env form)
       (and (cljs-seq? form) (some? (seq form))) (analyze-seq env form name opts)
       (record? form) (analyze-record env form)
       (cljs-map? form) (analyze-map env form)
       (cljs-vector? form) (analyze-vector env form)
       (cljs-set? form) (analyze-set env form)
       (keyword? form) (analyze-keyword env form)
       (instance? cljs.tagged-literals/JSValue form) (analyze-js-value env form)
       :else
       (let [tag (cond
                   (nil? form) CLJ_NIL_SYM
                   (number? form) NUMBER_SYM
                   (string? form) STRING_SYM
                   (true? form) BOOLEAN_SYM
                   (false? form) BOOLEAN_SYM
                   (= () form) 'cljs.core/IList)]
         (cond-> {:op :const :val form :env env :form form}
           tag (assoc :tag tag))))))

(defn analyze* [env form name opts]
  (let [passes *passes*
        passes (if (nil? passes)
                 #?(:clj  [infer-type check-invoke-arg-types ns-side-effects]
                    :cljs [infer-type check-invoke-arg-types])
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
  nested exprs, must have a :children entry. This must be a vector of keywords naming
  the immediately nested fields mapped to an expr or vector of exprs. This will
  facilitate code walking without knowing the details of the op set."
  ([env form] (analyze env form nil))
  ([env form name]
   (analyze env form name
     (when env/*compiler*
       (:options @env/*compiler*))))
  ([env form name opts]
   (wrapping-errors env
     (if (analyzed? form)
       (no-warn (analyze* env form name opts))
       (analyze* env form name opts)))))

(defn add-consts
  "Given a compiler state and a map from fully qualified symbols to constant
  EDN values, update the compiler state marking these vars as const to support
  direct substitution of these vars in source."
  [compiler-state constants-map]
  (reduce-kv
    (fn [compiler-state sym value]
      (let [ns (symbol (namespace sym))]
        (update-in compiler-state
          [::namespaces ns :defs (symbol (name sym))] merge
          {:const-expr
           (binding [*passes* (conj *passes* (replace-env-pass {:context :expr}))]
             (analyze (empty-env) value))})))
    compiler-state constants-map))

#?(:clj
   (defn- source-path
     "Returns a path suitable for providing to tools.reader as a 'filename'."
     [x]
     (cond
       (instance? File x) (.getAbsolutePath ^File x)
       :default (str x))))

(defn resolve-symbol [sym]
  (if (and (not (namespace sym))
           (dotted-symbol? sym))
    sym
    (:name (binding [*private-var-access-nowarn* true]
             (resolve-var (assoc @env/*compiler* :ns (get-namespace *cljs-ns*))
               sym)))))

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
   (defn gen-user-ns
     [src]
     (if (sequential? src)
       (symbol (str "cljs.user.source$form$" (util/content-sha (pr-str src) 7)))
       (let [full-name (str src)
             name (.substring full-name
                    (inc (.lastIndexOf full-name "/"))
                    (.lastIndexOf full-name "."))]
         (symbol (str "cljs.user." name (util/content-sha full-name 7)))))))

#?(:clj
   (defn ^:dynamic parse-ns
     "Helper for parsing only the essential namespace information from a
      ClojureScript source file and returning a cljs.closure/IJavaScript compatible
      map _not_ a namespace AST node.

      By default does not load macros or perform any analysis of dependencies. If
      opts parameter provided :analyze-deps and :load-macros keys their values will
      be used for *analyze-deps* and *load-macros* bindings respectively. This
      function does _not_ side-effect the ambient compilation environment unless
      requested via opts where :restore is false."
     ([src]
      (parse-ns src nil
        (when env/*compiler*
          (:options @env/*compiler*))))
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
                                   src)
                           ret (merge
                                 {:file         dest
                                  :source-file  (when rdr src)
                                  :source-forms (when-not rdr src)
                                  :macros-ns    (:macros-ns opts)
                                  :requires     (cond-> #{'cljs.core}
                                                  (get-in @env/*compiler* [:options :emit-constants])
                                                  (conj constants-ns-sym))}
                                 (when (and dest (.exists ^File dest))
                                   {:lines (with-open [reader (io/reader dest)]
                                             (-> reader line-seq count))}))]
                      (if (seq forms)
                        (let [env (empty-env)
                              ast (no-warn (analyze env (first forms) nil opts))]
                          (cond
                            (= :ns (:op ast))
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
                                                   (conj constants-ns-sym)))
                                 :file         dest
                                 :source-file  (when rdr src)
                                 :source-forms (when-not rdr src)
                                 :ast          ast
                                 :macros-ns    (or (:macros-ns opts)
                                                   (= 'cljs.core$macros ns-name))}
                                (when (and dest (.exists ^File dest))
                                  {:lines (with-open [reader (io/reader dest)]
                                            (-> reader line-seq count))})))

                            (= :ns* (:op ast))
                            (let [deps (merge (:uses ast) (:requires ast))]
                              (recur (rest forms)
                                (cond-> (update-in ret [:requires] into (set (vals deps)))
                                  ;; we need to defer generating the user namespace
                                  ;; until we actually need or it will break when
                                  ;; `src` is a sequence of forms - António Monteiro
                                  (not (:ns ret))
                                  (assoc :ns (gen-user-ns src) :provides [(gen-user-ns src)]))))

                            :else ret))
                        ret))
                    (finally
                      (when rdr
                        (.close ^Reader rdr))))))]
          (cond-> ijs
            (not (contains? ijs :ns))
            (merge
              {:ns (gen-user-ns src)
               :provides [(gen-user-ns src)]})))))))

#?(:clj
   (defn- cache-analysis-ext
     ([] (cache-analysis-ext (get-in @env/*compiler* [:options :cache-analysis-format] :transit)))
     ([format]
      (if (and (= format :transit) @transit) "json" "edn"))))

#?(:clj
   (defn build-affecting-options [opts]
     (select-keys opts
       [:static-fns :fn-invoke-direct :optimize-constants :elide-asserts :target
        :cache-key :checked-arrays :language-out])))

#?(:clj
   (defn build-affecting-options-sha [path opts]
     (let [m (assoc (build-affecting-options opts) :path path)]
       (util/content-sha (pr-str m) 7))))

#?(:clj
   (defn ^File cache-base-path
     ([path]
      (cache-base-path path nil))
     ([path opts]
      (io/file (System/getProperty "user.home")
        ".cljs" ".aot_cache" (util/clojurescript-version)
        (build-affecting-options-sha path opts)))))

#?(:clj
   (defn cacheable-files
     ([rsrc ext]
      (cacheable-files rsrc ext nil))
     ([rsrc ext opts]
      (let [{:keys [ns]} (parse-ns rsrc)
            path (cache-base-path (util/path rsrc) opts)
            name (util/ns->relpath ns nil File/separatorChar)]
        (into {}
          (map
            (fn [[k v]]
              [k (io/file path
                   (if (and (= (str "cljs" File/separatorChar "core$macros") name)
                         (= :source k))
                     (str "cljs" File/separatorChar "core.cljc")
                     (str name v)))]))
          {:source (str "." ext)
           :output-file ".js"
           :source-map ".js.map"
           :analysis-cache-edn (str "." ext ".cache.edn")
           :analysis-cache-json (str "." ext ".cache.json")})))))

#?(:clj
   (defn cache-file
     "Given a ClojureScript source file returns the read/write path to the analysis
      cache file. Defaults to the read path which is usually also the write path."
     ([src] (cache-file src "out"))
     ([src output-dir] (cache-file src (parse-ns src) output-dir))
     ([src ns-info output-dir]
      (cache-file src ns-info output-dir :read nil))
     ([src ns-info output-dir mode]
      (cache-file src ns-info output-dir mode nil))
     ([src ns-info output-dir mode opts]
      {:pre [(map? ns-info)]}
      (let [ext (cache-analysis-ext)]
        (if-let [core-cache
                 (and (= mode :read)
                      (= (:ns ns-info) 'cljs.core)
                      (io/resource (str "cljs/core.cljs.cache.aot." ext)))]
          core-cache
          (let [aot-cache-file
                (when (util/url? src)
                  ((keyword (str "analysis-cache-" ext))
                    (cacheable-files src (util/ext src) opts)))]
            (if (and aot-cache-file (.exists ^File aot-cache-file))
              aot-cache-file
              (let [target-file (util/to-target-file output-dir ns-info
                                  (util/ext (:source-file ns-info)))]
                (io/file (str target-file ".cache." ext))))))))))

#?(:clj
   (defn requires-analysis?
     "Given a src, a resource, and output-dir, a compilation output directory
      return true or false depending on whether src needs to be (re-)analyzed.
      Can optionally pass cache, the analysis cache file."
     ([src] (requires-analysis? src "out"))
     ([src output-dir]
      (let [cache (cache-file src output-dir)]
        (requires-analysis? src cache output-dir nil)))
     ([src cache output-dir]
      (requires-analysis? src cache output-dir nil))
     ([src cache output-dir opts]
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
        (let [out-src   (util/to-target-file output-dir (parse-ns src))
              cache-src (:output-file (cacheable-files src (util/ext src) opts))]
          (if (and (not (.exists out-src))
                   (not (.exists ^File cache-src)))
            true
            (or (not cache) (util/changed? src cache))))))))

#?(:clj
   (defn- get-spec-vars
     []
     (when-let [spec-ns (find-ns 'cljs.spec.alpha)]
       (locking load-mutex
         {:registry-ref (ns-resolve spec-ns 'registry-ref)
          :speced-vars  (ns-resolve spec-ns '_speced_vars)})))
   :cljs
   (let [registry-ref (delay (get (ns-interns* 'cljs.spec.alpha$macros) 'registry-ref))
         ;; Here, we look up the symbol '-speced-vars because ns-interns*
         ;; is implemented by invoking demunge on the result of js-keys.
         speced-vars  (delay (get (ns-interns* 'cljs.spec.alpha$macros) '-speced-vars))]
     (defn- get-spec-vars []
       (when (some? (find-ns-obj 'cljs.spec.alpha$macros))
         {:registry-ref @registry-ref
          :speced-vars  @speced-vars}))))

(defn dump-specs
  "Dumps registered speced vars for a given namespace into the compiler
  environment."
  [ns]
  (let [spec-vars (get-spec-vars)
        ns-str    (str ns)]
    (swap! env/*compiler* update-in [::namespaces ns]
      merge
      (when-let [registry-ref (:registry-ref spec-vars)]
        {:cljs.spec/registry-ref
         (into []
           (filter (fn [[k _]] (= ns-str (namespace k))))
           @@registry-ref)})
      (when-let [speced-vars (:speced-vars spec-vars)]
        {:cljs.spec/speced-vars
         (into []
           (filter
             (fn [v]
               (or (= ns-str (namespace v))
                   (= ns (-> v meta :fdef-ns)))))
           @@speced-vars)}))))

(defn register-specs
  "Registers speced vars found in a namespace analysis cache."
  [cached-ns]
  #?(:clj (try
            (locking load-mutex
              (clojure.core/require 'cljs.spec.alpha))
            (catch Throwable t)))
  (let [{:keys [registry-ref speced-vars]} (get-spec-vars)]
    (when-let [registry (seq (:cljs.spec/registry-ref cached-ns))]
      (when registry-ref
        (swap! @registry-ref into registry)))
    (when-let [vars (seq (:cljs.spec/speced-vars cached-ns))]
      (when speced-vars
        (swap! @speced-vars into vars)))))

#?(:clj
   (defn write-analysis-cache
     ([ns cache-file]
       (write-analysis-cache ns cache-file nil))
     ([ns ^File cache-file src]
      (util/mkdirs cache-file)
      (dump-specs ns)
      (let [ext (util/ext cache-file)
            analysis (dissoc (get-in @env/*compiler* [::namespaces ns]) :macros)]
        (case ext
          "edn"  (spit cache-file
                   (str ";; Analyzed by ClojureScript " (util/clojurescript-version) "\n"
                     (pr-str analysis)))
          "json" (when-let [{:keys [writer write]} @transit]
                   (write
                     (writer (FileOutputStream. cache-file) :json
                       transit-write-opts)
                     analysis))))
      (when src
        (.setLastModified ^File cache-file (util/last-modified src))))))

#?(:clj
   (defn read-analysis-cache
     ([cache-file src]
      (read-analysis-cache cache-file src nil))
     ([^File cache-file src opts]
       ;; we want want to keep dependency analysis information
       ;; don't revert the environment - David
      (let [{:keys [ns]} (parse-ns src
                           (merge opts
                             {:restore false
                              :analyze-deps true
                              :load-macros true}))
            ext          (util/ext cache-file)
            cached-ns    (case ext
                           "edn"  (edn/read-string (slurp cache-file))
                           "json" (let [{:keys [reader read]} @transit]
                                    (with-open [is (io/input-stream cache-file)]
                                      (read (reader is :json transit-read-opts)))))]
        (when (or *verbose* (:verbose opts))
          (util/debug-prn "Reading analysis cache for" (str src)))
        (swap! env/*compiler*
          (fn [cenv]
            (do
              (register-specs cached-ns)
              (doseq [x (get-in cached-ns [::constants :order])]
                (register-constant! x))
              (-> cenv
                (assoc-in [::namespaces ns] cached-ns)))))))))

(defn analyze-form-seq
  ([forms]
   (analyze-form-seq forms
     (when env/*compiler*
       (:options @env/*compiler*))))
  ([forms opts]
   (analyze-form-seq forms opts false))
  ([forms opts return-last?]
   (let [env (assoc (empty-env) :build-options opts)]
     (binding [*file-defs* nil
               #?@(:clj [*unchecked-if* false
                         *unchecked-arrays* false])
               *cljs-ns* 'cljs.user
               *cljs-file* nil
               reader/*alias-map* (or reader/*alias-map* {})]
       (loop [ns nil forms forms last-ast nil]
         (if (some? forms)
           (let [form (first forms)
                 env  (assoc env :ns (get-namespace *cljs-ns*))
                 ast  (analyze env form nil opts)]
             (if (= (:op ast) :ns)
               (recur (:name ast) (next forms) ast)
               (recur ns (next forms) ast)))
           (if return-last?
             last-ast
             ns)))))))

(defn ensure-defs
  "Ensures that a non-nil defs map exists in the compiler state for a given
  ns. (A non-nil defs map signifies that the namespace has been analyzed.)"
  [ns]
  (swap! env/*compiler* update-in [::namespaces ns :defs] #(or % {})))

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
      (analyze-file f
        (when env/*compiler*
          (:options @env/*compiler*))))
     ([f opts]
      (analyze-file f false opts))
     ([f skip-cache opts]
      (binding [*file-defs*        (atom #{})
                *unchecked-if*     false
                *unchecked-arrays* false
                *cljs-warnings*    *cljs-warnings*]
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
                            (cache-file res ns-info output-dir :read opts))]
              (when-not (get-in @env/*compiler* [::namespaces (:ns ns-info) :defs])
                (if (or skip-cache (not cache) (requires-analysis? res cache output-dir opts))
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
                                      (cond
                                        (= (:op ast) :ns)
                                        (recur (:name ast) (next forms))

                                        (and (nil? ns) (= (:op ast) :ns*))
                                        (recur (gen-user-ns res) (next forms))

                                        :else
                                        (recur ns (next forms))))
                                    ns)))]
                      (ensure-defs ns)
                      (when (and cache (true? (:cache-analysis opts)))
                        (write-analysis-cache ns cache res))))
                  (try
                    (read-analysis-cache cache res opts)
                    (catch Throwable e
                      (analyze-file f true opts))))))))))))
