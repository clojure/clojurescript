;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(set! *warn-on-reflection* true)

(ns cljs.analyzer
  (:refer-clojure :exclude [macroexpand-1])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [cljs.env :as env]
            [cljs.tagged-literals :as tags]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers])
  (:import java.lang.StringBuilder
           java.io.File))

(def ^:dynamic *cljs-ns* 'cljs.user)
(def ^:dynamic *cljs-file* nil)
(def ^:dynamic *unchecked-if* (atom false))
(def ^:dynamic *cljs-static-fns* false)
(def ^:dynamic *cljs-macros-path* "/cljs/core")
(def ^:dynamic *cljs-macros-is-classpath* true)
(def -cljs-macros-loaded (atom false))

(def ^:dynamic *cljs-warnings*
  {:undeclared-var false
   :undeclared-ns false
   :undeclared-ns-form true
   :redef true
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
   :extending-base-js-type true})

(declare message namespaces)

(defmulti error-message (fn [warning-type & _] warning-type))

(defmethod error-message :undeclared-var
  [warning-type info]
  (str "Use of undeclared Var " (:prefix info) "/" (:suffix info)))

(defmethod error-message :undeclared-ns
  [warning-type info]
  (str "No such namespace: " (:ns-sym info)))

(defmethod error-message :dynamic
  [warning-type info]
  (str (:name info) " not declared ^:dynamic"))

(defmethod error-message :redef
  [warning-type info]
  (str (:sym info) " already refers to: " (symbol (str (:ns info)) (str (:sym info)))
    " being replaced by: " (symbol (str (:ns-name info)) (str (:sym info)))))

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
  (str "Referred " (:type info) " " (:lib info) "/" (:sym info) " does not exist"))

(defmethod error-message :protocol-deprecated
  [warning-type info]
  (str "Protocol " (:protocol info) " is deprecated"))

(defmethod error-message :undeclared-protocol-symbol
  [warning-type info]
  (str "Can't resolve protocol symbol " (:protocol info)))

(defmethod error-message :invalid-protocol-symbol
  [warning-type info]
  (str "Symbol " (:protocol info) " is not a protocol"))

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

(defn ^:private default-warning-handler [warning-type env extra]
  (when (warning-type *cljs-warnings*)
    (when-let [s (error-message warning-type extra)]
      (binding [*out* *err*]
        (println (message env (str "WARNING: " s)))))))

(def ^:dynamic *cljs-warning-handlers*
  [default-warning-handler])

(defn with-warning-handlers [handlers]
  (binding [*cljs-warning-handlers* handlers]))

(defmacro with-warning-handlers [handlers & body]
  `(binding [*cljs-warning-handlers* ~handlers]
     ~@body))

(defn munge-path [ss]
  (clojure.lang.Compiler/munge (str ss)))

(defn ns->relpath [s]
  (str (string/replace (munge-path s) \. \/) ".cljs"))

(def ^:private constant-counter (atom 0))

(defn gen-constant-id [value]
  (let [prefix (cond
                 (keyword? value) "constant$keyword$"
                 :else
                 (throw
                   (Exception. (str "constant type " (type value) " not supported"))))]
    (symbol (str prefix (swap! constant-counter inc)))))

(defn- register-constant! [val]
  (swap! env/*compiler* update-in [::constant-table]
    (fn [table]
      (if (get table val)
        table
        (assoc table val (gen-constant-id val))))))

(def default-namespaces '{cljs.core {:name cljs.core}
                          cljs.user {:name cljs.user}})

;; this exists solely to support read-only namespace access from macros.
;; External tools should look at the authoritative ::namespaces slot in the
;; compiler-env atoms/maps they're using already; this value will yield only
;; `default-namespaces` when accessed outside the scope of a
;; compilation/analysis call
(def namespaces
  (reify clojure.lang.IDeref
    (deref [_]
      (if-not (nil? env/*compiler*)
        (::namespaces @env/*compiler*)
        default-namespaces))))

(defn get-namespace [key]
  (get-in @env/*compiler* [::namespaces key]))

(defmacro no-warn [& body]
  (let [no-warnings (zipmap (keys *cljs-warnings*) (repeat false))]
    `(binding [*cljs-warnings* ~no-warnings]
       ~@body)))

(defmacro all-warn [& body]
  (let [all-warnings (zipmap (keys *cljs-warnings*) (repeat true))]
    `(binding [*cljs-warnings* ~all-warnings]
       ~@body)))

(defn get-line [x env]
  (or (-> x meta :line) (:line env)))

(defn get-col [x env]
  (or (-> x meta :column) (:column env)))

(defn load-core []
  (when (not @-cljs-macros-loaded)
    (reset! -cljs-macros-loaded true)
    (if *cljs-macros-is-classpath*
      (load *cljs-macros-path*)
      (load-file *cljs-macros-path*))))

(defmacro with-core-macros
  [path & body]
  `(do
     (when (not= *cljs-macros-path* ~path)
       (reset! -cljs-macros-loaded false))
     (binding [*cljs-macros-path* ~path]
       ~@body)))

(defmacro with-core-macros-file
  [path & body]
  `(do
     (when (not= *cljs-macros-path* ~path)
       (reset! -cljs-macros-loaded false))
     (binding [*cljs-macros-path* ~path
               *cljs-macros-is-classpath* false]
       ~@body)))

(defn empty-env []
  {:ns (get-namespace *cljs-ns*) :context :statement :locals {}})

(defmacro ^:private debug-prn
  [& args]
  `(.println System/err (str ~@args)))

(defn source-info
  ([env]
     (when-let [line (:line env)]
       {:file *cljs-file*
        :line (get-line name env)
        :column (get-col name env)}))
  ([name env]
     {:file *cljs-file*
      :line (get-line name env)
      :column (get-col name env)}))

(defn message [env s]
  (str s (when (:line env)
           (str " at line " (:line env) " " *cljs-file*))))

(defn warning [warning-type env extra]
  (doseq [handler *cljs-warning-handlers*]
    (handler warning-type env extra)))

(defn error
  ([env s] (error env s nil))
  ([env s cause]
   (ex-info (message env s)
            (assoc (source-info env) :tag :cljs/analysis-error)
            cause)))

(defn analysis-error? [ex]
  (= :cljs/analysis-error (:tag (ex-data ex))))

(defmacro wrapping-errors [env & body]
  `(try
     ~@body
     (catch Throwable err#
       (if (analysis-error? err#)
         (throw err#)
         (throw (error ~env (.getMessage err#) err#))))))

(defn confirm-var-exists [env prefix suffix]
  (when (:undeclared-var *cljs-warnings*)
    (let [crnt-ns (-> env :ns :name)]
      (when (= prefix crnt-ns)
        (when-not (get-in @env/*compiler* [::namespaces crnt-ns :defs suffix])
          (warning :undeclared-var env {:prefix prefix :suffix suffix}))))))

(defn resolve-ns-alias [env name]
  (let [sym (symbol name)]
    (get (:requires (:ns env)) sym sym)))

(defn confirm-ns [env ns-sym]
  (when (and (nil? (get '#{cljs.core goog Math} ns-sym))
             (nil? (get (-> env :ns :requires) ns-sym))
             ;; macros may refer to namespaces never explicitly required
             ;; confirm that the library at least exists
             (nil? (io/resource (ns->relpath ns-sym)))
             (:undeclared *cljs-warnings*))
    (warning :undeclared-ns env {:ns-sym ns-sym})))

(defn core-name?
  "Is sym visible from core in the current compilation namespace?"
  [env sym]
  (and (get-in @env/*compiler* [::namespaces 'cljs.core :defs sym]) 
       (not (contains? (-> env :ns :excludes) sym))))

(defn resolve-var
  "Resolve a var. Accepts a side-effecting confirm fn for producing
   warnings about unresolved vars."
  ([env sym] (resolve-var env sym nil))
  ([env sym confirm]
     (if (= (namespace sym) "js")
       {:name sym :ns 'js}
       (let [s (str sym)
             lb (-> env :locals sym)]
         (cond
           lb lb

           (namespace sym)
           (let [ns (namespace sym)
                 ns (if (= "clojure.core" ns) "cljs.core" ns)
                 full-ns (resolve-ns-alias env ns)]
             (when confirm
               (when (not= (-> env :ns :name) full-ns)
                 (confirm-ns env full-ns))
               (confirm env full-ns (symbol (name sym))))
             (merge (get-in @env/*compiler* [::namespaces full-ns :defs (symbol (name sym))])
                    {:name (symbol (str full-ns) (str (name sym)))
                     :ns full-ns}))

           (.contains s ".")
           (let [idx (.indexOf s ".")
                 prefix (symbol (subs s 0 idx))
                 suffix (subs s (inc idx))
                 lb (-> env :locals prefix)]
             (if lb
               {:name (symbol (str (:name lb) suffix))}
               (merge (get-in @env/*compiler* [::namespaces prefix :defs (symbol suffix)])
                 {:name (if (= "" prefix) (symbol suffix) (symbol (str prefix) suffix))
                  :ns prefix})))

           (get-in @env/*compiler* [::namespaces (-> env :ns :name) :uses sym])
           (let [full-ns (get-in @env/*compiler* [::namespaces (-> env :ns :name) :uses sym])]
             (merge
              (get-in @env/*compiler* [::namespaces full-ns :defs sym])
              {:name (symbol (str full-ns) (str sym))
               :ns (-> env :ns :name)}))

           (get-in @env/*compiler* [::namespaces (-> env :ns :name) :imports sym])
           (recur env (get-in @env/*compiler* [::namespaces (-> env :ns :name) :imports sym]) confirm)

           :else
           (let [full-ns (if (core-name? env sym)
                           'cljs.core
                           (-> env :ns :name))]
             (when confirm
               (confirm env full-ns sym))
             (merge (get-in @env/*compiler* [::namespaces full-ns :defs sym])
                    {:name (symbol (str full-ns) (str sym))
                     :ns full-ns})))))))

(defn resolve-existing-var [env sym]
  (if-not (-> sym meta ::no-resolve)
    (resolve-var env sym confirm-var-exists)
    (resolve-var env sym)))

(defn confirm-bindings [env names]
  (doseq [name names]
    (let [env (assoc env :ns (get-namespace *cljs-ns*))
          ev (resolve-existing-var env name)]
      (when (and (:dynamic *cljs-warnings*)
                 ev (not (-> ev :dynamic)))
        (warning :dynamic env {:ev ev})))))

(declare analyze analyze-symbol analyze-seq)

(def specials '#{if def fn* do let* loop* letfn* throw try recur new set! ns deftype* defrecord* . js* & quote})

(def ^:dynamic *recur-frames* nil)
(def ^:dynamic *loop-lets* nil)

(defmacro disallowing-recur [& body]
  `(binding [*recur-frames* (cons nil *recur-frames*)] ~@body))

;; TODO: move this logic out - David
(defn analyze-keyword
  [env sym]
  (register-constant! sym)
  {:op :constant :env env :form sym})

(defmulti parse (fn [op & rest] op))

(defmethod parse 'if
  [op env [_ test then else :as form] name]
  (when (< (count form) 3)
    (throw (error env "Too few arguments to if")))
  (let [test-expr (disallowing-recur (analyze (assoc env :context :expr) test))
        then-expr (analyze env then)
        else-expr (analyze env else)]
    {:env env :op :if :form form
     :test test-expr :then then-expr :else else-expr
     :unchecked @*unchecked-if*
     :children [test-expr then-expr else-expr]}))

(defmethod parse 'throw
  [op env [_ throw :as form] name]
  (let [throw-expr (disallowing-recur (analyze (assoc env :context :expr) throw))]
    {:env env :op :throw :form form
     :throw throw-expr
     :children [throw-expr]}))

(defmethod parse 'try
  [op env [_ & body :as form] name]
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

(defmethod parse 'def
  [op env form name]
  (let [pfn (fn
              ([_ sym] {:sym sym})
              ([_ sym init] {:sym sym :init init})
              ([_ sym doc init] {:sym sym :doc doc :init init}))
        args (apply pfn form)
        sym (:sym args)
        sym-meta (meta sym)
        tag (-> sym meta :tag)
        protocol (-> sym meta :protocol)
        dynamic (-> sym meta :dynamic)
        ns-name (-> env :ns :name)]
    (when (namespace sym)
      (throw (error env "Can't def ns-qualified name")))
    (when-let [doc (:doc args)]
      (when-not (string? doc)
        (throw (error env "Too many arguments to def"))))
    (let [env (if (or (and (not= ns-name 'cljs.core)
                           (core-name? env sym))
                      (get-in @env/*compiler* [::namespaces ns-name :uses sym]))
                (let [ev (resolve-existing-var (dissoc env :locals) sym)]
                  (when (:redef *cljs-warnings*)
                    (warning :redef env {:ev ev :sym sym :ns-name ns-name}))
                  (swap! env/*compiler* update-in [::namespaces ns-name :excludes] conj sym)
                  (update-in env [:ns :excludes] conj sym))
                env)
          name (:name (resolve-var (dissoc env :locals) sym))
          var-expr (assoc (analyze (-> env (dissoc :locals)
                                       (assoc :context :expr)
                                       (assoc :def-var true))
                                   sym)
                     :op :var)
          init-expr (when (contains? args :init)
                      (disallowing-recur
                        (analyze (assoc env :context :expr) (:init args) sym)))
          fn-var? (and init-expr (= (:op init-expr) :fn))
          export-as (when-let [export-val (-> sym meta :export)]
                      (if (= true export-val) name export-val))
          doc (or (:doc args) (-> sym meta :doc))]
      (when-let [v (get-in @env/*compiler* [::namespaces ns-name :defs sym])]
        (when (and (:fn-var *cljs-warnings*)
                   (not (-> sym meta :declared))
                   (and (:fn-var v) (not fn-var?)))
          (warning :fn-var env {:ns-name ns-name :sym sym})))
      (swap! env/*compiler* assoc-in [::namespaces ns-name :defs sym]
             (merge 
              {:name name}
              sym-meta
              (when doc {:doc doc})
              (when dynamic {:dynamic true})
              (source-info name env)
              ;; the protocol a protocol fn belongs to
              (when protocol
                {:protocol protocol})
              ;; symbol for reified protocol
              (when-let [protocol-symbol (-> sym meta :protocol-symbol)]
                {:protocol-symbol protocol-symbol
                 :impls #{}})
              (when fn-var?
                {:fn-var true
                 ;; protocol implementation context
                 :protocol-impl (:protocol-impl init-expr)
                 ;; inline protocol implementation context
                 :protocol-inline (:protocol-inline init-expr)
                 :variadic (:variadic init-expr)
                 :max-fixed-arity (:max-fixed-arity init-expr)
                 :method-params (map :params (:methods init-expr))})))
      (merge {:env env :op :def :form form
              :name name :var var-expr :doc doc :init init-expr}
             (when tag {:tag tag})
             (when dynamic {:dynamic true})
             (when export-as {:export export-as})
             (when init-expr {:children [init-expr]})))))

(defn- analyze-fn-method [env locals form type]
  (let [param-names (first form)
        variadic (boolean (some '#{&} param-names))
        param-names (vec (remove '#{&} param-names))
        body (next form)
        [locals params] (reduce (fn [[locals params] name]
                                  (let [param {:name name
                                               :line (get-line name env)
                                               :column (get-col name env)
                                               :tag (-> name meta :tag)
                                               :shadow (when locals (locals name))
                                               ;; Give the fn params the same shape
                                               ;; as a :var, so it gets routed
                                               ;; correctly in the compiler
                                               :op :var
                                               :env (merge (select-keys env [:context])
                                                           {:line (get-line name env)
                                                            :column (get-col name env)})
                                               :info {:name name
                                                      :shadow (when locals (locals name))}
                                               :binding-form? true}]
                                    [(assoc locals name param) (conj params param)]))
                                [locals []] param-names)
        fixed-arity (count (if variadic (butlast params) params))
        recur-frame {:params params :flag (atom nil)}
        expr (binding [*recur-frames* (cons recur-frame *recur-frames*)]
               (analyze (assoc env :context :return :locals locals) `(do ~@body)))]
    {:env env :variadic variadic :params params :max-fixed-arity fixed-arity
     :type type :form form :recurs @(:flag recur-frame) :expr expr}))

(defmethod parse 'fn*
  [op env [_ & args :as form] name]
  (let [[name meths] (if (symbol? (first args))
                       [(first args) (next args)]
                       [name (seq args)])
        ;;turn (fn [] ...) into (fn ([]...))
        meths (if (vector? (first meths)) (list meths) meths)
        locals (:locals env)
        name-var (if name
                   {:name name
                    :shadow (locals name)
                    :tag (-> name meta :tag)}) 
        locals (if (and locals name) (assoc locals name name-var) locals)
        type (-> form meta ::type)
        fields (-> form meta ::fields)
        protocol-impl (-> form meta :protocol-impl)
        protocol-inline (-> form meta :protocol-inline)
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
                       locals fields)

        menv (if (> (count meths) 1) (assoc env :context :expr) env)
        menv (merge menv
               {:protocol-impl protocol-impl
                :protocol-inline protocol-inline})
        methods (map #(analyze-fn-method menv locals % type) meths)
        max-fixed-arity (apply max (map :max-fixed-arity methods))
        variadic (boolean (some :variadic methods))
        locals (if name
                 (update-in locals [name] assoc
                            :fn-var true
                            :variadic variadic
                            :max-fixed-arity max-fixed-arity
                            :method-params (map :params methods))
                 locals)
        methods (if name
                  ;; a second pass with knowledge of our function-ness/arity
                  ;; lets us optimize self calls
                  (no-warn (doall (map #(analyze-fn-method menv locals % type) meths)))
                  methods)]
    (let [variadic-methods (filter :variadic methods)
          variadic-params (count (:params (first variadic-methods)))
          param-counts (map (comp count :params) methods)]
      (when (and (:multiple-variadic-overloads *cljs-warnings*) (< 1 (count variadic-methods)))
        (warning :multiple-variadic-overloads env {:name name-var}))
      (when (and (:variadic-max-arity *cljs-warnings*)
                 (not (or (zero? variadic-params) (= variadic-params (+ 1 max-fixed-arity)))))
        (warning :variadic-max-arity env {:name name-var}))
      (when (and (:overload-arity *cljs-warnings*) (not= (distinct param-counts) param-counts))
        (warning :overload-arity env {:name name-var})))
    {:env env :op :fn :form form :name name-var :methods methods :variadic variadic
     :recur-frames *recur-frames* :loop-lets *loop-lets*
     :jsdoc [(when variadic "@param {...*} var_args")]
     :max-fixed-arity max-fixed-arity
     :protocol-impl protocol-impl
     :protocol-inline protocol-inline
     :children (mapv :expr methods)}))

(defmethod parse 'letfn*
  [op env [_ bindings & exprs :as form] name]
  (when-not (and (vector? bindings) (even? (count bindings))) 
    (throw (error env "bindings must be vector of even number of elements")))
  (let [n->fexpr (into {} (map (juxt first second) (partition 2 bindings)))
        names    (keys n->fexpr)
        context  (:context env)
        [meth-env bes]
        (reduce (fn [[{:keys [locals] :as env} bes] n]
                  (let [be {:name   n
                            :line (get-line n env)
                            :column (get-col n env)
                            :tag    (-> n meta :tag)
                            :local  true
                            :shadow (locals n)}]
                    [(assoc-in env [:locals n] be)
                     (conj bes be)]))
                [env []] names)
        meth-env (assoc meth-env :context :expr)
        bes (vec (map (fn [{:keys [name shadow] :as be}]
                        (let [env (assoc-in meth-env [:locals name] shadow)]
                          (assoc be :init (analyze env (n->fexpr name)))))
                      bes))
        expr (analyze (assoc meth-env :context (if (= :expr context) :return context)) `(do ~@exprs))]
    {:env env :op :letfn :bindings bes :expr expr :form form
     :children (conj (vec (map :init bes)) expr)}))

(defmethod parse 'do
  [op env [_ & exprs :as form] _]
  (let [statements (disallowing-recur
                     (seq (map #(analyze (assoc env :context :statement) %) (butlast exprs))))
        ret (if (<= (count exprs) 1)
              (analyze env (first exprs))
              (analyze (assoc env :context (if (= :statement (:context env)) :statement :return)) (last exprs)))]
    {:env env :op :do :form form
     :statements statements :ret ret
     :children (conj (vec statements) ret)}))

(defn analyze-let
  [encl-env [_ bindings & exprs :as form] is-loop]
  (when-not (and (vector? bindings) (even? (count bindings))) 
    (throw (error encl-env "bindings must be vector of even number of elements")))
  (let [context (:context encl-env)
        [bes env]
        (disallowing-recur
         (loop [bes []
                env (assoc encl-env :context :expr)
                bindings (seq (partition 2 bindings))]
           (if-let [[name init] (first bindings)]
             (do
               (when (or (namespace name) (.contains (str name) "."))
                 (throw (error encl-env (str "Invalid local name: " name))))
               (let [init-expr (binding [*loop-lets* (cons {:params bes} (or *loop-lets* ()))]
                                 (analyze env init))
                     be {:name name
                         :line (get-line name env)
                         :column (get-col name env)
                         :init init-expr
                         :tag (or (-> name meta :tag)
                                  (-> init-expr :tag)
                                  (-> init-expr :info :tag))
                         :local true
                         :shadow (-> env :locals name)
                         ;; Give let* bindings same shape as var so
                         ;; they get routed correctly in the compiler
                         :op :var
                         :env {:line (get-line name env)
                               :column (get-col name env)}
                         :info {:name name
                                :shadow (-> env :locals name)}
                         :binding-form? true}
                     be (if (= (:op init-expr) :fn)
                          (merge be
                            {:fn-var true
                             :variadic (:variadic init-expr)
                             :max-fixed-arity (:max-fixed-arity init-expr)
                             :method-params (map :params (:methods init-expr))})
                          be)]
                 (recur (conj bes be)
                        (assoc-in env [:locals name] be)
                        (next bindings))))
             [bes env])))
        recur-frame (when is-loop {:params bes :flag (atom nil)})
        expr
        (binding [*recur-frames* (if recur-frame (cons recur-frame *recur-frames*) *recur-frames*)
                  *loop-lets* (cond
                               is-loop (or *loop-lets* ())
                               *loop-lets* (cons {:params bes} *loop-lets*))]
          (analyze (assoc env :context (if (= :expr context) :return context)) `(do ~@exprs)))]
    {:env encl-env :op (if is-loop :loop :let)
     :bindings bes :expr expr :form form
     :children (conj (vec (map :init bes)) expr)}))

(defmethod parse 'let*
  [op encl-env form _]
  (analyze-let encl-env form false))

(defmethod parse 'loop*
  [op encl-env form _]
  (analyze-let encl-env form true))

(defmethod parse 'recur
  [op env [_ & exprs :as form] _]
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
  [_ env [_ x] _]
  (analyze (assoc env :quoted? true) x))

(defmethod parse 'new
  [_ env [_ ctor & args :as form] _]
  (when-not (symbol? ctor) 
    (throw (error env "First arg to new must be a symbol")))
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         ctorexpr (analyze enve ctor)
         argexprs (vec (map #(analyze enve %) args))
         known-num-fields (:num-fields (resolve-existing-var env ctor))
         argc (count args)]
     (when (and known-num-fields (not= known-num-fields argc) (:fn-arity *cljs-warnings*))
       (warning :fn-arity env {:argc argc :ctor ctor}))

     {:env env :op :new :form form :ctor ctorexpr :args argexprs
      :children (into [ctorexpr] argexprs)})))

(defmethod parse 'set!
  [_ env [_ target val alt :as form] _]
  (let [[target val] (if alt
                       ;; (set! o -prop val)
                       [`(. ~target ~val) alt]
                       [target val])]
    (disallowing-recur
     (let [enve (assoc env :context :expr)
           targetexpr (cond
                       ;; TODO: proper resolve
                       (= target '*unchecked-if*)
                       (do
                         (reset! *unchecked-if* val)
                         ::set-unchecked-if)

                       (symbol? target)
                       (do
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

(defn analyze-deps [deps]
  (doseq [dep deps]
    (when-not (contains? (::namespaces @env/*compiler*) dep)
      (let [relpath (ns->relpath dep)]
        (when (io/resource relpath)
          (no-warn
            (analyze-file relpath)))))))

(defn check-uses [uses env]
  (doseq [[sym lib] uses]
    (when (and (:undeclared *cljs-warnings*)
               (= (get-in @env/*compiler* [::namespaces lib :defs sym] ::not-found) ::not-found))
      (warning :undeclared-ns-form env {:type :var :lib lib :sym sym}))))

(defn check-use-macros [use-macros env]
  (doseq [[sym lib] use-macros]
    (when (and (:undeclared-ns *cljs-warnings*)
               (nil? (.findInternedVar ^clojure.lang.Namespace (find-ns lib) sym)))
      (warning :undeclared-ns env {:type :macro :lib lib :sym sym}))))

(defmethod parse 'ns
  [_ env [_ name & args :as form] _]
  (when-not (symbol? name) 
    (throw (error env "Namespaces must be named by a symbol.")))
  (let [docstring (if (string? (first args)) (first args))
        args      (if docstring (next args) args)
        metadata  (if (map? (first args)) (first args))
        args      (if metadata (next args) args)
        excludes
        (reduce (fn [s [k exclude xs]]
                  (if (= k :refer-clojure)
                    (do
                      (when-not (= exclude :exclude) 
                        (throw (error env "Only [:refer-clojure :exclude (names)] form supported")))
                      (when (seq s)
                        (throw (error env "Only one :refer-clojure form is allowed per namespace definition")))
                      (into s xs))
                    s))
                #{} args)
        deps (atom #{})
        aliases (atom {:fns #{} :macros #{}})
        valid-forms (atom #{:use :use-macros :require :require-macros :import})
        error-msg (fn [spec msg] (str msg "; offending spec: " (pr-str spec)))
        parse-require-spec (fn parse-require-spec [macros? spec]
                             (when-not (or (symbol? spec) (vector? spec))
                               (throw (error env (error-msg spec "Only [lib.ns & options] and lib.ns specs supported in :require / :require-macros"))))
                             (when (vector? spec)
                               (when-not (symbol? (first spec))
                                 (throw (error env (error-msg spec "Library name must be specified as a symbol in :require / :require-macros"))))
                               (when-not (odd? (count spec))
                                 (throw (error env (error-msg spec "Only :as alias and :refer (names) options supported in :require"))))
                               (when-not (every? #{:as :refer} (map first (partition 2 (next spec))))
                                 (throw (error env (error-msg spec "Only :as and :refer options supported in :require / :require-macros"))))
                               (when-not (let [fs (frequencies (next spec))]
                                           (and (<= (fs :as 0) 1)
                                                (<= (fs :refer 0) 1)))
                                 (throw (error env (error-msg spec "Each of :as and :refer options may only be specified once in :require / :require-macros")))))
                             (if (symbol? spec)
                               (recur macros? [spec])
                               (let [[lib & opts] spec
                                     {alias :as referred :refer :or {alias lib}} (apply hash-map opts)
                                     [rk uk] (if macros? [:require-macros :use-macros] [:require :use])]
                                 (when-not (or (symbol? alias) (nil? alias))
                                   (throw (error env (error-msg spec ":as must be followed by a symbol in :require / :require-macros"))))
                                 (when alias
                                   (let [alias-type (if macros? :macros :fns)]
                                     (when (contains? (alias-type @aliases) alias)
                                       (throw (error env (error-msg spec ":as alias must be unique"))))
                                     (swap! aliases
                                            update-in [alias-type]
                                            conj alias)))
                                 (when-not (or (and (sequential? referred) (every? symbol? referred))
                                               (nil? referred))
                                   (throw (error env (error-msg spec ":refer must be followed by a sequence of symbols in :require / :require-macros"))))
                                 (when-not macros?
                                   (swap! deps conj lib))
                                 (merge
                                   (when alias
                                     {rk (merge {alias lib} {lib lib})})
                                   (when referred {uk (apply hash-map (interleave referred (repeat lib)))})))))
        use->require (fn use->require [[lib kw referred :as spec]]
                       (when-not (and (symbol? lib) (= :only kw) (sequential? referred) (every? symbol? referred))
                         (throw (error env (error-msg spec "Only [lib.ns :only (names)] specs supported in :use / :use-macros"))))
                       [lib :refer referred])
        parse-import-spec (fn parse-import-spec [spec]
                            (when-not (or (and (sequential? spec)
                                               (every? symbol? spec))
                                          (and (symbol? spec) (nil? (namespace spec))))
                              (throw (error env (error-msg spec "Only lib.ns.Ctor or [lib.ns Ctor*] spec supported in :import"))))
                            (let [import-map (if (sequential? spec)
                                               (->> (rest spec)
                                                 (map #(vector % (symbol (str (first spec) "." %))))
                                                 (into {}))
                                               {(symbol (last (string/split (str spec) #"\."))) spec})]
                              (doseq [[_ spec] import-map]
                                (swap! deps conj spec))
                              {:import  import-map
                               :require import-map}))
        spec-parsers {:require        (partial parse-require-spec false)
                      :require-macros (partial parse-require-spec true)
                      :use            (comp (partial parse-require-spec false) use->require)
                      :use-macros     (comp (partial parse-require-spec true) use->require)
                      :import         parse-import-spec}
        {uses :use requires :require use-macros :use-macros require-macros :require-macros imports :import :as params}
        (reduce (fn [m [k & libs]]
                  (when-not (#{:use :use-macros :require :require-macros :import} k)
                    (throw (error env "Only :refer-clojure, :require, :require-macros, :use and :use-macros libspecs supported")))
                  (when-not (@valid-forms k)
                    (throw (error env (str "Only one " k " form is allowed per namespace definition"))))
                  (swap! valid-forms disj k)
                  (apply merge-with merge m (map (spec-parsers k) libs)))
                {} (remove (fn [[r]] (= r :refer-clojure)) args))]
    (when (seq @deps)
      (analyze-deps @deps))
    (when (seq uses)
      (check-uses uses env))
    (set! *cljs-ns* name)
    (load-core)
    (doseq [nsym (concat (vals require-macros) (vals use-macros))]
      (clojure.core/require nsym))
    (when (seq use-macros)
      (check-use-macros use-macros env))
    (swap! env/*compiler* update-in [::namespaces name] assoc
           :name name
           :doc docstring
           :excludes excludes
           :uses uses
           :requires requires
           :use-macros use-macros
           :require-macros require-macros
           :imports imports)
    {:env env :op :ns :form form :name name :doc docstring :uses uses :requires requires :imports imports
     :use-macros use-macros :require-macros require-macros :excludes excludes}))

(defmethod parse 'deftype*
  [_ env [_ tsym fields pmasks :as form] _]
  (let [t (:name (resolve-var (dissoc env :locals) tsym))]
    (swap! env/*compiler* update-in [::namespaces (-> env :ns :name) :defs tsym]
           (fn [m]
             (let [m (assoc (or m {})
                       :name t
                       :type true
                       :num-fields (count fields))]
               (merge m
                 {:protocols (-> tsym meta :protocols)}
                 (source-info tsym env)))))
    {:env env :op :deftype* :form form :t t :fields fields :pmasks pmasks}))

(defmethod parse 'defrecord*
  [_ env [_ tsym fields pmasks :as form] _]
  (let [t (:name (resolve-var (dissoc env :locals) tsym))]
    (swap! env/*compiler* update-in [::namespaces (-> env :ns :name) :defs tsym]
           (fn [m]
             (let [m (assoc (or m {}) :name t :type true)]
               (merge m
                 {:protocols (-> tsym meta :protocols)}
                 (source-info tsym env)))))
    {:env env :op :defrecord* :form form :t t :fields fields :pmasks pmasks}))

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
  (throw (Error. (str "Cannot provide arguments " args " on property access " prop))))

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
  (throw (Error. (str "Unknown dot form of " (list* '. dot-form) " with classification " (classify-dot-form dot-form)))))

(defmethod parse '.
  [_ env [_ target & [field & member+] :as form] _]
  (disallowing-recur
   (let [{:keys [dot-action target method field args]} (build-dot-form [target field member+])
         enve        (assoc env :context :expr)
         targetexpr  (analyze enve target)]
     (case dot-action
           ::access {:env env :op :dot :form form
                     :target targetexpr
                     :field field
                     :children [targetexpr]
                     :tag (-> form meta :tag)}
           ::call   (let [argexprs (map #(analyze enve %) args)]
                      {:env env :op :dot :form form
                       :target targetexpr
                       :method method
                       :args argexprs
                       :children (into [targetexpr] argexprs)
                       :tag (-> form meta :tag)})))))

(defmethod parse 'js*
  [op env [_ jsform & args :as form] _]
  (when-not (string? jsform)
    (throw (error env "Invalid js* form")))
  (if args
    (disallowing-recur
     (let [seg (fn seg [^String s]
                 (let [idx (.indexOf s "~{")]
                   (if (= -1 idx)
                     (list s)
                     (let [end (.indexOf s "}" idx)]
                       (lazy-seq
                         (cons (subs s 0 idx)
                           (seg (subs s (inc end)))))))))
           enve (assoc env :context :expr)
           argexprs (vec (map #(analyze enve %) args))]
       {:env env :op :js :segs (seg jsform) :args argexprs
        :tag (-> form meta :tag) :form form :children argexprs
        :js-op (-> form meta :js-op)}))
    (let [interp (fn interp [^String s]
                   (let [idx (.indexOf s "~{")]
                     (if (= -1 idx)
                       (list s)
                       (let [end (.indexOf s "}" idx)
                             inner (:name (resolve-existing-var env (symbol (subs s (+ 2 idx) end))))]
                         (lazy-seq
                           (cons (subs s 0 idx)
                             (cons inner
                               (interp (subs s (inc end))))))))))]
      {:env env :op :js :form form :code (apply str (interp jsform))
       :tag (-> form meta :tag) :js-op (-> form meta :js-op)})))

(defn parse-invoke
  [env [f & args :as form]]
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         fexpr (analyze enve f)
         argexprs (vec (map #(analyze enve %) args))
         argc (count args)]
     (if (and (:fn-arity *cljs-warnings*) (-> fexpr :info :fn-var))
       (let [{:keys [variadic max-fixed-arity method-params name]} (:info fexpr)]
         (when (and (not (some #{argc} (map count method-params)))
                    (or (not variadic)
                        (and variadic (< argc max-fixed-arity))))
           (warning :fn-arity env {:name name
                                   :argc argc}))))
     (if (and (:fn-deprecated *cljs-warnings*) (-> fexpr :info :deprecated)
              (not (-> form meta :deprecation-nowarn)))
       (warning :fn-deprecated env {:fexpr fexpr}))
     {:env env :op :invoke :form form :f fexpr :args argexprs
      :tag (or (-> fexpr :info :tag) (-> form meta :tag)) :children (into [fexpr] argexprs)})))

(defn analyze-symbol
  "Finds the var associated with sym"
  [env sym]
  (if (:quoted? env)
    {:op :constant :env env :form sym}
    (let [ret {:env env :form sym}
          lb (-> env :locals sym)]
      (if lb
        (assoc ret :op :var :info lb)
        (if-not (:def-var env)
          (assoc ret :op :var :info (resolve-existing-var env sym))
          (assoc ret :op :var :info (resolve-var env sym)))))))

(defn get-expander [sym env]
  (let [mvar
        (when-not (or (-> env :locals sym)        ;locals hide macros
                      (and (or (-> env :ns :excludes sym)
                               (get-in @env/*compiler* [::namespaces (-> env :ns :name) :excludes sym]))
                           (not (or (-> env :ns :use-macros sym)
                                    (get-in @env/*compiler* [::namespaces (-> env :ns :name) :use-macros sym])))))
          (if-let [nstr (namespace sym)]
            (when-let [ns (cond
                           (= "clojure.core" nstr) (find-ns 'cljs.core)
                           (.contains nstr ".") (find-ns (symbol nstr))
                           :else
                           (some-> env :ns :require-macros (get (symbol nstr)) find-ns))]
              (.findInternedVar ^clojure.lang.Namespace ns (symbol (name sym))))
            (if-let [nsym (-> env :ns :use-macros sym)]
              (.findInternedVar ^clojure.lang.Namespace (find-ns nsym) sym)
              (.findInternedVar ^clojure.lang.Namespace (find-ns 'cljs.core) sym))))]
    (when (and mvar (.isMacro ^clojure.lang.Var mvar))
      @mvar)))

(defn macroexpand-1 [env form]
  (env/ensure
    (let [op (first form)]
      (if (specials op)
        form
        (if-let [mac (and (symbol? op) (get-expander op env))]
          (binding [*ns* (create-ns *cljs-ns*)]
            (let [form' (apply mac form env (rest form))]
              (if (seq? form')
                (let [sym' (first form')
                       sym  (first form)]
                  (if (= sym' 'js*)
                    (vary-meta form' assoc
                      :js-op (if (namespace sym) sym (symbol "cljs.core" (str sym))))
                    form'))
                form')))
          (if (symbol? op)
            (let [opname (str op)]
              (cond
                (= (first opname) \.) (let [[target & args] (next form)]
                                        (with-meta (list* '. target (symbol (subs opname 1)) args)
                                          (meta form)))
                (= (last opname) \.) (with-meta
                                       (list* 'new (symbol (subs opname 0 (dec (count opname)))) (next form))
                                       (meta form))
                :else form))
            form))))))

(declare analyze-list)

(defn analyze-seq
  [env form name]
  (if (:quoted? env)
    (analyze-list env form)
    (let [env (assoc env
                :line (or (-> form meta :line)
                          (:line env))
                :column (or (-> form meta :column)
                            (:column env)))]
      (let [op (first form)]
        (when (nil? op)
          (throw (error env "Can't call nil")))
        (let [mform (macroexpand-1 env form)]
          (if (identical? form mform)
            (wrapping-errors env
              (if (specials op)
                (parse op env form name)
                (parse-invoke env form)))
            (analyze env mform name)))))))

(declare analyze-wrap-meta)

(defn analyze-map
  [env form]
  (let [expr-env (assoc env :context :expr)
        ks (disallowing-recur (vec (map #(analyze expr-env %) (keys form))))
        vs (disallowing-recur (vec (map #(analyze expr-env %) (vals form))))]
    (analyze-wrap-meta {:op :map :env env :form form
                        :keys ks :vals vs
                        :children (vec (interleave ks vs))})))

(defn analyze-list
  [env form]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (doall (map #(analyze expr-env %) form)))]
    (analyze-wrap-meta {:op :list :env env :form form :items items :children items})))

(defn analyze-vector
  [env form]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (vec (map #(analyze expr-env %) form)))]
    (analyze-wrap-meta {:op :vector :env env :form form :items items :children items})))

(defn analyze-set
  [env form ]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (vec (map #(analyze expr-env %) form)))]
    (analyze-wrap-meta {:op :set :env env :form form :items items :children items})))

(defn analyze-wrap-meta [expr]
  (let [form (:form expr)
        m (dissoc (meta form) :line :column)]
    (if (seq m)
      (let [env (:env expr) ; take on expr's context ourselves
            expr (assoc-in expr [:env :context] :expr) ; change expr to :expr
            meta-expr (analyze-map (:env expr) m)]
        {:op :meta :env env :form form
         :meta meta-expr :expr expr :children [meta-expr expr]})
      expr)))

(defn analyze
  "Given an environment, a map containing {:locals (mapping of names to bindings), :context
  (one of :statement, :expr, :return), :ns (a symbol naming the
  compilation ns)}, and form, returns an expression object (a map
  containing at least :form, :op and :env keys). If expr has any (immediately)
  nested exprs, must have :children [exprs...] entry. This will
  facilitate code walking without knowing the details of the op set."
  ([env form] (analyze env form nil))
  ([env form name]
    (env/ensure
      (wrapping-errors env
        (binding [reader/*alias-map* (or reader/*alias-map* {})]
          (let [form (if (instance? clojure.lang.LazySeq form)
                       (or (seq form) ())
                       form)]
            (load-core)
            (cond
              (symbol? form) (analyze-symbol env form)
              (and (seq? form) (seq form)) (analyze-seq env form name)
              (map? form) (analyze-map env form)
              (vector? form) (analyze-vector env form)
              (set? form) (analyze-set env form)
              (keyword? form) (analyze-keyword env form)
              (= form ()) (analyze-list env form)
              :else {:op :constant :env env :form form})))))))

(defn- source-path
  "Returns a path suitable for providing to tools.reader as a 'filename'."
  [x]
  (cond
   (instance? File x) (.getAbsolutePath ^File x)
   :default (str x)))

(defn forms-seq
  "Seq of Clojure/ClojureScript forms from [f], which can be anything for which
`clojure.java.io/reader` can produce a `java.io.Reader`. Optionally accepts a [filename]
argument, which the reader will use in any emitted errors."
  ([f] (forms-seq f (source-path f)))
  ([f filename]
     (let [rdr (readers/indexing-push-back-reader (java.io.PushbackReader. (io/reader f)) 1 filename)
           forms-seq*
           (fn forms-seq* []
             (lazy-seq
              (let [eof-sentinel (Object.)
                    form (binding [*ns* (create-ns *cljs-ns*)
                                   reader/*alias-map*
                                   (apply merge
                                          ((juxt :requires :requires-macros)
                                           (get-namespace *cljs-ns*)))]
                           (reader/read rdr nil eof-sentinel))]
                (if (identical? form eof-sentinel)
                  nil
                  (cons form (forms-seq*))))))]
       (forms-seq*))))

(defn analyze-file [f]
  (let [res (cond
             (instance? File f) f
             (re-find #"^file://" f) (java.net.URL. f)
             :else (io/resource f))]
    (assert res (str "Can't find " f " in classpath"))
    (env/ensure
      (binding [*cljs-ns* 'cljs.user
                *cljs-file* (if (instance? File res)
                              (.getPath ^File res)
                              (.getPath ^java.net.URL res))
                reader/*alias-map* (or reader/*alias-map* {})]
        (let [env (empty-env)]
          (doseq [form (seq (forms-seq res))]
            (let [env (assoc env :ns (get-namespace *cljs-ns*))]
              (analyze env form))))))))
