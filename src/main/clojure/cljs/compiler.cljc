;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.compiler
  #?(:clj (:refer-clojure :exclude [munge macroexpand-1 ensure])
     :cljs (:refer-clojure :exclude [munge macroexpand-1 ensure js-reserved]))
  #?(:cljs (:require-macros [cljs.compiler.macros :refer [emit-wrap]]
                            [cljs.env.macros :refer [ensure]]))
  #?(:clj (:require [cljs.util :as util]
                    [clojure.java.io :as io]
                    [clojure.string :as string]
                    [clojure.set :as set]
                    [clojure.tools.reader :as reader]
                    [cljs.env :as env :refer [ensure]]
                    [cljs.tagged-literals :as tags]
                    [cljs.analyzer :as ana]
                    [cljs.source-map :as sm]
                    [clojure.data.json :as json]
                    [cljs.js-deps :as deps])
     :cljs (:require [goog.string :as gstring]
                     [clojure.string :as string]
                     [clojure.set :as set]
                     [cljs.tools.reader :as reader]
                     [cljs.env :as env]
                     [cljs.analyzer :as ana]
                     [cljs.source-map :as sm]))
  #?(:clj (:import java.lang.StringBuilder
                   [java.io File Writer]
                   [java.util.concurrent Executors ExecutorService TimeUnit]
                   [java.util.concurrent.atomic AtomicLong]
                   [cljs.tagged_literals JSValue])
     :cljs (:import [goog.string StringBuffer])))

#?(:clj (set! *warn-on-reflection* true))

(def js-reserved ana/js-reserved)

(def ^:private es5>=
  (into #{}
    (comp
      (mapcat (fn [lang]
                [lang (keyword (string/replace (name lang) #"^ecmascript" "es"))])))
    [:ecmascript5 :ecmascript5-strict :ecmascript6 :ecmascript6-strict
     :ecmascript-2015 :ecmascript6-typed :ecmascript-2016 :ecmascript-2017
     :ecmascript-next]))

(def ^:dynamic *recompiled* nil)
(def ^:dynamic *inputs* nil)
(def ^:dynamic *source-map-data* nil)
(def ^:dynamic *source-map-data-gen-col* nil)
(def ^:dynamic *lexical-renames* {})

(def cljs-reserved-file-names #{"deps.cljs"})

(defn get-first-ns-segment
  "Gets the part up to the first `.` of a namespace.
   Returns the empty string for nil.
   Returns the entire string if no `.` in namespace"
  [ns]
  (let [ns (str ns)
        idx (.indexOf ns ".")]
    (if (== -1 idx)
      ns
      (subs ns 0 idx))))

(defn ^:dynamic find-ns-starts-with [needle]
  (reduce-kv
    (fn [xs ns _]
      (when (= needle (get-first-ns-segment ns))
        (reduced needle)))
    nil
    (::ana/namespaces @env/*compiler*)))

; Helper fn
(defn shadow-depth [s]
  (let [{:keys [name info]} s]
    (loop [d 0, {:keys [shadow]} info]
      (cond
        shadow (recur (inc d) shadow)
        (find-ns-starts-with (str name)) (inc d)
        :else d))))

(defn hash-scope [s]
  #?(:clj  (or (:identity s) (System/identityHashCode s))
     :cljs (hash-combine (-hash ^not-native (:name s))
             (shadow-depth s))))

(declare munge)

(defn fn-self-name [{:keys [name info] :as name-var}]
  (let [name (string/replace (str name) ".." "_DOT__DOT_")
        {:keys [ns fn-scope]} info
        scoped-name (apply str
                      (interpose "_$_"
                        (concat (map (comp str :name) fn-scope) [name])))]
    (symbol
      (munge
        (str (string/replace (str ns) "." "$") "$" scoped-name)))))

(defn munge-reserved [reserved]
  (fn [s]
    (if-not (nil? (get reserved s))
      (str s "$")
      s)))

(defn munge
  ([s] (munge s js-reserved))
  ([s reserved]
   (if #?(:clj  (map? s)
          :cljs (ana/cljs-map? s))
     (let [name-var s
           name     (:name name-var)
           field    (:field name-var)
           info     (:info name-var)]
       (if-not (nil? (:fn-self-name info))
         (fn-self-name s)
         ;; Unshadowing
         (let [depth       (shadow-depth s)
               code        (hash-scope s)
               renamed     (get *lexical-renames* code)
               name        (cond
                             (true? field) (str "self__." name)
                             (not (nil? renamed)) renamed
                             :else name)
               munged-name (munge name reserved)]
           (if (or (true? field) (zero? depth))
             munged-name
             (symbol (str munged-name "__$" depth))))))
     ;; String munging
     (let [ss (string/replace (str s) ".." "_DOT__DOT_")
           ss (string/replace ss
                #?(:clj #"\/(.)" :cljs (js/RegExp. "\\/(.)")) ".$1") ; Division is special
           rf (munge-reserved reserved)
           ss (map rf (string/split ss #"\."))
           ss (string/join "." ss)
           ms #?(:clj (clojure.lang.Compiler/munge ss)
                 :cljs (#'cljs.core/munge-str ss))]
       (if (symbol? s)
         (symbol ms)
         ms)))))

(defn- comma-sep [xs]
  (interpose "," xs))

(defn- escape-char [^Character c]
  (let [cp #?(:clj (.hashCode c)
              :cljs (gstring/hashCode c))]
    (case cp
      ; Handle printable escapes before ASCII
      34 "\\\""
      92 "\\\\"
      ; Handle non-printable escapes
      8 "\\b"
      12 "\\f"
      10 "\\n"
      13 "\\r"
      9 "\\t"
      (if (< 31 cp 127)
        c ; Print simple ASCII characters
        #?(:clj (format "\\u%04X" cp)                       ; Any other character is Unicode
           :cljs (let [unpadded (.toString cp 16)
                       pad      (subs "0000" (.-length unpadded))]
                   (str "\\u" pad unpadded)))))))

(defn- escape-string [^CharSequence s]
  (let [sb #?(:clj (StringBuilder. (count s))
              :cljs (StringBuffer.))]
    (doseq [c s]
      (.append sb (escape-char c)))
    (.toString sb)))

(defn- wrap-in-double-quotes [x]
  (str \" x \"))

(defmulti emit* :op)

(defn emit [ast]
  (when *source-map-data*
    (let [{:keys [env]} ast]
      (when (:line env)
        (let [{:keys [line column]} env]
          (swap! *source-map-data*
            (fn [m]
              (let [minfo (cond-> {:gcol  #?(:clj  (.get ^AtomicLong *source-map-data-gen-col*)
                                             :cljs (:gen-col m))
                                   :gline (:gen-line m)}
                            (#{:var :local :js-var :binding} (:op ast))
                            (assoc :name (str (-> ast :info :name))))]
                ; Dec the line/column numbers for 0-indexing.
                ; tools.reader uses 1-indexed sources, chrome
                ; expects 0-indexed source maps.
                (update-in m [:source-map (dec line)]
                  (fnil (fn [line]
                          (update-in line [(if column (dec column) 0)]
                            (fnil (fn [column] (conj column minfo)) [])))
                    (sorted-map))))))))))
  (emit* ast))

(defn emits 
  ([])
  ([^Object a]
   (cond
     (nil? a) nil
     #?(:clj (map? a) :cljs (ana/cljs-map? a)) (emit a)
     #?(:clj (seq? a) :cljs (ana/cljs-seq? a)) (apply emits a)
     #?(:clj (fn? a) :cljs ^boolean (goog/isFunction a)) (a)
     :else (let [^String s (cond-> a (not (string? a)) .toString)]
             #?(:clj  (when-some [^AtomicLong gen-col *source-map-data-gen-col*]
                        (.addAndGet gen-col (.length s)))
                :cljs (when-some [sm-data *source-map-data*]
                        (swap! sm-data update :gen-col #(+ % (.-length s)))))
             #?(:clj  (.write ^Writer *out* s)
                :cljs (print s))))
    nil)
  ([a b]
   (emits a) (emits b))
  ([a b c]
   (emits a) (emits b) (emits c))
  ([a b c d]
   (emits a) (emits b) (emits c) (emits d))
  ([a b c d e]
   (emits a) (emits b) (emits c) (emits d) (emits e))
  ([a b c d e & xs]
   (emits a) (emits b) (emits c) (emits d) (emits e)
   (doseq [x xs] (emits x))))

(defn ^:private _emitln []
  (newline)
  (when *source-map-data*
    #?(:clj (.set ^AtomicLong *source-map-data-gen-col* 0))
    (swap! *source-map-data*
      (fn [{:keys [gen-line] :as m}]
        (assoc m
          :gen-line (inc gen-line)
          #?@(:cljs [:gen-col 0])))))
  nil)

(defn emitln
  ([] (_emitln))
  ([a]
   (emits a) (_emitln))
  ([a b]
   (emits a) (emits b) (_emitln))
  ([a b c]
   (emits a) (emits b) (emits c) (_emitln))
  ([a b c d]
   (emits a) (emits b) (emits c) (emits d) (_emitln))
  ([a b c d e]
   (emits a) (emits b) (emits c) (emits d) (emits e) (_emitln))
  ([a b c d e & xs]
   (emits a) (emits b) (emits c) (emits d) (emits e)
   (doseq [x xs] (emits x))
   (_emitln)))

(defn ^String emit-str [expr]
  (with-out-str (emit expr)))

#?(:clj
   (defmulti emit-constant* class)
   :cljs
   (defmulti emit-constant* type))

(declare emit-map emit-list emit-vector emit-set emit-js-object emit-js-array
         emit-with-meta emit-constants-comma-sep emit-constant emit-record-value)

(defn all-distinct? [xs]
  (apply distinct? xs))

#?(:clj
   (defn emit-constant-no-meta [x]
     (cond
       (seq? x) (emit-list x emit-constants-comma-sep)
       (record? x) (let [[ns name] (ana/record-ns+name x)]
                     (emit-record-value ns name #(emit-constant (into {} x))))
       (map? x) (emit-map (keys x) (vals x) emit-constants-comma-sep all-distinct?)
       (vector? x) (emit-vector x emit-constants-comma-sep)
       (set? x) (emit-set x emit-constants-comma-sep all-distinct?)
       :else (emit-constant* x)))
   :cljs
   (defn emit-constant-no-meta [x]
     (cond
       (ana/cljs-seq? x) (emit-list x emit-constants-comma-sep)
       (record? x) (let [[ns name] (ana/record-ns+name x)]
                     (emit-record-value ns name #(emit-constant (into {} x))))
       (ana/cljs-map? x) (emit-map (keys x) (vals x) emit-constants-comma-sep all-distinct?)
       (ana/cljs-vector? x) (emit-vector x emit-constants-comma-sep)
       (ana/cljs-set? x) (emit-set x emit-constants-comma-sep all-distinct?)
       :else (emit-constant* x))))

(defn emit-constant [v]
  (let [m (ana/elide-irrelevant-meta (meta v))]
    (if (some? (seq m))
      (emit-with-meta #(emit-constant-no-meta v) #(emit-constant-no-meta m))
      (emit-constant-no-meta v))))

(defmethod emit-constant* :default
  [x]
  (throw
    (ex-info (str "failed compiling constant: " x "; "
               (pr-str (type x)) " is not a valid ClojureScript constant.")
      {:constant x
       :type (type x)
       :clojure.error/phase :compilation})))

(defmethod emit-constant* nil [x] (emits "null"))

#?(:clj
   (defmethod emit-constant* Long [x] (emits "(" x ")")))

#?(:clj
   (defmethod emit-constant* Integer [x] (emits x))) ; reader puts Integers in metadata

#?(:clj
   (defmethod emit-constant* Double [x]
     (let [x (double x)]
       (cond (Double/isNaN x)
             (emits "NaN")

             (Double/isInfinite x)
             (emits (if (pos? x) "Infinity" "-Infinity"))

             :else (emits x))))
   :cljs
   (defmethod emit-constant* js/Number [x]
     (cond (js/isNaN x)
           (emits "NaN")

           (not (js/isFinite x))
           (emits (if (pos? x) "Infinity" "-Infinity"))

           :else (emits "(" x ")"))))

#?(:clj
   (defmethod emit-constant* BigDecimal [x] (emits (.doubleValue ^BigDecimal x))))

#?(:clj
   (defmethod emit-constant* clojure.lang.BigInt [x] (emits (.doubleValue ^clojure.lang.BigInt x))))

(defmethod emit-constant* #?(:clj String :cljs js/String) [x]
  (emits (wrap-in-double-quotes (escape-string x))))

(defmethod emit-constant* #?(:clj Boolean :cljs js/Boolean) [x] (emits (if x "true" "false")))

#?(:clj
   (defmethod emit-constant* Character [x]
     (emits (wrap-in-double-quotes (escape-char x)))))

(defmethod emit-constant* #?(:clj java.util.regex.Pattern :cljs js/RegExp) [x]
  (if (= "" (str x))
    (emits "(new RegExp(\"\"))")
    (let [[_ flags pattern] (re-find #"^(?:\(\?([idmsux]*)\))?(.*)" (str x))]
      #?(:clj  (emits \/
                 (.replaceAll (re-matcher #"/" pattern) "\\\\/")
                 \/ flags)
         :cljs (emits pattern)))))

(defn emits-keyword [kw]
  (let [ns   (namespace kw)
        name (name kw)]
    (emits "new cljs.core.Keyword(")
    (emit-constant ns)
    (emits ",")
    (emit-constant name)
    (emits ",")
    (emit-constant (if ns
                     (str ns "/" name)
                     name))
    (emits ",")
    (emit-constant (hash kw))
    (emits ")")))

(defn emits-symbol [sym]
  (let [ns     (namespace sym)
        name   (name sym)
        symstr (if-not (nil? ns)
                 (str ns "/" name)
                 name)]
    (emits "new cljs.core.Symbol(")
    (emit-constant ns)
    (emits ",")
    (emit-constant name)
    (emits ",")
    (emit-constant symstr)
    (emits ",")
    (emit-constant (hash sym))
    (emits ",")
    (emit-constant nil)
    (emits ")")))

(defmethod emit-constant* #?(:clj clojure.lang.Keyword :cljs Keyword) [x]
  (if-let [value (and (-> @env/*compiler* :options :emit-constants)
                      (-> @env/*compiler* ::ana/constant-table x))]
    (emits "cljs.core." value)
    (emits-keyword x)))

(defmethod emit-constant* #?(:clj clojure.lang.Symbol :cljs Symbol) [x]
  (if-let [value (and (-> @env/*compiler* :options :emit-constants)
                      (-> @env/*compiler* ::ana/constant-table x))]
    (emits "cljs.core." value)
    (emits-symbol x)))

(defn emit-constants-comma-sep [cs]
  (fn []
    (doall
      (map-indexed (fn [i m]
                     (if (even? i)
                       (emit-constant m)
                       (emits m)))
                   (comma-sep cs)))))

(def ^:private array-map-threshold 8)

;; tagged literal support

(defmethod emit-constant* #?(:clj java.util.Date :cljs js/Date) [^java.util.Date date]
  (emits "new Date(" (.getTime date) ")"))

(defmethod emit-constant* #?(:clj java.util.UUID :cljs UUID) [^java.util.UUID uuid]
  (let [uuid-str (.toString uuid)]
    (emits "new cljs.core.UUID(\"" uuid-str "\", " (hash uuid-str) ")")))

(defmethod emit-constant* #?(:clj JSValue :cljs cljs.tagged-literals/JSValue) [^JSValue v]
  (let [items (.-val v)]
    (if (map? items)
      (emit-js-object items #(fn [] (emit-constant %)))
      (emit-js-array items emit-constants-comma-sep))))

#?(:clj
   (defmacro emit-wrap [env & body]
     `(let [env# ~env]
        (when (= :return (:context env#)) (emits "return "))
        ~@body
        (when-not (= :expr (:context env#)) (emitln ";")))))

(defmethod emit* :no-op [m])

(defn emit-var
  [{:keys [info env form] :as ast}]
  (if-let [const-expr (:const-expr ast)]
    (emit (assoc const-expr :env env))
    (let [{:keys [options] :as cenv} @env/*compiler*
          var-name (:name info)
          info (if (= (namespace var-name) "js")
                 (let [js-module-name (get-in cenv [:js-module-index (name var-name) :name])]
                   (or js-module-name (name var-name)))
                 info)]
      ;; We need a way to write bindings out to source maps and javascript
      ;; without getting wrapped in an emit-wrap calls, otherwise we get
      ;; e.g. (function greet(return x, return y) {}).
      (if (:binding-form? ast)
        ;; Emit the arg map so shadowing is properly handled when munging
        ;; (prevents duplicate fn-param-names)
        (emits (munge ast))
        (when-not (= :statement (:context env))
          (let [reserved (cond-> js-reserved
                           (and (es5>= (:language-out options))
                                ;; we can skip munging things like `my.ns.default`
                                ;; but not standalone `default` variable names
                                ;; as they're not valid ES5 - Antonio
                                (some? (namespace var-name)))
                           (set/difference ana/es5-allowed))
                js-module (get-in cenv [:js-namespaces (or (namespace var-name) (name var-name))])
                info (cond-> info
                       (not= form 'js/-Infinity) (munge reserved))]
            (emit-wrap env
              (case (:module-type js-module)
                ;; Closure exports CJS exports through default property
                :commonjs
                (if (namespace var-name)
                  (emits (munge (namespace var-name) reserved) "[\"default\"]." (munge (name var-name) reserved))
                  (emits (munge (name var-name) reserved) "[\"default\"]"))

                ;; Emit bracket notation for default prop access instead of dot notation
                :es6
                (if (and (namespace var-name) (= "default" (name var-name)))
                  (emits (munge (namespace var-name) reserved) "[\"default\"]")
                  (emits info))

                (emits info)))))))))

(defmethod emit* :var [expr] (emit-var expr))
(defmethod emit* :binding [expr] (emit-var expr))
(defmethod emit* :js-var [expr] (emit-var expr))
(defmethod emit* :local [expr] (emit-var expr))

(defmethod emit* :the-var
  [{:keys [env var sym meta] :as arg}]
  {:pre [(ana/ast? sym) (ana/ast? meta)]}
  (let [{:keys [name]} (:info var)]
    (emit-wrap env
      (emits "new cljs.core.Var(function(){return " (munge name) ";},"
        sym "," meta ")"))))

(defn emit-with-meta [expr meta]
  (emits "cljs.core.with_meta(" expr "," meta ")"))

(defmethod emit* :with-meta
  [{:keys [expr meta env]}]
  (emit-wrap env
    (emit-with-meta expr meta)))

(defn distinct-keys? [keys]
  (let [keys (map ana/unwrap-quote keys)]
    (and (every? #(= (:op %) :const) keys)
         (= (count (into #{} keys)) (count keys)))))

(defn emit-map [keys vals comma-sep distinct-keys?]
  (cond
    (zero? (count keys))
    (emits "cljs.core.PersistentArrayMap.EMPTY")

    (<= (count keys) array-map-threshold)
    (if (distinct-keys? keys)
      (emits "new cljs.core.PersistentArrayMap(null, " (count keys) ", ["
        (comma-sep (interleave keys vals))
        "], null)")
      (emits "cljs.core.PersistentArrayMap.createAsIfByAssoc(["
        (comma-sep (interleave keys vals))
        "])"))

    :else
    (emits "cljs.core.PersistentHashMap.fromArrays(["
      (comma-sep keys)
      "],["
      (comma-sep vals)
      "])")))

(defmethod emit* :map
  [{:keys [env keys vals]}]
  (emit-wrap env
    (emit-map keys vals comma-sep distinct-keys?)))

(defn emit-list [items comma-sep]
  (if (empty? items)
    (emits "cljs.core.List.EMPTY")
    (emits "cljs.core.list(" (comma-sep items) ")")))

(defn emit-vector [items comma-sep]
  (if (empty? items)
    (emits "cljs.core.PersistentVector.EMPTY")
    (let [cnt (count items)]
      (if (< cnt 32)
        (emits "new cljs.core.PersistentVector(null, " cnt
          ", 5, cljs.core.PersistentVector.EMPTY_NODE, ["  (comma-sep items) "], null)")
        (emits "cljs.core.PersistentVector.fromArray([" (comma-sep items) "], true)")))))

(defmethod emit* :vector
  [{:keys [items env]}]
  (emit-wrap env
    (emit-vector items comma-sep)))

(defn distinct-constants? [items]
  (let [items (map ana/unwrap-quote items)]
    (and (every? #(= (:op %) :const) items)
         (= (count (into #{} items)) (count items)))))

(defn emit-set [items comma-sep distinct-constants?]
  (cond
    (empty? items)
    (emits "cljs.core.PersistentHashSet.EMPTY")

    (distinct-constants? items)
    (emits "new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, " (count items) ", ["
      (comma-sep (interleave items (repeat "null"))) "], null), null)")

    :else (emits "cljs.core.PersistentHashSet.createAsIfByAssoc([" (comma-sep items) "])")))

(defmethod emit* :set
  [{:keys [items env]}]
  (emit-wrap env
    (emit-set items comma-sep distinct-constants?)))

(defn emit-js-object [items emit-js-object-val]
  (emits "({")
  (when-let [items (seq items)]
    (let [[[k v] & r] items]
      (emits "\"" (name k) "\": " (emit-js-object-val v))
      (doseq [[k v] r]
        (emits ", \"" (name k) "\": " (emit-js-object-val v)))))
  (emits "})"))

(defn emit-js-array [items comma-sep]
  (emits "[" (comma-sep items) "]"))

(defmethod emit* :js-object 
  [{:keys [keys vals env]}]
  (emit-wrap env
    (emit-js-object (map vector keys vals) identity)))

(defmethod emit* :js-array 
  [{:keys [items env]}]
  (emit-wrap env
    (emit-js-array items comma-sep)))

(defn emit-record-value
  [ns name items]
  (emits ns ".map__GT_" name "(" items ")"))

(defmethod emit* :quote
  [{:keys [expr]}]
  (emit expr))

(defmethod emit* :const
  [{:keys [form env]}]
  (when-not (= :statement (:context env))
    (emit-wrap env (emit-constant form))))

(defn truthy-constant? [expr]
  (let [{:keys [op form const-expr]} (ana/unwrap-quote expr)]
    (or (and (= op :const)
             form
             (not (or (and (string? form) (= form ""))
                      (and (number? form) (zero? form)))))
        (and (some? const-expr)
             (truthy-constant? const-expr)))))

(defn falsey-constant? [expr]
  (let [{:keys [op form const-expr]} (ana/unwrap-quote expr)]
    (or (and (= op :const)
             (or (false? form) (nil? form)))
        (and (some? const-expr)
             (falsey-constant? const-expr)))))

(defn safe-test? [env e]
  (let [tag (ana/infer-tag env e)]
    (or (#{'boolean 'seq} tag) (truthy-constant? e))))

(defmethod emit* :if
  [{:keys [test then else env unchecked]}]
  (let [context (:context env)
        checked (not (or unchecked (safe-test? env test)))]
    (cond
      (truthy-constant? test) (emitln then)
      (falsey-constant? test) (emitln else)
      :else
      (if (= :expr context)
        (emits "(" (when checked "cljs.core.truth_") "(" test ")?" then ":" else ")")
        (do
          (if checked
            (emitln "if(cljs.core.truth_(" test ")){")
            (emitln "if(" test "){"))
          (emitln then "} else {")
          (emitln else "}"))))))

(defmethod emit* :case
  [{v :test :keys [nodes default env]}]
  (when (= (:context env) :expr)
    (emitln "(function(){"))
  (let [gs (gensym "caseval__")]
    (when (= :expr (:context env))
      (emitln "var " gs ";"))
    (emitln "switch (" v ") {")
    (doseq [{ts :tests {:keys [then]} :then} nodes]
      (doseq [test (map :test ts)]
        (emitln "case " test ":"))
      (if (= :expr (:context env))
        (emitln gs "=" then)
        (emitln then))
      (emitln "break;"))
    (when default
      (emitln "default:")
      (if (= :expr (:context env))
        (emitln gs "=" default)
        (emitln default)))
    (emitln "}")
    (when (= :expr (:context env))
      (emitln "return " gs ";})()"))))

(defmethod emit* :throw
  [{throw :exception :keys [env]}]
  (if (= :expr (:context env))
    (emits "(function(){throw " throw "})()")
    (emitln "throw " throw ";")))

(def base-types
  #{"null" "*" "...*"
    "boolean" "Boolean"
    "string" "String"
    "number" "Number"
    "array" "Array"
    "object" "Object"
    "RegExp"
    "Date"})

(def mapped-types
  {"nil" "null"})

(defn resolve-type [env ^String t]
  (cond
    (get base-types t) t

    (get mapped-types t) (get mapped-types t)

    #?(:clj  (.startsWith t "!")
       :cljs (gstring/startsWith t "!"))
    (str "!" (resolve-type env (subs t 1)))

    #?(:clj  (.startsWith t "{")
       :cljs (gstring/startsWith t "{")) t

    #?(:clj  (.startsWith t "function")
       :cljs (gstring/startsWith t "function"))
    (let [idx         (.lastIndexOf t ":")
          [fstr rstr] (if-not (== -1 idx)
                        [(subs t 0 idx) (subs t (inc idx) (count t))]
                        [t nil])
          ret-t       (when rstr (resolve-type env rstr))
          axstr       (subs fstr 9 (dec (count fstr)))
          args-ts     (when-not (string/blank? axstr)
                        (map (comp #(resolve-type env %) string/trim)
                          (string/split axstr #",")))]
      (cond-> (str "function(" (string/join "," args-ts) ")")
        ret-t (str ":" ret-t)))

    #?(:clj  (.endsWith t "=")
       :cljs (gstring/endsWith t "="))
    (str (resolve-type env (subs t 0 (dec (count t)))) "=")

    :else
    (munge (str (:name (ana/resolve-var env (symbol t)))))))

(defn resolve-types [env ts]
  (let [ts (-> ts string/trim (subs 1 (dec (count ts))))
        xs (string/split ts #"\|")]
    (str "{" (string/join "|" (map #(resolve-type env %) xs)) "}")))

(defn munge-param-return [env line]
  (cond
    (re-find #"@param" line)
    (let [[p ts n & xs] (map string/trim
                          (string/split (string/trim line) #" "))]
      (if (and (= "@param" p)
               ts #?(:clj  (.startsWith ^String ts "{")
                     :cljs (gstring/startsWith ts "{")))
        (string/join " " (concat [p (resolve-types env ts) (munge n)] xs))
        line))

    (re-find #"@return" line)
    (let [[p ts & xs] (map string/trim
                        (string/split (string/trim line) #" "))]
      (if (and (= "@return" p)
               ts #?(:clj  (.startsWith ^String ts "{")
                     :cljs (gstring/startsWith ts "{")))
        (string/join " " (concat [p (resolve-types env ts)] xs))
        line))

    :else line))

(defn checking-types? []
  (#{:error :warning}
    (get-in @env/*compiler*
      [:options :closure-warnings :check-types])))

(defn emit-comment
  "Emit a nicely formatted comment string."
  ([doc jsdoc]
    (emit-comment nil doc jsdoc))
  ([env doc jsdoc]
   (let [docs (when doc [doc])
         docs (if jsdoc (concat docs jsdoc) docs)
         docs (remove nil? docs)]
     (letfn [(print-comment-lines [e]
               (let [[x & ys]
                     (map #(if (checking-types?) (munge-param-return env %) %)
                       (string/split-lines e))]
                 (emitln " * " (string/replace x "*/" "* /"))
                 (doseq [next-line ys]
                   (emitln " * "
                     (-> next-line
                       (string/replace #"^   " "")
                       (string/replace "*/" "* /"))))))]
       (when (seq docs)
         (emitln "/**")
         (doseq [e docs]
           (when e
             (print-comment-lines e)))
         (emitln " */"))))))

(defn valid-define-value? [x]
  (or (string? x)
      (true? x)
      (false? x)
      (number? x)))

(defn get-define [mname jsdoc]
  (let [opts (get @env/*compiler* :options)]
    (and (some #?(:clj #(.startsWith ^String % "@define")
                  :cljs #(gstring/startsWith % "@define"))
           jsdoc)
         opts
         (= (:optimizations opts) :none)
         (let [define (get-in opts [:closure-defines (str mname)])]
           (when (valid-define-value? define)
             (pr-str define))))))

(defmethod emit* :def
  [{:keys [name var init env doc jsdoc export test var-ast]}]
  ;; We only want to emit if an init is supplied, this is to avoid dead code
  ;; elimination issues. The REPL is the exception to this rule.
  (when (or init (:def-emits-var env))
    (let [mname (munge name)]
     (emit-comment env doc (concat jsdoc (:jsdoc init)))
     (when (= :return (:context env))
         (emitln "return ("))
     (when (:def-emits-var env)
       (emitln "(function (){"))
     (emits var)
     (when init
       (emits " = "
         (if-let [define (get-define mname jsdoc)]
           define
           init)))
     (when (:def-emits-var env)
       (emitln "; return (")
       (emits (merge
                {:op  :the-var
                 :env (assoc env :context :expr)}
                var-ast))
       (emitln ");})()"))
     (when (= :return (:context env))
         (emitln ")"))
     ;; NOTE: JavaScriptCore does not like this under advanced compilation
     ;; this change was primarily for REPL interactions - David
     ;(emits " = (typeof " mname " != 'undefined') ? " mname " : undefined")
     (when-not (= :expr (:context env)) (emitln ";"))
     (when export
       (emitln "goog.exportSymbol('" (munge export) "', " mname ");"))
     (when (and ana/*load-tests* test)
       (when (= :expr (:context env))
         (emitln ";"))
       (emitln var ".cljs$lang$test = " test ";")))))

(defn emit-apply-to
  [{:keys [name params env]}]
  (let [arglist (gensym "arglist__")
        delegate-name (str (munge name) "__delegate")]
    (emitln "(function (" arglist "){")
    (doseq [[i param] (map-indexed vector (drop-last 2 params))]
      (emits "var ")
      (emit param)
      (emits " = cljs.core.first(")
      (emitln arglist ");")
      (emitln arglist " = cljs.core.next(" arglist ");"))
    (if (< 1 (count params))
      (do
        (emits "var ")
        (emit (last (butlast params)))
        (emitln " = cljs.core.first(" arglist ");")
        (emits "var ")
        (emit (last params))
        (emitln " = cljs.core.rest(" arglist ");")
        (emits "return " delegate-name "(")
        (doseq [param params]
          (emit param)
          (when-not (= param (last params)) (emits ",")))
        (emitln ");"))
      (do
        (emits "var ")
        (emit (last params))
        (emitln " = cljs.core.seq(" arglist ");")
        (emits "return " delegate-name "(")
        (doseq [param params]
          (emit param)
          (when-not (= param (last params)) (emits ",")))
        (emitln ");")))
    (emits "})")))

(defn emit-fn-params [params]
  (doseq [param params]
    (emit param)
    ; Avoid extraneous comma (function greet(x, y, z,)
    (when-not (= param (last params))
      (emits ","))))

(defn emit-fn-method
  [{expr :body :keys [type name params env recurs]}]
  (emit-wrap env
    (emits "(function " (munge name) "(")
    (emit-fn-params params)
    (emitln "){")
    (when type
      (emitln "var self__ = this;"))
    (when recurs (emitln "while(true){"))
    (emits expr)
    (when recurs
      (emitln "break;")
      (emitln "}"))
    (emits "})")))

(defn emit-arguments-to-array
  "Emit code that copies function arguments into an array starting at an index.
  Returns name of var holding the array."
  [startslice]
  (assert (and (>= startslice 0) (integer? startslice)))
  (let [mname (munge (gensym))
        i (str mname "__i")
        a (str mname "__a")]
    (emitln "var " i " = 0, "
                   a " = new Array(arguments.length -  " startslice ");")
    (emitln "while (" i " < " a ".length) {"
      a "[" i "] = arguments[" i " + " startslice "]; ++" i ";}")
    a))

(defn emit-variadic-fn-method
  [{expr :body max-fixed-arity :fixed-arity variadic :variadic? :keys [type name params env recurs] :as f}]
  (emit-wrap env
    (let [name (or name (gensym))
          mname (munge name)
          delegate-name (str mname "__delegate")]
      (emitln "(function() { ")
      (emits "var " delegate-name " = function (")
      (doseq [param params]
        (emit param)
        (when-not (= param (last params)) (emits ",")))
      (emitln "){")
      (when type
        (emitln "var self__ = this;"))
      (when recurs (emitln "while(true){"))
      (emits expr)
      (when recurs
        (emitln "break;")
        (emitln "}"))
      (emitln "};")

      (emitln "var " mname " = function (" (comma-sep
                                             (if variadic
                                               (concat (butlast params) ['var_args])
                                               params)) "){")
      (when type
        (emitln "var self__ = this;"))
      (when variadic
        (emits "var ")
        (emit (last params))
        (emitln " = null;")
        (emitln "if (arguments.length > " (dec (count params)) ") {")
        (let [a (emit-arguments-to-array (dec (count params)))]
          (emitln "  " (last params) " = new cljs.core.IndexedSeq(" a ",0,null);"))
        (emitln "} "))
      (emits "return " delegate-name ".call(this,")
      (doseq [param params]
        (emit param)
        (when-not (= param (last params)) (emits ",")))
      (emits ");")
      (emitln "};")

      (emitln mname ".cljs$lang$maxFixedArity = " max-fixed-arity ";")
      (emits mname ".cljs$lang$applyTo = ")
      (emit-apply-to (assoc f :name name))
      (emitln ";")
      (emitln mname ".cljs$core$IFn$_invoke$arity$variadic = " delegate-name ";")
      (emitln "return " mname ";")
      (emitln "})()"))))

(defmethod emit* :fn
  [{variadic :variadic? :keys [name env methods max-fixed-arity recur-frames loop-lets]}]
  ;;fn statements get erased, serve no purpose and can pollute scope if named
  (when-not (= :statement (:context env))
    (let [loop-locals (->> (concat (mapcat :params (filter #(and % @(:flag %)) recur-frames))
                                   (mapcat :params loop-lets))
                           (map munge)
                           seq)]
      (when loop-locals
        (when (= :return (:context env))
            (emits "return "))
        (emitln "((function (" (comma-sep (map munge loop-locals)) "){")
        (when-not (= :return (:context env))
            (emits "return ")))
      (if (= 1 (count methods))
        (if variadic
          (emit-variadic-fn-method (assoc (first methods) :name name))
          (emit-fn-method (assoc (first methods) :name name)))
        (let [name (or name (gensym))
              mname (munge name)
              maxparams (apply max-key count (map :params methods))
              mmap (into {}
                     (map (fn [method]
                            [(munge (symbol (str mname "__" (count (:params method)))))
                             method])
                          methods))
              ms (sort-by #(-> % second :params count) (seq mmap))]
          (when (= :return (:context env))
            (emits "return "))
          (emitln "(function() {")
          (emitln "var " mname " = null;")
          (doseq [[n meth] ms]
            (emits "var " n " = ")
            (if (:variadic? meth)
              (emit-variadic-fn-method meth)
              (emit-fn-method meth))
            (emitln ";"))
            (emitln mname " = function(" (comma-sep (if variadic
                                                      (concat (butlast maxparams) ['var_args])
                                                      maxparams)) "){")
          (when variadic
            (emits "var ")
            (emit (last maxparams))
            (emitln " = var_args;"))
          (emitln "switch(arguments.length){")
          (doseq [[n meth] ms]
            (if (:variadic? meth)
              (do (emitln "default:")
                  (let [restarg (munge (gensym))]
                    (emitln "var " restarg " = null;")
                    (emitln "if (arguments.length > " max-fixed-arity ") {")
                    (let [a (emit-arguments-to-array max-fixed-arity)]
                      (emitln restarg " = new cljs.core.IndexedSeq(" a ",0,null);"))
                    (emitln "}")
                    (emitln "return " n ".cljs$core$IFn$_invoke$arity$variadic("
                            (comma-sep (butlast maxparams))
                            (when (> (count maxparams) 1) ", ")
                            restarg ");")))
              (let [pcnt (count (:params meth))]
                (emitln "case " pcnt ":")
                (emitln "return " n ".call(this" (if (zero? pcnt) nil
                                                     (list "," (comma-sep (take pcnt maxparams)))) ");"))))
          (emitln "}")
          (let [arg-count-js (if (= 'self__ (-> ms first val :params first :name))
                               "(arguments.length - 1)"
                               "arguments.length")]
            (emitln "throw(new Error('Invalid arity: ' + " arg-count-js "));"))
          (emitln "};")
          (when variadic
            (emitln mname ".cljs$lang$maxFixedArity = " max-fixed-arity ";")
            (emitln mname ".cljs$lang$applyTo = " (some #(let [[n m] %] (when (:variadic? m) n)) ms) ".cljs$lang$applyTo;"))
          (doseq [[n meth] ms]
            (let [c (count (:params meth))]
              (if (:variadic? meth)
                (emitln mname ".cljs$core$IFn$_invoke$arity$variadic = " n ".cljs$core$IFn$_invoke$arity$variadic;")
                (emitln mname ".cljs$core$IFn$_invoke$arity$" c " = " n ";"))))
          (emitln "return " mname ";")
          (emitln "})()")))
      (when loop-locals
        (emitln ";})(" (comma-sep loop-locals) "))")))))

(defmethod emit* :do
  [{:keys [statements ret env]}]
  (let [context (:context env)]
    (when (and (seq statements) (= :expr context)) (emitln "(function (){"))
    (doseq [s statements] (emitln s))
    (emit ret)
    (when (and (seq statements) (= :expr context)) (emitln "})()"))))

(defmethod emit* :try
  [{try :body :keys [env catch name finally]}]
  (let [context (:context env)]
    (if (or name finally)
      (do
        (when (= :expr context)
          (emits "(function (){"))
        (emits "try{" try "}")
        (when name
          (emits "catch (" (munge name) "){" catch "}"))
        (when finally
          (assert (not= :const (:op (ana/unwrap-quote finally))) "finally block cannot contain constant")
          (emits "finally {" finally "}"))
        (when (= :expr context)
          (emits "})()")))
      (emits try))))

(defn emit-let
  [{expr :body :keys [bindings env]} is-loop]
  (let [context (:context env)]
    (when (= :expr context) (emits "(function (){"))
    (binding [*lexical-renames*
              (into *lexical-renames*
                (when (= :statement context)
                  (map
                    (fn [binding]
                      (let [name (:name binding)]
                        (vector (hash-scope binding)
                          (gensym (str name "-")))))
                    bindings)))]
      (doseq [{:keys [init] :as binding} bindings]
        (emits "var ")
        (emit binding) ; Binding will be treated as a var
        (emitln " = " init ";"))
      (when is-loop (emitln "while(true){"))
      (emits expr)
      (when is-loop
        (emitln "break;")
        (emitln "}")))
    (when (= :expr context) (emits "})()"))))

(defmethod emit* :let [ast]
  (emit-let ast false))

(defmethod emit* :loop [ast]
  (emit-let ast true))

(defmethod emit* :recur
  [{:keys [frame exprs env]}]
  (let [temps (vec (take (count exprs) (repeatedly gensym)))
        params (:params frame)]
    (dotimes [i (count exprs)]
      (emitln "var " (temps i) " = " (exprs i) ";"))
    (dotimes [i (count exprs)]
      (emitln (munge (params i)) " = " (temps i) ";"))
    (emitln "continue;")))

(defmethod emit* :letfn
  [{expr :body :keys [bindings env]}]
  (let [context (:context env)]
    (when (= :expr context) (emits "(function (){"))
    (doseq [{:keys [init] :as binding} bindings]
      (emitln "var " (munge binding) " = " init ";"))
    (emits expr)
    (when (= :expr context) (emits "})()"))))

(defn protocol-prefix [psym]
  (symbol (str (-> (str psym)
                 (.replace #?(:clj \. :cljs (js/RegExp. "\\." "g")) \$)
                 (.replace \/ \$))
            "$")))

(defmethod emit* :invoke
  [{f :fn :keys [args env] :as expr}]
  (let [info (:info f)
        fn? (and ana/*cljs-static-fns*
                 (not (:dynamic info))
                 (:fn-var info))
        protocol (:protocol info)
        tag      (ana/infer-tag env (first (:args expr)))
        proto? (and protocol tag
                 (or (and ana/*cljs-static-fns* protocol (= tag 'not-native)) 
                     (and
                       (or ana/*cljs-static-fns*
                           (:protocol-inline env))
                       (or (= protocol tag)
                           ;; ignore new type hints for now - David
                           (and (not (set? tag))
                                (not ('#{any clj clj-or-nil clj-nil number string boolean function object array js} tag))
                                (when-let [ps (:protocols (ana/resolve-existing-var env tag))]
                                  (ps protocol)))))))
        opt-not? (and (= (:name info) 'cljs.core/not)
                      (= (ana/infer-tag env (first (:args expr))) 'boolean))
        ns (:ns info)
        js? (or (= ns 'js) (= ns 'Math))
        goog? (when ns
                (or (= ns 'goog)
                    (when-let [ns-str (str ns)]
                      (= (get (string/split ns-str #"\.") 0 nil) "goog"))
                    (not (contains? (::ana/namespaces @env/*compiler*) ns))))

        keyword? (or (= 'cljs.core/Keyword (ana/infer-tag env f))
                     (let [f (ana/unwrap-quote f)]
                       (and (= (-> f :op) :const)
                            (keyword? (-> f :form)))))
        [f variadic-invoke]
        (if fn?
          (let [arity (count args)
                variadic? (:variadic? info)
                mps (:method-params info)
                mfa (:max-fixed-arity info)]
            (cond
             ;; if only one method, no renaming needed
             (and (not variadic?)
                  (= (count mps) 1))
             [f nil]

             ;; direct dispatch to variadic case
             (and variadic? (> arity mfa))
             [(update-in f [:info]
                (fn [info]
                  (-> info
                    (assoc :name (symbol (str (munge info) ".cljs$core$IFn$_invoke$arity$variadic")))
                    ;; bypass local fn-self-name munging, we're emitting direct
                    ;; shadowing already applied
                    (update-in [:info]
                      #(-> % (dissoc :shadow) (dissoc :fn-self-name))))))
              {:max-fixed-arity mfa}]

             ;; direct dispatch to specific arity case
             :else
             (let [arities (map count mps)]
               (if (some #{arity} arities)
                 [(update-in f [:info]
                    (fn [info]
                      (-> info
                        (assoc :name (symbol (str (munge info) ".cljs$core$IFn$_invoke$arity$" arity)))
                        ;; bypass local fn-self-name munging, we're emitting direct
                        ;; shadowing already applied
                        (update-in [:info]
                          #(-> % (dissoc :shadow) (dissoc :fn-self-name)))))) nil]
                 [f nil]))))
          [f nil])]
    (emit-wrap env
      (cond
       opt-not?
       (emits "(!(" (first args) "))")

       proto?
       (let [pimpl (str (munge (protocol-prefix protocol))
                        (munge (name (:name info))) "$arity$" (count args))]
         (emits (first args) "." pimpl "(" (comma-sep (cons "null" (rest args))) ")"))

       keyword?
       (emits f ".cljs$core$IFn$_invoke$arity$" (count args) "(" (comma-sep args) ")")

       variadic-invoke
       (let [mfa (:max-fixed-arity variadic-invoke)]
        (emits f "(" (comma-sep (take mfa args))
               (when-not (zero? mfa) ",")
               "cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["
               (comma-sep (drop mfa args)) "], 0))"))

       (or fn? js? goog?)
       (emits f "(" (comma-sep args)  ")")

       :else
       (if (and ana/*cljs-static-fns* (#{:var :local :js-var} (:op f)))
         ;; higher order case, static information missing
         (let [fprop (str ".cljs$core$IFn$_invoke$arity$" (count args))]
           (if ana/*fn-invoke-direct*
             (emits "(" f fprop " ? " f fprop "(" (comma-sep args) ") : "
                    f "(" (comma-sep args) "))")
             (emits "(" f fprop " ? " f fprop "(" (comma-sep args) ") : "
                    f ".call(" (comma-sep (cons "null" args)) "))")))
         (emits f ".call(" (comma-sep (cons "null" args)) ")"))))))

(defmethod emit* :new
  [{ctor :class :keys [args env]}]
  (emit-wrap env
             (emits "(new " ctor "("
                    (comma-sep args)
                    "))")))

(defmethod emit* :set!
  [{:keys [target val env]}]
  (emit-wrap env (emits target " = " val)))

(defn emit-global-export [ns-name global-exports lib]
  (emitln (munge ns-name) "."
          (ana/munge-global-export lib)
          " = goog.global"
          ;; Convert object dot access to bracket access
          (->> (string/split (name (or (get global-exports (symbol lib))
                                       (get global-exports (name lib))))
                             #"\.")
               (map (fn [prop]
                      (str "[\"" prop "\"]")))
               (apply str))
          ";"))

(defn load-libs
  [libs seen reloads deps ns-name]
  (let [{:keys [options js-dependency-index]} @env/*compiler*
        {:keys [target optimizations]} options
        loaded-libs (munge 'cljs.core.*loaded-libs*)
        loaded-libs-temp (munge (gensym 'cljs.core.*loaded-libs*))
        [node-libs libs-to-load] (let [libs (remove (set (vals seen)) (filter (set (vals libs)) deps))]
                                   (if (= :nodejs target)
                                     (let [{node-libs true libs-to-load false} (group-by ana/node-module-dep? libs)]
                                       [node-libs libs-to-load])
                                     [nil libs]))
        global-exports-libs (filter ana/dep-has-global-exports? libs-to-load)]
    (when (-> libs meta :reload-all)
      (emitln "if(!COMPILED) " loaded-libs-temp " = " loaded-libs " || cljs.core.set([\"cljs.core\"]);")
      (emitln "if(!COMPILED) " loaded-libs " = cljs.core.set([\"cljs.core\"]);"))
    (doseq [lib libs-to-load]
      (cond
        #?@(:clj
            [(ana/foreign-dep? lib)
             ;; we only load foreign libraries under optimizations :none
             ;; under :modules we also elide loads, as the module loader will
             ;; have handled it - David
             (when (and (= :none optimizations)
                        (not (contains? options :modules)))
               (if (= :nodejs target)
                 ;; under node.js we load foreign libs globally
                 (let [ijs (get js-dependency-index (name lib))]
                   (emitln "cljs.core.load_file("
                     (-> (io/file (util/output-directory options)
                                  (or (deps/-relative-path ijs)
                                      (util/relative-name (:url ijs))))
                         str
                         escape-string
                         wrap-in-double-quotes)
                     ");"))
                 (emitln "goog.require('" (munge lib) "');")))]
            :cljs
            [(and (ana/foreign-dep? lib)
                  (not (keyword-identical? optimizations :none)))
             nil])

        (or (-> libs meta :reload)
            (= (get reloads lib) :reload))
        (emitln "goog.require('" (munge lib) "', 'reload');")

        (or (-> libs meta :reload-all)
            (= (get reloads lib) :reload-all))
        (emitln "goog.require('" (munge lib) "', 'reload-all');")

        :else
        (when-not (= lib 'goog)
          (emitln "goog.require('" (munge lib) "');"))))
    (doseq [lib node-libs]
      (emitln (munge ns-name) "."
        (ana/munge-node-lib lib)
        " = require('" lib "');"))
    (doseq [lib global-exports-libs]
      (let [{:keys [global-exports]} (get js-dependency-index (name lib))]
        (emit-global-export ns-name global-exports lib)))
    (when (-> libs meta :reload-all)
      (emitln "if(!COMPILED) " loaded-libs " = cljs.core.into(" loaded-libs-temp ", " loaded-libs ");"))))

(defmethod emit* :ns*
  [{:keys [name requires uses require-macros reloads env deps]}]
  (load-libs requires nil (:require reloads) deps name)
  (load-libs uses requires (:use reloads) deps name)
  (when (:repl-env env)
    (emitln "'nil';")))

(defmethod emit* :ns
  [{:keys [name requires uses require-macros reloads env deps]}]
  (emitln "goog.provide('" (munge name) "');")
  (when-not (= name 'cljs.core)
    (emitln "goog.require('cljs.core');")
    (when (-> @env/*compiler* :options :emit-constants)
      (emitln "goog.require('" (munge ana/constants-ns-sym) "');")))
  (load-libs requires nil (:require reloads) deps name)
  (load-libs uses requires (:use reloads) deps name))

(defmethod emit* :deftype
  [{:keys [t fields pmasks body protocols]}]
  (let [fields (map munge fields)]
    (emitln "")
    (emitln "/**")
    (emitln "* @constructor")
    (doseq [protocol protocols]
      (emitln " * @implements {" (munge (str protocol)) "}"))
    (emitln "*/")
    (emitln (munge t) " = (function (" (comma-sep fields) "){")
    (doseq [fld fields]
      (emitln "this." fld " = " fld ";"))
    (doseq [[pno pmask] pmasks]
      (emitln "this.cljs$lang$protocol_mask$partition" pno "$ = " pmask ";"))
    (emitln "});")
    (emit body)))

(defmethod emit* :defrecord
  [{:keys [t fields pmasks body protocols]}]
  (let [fields (concat (map munge fields) '[__meta __extmap __hash])]
    (emitln "")
    (emitln "/**")
    (emitln "* @constructor")
    (doseq [protocol protocols]
      (emitln " * @implements {" (munge (str protocol)) "}"))
    (emitln "*/")
    (emitln (munge t) " = (function (" (comma-sep fields) "){")
    (doseq [fld fields]
      (emitln "this." fld " = " fld ";"))
    (doseq [[pno pmask] pmasks]
      (emitln "this.cljs$lang$protocol_mask$partition" pno "$ = " pmask ";"))
    (emitln "});")
    (emit body)))

(defn emit-dot
  [{:keys [target field method args env]}]
  (emit-wrap env
    (if field
      (emits target "." (munge field #{}))
      (emits target "." (munge method #{}) "("
        (comma-sep args)
        ")"))))

(defmethod emit* :host-field [ast] (emit-dot ast))
(defmethod emit* :host-call [ast] (emit-dot ast))

(defmethod emit* :js
  [{:keys [op env code segs args]}]
  (if (and code #?(:clj  (.startsWith ^String (string/trim code) "/*")
                   :cljs (gstring/startsWith (string/trim code) "/*")))
    (emits code)
    (emit-wrap env
      (if code
        (emits code)
        (emits (interleave (concat segs (repeat nil))
                           (concat args [nil])))))))

;; TODO: unify renaming helpers - this one was hard to find - David

#?(:clj
   (defn rename-to-js
     "Change the file extension from .cljs to .js. Takes a File or a
     String. Always returns a String."
     [^String file-str]
     (cond
       (.endsWith file-str ".cljs")
       (clojure.string/replace file-str #"\.cljs$" ".js")

       (.endsWith file-str ".cljc")
       (if (= "cljs/core.cljc" file-str)
         "cljs/core$macros.js"
         (clojure.string/replace file-str #"\.cljc$" ".js"))

       :else
       (throw (util/compilation-error (IllegalArgumentException.
                                        (str "Invalid source file extension " file-str)))))))

#?(:clj
   (defn with-core-cljs
     "Ensure that core.cljs has been loaded."
     ([] (with-core-cljs
           (when env/*compiler*
             (:options @env/*compiler*))))
     ([opts] (with-core-cljs opts (fn [])))
     ([opts body]
      {:pre [(or (nil? opts) (map? opts))
             (fn? body)]}
      (when-not (get-in @env/*compiler* [::ana/namespaces 'cljs.core :defs])
        (ana/analyze-file "cljs/core.cljs" opts))
      (body))))

#?(:clj
   (defn url-path [^File f]
     (.getPath (.toURL (.toURI f)))))

#?(:clj
   (defn compiled-by-string
     ([]
      (compiled-by-string
        (when env/*compiler*
          (:options @env/*compiler*))))
     ([opts]
      (str "// Compiled by ClojureScript "
        (util/clojurescript-version)
        (when opts
          (str " " (pr-str (ana/build-affecting-options opts))))))))

#?(:clj
   (defn cached-core [ns ext opts]
     (and (= :none (:optimizations opts))
          (not= "cljc" ext)
          (= 'cljs.core ns)
          (io/resource "cljs/core.aot.js"))))

#?(:clj
   (defn macro-ns? [ns ext opts]
     (or (= "clj" ext)
         (= 'cljs.core$macros ns)
         (and (= ns 'cljs.core) (= "cljc" ext))
         (:macros-ns opts))))

#?(:clj
   (defn emit-cached-core [src dest cached opts]
     ;; no need to bother with analysis cache reading, handled by
     ;; with-core-cljs
     (when (or ana/*verbose* (:verbose opts))
       (util/debug-prn "Using cached cljs.core" (str src)))
     (spit dest (slurp cached))
     (.setLastModified ^File dest (util/last-modified src))
     (when (true? (:source-map opts))
       (spit (io/file (str dest ".map"))
         (json/write-str
           (assoc
             (json/read-str (slurp (io/resource "cljs/core.aot.js.map")))
             "file"
             (str (io/file (util/output-directory opts) "cljs" "core.js"))))))
     (merge
       (ana/parse-ns src dest nil)
       {:out-file dest})))

#?(:clj
   (defn emit-source-map [src dest sm-data opts]
     (let [sm-file (io/file (str (.getPath ^File dest) ".map"))]
       (if-let [smap (:source-map-asset-path opts)]
         (emitln "\n//# sourceMappingURL=" smap
           (string/replace (util/path sm-file)
             (str (util/path (io/file (:output-dir opts))))
             "")
           (if (true? (:source-map-timestamp opts))
             (str
               (if-not (string/index-of smap "?") "?" "&")
               "rel=" (System/currentTimeMillis))
             ""))
         (emitln "\n//# sourceMappingURL="
           (or (:source-map-url opts) (.getName sm-file))
           (if (true? (:source-map-timestamp opts))
             (str "?rel=" (System/currentTimeMillis))
             "")))
       (spit sm-file
         (sm/encode {(url-path src) (:source-map sm-data)}
           {:lines (+ (:gen-line sm-data) 2)
            :file (url-path dest)
            :source-map-path (:source-map-path opts)
            :source-map-timestamp (:source-map-timestamp opts)
            :source-map-pretty-print (:source-map-pretty-print opts)
            :relpaths {(util/path src)
                       (util/ns->relpath (first (:provides opts)) (:ext opts))}})))))

#?(:clj
   (defn emit-source [src dest ext opts]
     (with-open [out ^java.io.Writer (io/make-writer dest {})]
       (binding [*out*                 out
                 ana/*cljs-ns*         'cljs.user
                 ana/*cljs-file*       (.getPath ^File src)
                 reader/*alias-map*    (or reader/*alias-map* {})
                 ana/*checked-arrays*  (or ana/*checked-arrays* (:checked-arrays opts))
                 ana/*cljs-static-fns* (or ana/*cljs-static-fns* (:static-fns opts))
                 *source-map-data*     (when (:source-map opts)
                                         (atom
                                           {:source-map (sorted-map)
                                            :gen-line 0}))
                 *source-map-data-gen-col* (AtomicLong.)
                 find-ns-starts-with   (memoize find-ns-starts-with)]
         (emitln (compiled-by-string opts))
         (with-open [rdr (io/reader src)]
           (let [env (ana/empty-env)
                 emitter (when (:parallel-build opts)
                           (Executors/newSingleThreadExecutor))
                 emit (if emitter
                        #(.execute emitter ^Runnable (bound-fn [] (emit %)))
                        emit)]
             (loop [forms       (ana/forms-seq* rdr (util/path src))
                    ns-name     nil
                    deps        nil]
               (if (seq forms)
                 (let [env (assoc env :ns (ana/get-namespace ana/*cljs-ns*))
                       {:keys [op] :as ast} (ana/analyze env (first forms) nil opts)]
                   (cond
                     (= op :ns)
                     (let [ns-name (:name ast)
                           ns-name (if (and (= 'cljs.core ns-name)
                                         (= "cljc" ext))
                                     'cljs.core$macros
                                     ns-name)]
                       (emit ast)
                       (recur (rest forms) ns-name (merge (:uses ast) (:requires ast))))

                     (= :ns* (:op ast))
                     (let [ns-emitted? (some? ns-name)
                           ns-name (ana/gen-user-ns src)]
                       (if-not ns-emitted?
                         (emit (assoc ast :name ns-name :op :ns))
                         (emit ast))
                       (recur (rest forms) ns-name (merge deps (:uses ast) (:requires ast))))

                     :else
                     (let [ns-emitted? (some? ns-name)
                           ns-name (if-not ns-emitted?
                                     (ana/gen-user-ns src)
                                     ns-name)]
                       (when-not ns-emitted?
                         (emit {:op :ns
                                :name ns-name}))
                       (emit ast)
                       (recur (rest forms) ns-name deps))))
                 (let [_ (when emitter
                           (.shutdown emitter)
                           (.awaitTermination emitter 1000 TimeUnit/HOURS))
                       sm-data (when *source-map-data* (assoc @*source-map-data*
                                                         :gen-col (.get ^AtomicLong *source-map-data-gen-col*)))
                       ret (merge
                             {:ns         (or ns-name 'cljs.user)
                              :macros-ns  (:macros-ns opts)
                              :provides   [ns-name]
                              :requires   (if (= ns-name 'cljs.core)
                                            (set (vals deps))
                                            (cond-> (conj (set (vals deps)) 'cljs.core)
                                              (get-in @env/*compiler* [:options :emit-constants])
                                              (conj ana/constants-ns-sym)))
                              :file        dest
                              :out-file    (.toString ^File dest)
                              :source-file src}
                             (when sm-data
                               {:source-map (:source-map sm-data)}))]
                   (when (and sm-data (= :none (:optimizations opts)))
                     (emit-source-map src dest sm-data
                       (merge opts {:ext ext :provides [ns-name]})))
                   (let [path (.getPath (.toURL ^File dest))]
                     (swap! env/*compiler* assoc-in [::compiled-cljs path] ret))
                   (ana/ensure-defs ns-name)
                   (let [{:keys [output-dir cache-analysis]} opts]
                     (when (and (true? cache-analysis) output-dir)
                       (ana/write-analysis-cache ns-name
                         (ana/cache-file src (ana/parse-ns src) output-dir :write)
                         src))
                     ret))))))))))

#?(:clj
   (defn compile-file*
     ([^File src ^File dest]
      (compile-file* src dest
        (when env/*compiler*
          (:options @env/*compiler*))))
     ([^File src ^File dest opts]
      (ensure
        (with-core-cljs opts
          (fn []
            (when (and (or ana/*verbose* (:verbose opts))
                       (not (:compiler-stats opts)))
              (util/debug-prn "Compiling" (str src) "to" (str dest)))
            (util/measure (and (or ana/*verbose* (:verbose opts))
                               (:compiler-stats opts))
              (str "Compiling " (str src) " to " (str dest))
              (let [ext (util/ext src)
                   {:keys [ns] :as ns-info} (ana/parse-ns src)]
               (if-let [cached (cached-core ns ext opts)]
                 [(emit-cached-core src dest cached opts) false]
                 (let [opts (if (macro-ns? ns ext opts)
                              (assoc opts :macros-ns true)
                              opts)
                       dest-exists? (.exists dest)
                       ret [(emit-source src dest ext opts) dest-exists?]]
                   (.setLastModified ^File dest (util/last-modified src))
                   ret))))))))))

#?(:clj
   (defn requires-compilation?
     "Return true if the src file requires compilation."
     ([src dest]
      (requires-compilation? src dest
        (when env/*compiler*
          (:options @env/*compiler*))))
     ([^File src ^File dest opts]
      (let [{:keys [ns requires]} (ana/parse-ns src)]
        (if (and (= 'cljs.loader ns) (not (contains? opts :cache-key)))
          false
          (ensure
           (or (not (.exists dest))
               (util/changed? src dest)
               (let [version' (util/compiled-by-version dest)
                     version (util/clojurescript-version)]
                 (and version (not= version version')))
               (and opts
                    (not (and (io/resource "cljs/core.aot.js") (= 'cljs.core ns)))
                    (not= (ana/build-affecting-options opts)
                          (ana/build-affecting-options (util/build-options dest))))
               (and opts (:source-map opts)
                    (if (= (:optimizations opts) :none)
                      (not (.exists (io/file (str (.getPath dest) ".map"))))
                      (not (get-in @env/*compiler* [::compiled-cljs (.getAbsolutePath dest)]))))
               (when-let [recompiled' (and *recompiled* @*recompiled*)]
                 (some requires recompiled')))))))))

#?(:clj
   (defn compile-file
     "Compiles src to a file of the same name, but with a .js extension,
      in the src file's directory.

      With dest argument, write file to provided location. If the dest
      argument is a file outside the source tree, missing parent
      directories will be created. The src file will only be compiled if
      the dest file has an older modification time.

      Both src and dest may be either a String or a File.

      Returns a map containing {:ns .. :provides .. :requires .. :file ..}.
      If the file was not compiled returns only {:file ...}"
     ([src]
      (let [dest (rename-to-js src)]
        (compile-file src dest
          (when env/*compiler*
            (:options @env/*compiler*)))))
     ([src dest]
      (compile-file src dest
        (when env/*compiler*
          (:options @env/*compiler*))))
     ([src dest opts]
      {:post [map?]}
      (binding [ana/*file-defs*        (atom #{})
                ana/*unchecked-if*     false
                ana/*unchecked-arrays* false
                ana/*cljs-warnings*    ana/*cljs-warnings*]
        (let [nses      (get @env/*compiler* ::ana/namespaces)
              src-file  (io/file src)
              dest-file (io/file dest)
              opts      (merge {:optimizations :none} opts)]
          (if (.exists src-file)
            (try
              (let [{ns :ns :as ns-info} (ana/parse-ns src-file dest-file opts)
                    opts (if (and (not= (util/ext src) "clj") ;; skip cljs.core macro-ns
                                  (= ns 'cljs.core))
                           (cond-> opts
                             (not (false? (:static-fns opts))) (assoc :static-fns true)
                             true (dissoc :checked-arrays))
                           opts)]
                (if (or (requires-compilation? src-file dest-file opts)
                        (:force opts))
                  (do
                    (util/mkdirs dest-file)
                    (when (and (get-in nses [ns :defs])
                               (not= 'cljs.core ns)
                               (not= :interactive (:mode opts)))
                      (swap! env/*compiler* update-in [::ana/namespaces] dissoc ns))
                    (let [[ret recompiled?] (compile-file* src-file dest-file opts)]
                      (when (and *recompiled*
                                 recompiled?)
                        (swap! *recompiled* conj ns))
                      ret))
                  (do
                    ;; populate compilation environment with analysis information
                    ;; when constants are optimized
                    (when (or (and (= ns 'cljs.loader)
                                   (not (contains? opts :cache-key)))
                              (and (true? (:optimize-constants opts))
                                   (nil? (get-in nses [ns :defs]))))
                      (with-core-cljs opts (fn [] (ana/analyze-file src-file opts))))
                    (assoc ns-info :out-file (.toString dest-file)))))
              (catch Exception e
                (throw (ex-info (str "failed compiling file:" src) {:file src :clojure.error/phase :compilation} e))))
            (throw (util/compilation-error (java.io.FileNotFoundException. (str "The file " src " does not exist."))))))))))

#?(:clj
   (defn cljs-files-in
     "Return a sequence of all .cljs and .cljc files in the given directory."
     [dir]
     (map io/file
       (reduce
         (fn [m x]
           (if (.endsWith ^String x ".cljs")
             (cond-> (conj m x)
               (contains? m (str (subs x 0 (dec (count x))) "c"))
               (set/difference #{(str (subs x 0 (dec (count x))) "c")}))
             ;; ends with .cljc
             (cond-> m
               (not (contains? m (str (subs x 0 (dec (count x))) "s")))
               (conj x))))
         #{}
         (into []
           (comp
             (filter
               #(let [name (.getName ^File %)]
                  (and (or (.endsWith name ".cljs")
                         (.endsWith name ".cljc"))
                    (not= \. (first name))
                    (not (contains? cljs-reserved-file-names name)))))
             (map #(.getPath ^File %)))
           (file-seq dir))))))

#?(:clj
   (defn compile-root
     "Looks recursively in src-dir for .cljs files and compiles them to
      .js files. If target-dir is provided, output will go into this
      directory mirroring the source directory structure. Returns a list
      of maps containing information about each file which was compiled
      in dependency order."
     ([src-dir]
      (compile-root src-dir "out"))
     ([src-dir target-dir]
      (compile-root src-dir target-dir
        (when env/*compiler*
          (:options @env/*compiler*))))
     ([src-dir target-dir opts]
      (swap! env/*compiler* assoc :root src-dir)
      (let [src-dir-file (io/file src-dir)
            inputs (deps/dependency-order
                     (map #(ana/parse-ns %)
                       (cljs-files-in src-dir-file)))]
        (binding [*inputs* (zipmap (map :ns inputs) inputs)]
          (loop [inputs (seq inputs) compiled []]
            (if inputs
              (let [{:keys [source-file] :as ns-info} (first inputs)
                    output-file (util/to-target-file target-dir ns-info)
                    ijs (compile-file source-file output-file opts)]
                (recur
                  (next inputs)
                  (conj compiled
                    (assoc ijs :file-name (.getPath output-file)))))
              compiled)))))))

#?(:clj
   (defn find-source [file]
     (ana/parse-ns file)))

#?(:clj
   (defn find-root-sources
     [src-dir]
      (let [src-dir-file (io/file src-dir)]
        (map find-source (cljs-files-in src-dir-file)))))

;; TODO: needs fixing, table will include other things than keywords - David

(defn emit-constants-table [table]
  (emitln "goog.provide('" (munge ana/constants-ns-sym) "');")
  (emitln "goog.require('cljs.core');")
  (doseq [[sym value] table]
    (let [ns   (namespace sym)
          name (name sym)]
      (emits "cljs.core." value " = ")
      (cond
        (keyword? sym) (emits-keyword sym)
        (symbol? sym) (emits-symbol sym)
        :else (throw
                (ex-info
                  (str "Cannot emit constant for type " (type sym))
                  {:error :invalid-constant-type
                   :clojure.error/phase :compilation})))
      (emits ";\n"))))

#?(:clj
   (defn emit-constants-table-to-file [table dest]
     (io/make-parents dest)
     (with-open [out ^java.io.Writer (io/make-writer dest {})]
       (binding [*out* out]
         (emit-constants-table table)))))

(defn emit-externs
  ([externs]
   (emit-externs [] externs (atom #{})
     (when env/*compiler*
       (::ana/externs @env/*compiler*))))
  ([prefix externs top-level known-externs]
   (loop [ks (seq (keys externs))]
     (when ks
       (let [k (first ks)
             [top :as prefix'] (conj prefix k)]
         (when (and (not= 'prototype k)
                    (nil? (get-in known-externs prefix')))
           (if-not (or (contains? @top-level top)
                       (contains? known-externs top))
             (do
               (emitln "var " (string/join "." (map munge prefix')) ";")
               (swap! top-level conj top))
             (emitln (string/join "." (map munge prefix')) ";")))
         (let [m (get externs k)]
           (when-not (empty? m)
             (emit-externs prefix' m top-level known-externs))))
       (recur (next ks))))))

#?(:clj
   (defn emit-inferred-externs-to-file [externs dest]
     (io/make-parents dest)
     (with-open [out ^java.io.Writer (io/make-writer dest {})]
       (binding [*out* out]
         (emit-externs externs)))))
