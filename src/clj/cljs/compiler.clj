;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.compiler
  (:refer-clojure :exclude [munge macroexpand-1])
  (:require [cljs.util :as util]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.reader :as reader]
            [cljs.env :as env]
            [cljs.tagged-literals :as tags]
            [cljs.analyzer :as ana]
            [cljs.source-map :as sm])
  (:import java.lang.StringBuilder
           java.io.File))

(set! *warn-on-reflection* true)

(def js-reserved
  #{"abstract" "boolean" "break" "byte" "case"
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
    "null"})

(def ^:dynamic *source-map-data* nil)
(def ^:dynamic *lexical-renames* {})
;; NOTE: explicitly threading opts as cljs.analyzer is considerably more
;; invasive given the current approach to emission - David
(def ^:dynamic *build-options* nil)

(def cljs-reserved-file-names #{"deps.cljs"})

(defn ns-first-segments []
  (letfn [(get-first-ns-segment [ns] (first (string/split (str ns) #"\.")))]
    (map get-first-ns-segment (keys (::ana/namespaces @env/*compiler*)))))

; Helper fn
(defn shadow-depth [s]
  (let [{:keys [name info]} s]
    (loop [d 0, {:keys [shadow]} info]
      (cond
       shadow (recur (inc d) shadow)
       (some #{(str name)} (ns-first-segments)) (inc d)
       :else d))))

(defn munge
  ([s] (munge s js-reserved))
  ([s reserved]
    (if (map? s)
      ; Unshadowing
      (let [{:keys [name field] :as info} s
            depth (shadow-depth s)
            renamed (*lexical-renames* (System/identityHashCode s))
            munged-name (munge (cond field (str "self__." name)
                                     renamed renamed
                                     :else name)
                               reserved)]
        (if (or field (zero? depth))
          munged-name
          (symbol (str munged-name "__$" depth))))
      ; String munging
      (let [ss (string/replace (str s) #"\/(.)" ".$1") ; Division is special
            ss (apply str (map #(if (reserved %) (str % "$") %)
                               (string/split ss #"(?<=\.)|(?=\.)")))
            ms (clojure.lang.Compiler/munge ss)]
        (if (symbol? s)
          (symbol ms)
          ms)))))

(defn- comma-sep [xs]
  (interpose "," xs))

(defn- escape-char [^Character c]
  (let [cp (.hashCode c)]
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
        (format "\\u%04X" cp))))) ; Any other character is Unicode

(defn- escape-string [^CharSequence s]
  (let [sb (StringBuilder. (count s))]
    (doseq [c s]
      (.append sb (escape-char c)))
    (.toString sb)))

(defn- wrap-in-double-quotes [x]
  (str \" x \"))

(defmulti emit* :op)

(defn emit [ast]
  (env/ensure
    (when *source-map-data*
      (let [{:keys [env]} ast]
        (when (:line env)
          (let [{:keys [line column]} env]
            (swap! *source-map-data*
              (fn [m]
                (let [minfo (cond-> {:gcol (:gen-col m)
                                     :gline (:gen-line m)}
                              (= (:op ast) :var)
                              (assoc :name (str (-> ast :info :name))))]
                  ; Dec the line/column numbers for 0-indexing.
                  ; tools.reader uses 1-indexed sources, chrome
                  ; expects 0-indexed source maps.
                  (update-in m [:source-map (dec line)]
                    (fnil (fn [line]
                            (update-in line [(if column (dec column) 0)]
                              (fnil (fn [column] (conj column minfo)) [])))
                      (sorted-map))))))))))
    (emit* ast)))

(defn emits [& xs]
  (doseq [x xs]
    (cond
     (nil? x) nil
     (map? x) (emit x)
     (seq? x) (apply emits x)
     (fn? x)  (x)
     :else (let [s (print-str x)]
             (when *source-map-data*
               (swap! *source-map-data*
                 update-in [:gen-col] #(+ % (count s))))
             (print s))))
  nil)

(defn emitln [& xs]
  (apply emits xs)
  (println)
  (when *source-map-data*
    (swap! *source-map-data*
      (fn [{:keys [gen-line] :as m}]
        (assoc m
          :gen-line (inc gen-line)
          :gen-col 0))))
  nil)

(defn ^String emit-str [expr]
  (with-out-str (emit expr)))

(defmulti emit-constant class)
(defmethod emit-constant nil [x] (emits "null"))
(defmethod emit-constant Long [x] (emits "(" x ")"))
(defmethod emit-constant Integer [x] (emits x)) ; reader puts Integers in metadata
(defmethod emit-constant Double [x] (emits x))
(defmethod emit-constant BigDecimal [x] (emits (.doubleValue ^BigDecimal x)))
(defmethod emit-constant clojure.lang.BigInt [x] (emits (.doubleValue ^clojure.lang.BigInt x)))
(defmethod emit-constant String [x]
  (emits (wrap-in-double-quotes (escape-string x))))
(defmethod emit-constant Boolean [x] (emits (if x "true" "false")))
(defmethod emit-constant Character [x]
  (emits (wrap-in-double-quotes (escape-char x))))

(defmethod emit-constant java.util.regex.Pattern [x]
  (if (= "" (str x))
    (emits "(new RegExp(\"\"))")
    (let [[_ flags pattern] (re-find #"^(?:\(\?([idmsux]*)\))?(.*)" (str x))]
      (emits \/ (.replaceAll (re-matcher #"/" pattern) "\\\\/") \/ flags))))

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

(defmethod emit-constant clojure.lang.Keyword [x]
  (if (-> @env/*compiler* :opts :emit-constants)
    (let [value (-> @env/*compiler* ::ana/constant-table x)]
      (emits "cljs.core." value))
    (emits-keyword x)
    ))

(defmethod emit-constant clojure.lang.Symbol [x]
  (let [ns     (namespace x)
        name   (name x)
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
    (emit-constant (hash x))
    (emits ",")
    (emit-constant nil)
    (emits ")")))

;; tagged literal support

(defmethod emit-constant java.util.Date [^java.util.Date date]
  (emits "new Date(" (.getTime date) ")"))

(defmethod emit-constant java.util.UUID [^java.util.UUID uuid]
  (emits "new cljs.core.UUID(\"" (.toString uuid) "\")"))

(defmacro emit-wrap [env & body]
  `(let [env# ~env]
     (when (= :return (:context env#)) (emits "return "))
     ~@body
     (when-not (= :expr (:context env#)) (emitln ";"))))

(defmethod emit* :no-op [m])

(defmethod emit* :var
  [{:keys [info env] :as arg}]
  (let [var-name (:name info)
        info (if (= (namespace var-name) "js")
               (name var-name)
               info)]
    ; We need a way to write bindings out to source maps and javascript
    ; without getting wrapped in an emit-wrap calls, otherwise we get
    ; e.g. (function greet(return x, return y) {}).
    (if (:binding-form? arg)
      ; Emit the arg map so shadowing is properly handled when munging
      ; (prevents duplicate fn-param-names)
      (emits (munge arg))
      (when-not (= :statement (:context env))
        (emit-wrap env (emits (munge info)))))))

(defmethod emit* :var-special
  [{:keys [env var sym meta] :as arg}]
  (emit-wrap env
    (emits "new cljs.core.Var(" var "," sym ","  meta ")")))

(defmethod emit* :meta
  [{:keys [expr meta env]}]
  (emit-wrap env
    (emits "cljs.core.with_meta(" expr "," meta ")")))

(def ^:private array-map-threshold 8)
(def ^:private obj-map-threshold 8)

(defn distinct-keys? [keys]
  (and (every? #(= (:op %) :constant) keys)
       (= (count (into #{} keys)) (count keys))))

(defmethod emit* :map
  [{:keys [env keys vals]}]
  (let [simple-keys? (every? #(or (string? %) (keyword? %)) keys)]
    (emit-wrap env
      (cond
        (zero? (count keys))
        (emits "cljs.core.PersistentArrayMap.EMPTY")

        (<= (count keys) array-map-threshold)
        (if (distinct-keys? keys)
          (emits "new cljs.core.PersistentArrayMap(null, " (count keys) ", ["
            (comma-sep (interleave keys vals))
            "], null)")
          (emits "new cljs.core.PersistentArrayMap.fromArray(["
            (comma-sep (interleave keys vals))
            "], true, false)"))        

        :else
        (emits "cljs.core.PersistentHashMap.fromArrays(["
               (comma-sep keys)
               "],["
               (comma-sep vals)
               "])")))))

(defmethod emit* :list
  [{:keys [items env]}]
  (emit-wrap env
    (if (empty? items)
      (emits "cljs.core.List.EMPTY")
      (emits "cljs.core.list(" (comma-sep items) ")"))))

(defmethod emit* :vector
  [{:keys [items env]}]
  (emit-wrap env
    (if (empty? items)
      (emits "cljs.core.PersistentVector.EMPTY")
      (let [cnt (count items)]
        (if (< cnt 32)
          (emits "new cljs.core.PersistentVector(null, " cnt
            ", 5, cljs.core.PersistentVector.EMPTY_NODE, ["  (comma-sep items) "], null)")
          (emits "cljs.core.PersistentVector.fromArray([" (comma-sep items) "], true)"))))))

(defn distinct-constants? [items]
  (and (every? #(= (:op %) :constant) items)
       (= (count (into #{} items)) (count items))))

(defmethod emit* :set
  [{:keys [items env]}]
  (emit-wrap env
    (cond
      (empty? items)
      (emits "cljs.core.PersistentHashSet.EMPTY")

      (distinct-constants? items)
      (emits "new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, " (count items) ", ["
        (comma-sep (interleave items (repeat "null"))) "], null), null)")

      :else (emits "cljs.core.PersistentHashSet.fromArray([" (comma-sep items) "], true)"))))

(defmethod emit* :js-value
  [{:keys [items js-type env]}]
  (emit-wrap env
    (if (= js-type :object)
      (do
        (emits "{")
        (when-let [items (seq items)]
          (let [[[k v] & r] items]
            (emits "\"" (name k) "\": " v)
            (doseq [[k v] r]
              (emits ", \"" (name k) "\": " v))))
        (emits "}"))
      (emits "[" (comma-sep items) "]"))))

(defmethod emit* :constant
  [{:keys [form env]}]
  (when-not (= :statement (:context env))
    (emit-wrap env (emit-constant form))))

(defn truthy-constant? [{:keys [op form]}]
  (and (= op :constant)
       form
       (not (or (and (string? form) (= form ""))
                (and (number? form) (zero? form))))))

(defn falsey-constant? [{:keys [op form]}]
  (and (= op :constant)
       (or (false? form) (nil? form))))

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

(defmethod emit* :case*
  [{:keys [v tests thens default env]}]
  (when (= (:context env) :expr)
    (emitln "(function(){"))
  (let [gs (gensym "caseval__")]
    (when (= :expr (:context env))
      (emitln "var " gs ";"))
    (emitln "switch (" v ") {")
    (doseq [[ts then] (partition 2 (interleave tests thens))]
      (doseq [test ts]
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
  [{:keys [throw env]}]
  (if (= :expr (:context env))
    (emits "(function(){throw " throw "})()")
    (emitln "throw " throw ";")))

(defn emit-comment
  "Emit a nicely formatted comment string."
  [doc jsdoc]
  (let [docs (when doc [doc])
        docs (if jsdoc (concat docs jsdoc) docs)
        docs (remove nil? docs)]
    (letfn [(print-comment-lines [e] (doseq [next-line (string/split-lines e)]
                                       (emitln "* " (string/trim next-line))))]
      (when (seq docs)
        (emitln "/**")
        (doseq [e docs]
          (when e
            (print-comment-lines e)))
        (emitln "*/")))))

(defmethod emit* :def
  [{:keys [name var init env doc export test]}]
  (let [mname (munge name)]
    (when init
      (emit-comment doc (:jsdoc init))
      (emits var)
      (emits " = " init)
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
  [{:keys [type name variadic params expr env recurs max-fixed-arity]}]
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

(defn emit-variadic-fn-method
  [{:keys [type name variadic params expr env recurs max-fixed-arity] :as f}]
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
                 (emits "  ")
                 (emit (last params))
                 (emitln " = cljs.core.array_seq(Array.prototype.slice.call(arguments, " (dec (count params)) "),0);")
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
  [{:keys [name env methods max-fixed-arity variadic recur-frames loop-lets]}]
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
            (if (:variadic meth)
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
            (if (:variadic meth)
              (do (emitln "default:")
                  (emitln "return " n ".cljs$core$IFn$_invoke$arity$variadic("
                          (comma-sep (butlast maxparams))
                          (when (> (count maxparams) 1) ", ")
                          "cljs.core.array_seq(arguments, " max-fixed-arity "));"))
              (let [pcnt (count (:params meth))]
                (emitln "case " pcnt ":")
                (emitln "return " n ".call(this" (if (zero? pcnt) nil
                                                     (list "," (comma-sep (take pcnt maxparams)))) ");"))))
          (emitln "}")
          (emitln "throw(new Error('Invalid arity: ' + arguments.length));")
          (emitln "};")
          (when variadic
            (emitln mname ".cljs$lang$maxFixedArity = " max-fixed-arity ";")
            (emitln mname ".cljs$lang$applyTo = " (some #(let [[n m] %] (when (:variadic m) n)) ms) ".cljs$lang$applyTo;"))
          (doseq [[n meth] ms]
            (let [c (count (:params meth))]
              (if (:variadic meth)
                (emitln mname ".cljs$core$IFn$_invoke$arity$variadic = " n ".cljs$core$IFn$_invoke$arity$variadic;")
                (emitln mname ".cljs$core$IFn$_invoke$arity$" c " = " n ";"))))
          (emitln "return " mname ";")
          (emitln "})()")))
      (when loop-locals
        (emitln ";})(" (comma-sep loop-locals) "))")))))

(defmethod emit* :do
  [{:keys [statements ret env]}]
  (let [context (:context env)]
    (when (and statements (= :expr context)) (emitln "(function (){"))
    (doseq [s statements] (emitln s))
    (emit ret)
    (when (and statements (= :expr context)) (emitln "})()"))))

(defmethod emit* :try
  [{:keys [env try catch name finally]}]
  (let [context (:context env)]
    (if (or name finally)
      (do
        (when (= :expr context)
          (emits "(function (){"))
        (emits "try{" try "}")
        (when name
          (emits "catch (" (munge name) "){" catch "}"))
        (when finally
          (assert (not= :constant (:op finally)) "finally block cannot contain constant")
          (emits "finally {" finally "}"))
        (when (= :expr context)
          (emits "})()")))
      (emits try))))

(defn emit-let
  [{:keys [bindings expr env]} is-loop]
  (let [context (:context env)]
    (when (= :expr context) (emits "(function (){"))
    (binding [*lexical-renames* (into *lexical-renames*
                                      (when (= :statement context)
                                        (map #(vector (System/identityHashCode %)
                                                      (gensym (str (:name %) "-")))
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
  [{:keys [bindings expr env]}]
  (let [context (:context env)]
    (when (= :expr context) (emits "(function (){"))
    (doseq [{:keys [init] :as binding} bindings]
      (emitln "var " (munge binding) " = " init ";"))
    (emits expr)
    (when (= :expr context) (emits "})()"))))

(defn protocol-prefix [psym]
  (symbol (str (-> (str psym) (.replace \. \$) (.replace \/ \$)) "$")))

(defmethod emit* :invoke
  [{:keys [f args env] :as expr}]
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
                                (not ('#{any clj clj-or-nil} tag))
                                (when-let [ps (:protocols (ana/resolve-existing-var (dissoc env :locals) tag))]
                                  (ps protocol)))))))
        opt-not? (and (= (:name info) 'cljs.core/not)
                      (= (ana/infer-tag env (first (:args expr))) 'boolean))
        ns (:ns info)
        js? (= ns 'js)
        goog? (when ns
                (or (= ns 'goog)
                    (when-let [ns-str (str ns)]
                      (= (get (string/split ns-str #"\.") 0 nil) "goog"))))
        keyword? (and (= (-> f :op) :constant)
                      (keyword? (-> f :form)))
        [f variadic-invoke]
        (if fn?
          (let [arity (count args)
                variadic? (:variadic info)
                mps (:method-params info)
                mfa (:max-fixed-arity info)]
            (cond
             ;; if only one method, no renaming needed
             (and (not variadic?)
                  (= (count mps) 1))
             [f nil]

             ;; direct dispatch to variadic case
             (and variadic? (> arity mfa))
             [(update-in f [:info :name]
                             (fn [name] (symbol (str (munge info) ".cljs$core$IFn$_invoke$arity$variadic"))))
              {:max-fixed-arity mfa}]

             ;; direct dispatch to specific arity case
             :else
             (let [arities (map count mps)]
               (if (some #{arity} arities)
                 [(update-in f [:info :name]
                             (fn [name] (symbol (str (munge info) ".cljs$core$IFn$_invoke$arity$" arity)))) nil]
                 [f nil]))))
          [f nil])]
    (emit-wrap env
      (cond
       opt-not?
       (emits "!(" (first args) ")")

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
               "cljs.core.array_seq([" (comma-sep (drop mfa args)) "], 0))"))
       
       (or fn? js? goog?)
       (emits f "(" (comma-sep args)  ")")
       
       :else
       (if (and ana/*cljs-static-fns* (= (:op f) :var))
         (let [fprop (str ".cljs$core$IFn$_invoke$arity$" (count args))]
           (emits "(" f fprop " ? " f fprop "(" (comma-sep args) ") : " f ".call(" (comma-sep (cons "null" args)) "))"))
         (emits f ".call(" (comma-sep (cons "null" args)) ")"))))))

(defmethod emit* :new
  [{:keys [ctor args env]}]
  (emit-wrap env
             (emits "(new " ctor "("
                    (comma-sep args)
                    "))")))

(defmethod emit* :set!
  [{:keys [target val env]}]
  (emit-wrap env (emits target " = " val)))

(defmethod emit* :ns
  [{:keys [name requires uses require-macros env]}]
  (when (= (:optimizations *build-options*) :none)
    (emitln "if(!goog.isProvided_('" (munge name) "')) {"))
  (emitln "goog.provide('" (munge name) "');")
  (when (= (:optimizations *build-options*) :none)
    (emitln "}"))
  (when-not (= name 'cljs.core)
    (emitln "goog.require('cljs.core');"))
  (doseq [lib (distinct (vals requires))]
    (if (-> requires meta :reload)
      (emitln "goog.require('" (munge lib) "', true);")
      (emitln "goog.require('" (munge lib) "');")))
  (doseq [lib (remove (set (vals requires)) (distinct (vals uses)))]
    (if (-> uses meta :reload)
      (emitln "goog.require('" (munge lib) "', true);")
      (emitln "goog.require('" (munge lib) "');"))))

(defmethod emit* :deftype*
  [{:keys [t fields pmasks body]}]
  (let [fields (map munge fields)]
    (emitln "")
    (emitln "/**")
    (emitln "* @constructor")
    (emitln "*/")
    (emitln (munge t) " = (function (" (comma-sep fields) "){")
    (doseq [fld fields]
      (emitln "this." fld " = " fld ";"))
    (doseq [[pno pmask] pmasks]
      (emitln "this.cljs$lang$protocol_mask$partition" pno "$ = " pmask ";"))
    (emitln "})")
    (emit body)))

(defmethod emit* :defrecord*
  [{:keys [t fields pmasks body]}]
  (let [fields (concat (map munge fields) '[__meta __extmap __hash])]
    (emitln "")
    (emitln "/**")
    (emitln "* @constructor")
    (doseq [fld fields]
      (emitln "* @param {*} " fld))
    (emitln "* @param {*=} __meta ")
    (emitln "* @param {*=} __extmap")
    (emitln "* @param {number|null} __hash")
    (emitln "*/")
    (emitln (munge t) " = (function (" (comma-sep fields) "){")
    (doseq [fld fields]
      (emitln "this." fld " = " fld ";"))
    (doseq [[pno pmask] pmasks]
      (emitln "this.cljs$lang$protocol_mask$partition" pno "$ = " pmask ";"))
    (emitln "})")
    (emit body)))

(defmethod emit* :dot
  [{:keys [target field method args env]}]
  (emit-wrap env
             (if field
               (emits target "." (munge field #{}))
               (emits target "." (munge method #{}) "("
                      (comma-sep args)
                      ")"))))

(defmethod emit* :js
  [{:keys [env code segs args]}]
  (emit-wrap env
             (if code
               (emits code)
               (emits (interleave (concat segs (repeat nil))
                                  (concat args [nil]))))))

(defn rename-to-js
  "Change the file extension from .cljs to .js. Takes a File or a
  String. Always returns a String."
  [file-str]
  (clojure.string/replace file-str #"\.cljs$" ".js"))

(defn with-core-cljs
  "Ensure that core.cljs has been loaded."
  ([] (with-core-cljs nil))
  ([opts] (with-core-cljs opts (fn [])))
  ([opts body]
     (do
       (when-not (get-in @env/*compiler* [::ana/namespaces 'cljs.core :defs])
         (ana/analyze-file "cljs/core.cljs" opts))
       (body))))

(defn url-path [^File f]
  (.getPath (.toURL (.toURI f))))

(defn- build-affecting-options
  [opts]
  (select-keys opts [:static-fns :optimize-constants :elide-asserts]))

(defn compiled-by-string
  ([] (compiled-by-string nil))
  ([opts]
    (str "// Compiled by ClojureScript "
      (util/clojurescript-version)
      (when opts
        (str " " (pr-str (build-affecting-options opts)))))))

(defn compile-file*
  ([src dest] (compile-file* src dest nil))
  ([src dest opts]
    (env/ensure
      (with-core-cljs opts
        (fn []
          (when (or ana/*verbose* (:verbose opts))
            (util/debug-prn "Compiling " src))
          (with-open [out ^java.io.Writer (io/make-writer dest {})]
            (binding [*out* out
                      ana/*cljs-ns* 'cljs.user
                      ana/*cljs-file* (.getPath ^File src)
                      reader/*alias-map* (or reader/*alias-map* {})
                      ana/*cljs-static-fns* (or ana/*cljs-static-fns* (:static-fns opts))
                      *source-map-data* (when (:source-map opts)
                                          (atom
                                            {:source-map (sorted-map)
                                             :gen-col 0
                                             :gen-line 0}))]
              (emitln (compiled-by-string opts))
              (loop [forms (ana/forms-seq src)
                     ns-name nil
                     deps nil]
                (if (seq forms)
                  (let [env (ana/empty-env)
                        ast (ana/analyze env (first forms) nil opts)]
                    (do (emit ast)
                        (if (= (:op ast) :ns)
                          (recur (rest forms) (:name ast) (merge (:uses ast) (:requires ast)))
                          (recur (rest forms) ns-name deps))))
                  (let [sm-data (when *source-map-data* @*source-map-data*)
                        ret (merge
                              {:ns (or ns-name 'cljs.user)
                               :provides [ns-name]
                               :requires (if (= ns-name 'cljs.core)
                                           (set (vals deps))
                                           (cond-> (conj (set (vals deps)) 'cljs.core)
                                             (get-in @env/*compiler* [:opts :emit-constants])
                                             (conj 'constants-table)))
                               :file dest
                               :source-file src}
                              (when sm-data
                                {:source-map (:source-map sm-data)}))]
                    (when (and sm-data (= (:optimizations opts) :none))
                      (let [sm-file (io/file (str (.getPath ^File dest) ".map"))]
                        (emits "\n//# sourceMappingURL=" (.getName sm-file)
                          (if (true? (:source-map-timestamp opts))
                            (str "?rel=" (System/currentTimeMillis))
                            ""))
                        (spit sm-file
                          (sm/encode {(url-path src) (:source-map sm-data)}
                            {:lines (+ (:gen-line sm-data) 2)
                             :file (url-path dest)}))))
                    (let [path (.getPath (.toURL ^File dest))]
                      (swap! env/*compiler* assoc-in [::compiled-cljs path] ret)
                      (swap! env/*compiler* assoc-in [::ana/analyzed-cljs path] true))
                    (let [{:keys [output-dir cache-analysis]} opts]
                      (when (and (true? cache-analysis) output-dir)
                        (ana/write-analysis-cache ns-name
                          (ana/cache-file src output-dir)))
                      ret)))))))))))

(defn requires-compilation?
  "Return true if the src file requires compilation."
  ([src dest] (requires-compilation? src dest nil))
  ([^File src ^File dest opts]
    (env/ensure
      (or (not (.exists dest))
          (> (.lastModified src) (.lastModified dest))
          (let [version' (util/compiled-by-version dest)
                version  (util/clojurescript-version)]
            (and version (not= version version')))
          (and opts
               (not= (build-affecting-options opts) (build-affecting-options (util/build-options dest))))
          (and opts
            (:source-map opts)
            (if (= (:optimizations opts) :none)
              (not (.exists (io/file (str (.getPath dest) ".map"))))
              (not (get-in @env/*compiler* [::compiled-cljs (.getAbsolutePath dest)]))))))))

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
      (compile-file src dest nil)))
  ([src dest]
    (compile-file src dest nil))
  ([src dest opts]
    (let [src-file  (io/file src)
          dest-file (io/file dest)
          opts      (merge {:optimizations :none} opts)]
      (if (.exists src-file)
        (try
          (let [{ns :ns :as ns-info} (ana/parse-ns src-file dest-file opts)]
            (if (requires-compilation? src-file dest-file opts)
              (do (util/mkdirs dest-file)
                (when (contains? (::ana/namespaces @env/*compiler*) ns)
                  (swap! env/*compiler* update-in [::ana/namespaces] dissoc ns))
                (compile-file* src-file dest-file opts))
              (do
                (when-not (contains? (::ana/namespaces @env/*compiler*) ns)
                  (with-core-cljs opts (fn [] (ana/analyze-file src-file opts))))
                ns-info)))
          (catch Exception e
            (throw (ex-info (str "failed compiling file:" src) {:file src} e))))
        (throw (java.io.FileNotFoundException. (str "The file " src " does not exist.")))))))

(defn cljs-files-in
  "Return a sequence of all .cljs files in the given directory."
  [dir]
  (filter #(let [name (.getName ^File %)]
             (and (.endsWith name ".cljs")
                  (not= \. (first name))
                  (not (contains? cljs-reserved-file-names name))))
          (file-seq dir)))

(defn compile-root
  "Looks recursively in src-dir for .cljs files and compiles them to
   .js files. If target-dir is provided, output will go into this
   directory mirroring the source directory structure. Returns a list
   of maps containing information about each file which was compiled
   in dependency order."
  ([src-dir]
     (compile-root src-dir "out"))
  ([src-dir target-dir]
     (compile-root src-dir target-dir nil))
  ([src-dir target-dir opts]
     (swap! env/*compiler* assoc :root src-dir)
     (let [src-dir-file (io/file src-dir)]
       (loop [cljs-files (cljs-files-in src-dir-file)
              output-files []]
         (if (seq cljs-files)
           (let [cljs-file (first cljs-files)
                 output-file (util/to-target-file target-dir (ana/parse-ns cljs-file))
                 ns-info (compile-file cljs-file output-file opts)]
             (recur (rest cljs-files) (conj output-files (assoc ns-info :file-name (.getPath output-file)))))
           output-files)))))

;; TODO: needs fixing, table will include other things than keywords - David

(defn emit-constants-table [table]
  (doseq [[keyword value] table]
    (let [ns   (namespace keyword)
          name (name keyword)]
      (emits "cljs.core." value " = ")
      (emits-keyword keyword)
      (emits ";\n"))))

(defn emit-constants-table-to-file [table dest]
  (io/make-parents dest)
  (with-open [out ^java.io.Writer (io/make-writer dest {})]
    (binding [*out* out]
      (emit-constants-table table))))
