;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(set! *warn-on-reflection* true)

(ns cljs.compiler
  (:refer-clojure :exclude [munge macroexpand-1])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [cljs.tagged-literals :as tags]
            [cljs.analyzer :as ana])
  (:import java.lang.StringBuilder))

(declare munge)

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

(def ^:dynamic *cljs-source-map* nil)
(def ^:dynamic *cljs-gen-col* nil)
(def ^:dynamic *cljs-gen-line* nil)

(def ^:dynamic *emitted-provides* nil)
(def ^:dynamic *lexical-renames* {})
(def cljs-reserved-file-names #{"deps.cljs"})

(defmacro ^:private debug-prn
  [& args]
  `(.println System/err (str ~@args)))

(defonce ns-first-segments (atom '#{"cljs" "clojure"}))

(defn munge
  ([s] (munge s js-reserved))
  ([s reserved]
    (if (map? s)
      ; Unshadowing
      (let [{:keys [name field] :as info} s
            depth (loop [d 0, {:keys [shadow]} info]
                    (cond
                      shadow (recur (inc d) shadow)
                      (@ns-first-segments (str name)) (inc d)
                      :else d))
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

(defmulti emit :op)

(defn emits [& xs]
  (doseq [x xs]
    (cond
     (nil? x) nil
     (map? x) (emit x)
     (seq? x) (apply emits x)
     (fn? x)  (x)
     :else (let [s (print-str x)]
             (when *cljs-gen-col*
               (swap! *cljs-gen-col* (fn [col] (+ col (count s)))))
             (print s))))
  nil)

(defn ^String emit-str [expr]
  (with-out-str (emit expr)))

(defn emitln [& xs]
  (apply emits xs)
  (println)
  (when *cljs-gen-line*
    (swap! *cljs-gen-line* inc))
  (when *cljs-gen-col*
    (reset! *cljs-gen-col* 0))
  nil)

(defn ^String emit-str [expr]
  (with-out-str (emit expr)))

(defn emit-provide [sym]
  (when-not (or (nil? *emitted-provides*) (contains? @*emitted-provides* sym))
    (swap! *emitted-provides* conj sym)
    (emitln "goog.provide('" (munge sym) "');")))

(defmulti emit-constant class)
(defmethod emit-constant nil [x] (emits "null"))
(defmethod emit-constant Long [x] (emits x))
(defmethod emit-constant Integer [x] (emits x)) ; reader puts Integers in metadata
(defmethod emit-constant Double [x] (emits x))
(defmethod emit-constant String [x]
  (emits (wrap-in-double-quotes (escape-string x))))
(defmethod emit-constant Boolean [x] (emits (if x "true" "false")))
(defmethod emit-constant Character [x]
  (emits (wrap-in-double-quotes (escape-char x))))

(defmethod emit-constant java.util.regex.Pattern [x]
  (let [[_ flags pattern] (re-find #"^(?:\(\?([idmsux]*)\))?(.*)" (str x))]
    (emits \/ (.replaceAll (re-matcher #"/" pattern) "\\\\/") \/ flags)))

(defmethod emit-constant clojure.lang.Keyword [x]
           (emits \" "\\uFDD0" \:
                  (if (namespace x)
                    (str (namespace x) "/") "")
                  (name x)
                  \"))

(def ^:const goog-hash-max 0x100000000)

(defn goog-string-hash [s]
  (reduce
    (fn [r c]
      (mod (+ (* 31 r) (int c)) goog-hash-max))
    0 s))

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
    (emit-constant (clojure.lang.Util/hashCombine
                     (unchecked-int (goog-string-hash ns))
                     (unchecked-int (goog-string-hash name))))
    (emits ",")
    (emit-constant nil)
    (emits ")")))

(defmacro emit-wrap [env & body]
  `(let [env# ~env]
     (when (= :return (:context env#)) (emits "return "))
     ~@body
     (when-not (= :expr (:context env#)) (emitln ";"))))

(defmethod emit :no-op [m])

(defmethod emit :var
  [{:keys [info env] :as arg}]
  (let [var-name (:name info)
        info (if (= (namespace var-name) "js")
               (name var-name)
               info)]
    (when *cljs-source-map*
      (when (and (:line env) (symbol? var-name))
        (let [{:keys [line column]} env]
          (swap! *cljs-source-map*
            (fn [m]
              (let [minfo {:gcol  @*cljs-gen-col*
                           :gline @*cljs-gen-line*
                           :name  var-name}]
                (update-in m [line]
                  (fnil (fn [m]
                          (update-in m [(or column 0)]
                            (fnil (fn [v] (conj v minfo)) [])))
                    (sorted-map)))))))))
    (when-not (= :statement (:context env))
      (emit-wrap env (emits (munge info))))))

(defmethod emit :meta
  [{:keys [expr meta env]}]
  (emit-wrap env
    (emits "cljs.core.with_meta(" expr "," meta ")")))

(def ^:private array-map-threshold 8)
(def ^:private obj-map-threshold 8)

(defmethod emit :map
  [{:keys [env keys vals]}]
  (let [simple-keys? (every? #(or (string? %) (keyword? %)) keys)]
    (emit-wrap env
      (cond
        (zero? (count keys))
        (emits "cljs.core.ObjMap.EMPTY")

        (and simple-keys? (<= (count keys) obj-map-threshold))
        (emits "cljs.core.ObjMap.fromObject(["
               (comma-sep keys)          ; keys
               "],{"
               (comma-sep (map (fn [k v]
                                 (with-out-str (emit k) (print ":") (emit v)))
                               keys vals)) ; js obj
               "})")

        (<= (count keys) array-map-threshold)
        (emits "cljs.core.PersistentArrayMap.fromArray(["
               (comma-sep (interleave keys vals))
               "], true)")

        :else
        (emits "cljs.core.PersistentHashMap.fromArrays(["
               (comma-sep keys)
               "],["
               (comma-sep vals)
               "])")))))

(defmethod emit :list
  [{:keys [items env]}]
  (emit-wrap env
    (if (empty? items)
      (emits "cljs.core.List.EMPTY")
      (emits "cljs.core.list(" (comma-sep items) ")"))))

(defmethod emit :vector
  [{:keys [items env]}]
  (emit-wrap env
    (if (empty? items)
      (emits "cljs.core.PersistentVector.EMPTY")
      (emits "cljs.core.PersistentVector.fromArray(["
             (comma-sep items) "], true)"))))

(defmethod emit :set
  [{:keys [items env]}]
  (emit-wrap env
    (if (empty? items)
      (emits "cljs.core.PersistentHashSet.EMPTY")
      (emits "cljs.core.PersistentHashSet.fromArray(["
             (comma-sep (interleave items (repeat "null"))) "], true)"))))

(defmethod emit :constant
  [{:keys [form env]}]
  (when-not (= :statement (:context env))
    (emit-wrap env (emit-constant form))))

(defn get-tag [e]
  (or (-> e :tag)
      (-> e :info :tag)
      (-> e :form meta :tag)))

(defn infer-tag [e]
  (if-let [tag (get-tag e)]
    tag
    (case (:op e)
      :let (infer-tag (:expr e))
      :do  (infer-tag (:ret e))
      :if (let [then-tag (infer-tag (:then e))
                else-tag (infer-tag (:else e))]
            (when (= then-tag else-tag)
              then-tag))
      :constant (case (:form e)
                  true 'boolean
                  false 'boolean
                  nil)
      nil)))

(defn safe-test? [e]
  (let [tag (infer-tag e)]
    (or (#{'boolean 'seq} tag)
        (when (= (:op e) :constant)
          (let [form (:form e)]
            (not (or (and (string? form) (= form ""))
                     (and (number? form) (zero? form)))))))))

(defmethod emit :if
  [{:keys [test then else env unchecked]}]
  (let [context (:context env)
        checked (not (or unchecked (safe-test? test)))]
    (if (= :expr context)
      (emits "(" (when checked "cljs.core.truth_") "(" test ")?" then ":" else ")")
      (do
        (if checked
          (emitln "if(cljs.core.truth_(" test "))")
          (emitln "if(" test ")"))
        (emitln "{" then "} else")
        (emitln "{" else "}")))))

(defmethod emit :throw
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

(defmethod emit :def
  [{:keys [name var init env doc export]}]
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
        (emitln "goog.exportSymbol('" (munge export) "', " mname ");")))))

(defn emit-apply-to
  [{:keys [name params env]}]
  (let [arglist (gensym "arglist__")
        delegate-name (str (munge name) "__delegate")
        params (map munge params)]
    (emitln "(function (" arglist "){")
    (doseq [[i param] (map-indexed vector (drop-last 2 params))]
      (emits "var " param " = cljs.core.first(")
      (emitln arglist ");")
      (emitln arglist " = cljs.core.next(" arglist ");"))
    (if (< 1 (count params))
      (do
        (emitln "var " (last (butlast params)) " = cljs.core.first(" arglist ");")
        (emitln "var " (last params) " = cljs.core.rest(" arglist ");")
        (emitln "return " delegate-name "(" (string/join ", " params) ");"))
      (do
        (emitln "var " (last params) " = cljs.core.seq(" arglist ");")
        (emitln "return " delegate-name "(" (string/join ", " params) ");")))
    (emits "})")))

(defn emit-fn-method
  [{:keys [type name variadic params expr env recurs max-fixed-arity]}]
  (emit-wrap env
             (emitln "(function " (munge name) "(" (comma-sep (map munge params)) "){")
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
                   params (map munge params)
                   delegate-name (str mname "__delegate")]
               (emitln "(function() { ")
               (emitln "var " delegate-name " = function (" (comma-sep params) "){")
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
                 (emitln "var " (last params) " = null;")
                 (emitln "if (arguments.length > " (dec (count params)) ") {")
                 (emitln "  " (last params) " = cljs.core.array_seq(Array.prototype.slice.call(arguments, " (dec (count params)) "),0);")
                 (emitln "} "))
               (emitln "return " delegate-name ".call(" (string/join ", " (cons "this" params)) ");")
               (emitln "};")

               (emitln mname ".cljs$lang$maxFixedArity = " max-fixed-arity ";")
               (emits mname ".cljs$lang$applyTo = ")
               (emit-apply-to (assoc f :name name))
               (emitln ";")
               (emitln mname ".cljs$core$IFn$_invoke$arity$variadic = " delegate-name ";")
               (emitln "return " mname ";")
               (emitln "})()"))))

(defmethod emit :fn
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
        (let [has-name? (and name true)
              name (or name (gensym))
              mname (munge name)
              maxparams (map munge (apply max-key count (map :params methods)))
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
            (emitln "var " (last maxparams) " = var_args;"))
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
          (when has-name?
            (doseq [[n meth] ms]
              (let [c (count (:params meth))]
                (if (:variadic meth)
                  (emitln mname ".cljs$core$IFn$_invoke$arity$variadic = " n ".cljs$core$IFn$_invoke$arity$variadic;")
                  (emitln mname ".cljs$core$IFn$_invoke$arity$" c " = " n ";")))))
          (emitln "return " mname ";")
          (emitln "})()")))
      (when loop-locals
        (emitln ";})(" (comma-sep loop-locals) "))")))))

(defmethod emit :do
  [{:keys [statements ret env]}]
  (let [context (:context env)]
    (when (and statements (= :expr context)) (emits "(function (){"))
    (when statements
      (emits statements))
    (emit ret)
    (when (and statements (= :expr context)) (emits "})()"))))

(defmethod emit :try*
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
        (emitln "var " (munge binding) " = " init ";"))
      (when is-loop (emitln "while(true){"))
      (emits expr)
      (when is-loop
        (emitln "break;")
        (emitln "}")))
    (when (= :expr context) (emits "})()"))))

(defmethod emit :let [ast]
  (emit-let ast false))

(defmethod emit :loop [ast]
  (emit-let ast true))

(defmethod emit :recur
  [{:keys [frame exprs env]}]
  (let [temps (vec (take (count exprs) (repeatedly gensym)))
        params (:params frame)]
    (emitln "{")
    (dotimes [i (count exprs)]
      (emitln "var " (temps i) " = " (exprs i) ";"))
    (dotimes [i (count exprs)]
      (emitln (munge (params i)) " = " (temps i) ";"))
    (emitln "continue;")
    (emitln "}")))

(defmethod emit :letfn
  [{:keys [bindings expr env]}]
  (let [context (:context env)]
    (when (= :expr context) (emits "(function (){"))
    (doseq [{:keys [init] :as binding} bindings]
      (emitln "var " (munge binding) " = " init ";"))
    (emits expr)
    (when (= :expr context) (emits "})()"))))

(defn protocol-prefix [psym]
  (symbol (str (-> (str psym) (.replace \. \$) (.replace \/ \$)) "$")))

(defmethod emit :invoke
  [{:keys [f args env] :as expr}]
  (let [info (:info f)
        fn? (and ana/*cljs-static-fns*
                 (not (:dynamic info))
                 (:fn-var info))
        protocol (:protocol info)
        tag      (infer-tag (first (:args expr)))
        proto? (and protocol tag
                 (or (and ana/*cljs-static-fns* protocol (= tag 'not-native)) 
                     (and
                       (or ana/*cljs-static-fns*
                           (:protocol-inline env))
                       (or (= protocol tag)
                           (when-let [ps (:protocols (ana/resolve-existing-var (dissoc env :locals) tag))]
                             (ps protocol))))))
        opt-not? (and (= (:name info) 'cljs.core/not)
                      (= (infer-tag (first (:args expr))) 'boolean))
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
                             (fn [name] (symbol (str (munge name) ".cljs$core$IFn$_invoke$arity$variadic"))))
              {:max-fixed-arity mfa}]

             ;; direct dispatch to specific arity case
             :else
             (let [arities (map count mps)]
               (if (some #{arity} arities)
                 [(update-in f [:info :name]
                             (fn [name] (symbol (str (munge name) ".cljs$core$IFn$_invoke$arity$" arity)))) nil]
                 [f nil]))))
          [f nil])]
    (emit-wrap env
      (cond
       opt-not?
       (emits "!(" (first args) ")")

       proto?
       (let [pimpl (str (munge (protocol-prefix protocol))
                        (munge (name (:name info))) "$arity$" (count args))]
         (emits (first args) "." pimpl "(" (comma-sep args) ")"))

       keyword?
       (emits "(new cljs.core.Keyword(" f ")).call(" (comma-sep (cons "null" args)) ")")
       
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

(defmethod emit :new
  [{:keys [ctor args env]}]
  (emit-wrap env
             (emits "(new " ctor "("
                    (comma-sep args)
                    "))")))

(defmethod emit :set!
  [{:keys [target val env]}]
  (emit-wrap env (emits target " = " val)))

(defmethod emit :ns
  [{:keys [name requires uses requires-macros env]}]
  (swap! ns-first-segments conj (first (string/split (str name) #"\.")))
  (emitln "goog.provide('" (munge name) "');")
  (when-not (= name 'cljs.core)
    (emitln "goog.require('cljs.core');"))
  (doseq [lib (into (vals requires) (distinct (vals uses)))]
    (emitln "goog.require('" (munge lib) "');")))

(defmethod emit :deftype*
  [{:keys [t fields pmasks]}]
  (let [fields (map munge fields)]
    (emit-provide t)
    (emitln "")
    (emitln "/**")
    (emitln "* @constructor")
    (emitln "*/")
    (emitln (munge t) " = (function (" (comma-sep fields) "){")
    (doseq [fld fields]
      (emitln "this." fld " = " fld ";"))
    (doseq [[pno pmask] pmasks]
      (emitln "this.cljs$lang$protocol_mask$partition" pno "$ = " pmask ";"))
    (emitln "})")))

(defmethod emit :defrecord*
  [{:keys [t fields pmasks]}]
  (let [fields (concat (map munge fields) '[__meta __extmap])]
    (emit-provide t)
    (emitln "")
    (emitln "/**")
    (emitln "* @constructor")
    (doseq [fld fields]
      (emitln "* @param {*} " fld))
    (emitln "* @param {*=} __meta ")
    (emitln "* @param {*=} __extmap")
    (emitln "*/")
    (emitln (munge t) " = (function (" (comma-sep fields) "){")
    (doseq [fld fields]
      (emitln "this." fld " = " fld ";"))
    (doseq [[pno pmask] pmasks]
      (emitln "this.cljs$lang$protocol_mask$partition" pno "$ = " pmask ";"))
    (emitln "if(arguments.length>" (- (count fields) 2) "){")
    (emitln "this.__meta = __meta;")
    (emitln "this.__extmap = __extmap;")
    (emitln "} else {")
    (emits "this.__meta=")
    (emit-constant nil)
    (emitln ";")
    (emits "this.__extmap=")
    (emit-constant nil)
    (emitln ";")
    (emitln "}")
    (emitln "})")))

(defmethod emit :dot
  [{:keys [target field method args env]}]
  (emit-wrap env
             (if field
               (emits target "." (munge field #{}))
               (emits target "." (munge method #{}) "("
                      (comma-sep args)
                      ")"))))

(defmethod emit :js
  [{:keys [env code segs args]}]
  (emit-wrap env
             (if code
               (emits code)
               (emits (interleave (concat segs (repeat nil))
                                  (concat args [nil]))))))

(defn forms-seq
  "Seq of forms in a Clojure or ClojureScript file."
  ([f]
     (forms-seq f (clojure.lang.LineNumberingPushbackReader. (io/reader f))))
  ([f ^java.io.PushbackReader rdr]
     (if-let [form (binding [*ns* ana/*reader-ns*] (read rdr nil nil))]
       (lazy-seq (cons form (forms-seq f rdr)))
       (.close rdr))))

(defn rename-to-js
  "Change the file extension from .cljs to .js. Takes a File or a
  String. Always returns a String."
  [file-str]
  (clojure.string/replace file-str #"\.cljs$" ".js"))

(defn mkdirs
  "Create all parent directories for the passed file."
  [^java.io.File f]
  (.mkdirs (.getParentFile (.getCanonicalFile f))))

(defmacro with-core-cljs
  "Ensure that core.cljs has been loaded."
  [& body]
  `(do (when-not (:defs (get @ana/namespaces 'cljs.core))
         (ana/analyze-file "cljs/core.cljs"))
       ~@body))

(defn compile-file*
  ([src dest] (compile-file* src dest nil))
  ([src dest opts]
     (with-core-cljs
       (with-open [out ^java.io.Writer (io/make-writer dest {})]
         (binding [*out* out
                   ana/*cljs-ns* 'cljs.user
                   ana/*cljs-file* (.getPath ^java.io.File src)
                   *data-readers* tags/*cljs-data-readers*
                   *emitted-provides* (atom #{})
                   *cljs-source-map* (when (:source-map opts) (atom (sorted-map)))
                   *cljs-gen-line* (atom 0)
                   *cljs-gen-col* (atom 0)]
           (loop [forms (forms-seq src)
                  ns-name nil
                  deps nil]
             (if (seq forms)
               (let [env (ana/empty-env)
                     ast (ana/analyze env (first forms))]
                 (do (emit ast)
                     (if (= (:op ast) :ns)
                       (recur (rest forms) (:name ast) (merge (:uses ast) (:requires ast)))
                       (recur (rest forms) ns-name deps))))
               (merge
                 {:ns (or ns-name 'cljs.user)
                  :provides [ns-name]
                  :requires (if (= ns-name 'cljs.core) (set (vals deps)) (conj (set (vals deps)) 'cljs.core))
                  :file dest
                  :source-file src
                  :lines @*cljs-gen-line*}
                 (when (:source-map opts)
                   {:source-map @*cljs-source-map*})))))))))

(defn requires-compilation?
  "Return true if the src file requires compilation."
  [^java.io.File src ^java.io.File dest]
  (or (not (.exists dest))
      (> (.lastModified src) (.lastModified dest))))

(defn parse-ns [src dest opts]
  (with-core-cljs
    (binding [ana/*cljs-ns* 'cljs.user]
      (loop [forms (forms-seq src)]
        (if (seq forms)
          (let [env (ana/empty-env)
                ast (ana/analyze env (first forms))]
            (if (= (:op ast) :ns)
              (let [ns-name (:name ast)
                    deps    (merge (:uses ast) (:requires ast))]
                {:ns (or ns-name 'cljs.user)
                 :provides [ns-name]
                 :requires (if (= ns-name 'cljs.core)
                             (set (vals deps))
                             (conj (set (vals deps)) 'cljs.core))
                 :file dest
                 :source-file src
                 :lines (-> dest io/reader line-seq count)})
              (recur (rest forms)))))))))

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
    (let [src-file (io/file src)
           dest-file (io/file dest)]
      (if (.exists src-file)
        (try
          (if (or (requires-compilation? src-file dest-file) (:source-map opts))
            (do (mkdirs dest-file)
              (compile-file* src-file dest-file opts))
            (parse-ns src-file dest-file opts))
          (catch Exception e
            (throw (ex-info (str "failed compiling file:" src) {:file src} e))))
        (throw (java.io.FileNotFoundException. (str "The file " src " does not exist.")))))))

(comment
  ;; flex compile-file
  (do
    (compile-file "/tmp/hello.cljs" "/tmp/something.js")
    (slurp "/tmp/hello.js")

    (compile-file "/tmp/somescript.cljs")
    (slurp "/tmp/somescript.js")))

(defn path-seq
  [file-str]
  (->> java.io.File/separator
       java.util.regex.Pattern/quote
       re-pattern
       (string/split file-str)))

(defn to-path
  ([parts]
     (to-path parts java.io.File/separator))
  ([parts sep]
     (apply str (interpose sep parts))))

(defn to-target-file
  "Given the source root directory, the output target directory and
  file under the source root, produce the target file."
  [^java.io.File dir ^String target ^java.io.File file]
  (let [dir-path (path-seq (.getAbsolutePath dir))
        file-path (path-seq (.getAbsolutePath file))
        relative-path (drop (count dir-path) file-path)
        parents (butlast relative-path)
        parent-file (java.io.File. ^String (to-path (cons target parents)))]
    (java.io.File. parent-file ^String (rename-to-js (last relative-path)))))

(defn cljs-files-in
  "Return a sequence of all .cljs files in the given directory."
  [dir]
  (filter #(let [name (.getName ^java.io.File %)]
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
     (let [src-dir-file (io/file src-dir)]
       (loop [cljs-files (cljs-files-in src-dir-file)
              output-files []]
         (if (seq cljs-files)
           (let [cljs-file (first cljs-files)
                 output-file ^java.io.File (to-target-file src-dir-file target-dir cljs-file)
                 ns-info (compile-file cljs-file output-file opts)]
             (recur (rest cljs-files) (conj output-files (assoc ns-info :file-name (.getPath output-file)))))
           output-files)))))

(comment
  ;; compile-root
  ;; If you have a standard project layout with all file in src
  (compile-root "src")
  ;; will produce a mirrored directory structure under "out" but all
  ;; files will be compiled to js.
  )

(comment

;;the new way - use the REPL!!
(require '[cljs.compiler :as comp])
(def repl-env (comp/repl-env))
(comp/repl repl-env)
;having problems?, try verbose mode
(comp/repl repl-env :verbose true)
;don't forget to check for uses of undeclared vars
(comp/repl repl-env :warn-on-undeclared true)

(test-stuff)
(+ 1 2 3)
([ 1 2 3 4] 2)
({:a 1 :b 2} :a)
({1 1 2 2} 1)
(#{1 2 3} 2)
(:b {:a 1 :b 2})
('b '{:a 1 b 2})

(extend-type number ISeq (-seq [x] x))
(seq 42)
;(aset cljs.core.ISeq "number" true)
;(aget cljs.core.ISeq "number")
(satisfies? ISeq 42)
(extend-type nil ISeq (-seq [x] x))
(satisfies? ISeq nil)
(seq nil)

(extend-type default ISeq (-seq [x] x))
(satisfies? ISeq true)
(seq true)

(test-stuff)

(array-seq [])
(defn f [& etc] etc)
(f)

(in-ns 'cljs.core)
;;hack on core


(deftype Foo [a] IMeta (-meta [_] (fn [] a)))
((-meta (Foo. 42)))

;;OLD way, don't you want to use the REPL?
(in-ns 'cljs.compiler)
(import '[javax.script ScriptEngineManager])
(def jse (-> (ScriptEngineManager.) (.getEngineByName "JavaScript")))
(.eval jse cljs.compiler/bootjs)
(def envx {:ns (@namespaces 'cljs.user) :context :expr :locals '{ethel {:name ethel__123 :init nil}}})
(analyze envx nil)
(analyze envx 42)
(analyze envx "foo")
(analyze envx 'fred)
(analyze envx 'fred.x)
(analyze envx 'ethel)
(analyze envx 'ethel.x)
(analyze envx 'my.ns/fred)
(analyze envx 'your.ns.fred)
(analyze envx '(if test then else))
(analyze envx '(if test then))
(analyze envx '(and fred ethel))
(analyze (assoc envx :context :statement) '(def test "fortytwo" 42))
(analyze (assoc envx :context :expr) '(fn* ^{::fields [a b c]} [x y] a y x))
(analyze (assoc envx :context :statement) '(let* [a 1 b 2] a))
(analyze (assoc envx :context :statement) '(defprotocol P (bar [a]) (baz [b c])))
(analyze (assoc envx :context :statement) '(. x y))
(analyze envx '(fn foo [x] (let [x 42] (js* "~{x}['foobar']"))))

(analyze envx '(ns fred (:require [your.ns :as yn]) (:require-macros [clojure.core :as core])))
(defmacro js [form]
  `(emit (ana/analyze {:ns (@ana/namespaces 'cljs.user) :context :statement :locals {}} '~form)))

(defn jscapture [form]
  "just grabs the js, doesn't print it"
  (with-out-str
    (emit (analyze {:ns (@namespaces 'cljs.user) :context :expr :locals {}} form))))

(defn jseval [form]
  (let [js (jscapture form)]
    ;;(prn js)
    (.eval jse (str "print(" js ")"))))

;; from closure.clj
(optimize (jscapture '(defn foo [x y] (if true 46 (recur 1 x)))))

(js (if a b c))
(js (def x 42))
(js (defn foo [a b] a))
(js (do 1 2 3))
(js (let [a 1 b 2 a b] a))

(js (ns fred (:require [your.ns :as yn]) (:require-macros [cljs.core :as core])))

(js (def foo? (fn* ^{::fields [a? b c]} [x y] (if true a? (recur 1 x)))))
(js (def foo (fn* ^{::fields [a b c]} [x y] (if true a (recur 1 x)))))
(js (defn foo [x y] (if true x y)))
(jseval '(defn foo [x y] (if true x y)))
(js (defn foo [x y] (if true 46 (recur 1 x))))
(jseval '(defn foo [x y] (if true 46 (recur 1 x))))
(jseval '(foo 1 2))
(js (and fred ethel))
(jseval '(ns fred (:require [your.ns :as yn]) (:require-macros [cljs.core :as core])))
(js (def x 42))
(jseval '(def x 42))
(jseval 'x)
(jseval '(if 42 1 2))
(jseval '(or 1 2))
(jseval '(fn* [x y] (if true 46 (recur 1 x))))
(.eval jse "print(test)")
(.eval jse "print(cljs.user.Foo)")
(.eval jse  "print(cljs.user.Foo = function (){\n}\n)")
(js (def fred 42))
(js (deftype* Foo [a b-foo c]))
(jseval '(deftype* Foo [a b-foo c]))
(jseval '(. (new Foo 1 2 3) b-foo))
(js (. (new Foo 1 2 3) b))
(.eval jse "print(new cljs.user.Foo(1, 42, 3).b)")
(.eval jse "(function (x, ys){return Array.prototype.slice.call(arguments, 1);})(1,2)[0]")

(macroexpand-1 '(cljs.core/deftype Foo [a b c] Fred (fred [x] a) (fred [x y] b) (ethel [x] c) Ethel (foo [] d)))
(-> (macroexpand-1 '(cljs.core/deftype Foo [a b c] Fred (fred [x] a) (fred [x y] b) (ethel [x] c) Ethel (foo [] d)))
    last last last first meta)

(macroexpand-1 '(cljs.core/extend-type Foo Fred (fred ([x] a) ([x y] b)) (ethel ([x] c)) Ethel (foo ([] d))))
(js (new foo.Bar 65))
(js (defprotocol P (bar [a]) (baz [b c])))
(js (. x y))
(js (. "fred" (y)))
(js (. x y 42 43))
(js (.. a b c d))
(js (. x (y 42 43)))
(js (fn [x] x))
(js (fn ([t] t) ([x y] y) ([ a b & zs] b)))

(js (. (fn foo ([t] t) ([x y] y) ([a b & zs] b)) call nil 1 2))
(js (fn foo
      ([t] t)
      ([x y] y)
      ([ a b & zs] b)))

(js ((fn foo
       ([t] (foo t nil))
       ([x y] y)
       ([ a b & zs] b)) 1 2 3))


(jseval '((fn foo ([t] t) ([x y] y) ([ a b & zs] zs)) 12 13 14 15))

(js (defn foo [this] this))

(js (defn foo [a b c & ys] ys))
(js ((fn [x & ys] ys) 1 2 3 4))
(jseval '((fn [x & ys] ys) 1 2 3 4))
(js (cljs.core/deftype Foo [a b c] Fred (fred [x] a) (fred [x y] a)  (ethel [x] c) Ethel (foo [] d)))
(jseval '(cljs.core/deftype Foo [a b c] Fred (fred [x] a) (fred [x y] a)  (ethel [x] c) Ethel (foo [] d)))

(js (do
           (defprotocol Proto (foo [this]))
           (deftype Type [a] Proto (foo [this] a))
           (foo (new Type 42))))

(jseval '(do
           (defprotocol P-roto (foo? [this]))
           (deftype T-ype [a] P-roto (foo? [this] a))
           (foo? (new T-ype 42))))

(js (def x (fn foo [x] (let [x 42] (js* "~{x}['foobar']")))))
(js (let [a 1 b 2 a b] a))

(doseq [e '[nil true false 42 "fred" fred ethel my.ns/fred your.ns.fred
            (if test then "fooelse")
            (def x 45)
            (do x y y)
            (fn* [x y] x y x)
            (fn* [x y] (if true 46 (recur 1 x)))
            (let* [a 1 b 2 a a] a b)
            (do "do1")
            (loop* [x 1 y 2] (if true 42 (do (recur 43 44))))
            (my.foo 1 2 3)
            (let* [a 1 b 2 c 3] (set! y.s.d b) (new fred.Ethel a b c))
            (let [x (do 1 2 3)] x)
            ]]
  (->> e (analyze envx) emit)
  (newline)))
