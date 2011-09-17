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
            [clojure.string :as string]))

(declare resolve-var)
(require 'cljs.core)

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
    "volatile" "while" "with" "yield" "methods"})

(defonce namespaces (atom '{cljs.core {:name cljs.core}
                            cljs.user {:name cljs.user}}))

(def ^:dynamic *cljs-ns* 'cljs.user)
(def ^:dynamic *cljs-warn-on-undeclared* false)

(defn munge [s]
  (let [ss (str s)
        ms (if (.contains ss "]")
             (let [idx (inc (.lastIndexOf ss "]"))]
               (str (subs ss 0 idx)
                    (clojure.lang.Compiler/munge (subs ss idx))))
             (clojure.lang.Compiler/munge ss))
        ms (if (js-reserved ms) (str ms "$") ms)]
    (if (symbol? s)
      (symbol ms)
      ms)))

(defn confirm-var-exists [env prefix suffix]
  (when *cljs-warn-on-undeclared*
    (let [crnt-ns (-> env :ns :name)]
      (when (= prefix crnt-ns)
        (when-not (-> @namespaces crnt-ns :defs suffix)
          (binding [*out* *err*]
            (println
              (str "WARNING: Use of undeclared Var " prefix "/" suffix
                   (when (:line env)
                     (str " at line " (:line env)))))))))))

(defn resolve-ns-alias [env name]
  (let [sym (symbol name)]
    (get (:requires (:ns env)) sym sym)))

(defn core-name?
  "Is sym visible from core in the current compilation namespace?"
  [env sym]
  (and (get (:defs (@namespaces 'cljs.core)) sym)
       (not (contains? (-> env :ns :excludes) sym))))

(defn js-var [sym]
  (let [parts (string/split (name sym) #"\.")
        first (first parts)
        step (fn [part] (str "['" part "']"))]
    (apply str first (map step (rest parts)))))

(defn resolve-existing-var [env sym]
  (if (= (namespace sym) "js")
    {:name (js-var sym)}
    (let [s (str sym)
          lb (-> env :locals sym)
          nm
          (cond
           lb (:name lb)

           (namespace sym)
           (let [ns (namespace sym)
                 ns (if (= "clojure.core" ns) "cljs.core" ns)
                 full-ns (resolve-ns-alias env ns)]
             (confirm-var-exists env full-ns (symbol (name sym)))
             (symbol (str full-ns "." (munge (name sym)))))

           (.contains s ".")
           (munge (let [idx (.indexOf s ".")
                        prefix (symbol (subs s 0 idx))
                        suffix (subs s idx)
                        lb (-> env :locals prefix)]
                    (if lb
                      (symbol (str (:name lb) suffix))
                      (do
                        (confirm-var-exists env prefix (symbol suffix))
                        sym))))

           (get-in @namespaces [(-> env :ns :name) :uses sym])
           (symbol (str (get-in @namespaces [(-> env :ns :name) :uses sym]) "." (munge (name sym))))

           :else
           (let [full-ns (if (core-name? env sym)
                           'cljs.core
                           (-> env :ns :name))]
             (confirm-var-exists env full-ns sym)
             (munge (symbol (str full-ns "." (munge (name sym)))))))]
      {:name nm})))

(defn resolve-var [env sym]
  (if (= (namespace sym) "js")
    {:name (js-var sym)}
    (let [s (str sym)
          lb (-> env :locals sym)
          nm 
          (cond
           lb (:name lb)
         
           (namespace sym)
           (let [ns (namespace sym)
                 ns (if (= "clojure.core" ns) "cljs.core" ns)]
             (symbol (str (resolve-ns-alias env ns) "." (munge (name sym)))))

           (.contains s ".")
           (munge (let [idx (.indexOf s ".")
                        prefix (symbol (subs s 0 idx))
                        suffix (subs s idx)
                        lb (-> env :locals prefix)]
                    (if lb
                      (symbol (str (:name lb) suffix))
                      sym)))

           :else
           (munge (symbol (str
                           (if (core-name? env sym)
                             'cljs.core
                             (-> env :ns :name))
                           "." (munge (name sym))))))]
      {:name nm})))

(defn- comma-sep [xs]
  (apply str (interpose "," xs)))

(defmulti emit-constant class)
(defmethod emit-constant nil [x] (print "null"))
(defmethod emit-constant Long [x] (print x))
(defmethod emit-constant Integer [x] (print x)) ; reader puts Integers in metadata
(defmethod emit-constant Double [x] (print x))
(defmethod emit-constant String [x] (pr x))
(defmethod emit-constant Boolean [x] (print (if x "true" "false")))
(defmethod emit-constant Character [x] (pr (str x)))

(defmethod emit-constant java.util.regex.Pattern [x]
  (let [[_ flags pattern] (re-find #"^(?:\(\?([idmsux]*)\))?(.*)" (str x))]
    (print (str \/ (.replaceAll (re-matcher #"/" pattern) "\\\\/") \/ flags))))

(defmethod emit-constant clojure.lang.Keyword [x]
           (pr (str \uFDD0 \'
                    (if (namespace x)
                      (str (namespace x) "/") "")
                    (name x))))

(defmethod emit-constant clojure.lang.Symbol [x]
           (pr (str \uFDD1 \'
                    (if (namespace x)
                      (str (namespace x) "/") "")
                    (name x))))

(defn- emit-meta-constant [x string]
  (if (meta x)
    (do
      (print (str "cljs.core.with_meta(" string ","))
      (emit-constant (meta x))
      (print ")"))
    (print string)))

(defmethod emit-constant clojure.lang.PersistentList$EmptyList [x]
  (emit-meta-constant x "cljs.core.List.EMPTY"))

(defmethod emit-constant clojure.lang.PersistentList [x]
  (emit-meta-constant x
    (str "cljs.core.list("
         (comma-sep (map #(with-out-str (emit-constant %)) x))
         ")")))

(defmethod emit-constant clojure.lang.Cons [x]
  (emit-meta-constant x
    (str "cljs.core.list("
         (comma-sep (map #(with-out-str (emit-constant %)) x))
         ")")))

(defmethod emit-constant clojure.lang.IPersistentVector [x]
  (emit-meta-constant x
    (str "(new cljs.core.Vector(null, ["
         (comma-sep (map #(with-out-str (emit-constant %)) x))
         "]))")))

(defmethod emit-constant clojure.lang.IPersistentMap [x]
  (emit-meta-constant x
    (str "cljs.core.hash_map("
         (comma-sep (map #(with-out-str (emit-constant %))
                         (apply concat x)))
         ")")))

(defmethod emit-constant clojure.lang.PersistentHashSet [x]
  (emit-meta-constant x
    (str "cljs.core.set(["
         (comma-sep (map #(with-out-str (emit-constant %)) x))
         "])")))

(defmulti emit :op)

(defn ^String emits [expr]
  (with-out-str (emit expr)))

(defn emit-block
  [context statements ret]
  (if statements
    (let [body (str (apply str (map emits statements)) (emits ret))]
      (print body))
    (emit ret)))

(defmacro emit-wrap [env & body]
  `(let [env# ~env]
     (when (= :return (:context env#)) (print "return "))
     ~@body
     (when-not (= :expr (:context env#)) (print ";\n"))))

(defmethod emit :var
  [{:keys [info env] :as arg}]
  (emit-wrap env (print (munge (:name info)))))

(defmethod emit :meta
  [{:keys [expr meta env]}]
  (emit-wrap env
    (print (str "cljs.core.with_meta(" (emits expr) "," (emits meta) ")"))))

(defmethod emit :map
  [{:keys [children env simple-keys? keys vals]}]
  (emit-wrap env
    (if simple-keys?
      (print (str "cljs.core.ObjMap.fromObject(["
                  (comma-sep (map emits keys)) ; keys
                  "],{"
                  (comma-sep (map (fn [k v] (str (emits k) ":" (emits v)))
                                  keys vals)) ; js obj
                  "})"))
      (print (str "cljs.core.HashMap.fromArrays(["
                  (comma-sep (map emits keys))
                  "],["
                  (comma-sep (map emits vals))
                  "])")))))

(defmethod emit :vector
  [{:keys [children env]}]
  (emit-wrap env
    (print (str "cljs.core.Vector.fromArray(["
                (comma-sep (map emits children)) "])"))))

(defmethod emit :set
  [{:keys [children env]}]
  (emit-wrap env
    (print (str "cljs.core.set(["
                (comma-sep (map emits children)) "])"))))

(defmethod emit :constant
  [{:keys [form env]}]
  (when-not (= :statement (:context env))
    (emit-wrap env (emit-constant form))))

(defmethod emit :if
  [{:keys [test then else env]}]
  (let [context (:context env)]
    (if (= :expr context)
      (print (str "(cljs.core.truth_(" (emits test) ")?" (emits then) ":" (emits else) ")"))
      (print (str "if(cljs.core.truth_(" (emits test) "))\n{" (emits then) "} else\n{" (emits else) "}\n")))))

(defmethod emit :throw
  [{:keys [throw env]}]
  (if (= :expr (:context env))
    (print (str "(function(){throw " (emits throw) "})()"))
    (print (str "throw " (emits throw) ";\n"))))

(defn emit-comment
  "Emit a nicely formatted comment string."
  [doc jsdoc]
  (let [docs (when doc [doc])
        docs (if jsdoc (concat docs jsdoc) docs)
        docs (remove nil? docs)]
    (letfn [(print-comment-lines [e] (doseq [next-line (string/split-lines e)]
                                       (println "*" (string/trim next-line))))]
      (when (seq docs)
        (println "/**")
        (doseq [e docs]
          (when e
            (print-comment-lines e)))
        (println "*/")))))

(defmethod emit :def
  [{:keys [name init env doc export]}]
  (when init
    (emit-comment doc (:jsdoc init))
    (print name)
    (print (str " = " (emits init)))
    (when-not (= :expr (:context env)) (print ";\n"))
    (when export
      (println (str "goog.exportSymbol('" export "', " name ");")))))

(defn emit-apply-to
  [{:keys [name params env]}]
  (let [arglist (gensym "arglist__")
        delegate-name (str name "__delegate")]
    (println (str "(function (" arglist "){"))
    (doseq [[i param] (map-indexed vector (butlast params))]
      (print (str "var " param " = cljs.core.first("))
      (dotimes [_ i] (print "cljs.core.next("))
      (print (str arglist ")"))
      (dotimes [_ i] (print ")"))
      (println ";"))
    (if (< 1 (count params))
      (do
        (print (str "var " (last params) " = cljs.core.rest("))
        (dotimes [_ (- (count params) 2)] (print "cljs.core.next("))
        (print arglist)
        (dotimes [_ (- (count params) 2)] (print ")"))
        (println ");")
        (println (str "return " delegate-name ".call(" (string/join ", " (cons "this" params)) ");")))
      (do
        (print (str "var " (last params) " = "))
        (print "cljs.core.seq(" arglist ");")
        (println ";")
        (println (str "return " delegate-name ".call(" (string/join ", " (cons "this" params)) ");"))))
    (print "})")))

(defn emit-fn-method
  [{:keys [gthis name variadic params statements ret env recurs max-fixed-arity]}]
  (emit-wrap env
             (print (str "(function " name "(" (comma-sep params) "){\n"))
             (when gthis
               (println (str "var " gthis " = this;")))
             (when recurs (print "while(true){\n"))
             (emit-block :return statements ret)
             (when recurs (print "break;\n}\n"))
             (print "})")))

(defn emit-variadic-fn-method
  [{:keys [gthis name variadic params statements ret env recurs max-fixed-arity] :as f}]
  (emit-wrap env
             (let [name (or name (gensym))
                   delegate-name (str name "__delegate")]
               (println "(function() { ")
               (println (str "var " delegate-name " = function (" (comma-sep params) "){"))
               (when recurs (print "while(true){\n"))
               (emit-block :return statements ret)
               (when recurs (print "break;\n}\n"))
               (println "};")

               (print (str "var " name " = function (" (comma-sep
                                                        (if variadic
                                                          (concat (butlast params) ['var_args])
                                                          params)) "){\n"))
               (when gthis
                 (println (str "var " gthis " = this;")))
               (when variadic
                 (println (str "var " (last params) " = null;"))
                 (println (str "if (goog.isDef(var_args)) {"))
                 (println (str "  " (last params) " = cljs.core.array_seq(Array.prototype.slice.call(arguments, " (dec (count params)) "),0);"))
                 (println (str "} ")))
               (println (str "return " delegate-name ".call(" (string/join ", " (cons "this" params)) ");"))
               (println "};")

               (println (str name ".cljs$lang$maxFixedArity = " max-fixed-arity ";"))
               (println (str name ".cljs$lang$applyTo = "
                             (with-out-str
                               (emit-apply-to (assoc f :name name)))
                             ";"))
               (println (str "return " name ";"))
               (println "})()"))))

(defmethod emit :fn
  [{:keys [name env methods max-fixed-arity variadic recur-frames]}]
  ;;fn statements get erased, serve no purpose and can pollute scope if named
  (when-not (= :statement (:context env))
    (let [loop-locals (seq (mapcat :names (filter #(and % @(:flag %)) recur-frames)))]
      (when loop-locals
        (when (= :return (:context env))
            (print "return "))
        (println (str "((function (" (comma-sep loop-locals) "){"))
        (when-not (= :return (:context env))
            (print "return ")))
      (if (= 1 (count methods))
        (if variadic
          (emit-variadic-fn-method (assoc (first methods) :name name))
          (emit-fn-method (assoc (first methods) :name name)))
        (let [name (or name (gensym))
              maxparams (apply max-key count (map :params methods))
              mmap (zipmap (repeatedly #(gensym (str name  "__"))) methods)
              ms (sort-by #(-> % second :params count) (seq mmap))]
          (when (= :return (:context env))
            (print "return "))
          (println "(function() {")
          (println (str "var " name " = null;"))
          (doseq [[n meth] ms]
            (println (str "var " n " = " (with-out-str (if (:variadic meth)
                                                         (emit-variadic-fn-method meth)
                                                         (emit-fn-method meth))) ";")))
          (println (str name " = function(" (comma-sep (if variadic
                                                         (concat (butlast maxparams) ['var_args])
                                                         maxparams)) "){"))
          (when variadic
            (println (str "var " (last maxparams) " = var_args;")))
          (println "switch(arguments.length){")
          (doseq [[n meth] ms]
            (if (:variadic meth)
              (do (println "default:")
                  (println (str "return " n ".apply(this,arguments);")))
              (let [pcnt (count (:params meth))]
                (println "case " pcnt ":")
                (println (str "return " n ".call(this" (if (zero? pcnt) nil
                                                           (str "," (comma-sep (take pcnt maxparams)))) ");")))))
          (println "}")
          (println "throw('Invalid arity: ' + arguments.length);")
          (println "};")
          (when variadic
            (println (str name ".cljs$lang$maxFixedArity = " max-fixed-arity ";"))
            (println (str name ".cljs$lang$applyTo = " (some #(let [[n m] %] (when (:variadic m) n)) ms) ".cljs$lang$applyTo;")))
          (println (str "return " name ";"))
          (println "})()")))
      (when loop-locals
        (println (str ";})(" (comma-sep loop-locals) "))"))))))

(defmethod emit :do
  [{:keys [statements ret env]}]
  (let [context (:context env)]
    (when (and statements (= :expr context)) (print "(function (){"))
    ;(when statements (print "{\n"))
    (emit-block context statements ret)
    ;(when statements (print "}"))
    (when (and statements (= :expr context)) (print "})()"))))

(defmethod emit :try*
  [{:keys [env try catch name finally]}]
  (let [context (:context env)
        subcontext (if (= :expr context) :return context)]
    (if (or name finally)
      (do
        (when (= :expr context) (print "(function (){"))
        (print "try{")
        (let [{:keys [statements ret]} try]
          (emit-block subcontext statements ret))
        (print "}")
        (when name
          (print (str "catch (" name "){"))
          (when catch
            (let [{:keys [statements ret]} catch]
              (emit-block subcontext statements ret)))      
          (print "}"))
        (when finally
          (let [{:keys [statements ret]} finally]
            (assert (not= :constant (:op ret)) "finally block cannot contain constant")
            (print "finally {")
            (emit-block subcontext statements ret)
            (print "}")))
        (when (= :expr context) (print "})()")))
      (let [{:keys [statements ret]} try]
        (when (and statements (= :expr context)) (print "(function (){"))
        (emit-block subcontext statements ret)
        (when (and statements (= :expr context)) (print "})()"))))))

(defmethod emit :let
  [{:keys [bindings statements ret env loop]}]
  (let [context (:context env)
        bs (map (fn [{:keys [name init]}]
                  (str "var " name " = " (emits init) ";\n"))
                bindings)]
    (when (= :expr context) (print "(function (){"))
    (print (str (apply str bs) "\n"))
    (when loop (print "while(true){\n"))
    (emit-block (if (= :expr context) :return context) statements ret)
    (when loop (print "break;\n}\n"))
    ;(print "}")
    (when (= :expr context) (print "})()"))))

(defmethod emit :recur
  [{:keys [frame exprs env]}]
  (let [temps (vec (take (count exprs) (repeatedly gensym)))
        names (:names frame)]
    (print "{\n")
    (dotimes [i (count exprs)]
      (print (str "var " (temps i) " = " (emits (exprs i)) ";\n")))
    (dotimes [i (count exprs)]
      (print (str (names i) " = " (temps i) ";\n")))
    (print "continue;\n")
    (print "}\n")))

(defmethod emit :invoke
  [{:keys [f args env]}]
  (emit-wrap env
             (print (str (emits f) ".call("
                         (comma-sep (cons "null" (map emits args)))
                         ")"))))

(defmethod emit :new
  [{:keys [ctor args env]}]
  (emit-wrap env
             (print (str "(new " (emits ctor) "("
                         (comma-sep (map emits args))
                         "))"))))

(defmethod emit :set!
  [{:keys [target val env]}]
  (emit-wrap env (print (str (emits target) " = "(emits val)))))

(defmethod emit :ns
  [{:keys [name requires uses requires-macros env]}]
  (println (str "goog.provide('" (munge name) "');"))
  (when-not (= name 'cljs.core)
    (println (str "goog.require('cljs.core');")))
  (doseq [lib (into (vals requires) (vals uses))]
    (println (str "goog.require('" (munge lib) "');"))))

(defmethod emit :deftype*
  [{:keys [t fields]}]
  (let [fields (map munge fields)]
    (println "\n/**\n* @constructor\n*/")
    (println (str t " = (function (" (comma-sep (map str fields)) "){"))
    (doseq [fld fields]
      (println (str "this." fld " = " fld ";")))
    (println "})")))

(defmethod emit :dot
  [{:keys [target field method args env]}]
  (emit-wrap env
             (if field
               (print (str (emits target) "." field))
               (print (str (emits target) "." method "("
                           (comma-sep (map emits args))
                           ")")))))

(defmethod emit :js
  [{:keys [env code segs args]}]
  (emit-wrap env
             (if code
               (print code)
               (print (apply str (interleave (concat segs (repeat nil))
                                             (concat (map emits args) [nil])))))))

(declare analyze analyze-symbol analyze-seq)

(def specials '#{if def fn* do let* loop* throw try* recur new set! ns deftype* . js* & quote})

(def ^:dynamic *recur-frames* nil)

(defmacro disallowing-recur [& body]
  `(binding [*recur-frames* (cons nil *recur-frames*)] ~@body))

(defn analyze-block
  "returns {:statements .. :ret .. :children ..}"
  [env exprs]
  (let [statements (disallowing-recur
                     (seq (map #(analyze (assoc env :context :statement) %) (butlast exprs))))
        ret (if (<= (count exprs) 1)
              (analyze env (first exprs))
              (analyze (assoc env :context (if (= :statement (:context env)) :statement :return)) (last exprs)))]
    {:statements statements :ret ret :children (vec (cons ret statements))}))

(defmulti parse (fn [op & rest] op))

(defmethod parse 'if
  [op env [_ test then else :as form] name]
  (let [test-expr (disallowing-recur (analyze (assoc env :context :expr) test)) 
        then-expr (analyze env then)
        else-expr (analyze env else)]
    {:env env :op :if :form form
     :test test-expr :then then-expr :else else-expr
     :children [test-expr then-expr else-expr]}))

(defmethod parse 'throw
  [op env [_ throw :as form] name]
  (let [throw-expr (disallowing-recur (analyze (assoc env :context :expr) throw))]
    {:env env :op :throw :form form
     :throw throw-expr
     :children [throw-expr]}))

(defmethod parse 'try*
  [op env [_ & body :as form] name]
  (let [body (vec body)
        catchenv (update-in env [:context] #(if (= :expr %) :return %))
        tail (peek body)
        fblock (when (and (seq? tail) (= 'finally (first tail)))
                  (rest tail))
        finally (when fblock
                  (analyze-block
                   (assoc env :context :statement)
                   fblock))
        body (if finally (pop body) body)
        tail (peek body)
        cblock (when (and (seq? tail)
                          (= 'catch (first tail)))
                 (rest tail))
        name (first cblock)
        locals (:locals catchenv)
        mname (when name (munge name))
        locals (if name
                 (assoc locals name {:name mname})
                 locals)
        catch (when cblock
                (analyze-block (assoc catchenv :locals locals) (rest cblock)))
        body (if name (pop body) body)
        try (when body
              (analyze-block (if (or name finally) catchenv env) body))]
    (when name (assert (not (namespace name)) "Can't qualify symbol in catch"))
    {:env env :op :try* :form form
     :try try
     :finally finally
     :name mname
     :catch catch
     :children [try {:name mname} catch finally]}))

(defmethod parse 'def
  [op env form name]
  (let [pfn (fn ([_ sym] {:sym sym})
              ([_ sym init] {:sym sym :init init})
              ([_ sym doc init] {:sym sym :doc doc :init init}))
        args (apply pfn form)
        sym (:sym args)]
    (assert (not (namespace sym)) "Can't def ns-qualified name")
    (let [name (munge (:name (resolve-var (dissoc env :locals) sym)))
          init-expr (when (contains? args :init) (disallowing-recur
                                                  (analyze (assoc env :context :expr) (:init args) sym)))
          export-as (when-let [export-val (-> sym meta :export)]
                      (if (= true export-val) name export-val))
          doc (or (:doc args) (-> sym meta :doc))]
      (swap! namespaces assoc-in [(-> env :ns :name) :defs sym] name)
      (merge {:env env :op :def :form form
              :name name :doc doc :init init-expr}
             (when init-expr {:children [init-expr]})
             (when export-as {:export export-as})))))

(defn- analyze-fn-method [env locals meth]
  (let [params (first meth)
        fields (-> params meta ::fields)
        variadic (boolean (some '#{&} params))
        params (remove '#{&} params)
        fixed-arity (count (if variadic (butlast params) params))
        body (next meth)
        gthis (and fields (gensym "this__"))
        locals (reduce (fn [m fld] (assoc m fld {:name (symbol (str gthis "." (munge fld)))})) locals fields)
        locals (reduce (fn [m name] (assoc m name {:name (munge name)})) locals params)
        recur-frame {:names (vec (map munge params)) :flag (atom nil)}
        block (binding [*recur-frames* (cons recur-frame *recur-frames*)]
                (analyze-block (assoc env :context :return :locals locals) body))]
    
    (merge {:env env :variadic variadic :params (map munge params) :max-fixed-arity fixed-arity :gthis gthis :recurs @(:flag recur-frame)} block)))

(defmethod parse 'fn*
  [op env [_ & args] name]
  (let [[name meths] (if (symbol? (first args))
                       [(first args) (next args)]
                       [name (seq args)])
        ;;turn (fn [] ...) into (fn ([]...))
        meths (if (vector? (first meths)) (list meths) meths)
        mname (when name (munge name))
        locals (:locals env)
        locals (if name (assoc locals name {:name mname}) locals)
        menv (if (> (count meths) 1) (assoc env :context :expr) env)
        methods (map #(analyze-fn-method menv locals %) meths)
        max-fixed-arity (apply max (map :max-fixed-arity methods))
        variadic (boolean (some :variadic methods))]
    ;;(assert (= 1 (count methods)) "Arity overloading not yet supported")
    ;;todo - validate unique arities, at most one variadic, variadic takes max required args
    {:env env :op :fn :name mname :methods methods :variadic variadic :recur-frames *recur-frames*
     :jsdoc [(when variadic "@param {...*} var_args")]
     :max-fixed-arity max-fixed-arity}))

(defmethod parse 'do
  [op env [_ & exprs] _]
  (merge {:env env :op :do} (analyze-block env exprs)))

(defn analyze-let
  [encl-env [_ bindings & exprs :as form] is-loop]
  (assert (and (vector? bindings) (even? (count bindings))) "bindings must be vector of even number of elements")
  (let [context (:context encl-env)
        [bes env]
        (disallowing-recur
         (loop [bes []
                env (assoc encl-env :context :expr)
                bindings (seq (partition 2 bindings))]
           (if-let [[name init] (first bindings)]
             (do
               (assert (not (or (namespace name) (.contains (str name) "."))) (str "Invalid local name: " name))
               (let [init-expr (analyze env init)
                     be {:name (gensym (str (munge name) "__")) :init init-expr}]
                 (recur (conj bes be)
                        (assoc-in env [:locals name] be)
                        (next bindings))))
             [bes env])))
        recur-frame (when is-loop {:names (vec (map :name bes)) :flag (atom nil)})
        {:keys [statements ret children]}
        (binding [*recur-frames* (if recur-frame (cons recur-frame *recur-frames*) *recur-frames*)]
          (analyze-block (assoc env :context (if (= :expr context) :return context)) exprs))]
    {:env encl-env :op :let :loop is-loop
     :bindings bes :statements statements :ret ret :form form :children (into [children] (map :init bes))}))

(defmethod parse 'let*
  [op encl-env form _]
  (analyze-let encl-env form false))

(defmethod parse 'loop*
  [op encl-env form _]
  (analyze-let encl-env form true))

(defmethod parse 'recur
  [op env [_ & exprs] _]
  (let [context (:context env)
        frame (first *recur-frames*)]
    (assert frame "Can't recur here")
    (assert (= (count exprs) (count (:names frame))) "recur argument count mismatch")
    (reset! (:flag frame) true)
    (assoc {:env env :op :recur}
      :frame frame
      :exprs (disallowing-recur (vec (map #(analyze (assoc env :context :expr) %) exprs))))))

(defmethod parse 'quote
  [_ env [_ x] _]
  {:op :constant :env env :form x})

(defmethod parse 'new
  [_ env [_ ctor & args] _]
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         ctorexpr (analyze enve ctor)
         argexprs (vec (map #(analyze enve %) args))]
     {:env env :op :new :ctor ctorexpr :args argexprs :children (conj argexprs ctorexpr)})))

(defmethod parse 'set!
  [_ env [_ target val] _]
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         targetexpr (if (symbol? target)
                      (do
                        (assert (nil? (-> env :locals target))
                                "Can't set! local var")
                        (analyze-symbol enve target))
                      (when (seq? target)
                        (let [targetexpr (analyze-seq enve target nil)]
                          (when (:field targetexpr)
                            targetexpr))))
         valexpr (analyze enve val)]
     (assert targetexpr "set! target must be a field or a symbol naming a var")
     {:env env :op :set! :target targetexpr :val valexpr :children [targetexpr valexpr]})))

(defmethod parse 'ns
  [_ env [_ name & args] _]
  (let [excludes
        (reduce (fn [s [k exclude xs]]
                  (if (= k :refer-clojure)
                    (do
                      (assert (= exclude :exclude) "Only [:refer-clojure :exclude [names]] form supported")
                      (into s xs))
                    s))
                #{} args)
        {uses :use requires :require uses-macros :use-macros requires-macros :require-macros :as params}
        (reduce (fn [m [k & libs]]
                  (assert (#{:use :use-macros :require :require-macros} k)
                          "Only :refer-clojure, :require, :require-macros, :use and :use-macros libspecs supported")
                  (assoc m k (into {}
                                   (mapcat (fn [[lib kw expr]]
                                             (case k
                                               (:require :require-macros)
                                               (do (assert (and expr (= :as kw))
                                                           "Only (:require [lib.ns :as alias]*) form of :require / :require-macros is supported")
                                                   [[expr lib]])
                                               (:use :use-macros)
                                               (do (assert (and expr (= :only kw))
                                                           "Only (:use [lib.ns :only [names]]*) form of :use / :use-macros is supported")
                                                   (map vector expr (repeat lib)))))
                                           libs))))
                {} (remove (fn [[r]] (= r :refer-clojure)) args))]
    (set! *cljs-ns* name)
    (require 'cljs.core)
    (doseq [nsym (concat (vals requires-macros) (vals uses-macros))]
      (clojure.core/require nsym))
    (swap! namespaces #(-> %
                           (assoc-in [name :name] name)
                           (assoc-in [name :excludes] excludes)
                           (assoc-in [name :uses] uses)
                           (assoc-in [name :requires] requires)
                           (assoc-in [name :uses-macros] uses-macros)
                           (assoc-in [name :requires-macros]
                                     (into {} (map (fn [[alias nsym]]
                                                     [alias (find-ns nsym)])
                                                   requires-macros)))))
    {:env env :op :ns :name name :uses uses :requires requires
     :uses-macros uses-macros :requires-macros requires-macros :excludes excludes}))

(defmethod parse 'deftype*
  [_ env [_ tsym fields] _]
  (let [t (munge (:name (resolve-var (dissoc env :locals) tsym)))]
    (swap! namespaces assoc-in [(-> env :ns :name) :defs tsym] t)
    {:env env :op :deftype* :t t :fields fields}))

(defmethod parse '.
  [_ env [_ target & member+] _]
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         targetexpr (analyze enve target)
         children [enve]]
     (if (and (symbol? (first member+)) (nil? (next member+))) ;;(. target field)
       {:env env :op :dot :target targetexpr :field (munge (first member+)) :children children}
       (let [[method args]
             (if (symbol? (first member+))
               [(first member+) (next member+)]
               [(ffirst member+) (nfirst member+)])
             argexprs (map #(analyze enve %) args)]
         {:env env :op :dot :target targetexpr :method (munge method) :args argexprs :children (into children argexprs)})))))

(defmethod parse 'js*
  [op env [_ form & args] _]
  (assert (string? form))
  (if args
    (disallowing-recur
     (let [seg (fn seg [^String s]
                 (let [idx (.indexOf s "~{")]
                   (if (= -1 idx)
                     (list s)
                     (let [end (.indexOf s "}" idx)]
                       (cons (subs s 0 idx) (seg (subs s (inc end))))))))
           enve (assoc env :context :expr)
           argexprs (vec (map #(analyze enve %) args))]
       {:env env :op :js :segs (seg form) :args argexprs :children argexprs}))
    (let [interp (fn interp [^String s]
                   (let [idx (.indexOf s "~{")]
                     (if (= -1 idx)
                       (list s)
                       (let [end (.indexOf s "}" idx)
                             inner (:name (resolve-existing-var env (symbol (subs s (+ 2 idx) end))))]
                         (cons (subs s 0 idx) (cons inner (interp (subs s (inc end)))))))))]
      {:env env :op :js :code (apply str (interp form))})))

(defn parse-invoke
  [env [f & args]]
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         fexpr (analyze enve f)
         argexprs (vec (map #(analyze enve %) args))]
     {:env env :op :invoke :f fexpr :args argexprs :children (conj argexprs fexpr)})))

(defn analyze-symbol
  "Finds the var associated with sym"
  [env sym]
  (let [ret {:env env :form sym}
        lb (-> env :locals sym)]
    (if lb
      (assoc ret :op :var :info lb)
      (assoc ret :op :var :info (resolve-existing-var env sym)))))

(defn get-expander [sym env]
  (let [mvar
        (when-not (-> env :locals sym)  ;locals hide macros
          (if-let [nstr (namespace sym)]
            (when-let [ns (cond
                           (= "clojure.core" nstr) (find-ns 'cljs.core)
                           (.contains nstr ".") (find-ns (symbol nstr))
                           :else
                           (-> env :ns :requires-macros (get (symbol nstr))))]
              (.findInternedVar ^clojure.lang.Namespace ns (symbol (name sym))))
            (if-let [nsym (-> env :ns :uses-macros sym)]
              (.findInternedVar ^clojure.lang.Namespace (find-ns nsym) sym)
              (.findInternedVar ^clojure.lang.Namespace (find-ns 'cljs.core) sym))))]
    (when (and mvar (.isMacro ^clojure.lang.Var mvar))
      @mvar)))

(defn macroexpand-1 [env form]
  (let [op (first form)]
    (if (specials op)
      form
      (if-let [mac (and (symbol? op) (get-expander op env))]
        (apply mac form env (rest form))
        (if (symbol? op)
          (let [opname (str op)]
            (cond
             (= (first opname) \.) (let [[target & args] (next form)]
                                     (list* '. target (symbol (subs opname 1)) args))
             (= (last opname) \.) (list* 'new (symbol (subs opname 0 (dec (count opname)))) (next form))
             :else form))
          form)))))

(defn analyze-seq
  [env form name]
  (let [env (assoc env :line (-> form meta :line))]
    (let [op (first form)]
      (assert (not (nil? op)) "Can't call nil")
      (let [mform (macroexpand-1 env form)]
        (if (identical? form mform)
          (if (specials op)
            (parse op env form name)
            (parse-invoke env form))
          (analyze env mform name))))))

(declare analyze-wrap-meta)

(defn analyze-map
  [env form name]
  (let [expr-env (assoc env :context :expr)
        simple-keys? (every? #(or (string? %) (keyword? %))
                             (keys form))
        ks (disallowing-recur (vec (map #(analyze expr-env % name) (keys form))))
        vs (disallowing-recur (vec (map #(analyze expr-env % name) (vals form))))]
    (analyze-wrap-meta {:op :map :env env :form form :children (vec (concat ks vs))
                        :keys ks :vals vs :simple-keys? simple-keys?}
                       name)))

(defn analyze-vector
  [env form name]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (vec (map #(analyze expr-env % name) form)))]
    (analyze-wrap-meta {:op :vector :env env :form form :children items} name)))

(defn analyze-set
  [env form name]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (vec (map #(analyze expr-env % name) form)))]
    (analyze-wrap-meta {:op :set :env env :form form :children items} name)))

(defn analyze-wrap-meta [expr name]
  (let [form (:form expr)]
    (if (meta form)
      (let [env (:env expr) ; take on expr's context ourselves
            expr (assoc-in expr [:env :context] :expr) ; change expr to :expr
            meta-expr (analyze-map (:env expr) (meta form) name)]
        {:op :meta :env env :form form :children [meta-expr expr]
         :meta meta-expr :expr expr})
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
     (let [form (if (instance? clojure.lang.LazySeq form)
                  (or (seq form) ())
                  form)]
       (cond
        (symbol? form) (analyze-symbol env form)
        (and (seq? form) (seq form)) (analyze-seq env form name)
        (map? form) (analyze-map env form name)
        (vector? form) (analyze-vector env form name)
        (set? form) (analyze-set env form name)
        :else {:op :constant :env env :form form}))))

(defn analyze-file
  [f]
  (binding [*cljs-ns* 'cljs.user]
    (let [res (if (= \/ (first f)) f (io/resource f))]
      (assert res (str "Can't find " f " in classpath"))
      (with-open [r (io/reader res)]
        (let [env {:ns (@namespaces *cljs-ns*) :context :statement :locals {}}
              pbr (clojure.lang.LineNumberingPushbackReader. r)
              eof (Object.)]
          (loop [r (read pbr false eof false)]
            (let [env (assoc env :ns (@namespaces *cljs-ns*))]
              (when-not (identical? eof r)
                (analyze env r)
                (recur (read pbr false eof false))))))))))

(defn forms-seq
  "Seq of forms in a Clojure or ClojureScript file."
  ([f]
     (forms-seq f (java.io.PushbackReader. (io/reader f))))
  ([f ^java.io.PushbackReader rdr]
     (if-let [form (read rdr nil nil)]
       (lazy-seq (cons form (forms-seq f rdr)))
       (.close rdr))))

(defn rename-to-js
  "Change the file extension from .cljs to .js. Takes a File or a
  String. Always returns a String."
  [file-str]
  (clojure.string/replace file-str #".cljs$" ".js"))

(defn mkdirs
  "Create all parent directories for the passed file."
  [^java.io.File f]
  (.mkdirs (.getParentFile (.getCanonicalFile f))))

(defmacro with-core-cljs
  "Ensure that core.cljs has been loaded."
  [& body]
  `(do (when-not (:defs (get @namespaces 'cljs.core))
         (analyze-file "cljs/core.cljs"))
       ~@body))

(defn compile-file* [src dest]
  (with-core-cljs
    (with-open [out ^java.io.Writer (io/make-writer dest {})]
      (binding [*out* out
                *cljs-ns* 'cljs.user]
        (loop [forms (forms-seq src)
               ns-name nil
               deps nil]
          (if (seq forms)
            (let [env {:ns (@namespaces *cljs-ns*) :context :statement :locals {}}
                  ast (analyze env (first forms))]
              (do (emit ast)
                  (if (= (:op ast) :ns)
                    (recur (rest forms) (:name ast) (merge (:uses ast) (:requires ast)))
                    (recur (rest forms) ns-name deps))))
            {:ns (or ns-name 'cljs.user)
             :provides [ns-name]
             :requires (if (= ns-name 'cljs.core) (set (vals deps)) (conj (set (vals deps)) 'cljs.core))
             :file dest}))))))

(defn requires-compilation?
  "Return true if the src file requires compilation."
  [^java.io.File src ^java.io.File dest]
  (or (not (.exists dest))
      (> (.lastModified src) (.lastModified dest))))

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
       (compile-file src dest)))
  ([src dest]
     (let [src-file (io/file src)
           dest-file (io/file dest)]
       (if (.exists src-file)
         (if (requires-compilation? src-file dest-file)
           (do (mkdirs dest-file)
               (compile-file* src-file dest-file))
           {:file dest-file})
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

(defn compile-root
  "Looks recursively in src-dir for .cljs files and compiles them to
   .js files. If target-dir is provided, output will go into this
   directory mirroring the source directory structure. Returns a list
   of maps containing information about each file which was compiled
   in dependency order."
  ([src-dir]
     (compile-root src-dir "out"))
  ([src-dir target-dir]
     (let [src-dir-file (io/file src-dir)
           cljs-files (filter #(.endsWith (.getName ^java.io.File %) ".cljs") (file-seq src-dir-file))]
       (loop [cljs-files cljs-files
              output-files []]
         (if (seq cljs-files)
           (let [cljs-file (first cljs-files)
                 output-file ^java.io.File (to-target-file src-dir-file target-dir cljs-file)
                 ns-info (compile-file cljs-file output-file)]
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
  `(emit (analyze {:ns (@namespaces 'cljs.user) :context :statement :locals {}} '~form)))

(defn jseval [form]
  (let [js (emits (analyze {:ns (@namespaces 'cljs.user) :context :expr :locals {}}
                           form))]
    ;;(prn js)
    (.eval jse (str "print(" js ")"))))

(defn jscapture [form]
  "just grabs the js, doesn't print it"
  (emits (analyze {:ns (@namespaces 'cljs.user) :context :expr :locals {}} form)))

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
