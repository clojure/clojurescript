;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(set! *warn-on-reflection* true)

(ns cljs.compiler
  (:refer-clojure :exclude [munge load-file loaded-libs macroexpand-1])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(declare resolve-var)
(require 'cljs.core)

(def js-reserved #{"new" "debugger" "enum" "default" "private" "finally" "in" "import" "package" "with" "throw"
                   "continue" "var" "for" "public" "do" "delete" "instanceof" "yield" "static" "protected" "return"
                   "case" "implements" "typeof" "while" "void" "switch" "export" "class" "function" "extends" "else"
                   "interface" "try" "let" "catch" "super" "if" "this" "break" "boolean"})

(defonce namespaces (atom '{cljs.core {:name cljs.core}
                            cljs.user {:name cljs.user}}))

(def ^:dynamic *cljs-ns* 'cljs.user)
(def ^:dynamic *cljs-verbose* false)

(defn munge [s]
  (let [ms (clojure.lang.Compiler/munge (str s))
        ms (if (js-reserved ms) (str ms "$") ms)]
    (if (symbol? s)
      (symbol ms)
      ms)))

;;todo - move to core.cljs, using js
(def ^String bootjs "
//goog.provide should do this for us
//cljs = {}
//cljs.lang = {}
//cljs.user = {}
//goog.provide('cljs.core');
//goog.provide('cljs.user');
//cljs.lang.truth_ = function(x){return x != null && x !== false;}
//cljs.lang.fnOf_ = function(f){return (f instanceof Function?f:f.cljs$core$Fn$invoke);}
//cljs.lang.original_goog_require = goog.require;
goog.require = function(rule){Packages.clojure.lang.RT[\"var\"](\"cljs.compiler\",\"goog-require\").invoke(goog.global.cljs_javascript_engine, rule);}
")

(defn resolve-ns-alias [env name]
  (let [sym (symbol name)]
    (get (:requires (:ns env)) sym sym)))

(defn resolve-var [env sym]
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
                         (if (get (:defs (@namespaces 'cljs.core)) sym)
                           'cljs.core
                           (-> env :ns :name))
                         "." (munge (name sym))))))]
    {:name nm}))

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
  (emit-wrap env (print (:name info))))

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

(defmethod emit :def
  [{:keys [name init env]}]
  (when init
    (print name)
    (print (str " = " (emits init)))
    (when-not (= :expr (:context env)) (print ";\n"))))

(defn emit-fn-method
  [{:keys [gthis name variadic params statements ret env recurs]}]
  (emit-wrap env
             (print (str "(function " name "(" (comma-sep params) "){\n"))
             (when gthis
               (println (str "var " gthis " = this;")))
             (when variadic
               (println (str (last params) " = cljs.core.array_seq(Array.prototype.slice.call(arguments, " (dec (count params)) "),0);"))
               #_(println (str (last params) " = Array.prototype.slice.call(arguments, " (dec (count params)) ");")))
             (when recurs (print "while(true){\n"))
             (emit-block :return statements ret)
             (when recurs (print "break;\n}\n"))
             (print "})")))

(defmethod emit :fn
  [{:keys [name env methods max-fixed-arity variadic]}]
  ;;fn statements get erased, serve no purpose and can pollute scope if named
  (when-not (= :statement (:context env))
    (if (= 1 (count methods))
      (emit-fn-method (assoc (first methods) :name name))
      (let [name (or name (gensym))
            maxparams (apply max-key count (map :params methods))
            mmap (zipmap (repeatedly #(gensym (str name  "__"))) methods)
            ms (sort-by #(-> % second :params count) (seq mmap))]
        (println "(function() {")
        (println (str "var " name " = null;"))
        (doseq [[n meth] ms]
          (println (str "var " n " = " (with-out-str (emit-fn-method meth)) ";")))
        (println (str name " = function(" (comma-sep maxparams) "){"))
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
          (println (str name ".cljs$lang$maxFixedArity = " max-fixed-arity)))
        (println (str "return " name ";"))
        (println "})()")))))

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
          (print "finally {")
          (let [{:keys [statements ret]} finally]
            (emit-block subcontext statements ret))
          (print "}"))
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
             (print (str "cljs.core.fn_of_(" (emits f) ")("
                         (comma-sep (map emits args))
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
  [{:keys [name requires requires-macros env]}]
  (println (str "goog.provide('" name "');"))
  (when-not (= name 'cljs.core)
    (println (str "goog.require('cljs.core');")))
  (doseq [lib (vals requires)]
    (println (str "goog.require('" lib "');"))))

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

(declare analyze analyze-symbol)

(def specials '#{if def fn* do let* loop* throw try* recur new set! ns deftype* . js* & quote})

(def ^:dynamic *recur-frame* nil)

(defmacro disallowing-recur [& body]
  `(binding [*recur-frame* nil] ~@body))

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
                                                  (analyze (assoc env :context :expr) (:init args) sym)))]
      (swap! namespaces assoc-in [(-> env :ns :name) :defs sym] name)
      (merge {:env env :op :def :form form
              :name name :doc (:doc args) :init init-expr}
             (when init-expr {:children [init-expr]})))))

(defn- analyze-fn-method [env locals meth]
  (let [params (first meth)
        fields (-> params meta ::fields)
        variadic (some '#{&} params)
        params (remove '#{&} params)
        fixed-arity (count (if variadic (butlast params) params))
        body (next meth)
        gthis (and fields (gensym "this__"))
        locals (reduce (fn [m fld] (assoc m fld {:name (symbol (str gthis "." (munge fld)))})) locals fields)
        locals (reduce (fn [m name] (assoc m name {:name (munge name)})) locals params)
        recur-frame {:names (vec (map munge params)) :flag (atom nil)}
        block (binding [*recur-frame* recur-frame]
                (analyze-block (assoc env :context :return :locals locals) body))]
    (merge {:env env :variadic variadic :params (map munge params) :fixed-arity fixed-arity :gthis gthis :recurs @(:flag recur-frame)} block)))

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
        max-fixed-arity (apply max (map :fixed-arity methods))]
    ;;(assert (= 1 (count methods)) "Arity overloading not yet supported")
    ;;todo - validate unique arities, at most one variadic, variadic takes max required args
    {:env env :op :fn :name mname :methods methods :variadic (some :variadic methods) :max-fixed-arity max-fixed-arity}))

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
        (binding [*recur-frame* (or recur-frame *recur-frame*)]
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
  (let [context (:context env)]
    (assert *recur-frame* "Can't recur here")
    (assert (= (count exprs) (count (:names *recur-frame*))) "recur argument count mismatch")
    (reset! (:flag *recur-frame*) true)
    (assoc {:env env :op :recur}
      :frame *recur-frame*
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
  (assert (symbol? target) "set! target must be a symbol naming var")
  (assert (nil? (-> env :locals target)) "Can't set! local var")
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         targetexpr (analyze-symbol enve target)
         valexpr (analyze enve val)]
     {:env env :op :set! :target targetexpr :val valexpr :children [targetexpr valexpr]})))

(defmethod parse 'ns
  [_ env [_ name & args] _]
  (let [{requires :require requires-macros :require-macros :as params}
        (reduce (fn [m [k & libs]]
                  (assoc m k (into {}
                                   (map (fn [[lib as alias]]
                                          (assert (and alias (= :as as)) "Only [lib.ns :as alias] form supported")
                                          [alias lib])
                                        libs))))
                {} args)]
    (set! *cljs-ns* name)
    (require 'cljs.core)
    (doseq [nsym (vals requires-macros)]
      (clojure.core/require nsym))
    (swap! namespaces #(-> %
                           (assoc-in [name :name] name)
                           (assoc-in [name :requires] requires)
                           (assoc-in [name :requires-macros]
                                     (into {} (map (fn [[alias nsym]]
                                                     [alias (find-ns nsym)])
                                                   requires-macros)))))
    {:env env :op :ns :name name :requires requires :requires-macros requires-macros}))

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
                             inner (:name (resolve-var env (symbol (subs s (+ 2 idx) end))))]
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
      (assoc ret :op :var :info (resolve-var env sym)))))

(defn get-expander [sym env]
  (let [mvar
        (when-not (-> env :locals sym)  ;locals hide macros
          (if-let [nstr (namespace sym)]
            (when-let [nsym (cond
                             (= "clojure.core" nstr) 'cljs.core
                             (.contains nstr ".") (symbol nstr)
                             :else
                             (-> env :ns :requires-macros (get (symbol nstr))))]
              (and (find-ns nsym) (.findInternedVar ^clojure.lang.Namespace (find-ns nsym) (symbol (name sym)))))
            (.findInternedVar ^clojure.lang.Namespace (find-ns 'cljs.core) sym)))]
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
  (let [op (first form)]
    (assert (not (nil? op)) "Can't call nil")
    (let [mform (macroexpand-1 env form)]
      (if (identical? form mform)
        (if (specials op)
          (parse op env form name)
          (parse-invoke env form))
        (analyze env mform name)))))

(declare analyze-wrap-meta)

(defn analyze-map
  [env form name]
  (let [expr-env (assoc env :context :expr)
        simple-keys? (every? #(or (string? %) (keyword? %) (symbol? %))
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
        :else {:op :constant :env env :form form}))))

(defn eval1
  [repl-env env form]
  (try
    (let [ast (analyze env form)
          js (emits ast)
          jse ^javax.script.ScriptEngine (:jse repl-env)]
      (try
        (when *cljs-verbose*
            (print js))
        (let [filename (.get jse javax.script.ScriptEngine/FILENAME)
              linenum (or (:line (meta form)) Integer/MIN_VALUE)
              ctx (sun.org.mozilla.javascript.internal.Context/enter)]
          (try
            (.evaluateString ctx (:global repl-env) js filename linenum nil)
          (finally
            (sun.org.mozilla.javascript.internal.Context/exit))))
        (catch Throwable ex
          ;;we eat ns errors because we know goog.provide() will throw when reloaded
          ;;TODO - file bug with google, this is bs error
          ;;this is what you get when you try to 'teach new developers' via errors (goog/base.js 104)
          (when-not (and (seq? form) (= 'ns (first form)))
            (prn "Error evaluating:" form :as js)
            (.printStackTrace ex)
            #_(println (str ex))))))
    (catch Throwable ex
      (.printStackTrace ex)
      (println (str ex)))))

(defn load-stream [repl-env stream]
  (with-open [r (io/reader stream)]
    (let [env {:ns (@namespaces *cljs-ns*) :context :statement :locals {}}
          pbr (clojure.lang.LineNumberingPushbackReader. r)
          eof (Object.)]
      (loop [r (read pbr false eof false)]
        (let [env (assoc env :ns (@namespaces *cljs-ns*))]
          (when-not (identical? eof r)
            (eval1 repl-env env r)
            (recur (read pbr false eof false))))))))

(defn load-file
  [repl-env f]
  (binding [*cljs-ns* 'cljs.user]
    (let [res (if (= \/ (first f)) f (io/resource f))]
      (assert res (str "Can't find " f " in classpath"))
      (.put ^javax.script.ScriptEngine (:jse repl-env)
            javax.script.ScriptEngine/FILENAME f)
      (load-stream repl-env res))))

(def loaded-libs (atom #{}))

(defn goog-require [repl-env rule]
  (when-not (contains? @loaded-libs rule)
    (let [jse ^javax.script.ScriptEngine (:jse repl-env)
          path (string/replace (munge rule) \. java.io.File/separatorChar)
          cljs-path (str path ".cljs")
          js-path (str "goog/" (.eval jse (str "goog.dependencies_.nameToPath['" rule "']")))]
      (if-let [res (io/resource cljs-path)]
        (binding [*cljs-ns* 'cljs.user]
          (load-stream repl-env res))
        (if-let [res (io/resource js-path)]
          (.eval jse (io/reader res))
          (throw (Exception. (str "Cannot find " cljs-path " or " js-path " in classpath")))))
      (swap! loaded-libs conj rule))))

(defn repl-env
  "Returns a fresh JS environment, suitable for passing to repl.
  Hang on to return for use across repl calls."
  []
  (let [jse (-> (javax.script.ScriptEngineManager.) (.getEngineByName "JavaScript"))
        base (io/resource "goog/base.js")
        deps (io/resource "goog/deps.js")
        new-repl-env {:jse jse :global (.eval jse "this")}]
    (assert base "Can't find goog/base.js in classpath")
    (assert deps "Can't find goog/deps.js in classpath")
    (.put jse javax.script.ScriptEngine/FILENAME "goog/base.js")
    (.put jse "cljs_javascript_engine" new-repl-env)
    (with-open [r (io/reader base)]
      (.eval jse r))
    (.eval jse bootjs)
    ;; Load deps.js line-by-line to avoid 64K method limit
    (doseq [^String line (line-seq (io/reader deps))]
      (.eval jse line))
    new-repl-env))

(defn repl
  "Note - repl will reload core.cljs every time, even if supplied old repl-env"
  [repl-env & {:keys [verbose]}]
  (prn "Type: " :cljs/quit " to quit")
  (binding [*cljs-ns* 'cljs.user
            *cljs-verbose* verbose]
    (let [env {:context :statement :locals {}}]
      (load-file repl-env "cljs/core.cljs")
      (eval1 repl-env (assoc env :ns (@namespaces *cljs-ns*))
             '(ns cljs.user))
      (.put ^javax.script.ScriptEngine (:jse repl-env)
            javax.script.ScriptEngine/FILENAME "<cljs repl>")
      (loop []
        (print (str "ClojureScript:" *cljs-ns* "> "))
        (flush)
        (let [form (read)]
          (cond
           (= form :cljs/quit) :quit
           
           (and (seq? form) (= (first form) 'in-ns))
           (do (set! *cljs-ns* (second (second form))) (newline) (recur))

           (and (seq? form) ('#{load-file clojure.core/load-file} (first form)))
           (do (load-file repl-env (second form)) (newline) (recur))
           
           :else
           (let [ret (eval1 repl-env
                            (assoc env :ns (@namespaces *cljs-ns*))
                            ;;form
                            (list 'cljs.core.prn form)
                            ;(list 'goog.global.print form)
                            )]
             ;(newline) (flush)
             ;;(prn (if (nil? ret) nil ret))
             (recur))))))))

(defn forms-seq
  "Seq of forms in a Clojure or ClojureScript file."
  ([f]
     (forms-seq f (java.io.PushbackReader. (io/reader f))))
  ([f ^java.io.PushbackReader rdr]
     (if-let [form (read rdr nil nil)]
       (lazy-seq (cons form (forms-seq f rdr)))
       (.close rdr))))

(defn compile-file
  "Compiles src to a file of the same name, but
   with a .js extension, in the src file's directory.

   With dest argument, write file to provided location."
  ([src]
     (let [dest (clojure.string/replace src #".cljs$" ".js")]
       (compile-file src dest)))
  ([src dest]
     (let [forms (forms-seq src)]
       (with-open [out ^java.io.Writer (io/make-writer (io/file dest) {})]
         (binding [*out* out
                   *cljs-ns* 'cljs.user]
           (doseq [form forms]
             (let [env {:ns (@namespaces *cljs-ns*) :context :statement :locals {}}]
               (emit (analyze env form)))))))))

(comment
  ;; flex compile-file
  (do
    (compile-file "/tmp/hello.cljs" "/tmp/something.js")
    (slurp "/tmp/hello.js")

    (compile-file "/tmp/somescript.cljs")
    (slurp "/tmp/somescript.js")))

(comment

;;the new way - use the REPL!!
(require '[cljs.compiler :as comp])
(def repl-env (comp/repl-env))
(comp/repl repl-env)
;having problems?, try verbose mode
(comp/repl repl-env :verbose true)

(test-stuff)
(+ 1 2 3)
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
  (newline))
)
