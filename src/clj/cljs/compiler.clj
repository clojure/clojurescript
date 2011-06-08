;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(set! *warn-on-reflection* true)

(ns cljs.compiler
  (:refer-clojure :exclude [munge]))

(def js-reserved #{"new" "debugger" "enum" "default" "private" "finally" "in" "import" "package" "with" "throw"
                   "continue" "var" "for" "public" "do" "delete" "instanceof" "yield" "static" "protected" "return"
                   "case" "implements" "typeof" "while" "void" "switch" "export" "class" "function" "extends" "else"
                   "interface" "try" "let" "catch" "super" "if" "this" "break"})

(defonce namespaces (atom '{cljs.core {:name cljs.core}
                            cljs.user {:name cljs.user}}))

(defn munge [s]
  (let [ms (clojure.lang.Compiler/munge (str s))
        ms (if (js-reserved ms) (str ms "$") ms)]
    (if (symbol? s)
      (symbol ms)
      ms)))

;;todo - move to core.cljs, using js
(def bootjs "
//goog.provide should do this for us
cljs = {}
cljs.core = {}
cljs.user = {}
cljs.core.truth_ = function(x){return x != null && x !== false;}
cljs.core.fnOf_ = function(f){return (f instanceof Function?f:f.cljs$core$Fn$invoke);}")

(defn resolve-var [env sym]
  (let [s (str sym)
        lb (-> env :locals sym)
        nm 
        (cond
         lb (:name lb)
         
         ;;todo - resolve ns aliases when we have them
         (namespace sym)
         (symbol (str (namespace sym) "." (munge (name sym))))

         (.contains s ".")
         (munge (let [idx (.indexOf s ".")
                      prefix (symbol (subs s 0 idx))
                      suffix (subs s idx)
                      lb (-> env :locals prefix)]
                  (if lb
                    (symbol (str (:name lb) suffix))
                    sym)))

         :else
         (munge (symbol (str (or (-> env :ns :name) 'cljs.user) "." (name sym)))))]
    {:name nm}))

(defn- comma-sep [xs]
  (apply str (interpose "," xs)))

(defmulti emit-constant class)
(defmethod emit-constant nil [x] (print "null"))
(defmethod emit-constant Long [x] (print x))
(defmethod emit-constant Double [x] (print x))
(defmethod emit-constant String [x] (pr x))
(defmethod emit-constant Boolean [x] (print (if x "true" "false")))

(defmulti emit :op)

(defn emits [expr]
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

(defmethod emit :constant
  [{:keys [form env]}]
  (emit-wrap env (emit-constant form)))

(defmethod emit :if
  [{:keys [test then else env]}]
  (let [context (:context env)]
    (if (= :expr context)
      (print (str "(cljs.core.truth_(" (emits test) ")?" (emits then) ":" (emits else) ")"))
      (print (str "if(cljs.core.truth_(" (emits test) "))\n{" (emits then) "} else\n{" (emits else) "}\n")))))

(defmethod emit :def
  [{:keys [name init env]}]
  (when init
    (print name)
    (print (str " = " (emits init)))
    (when-not (= :expr (:context env)) (print ";\n"))))

(defmethod emit :fn
  [{:keys [name variadic params statements ret env recurs]}]
  ;;fn statements get erased, serve no purpose and can pollute scope if named
  (when-not (= :statement (:context env))
    (emit-wrap env
               (print (str "(function " name "(" (comma-sep params) "){\n"))
               (when variadic
                 (println (str (last params) " = Array.prototype.slice.call(arguments, " (dec (count params)) ");")))
               (when recurs (print "while(true){\n"))
               (emit-block :return statements ret)
               (when recurs (print "break;\n}\n"))
               (print "})"))))

(defmethod emit :do
  [{:keys [statements ret env]}]
  (let [context (:context env)]
    (when (and statements (= :expr context)) (print "(function (){"))
    ;(when statements (print "{\n"))
    (emit-block context statements ret)
    ;(when statements (print "}"))
    (when (and statements (= :expr context)) (print "})()"))))

(defmethod emit :let
  [{:keys [bindings statements ret env loop]}]
  (let [context (:context env)
        bs (map (fn [{:keys [name init]}]
                  (str "var " name " = " (emits init) ";\n"))
                bindings)]
    (when (= :expr context) (print "(function ()"))
    (print (str (apply str bs) "\n"))
    (when loop (print "while(true){\n"))
    (emit-block (if (= :expr context) :return context) statements ret)
    (when loop (print "break;\n}\n"))
    ;(print "}")
    (when (= :expr context) (print ")()"))))

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
             (print (str "cljs.core.fnOf_(" (emits f) ")("
                         (comma-sep (map emits args))
                         ")"))))

(defmethod emit :new
  [{:keys [ctor args env]}]
  (emit-wrap env
             (print (str "new " (emits ctor) "("
                         (comma-sep (map emits args))
                         ")"))))

(defmethod emit :set!
  [{:keys [target val env]}]
  (emit-wrap env (print (str (emits target) " = "(emits val)))))

(defmethod emit :ns
  [{:keys [name requires requires-macros env]}]
  (println (str "//goog.provide('" name "');"))
  (when-not (= name 'cljs.core)
    (println (str "//goog.require('cljs.core');")))
  (doseq [lib (vals requires)]
    (println (str "//goog.require('" lib "');"))))

(defmethod emit :deftype*
  [{:keys [t fields]}]
  (let [fields (map munge fields)]
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
  [{:keys [code]}]
  (print code))

(declare analyze analyze-symbol)

(def specials '#{if def fn* do let* loop* recur new set! ns deftype* . js* &})

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

(defmethod parse 'fn*
  [op env [_ & args] name]
  (let [name (if (symbol? (first args))
               (first args)
               name)
        meths (if (symbol? (first args))
               (next args)
               args)
        ;;turn (fn [] ...) into (fn ([]...))
        meths (if (vector? (first meths)) (list meths) meths)
        ;;todo, merge meths, switch on arguments.length
        ;;no, doing that in fn, fn* is single arity
        meth (first meths)
        params (first meth)
        fields (-> params meta ::fields)
        variadic (some '#{&} params)
        params (remove '#{&} params)
        body (next meth)
        mname (when name (munge name))
        locals (:locals env)
        locals (if name (assoc locals name {:name mname}) locals)
        locals (reduce (fn [m fld] (assoc m fld {:name (symbol (str "this." (munge fld)))})) locals fields)
        locals (reduce (fn [m name] (assoc m name {:name (munge name)})) locals params)
        recur-frame {:names (vec (map munge params)) :flag (atom nil)}
        block (binding [*recur-frame* recur-frame]
                (analyze-block (assoc env :context :return :locals locals) body))]
    (assert (= 1 (count meths)) "Arity overloading not supported here, use fn")
    (merge {:env env :op :fn :name mname :variadic variadic
            :meths meths :params (map munge params) :recurs @(:flag recur-frame)} block)))

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
    ;;(require 'cljs.core)
    (doseq [nsym (vals requires-macros)]
      (require nsym))
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
  [op env [_ form] _]
  (assert (string? form))
  (let [interp (fn interp [^String s]
                 (let [idx (.indexOf s "~{")]
                   (if (= -1 idx)
                     (list s)
                     (let [end (.indexOf s "}" idx)
                           inner (:name (resolve-var env (symbol (subs s (+ 2 idx) end))))]
                       (cons (subs s 0 idx) (cons inner (interp (subs s (inc end)))))))))]
    {:env env :op :js :code (apply str (interp form))}))

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
            (when-let [nsym (if (= "clojure.core" nstr)
                            'cljs.core
                            (-> env :ns :requires-macros (symbol nstr)))]
              (.findInternedVar ^clojure.lang.Namespace (find-ns nsym) (symbol (name sym))))
            (.findInternedVar ^clojure.lang.Namespace (find-ns 'cljs.core) sym)))]
    (when (and mvar (.isMacro mvar))
      @mvar)))

(defn analyze-seq
  [env form name]
  (let [op (first form)]
    (assert (not (nil? op)) "Can't call nil")
    (if (specials op)
      (parse op env form name)
      (if-let [mac (and (symbol? op) (get-expander op env))]
        (analyze env (apply mac form env (rest form)))
        (parse-invoke env form)))))

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
        :else {:op :constant :env env :form form}))))

(comment
(in-ns 'cljs.compiler)
(import '[javax.script ScriptEngineManager])
(def jse (-> (ScriptEngineManager.) (.getEngineByName "JavaScript")))
(.eval jse bootjs)
(def envx {:ns (@namespaces 'cljs.user) :context :return :locals '{ethel {:name ethel__123 :init nil}}})
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

(js (def foo? (fn* ^{::fields [a? b c]} [x y] (if true a? (recur 1 x)))))
(jseval '(def foo (fn* ^{::fields [a b c]} [x y] (if true a (recur 1 x)))))
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
(js (defn foo [this] this))

(js (defn foo [a b c & ys] ys))
(js ((fn [x & ys] ys) 1 2 3 4))
(jseval '((fn [x & ys] ys) 1 2 3 4))
(js (cljs.core/deftype Foo [a b c] Fred (fred [x] a)  (ethel [x] c) Ethel (foo [] d)))
(jseval '(cljs.core/deftype Foo [a b c] Fred (fred [x] a)  (ethel [x] c) Ethel (foo [] d)))
(js (do
           (defprotocol P-roto (foo? [this]))
           (deftype T-ype [a] P-roto (foo? [this] a))
           (foo? (new T-ype 42))))
(jseval '(do
           (defprotocol P-roto (foo? [this]))
           (deftype T-ype [a] P-roto (foo? [this] a))
           (foo? (new T-ype 42))))

(js (def x (fn foo [x] (let [x 42] (js* "~{x}['foobar']")))))

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