;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core
  (:refer-clojure :exclude [-> ->> .. amap and areduce alength aclone assert assert-args binding bound-fn case comment
                            cond condp declare definline definterface defmethod defmulti defn defn- defonce
                            defprotocol defrecord defstruct deftype delay destructure doseq dosync dotimes doto
                            extend-protocol extend-type fn for future gen-class gen-interface
                            if-let if-not import io! lazy-cat lazy-seq let letfn locking loop
                            memfn ns or proxy proxy-super pvalues refer-clojure reify sync time
                            when when-first when-let when-not while with-bindings with-in-str
                            with-loading-context with-local-vars with-open with-out-str with-precision with-redefs
                            satisfies? identical? true? false? number? nil? instance? symbol? keyword? string? str get
                            make-array vector list hash-map array-map hash-set

                            aget aset
                            + - * / < <= > >= == zero? pos? neg? inc dec max min mod
                            byte char short int long float double
                            unchecked-byte unchecked-char unchecked-short unchecked-int
                            unchecked-long unchecked-float unchecked-double
                            unchecked-add unchecked-add-int unchecked-dec unchecked-dec-int
                            unchecked-divide unchecked-divide-int unchecked-inc unchecked-inc-int
                            unchecked-multiply unchecked-multiply-int unchecked-negate unchecked-negate-int
                            unchecked-subtract unchecked-subtract-int unchecked-remainder-int
                            unsigned-bit-shift-right

                            bit-and bit-and-not bit-clear bit-flip bit-not bit-or bit-set
                            bit-test bit-shift-left bit-shift-right bit-xor defmacro

                            cond-> cond->> as-> some-> some->>

                            require use refer-clojure

                            if-some when-some test ns-publics ns-imports ns-interns
                            ns-unmap var vswap! macroexpand-1 macroexpand
                            some? resolve
                            #?@(:cljs [alias coercive-not coercive-not= coercive-= coercive-boolean
                                       truth_ js-arguments js-delete js-in js-debugger exists? divide js-mod
                                       unsafe-bit-and bit-shift-right-zero-fill mask bitpos caching-hash
                                       defcurried rfn specify! js-this this-as implements? array js-obj
                                       simple-benchmark gen-apply-to js-str es6-iterable load-file* undefined?
                                       specify copy-arguments goog-define js-comment js-inline-comment
                                       unsafe-cast require-macros use-macros gen-apply-to-simple unchecked-get unchecked-set])])
  #?(:cljs (:require-macros [cljs.core :as core]
                            [cljs.support :refer [assert-args]]))
  (:require clojure.walk
            clojure.set
            [clojure.string :as string]
            [cljs.compiler :as comp]
            [cljs.env :as env]
            #?(:clj [cljs.support :refer [assert-args]])
            #?(:cljs [cljs.core :as core])
            #?(:cljs [cljs.analyzer :as ana])))

#?(:clj (alias 'core 'clojure.core))
#?(:clj (alias 'ana 'cljs.analyzer))

#?(:clj
   (core/defmacro import-macros [ns [& vars]]
     (core/let [ns (find-ns ns)
                vars (map #(ns-resolve ns %) vars)
                syms (map
                       (core/fn [^clojure.lang.Var v]
                         (core/-> v .sym
                           (with-meta
                             (merge
                               {:macro true}
                               (update-in (select-keys (meta v) [:arglists :doc :file :line])
                                 [:arglists] (core/fn [arglists] `(quote ~arglists)))))))
                       vars)
                defs (map
                       (core/fn [sym var]
                         (core/let [{:keys [arglists doc file line]} (meta sym)]
                           `(do
                              (def ~sym (deref ~var))
                              ;for AOT compilation
                              (alter-meta! (var ~sym) assoc
                                :macro true
                                :arglists ~arglists
                                :doc ~doc
                                :file ~file
                                :line ~line))))
                       syms vars)]
       `(do ~@defs
            :imported))))

#?(:clj
   (import-macros clojure.core
     [-> ->> .. assert comment cond
      declare defn-
      extend-protocol fn for
      if-let if-not letfn
      memfn
      when when-first when-let when-not while
      cond-> cond->> as-> some-> some->>
      if-some when-some]))

#?(:cljs
   (core/defmacro ->
     "Threads the expr through the forms. Inserts x as the
     second item in the first form, making a list of it if it is not a
     list already. If there are more forms, inserts the first form as the
     second item in second form, etc."
     [x & forms]
     (core/loop [x x, forms forms]
       (if forms
         (core/let [form (first forms)
                    threaded (if (seq? form)
                               (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                               (core/list form x))]
           (recur threaded (next forms)))
         x))))

#?(:cljs
   (core/defmacro ->>
     "Threads the expr through the forms. Inserts x as the
     last item in the first form, making a list of it if it is not a
     list already. If there are more forms, inserts the first form as the
     last item in second form, etc."
     [x & forms]
     (core/loop [x x, forms forms]
       (if forms
         (core/let [form (first forms)
                    threaded (if (seq? form)
                               (with-meta `(~(first form) ~@(next form) ~x) (meta form))
                               (core/list form x))]
           (recur threaded (next forms)))
         x))))

#?(:cljs
   (core/defmacro ..
     "form => fieldName-symbol or (instanceMethodName-symbol args*)

     Expands into a member access (.) of the first member on the first
     argument, followed by the next member on the result, etc. For
     instance:

     (.. System (getProperties) (get \"os.name\"))

     expands to:

     (. (. System (getProperties)) (get \"os.name\"))

     but is easier to write, read, and understand."
     ([x form] `(. ~x ~form))
     ([x form & more] `(.. (. ~x ~form) ~@more))))

#?(:cljs
   (core/defmacro comment
     "Ignores body, yields nil"
     [& body]))

#?(:cljs
   (core/defmacro cond
     "Takes a set of test/expr pairs. It evaluates each test one at a
     time.  If a test returns logical true, cond evaluates and returns
     the value of the corresponding expr and doesn't evaluate any of the
     other tests or exprs. (cond) returns nil."
     {:added "1.0"}
     [& clauses]
     (core/when clauses
       (core/list 'if (first clauses)
         (if (next clauses)
           (second clauses)
           (throw (js/Error. "cond requires an even number of forms")))
         (cons 'cljs.core/cond (next (next clauses)))))))

#?(:cljs
   (core/defmacro declare
     "defs the supplied var names with no bindings, useful for making forward declarations."
     [& names] `(do ~@(map #(core/list 'def (vary-meta % assoc :declared true)) names))))

(core/defmacro doto
  "Evaluates x then calls all of the methods and functions with the
  value of x supplied at the front of the given arguments.  The forms
  are evaluated in order.  Returns x.

  (doto (new js/Map) (.set \"a\" 1) (.set \"b\" 2))"
  [x & forms]
  (core/let [gx (gensym)]
    `(let [~gx ~x]
       ~@(map (core/fn [f]
                (if (seq? f)
                  `(~(first f) ~gx ~@(next f))
                  `(~f ~gx)))
           forms)
       ~gx)))

#?(:cljs
   (core/defn- parse-impls [specs]
     (core/loop [ret {} s specs]
       (if (seq s)
         (recur (assoc ret (first s) (take-while seq? (next s)))
           (drop-while seq? (next s)))
         ret))))

#?(:cljs
   (core/defn- emit-extend-protocol [p specs]
     (core/let [impls (parse-impls specs)]
       `(do
          ~@(map (core/fn [[t fs]]
                   `(extend-type ~t ~p ~@fs))
              impls)))))

#?(:cljs
   (core/defmacro extend-protocol
     "Useful when you want to provide several implementations of the same
     protocol all at once. Takes a single protocol and the implementation
     of that protocol for one or more types. Expands into calls to
     extend-type:

     (extend-protocol Protocol
       AType
         (foo [x] ...)
         (bar [x y] ...)
       BType
         (foo [x] ...)
         (bar [x y] ...)
       AClass
         (foo [x] ...)
         (bar [x y] ...)
       nil
         (foo [x] ...)
         (bar [x y] ...))

     expands into:

     (do
      (clojure.core/extend-type AType Protocol
        (foo [x] ...)
        (bar [x y] ...))
      (clojure.core/extend-type BType Protocol
        (foo [x] ...)
        (bar [x y] ...))
      (clojure.core/extend-type AClass Protocol
        (foo [x] ...)
        (bar [x y] ...))
      (clojure.core/extend-type nil Protocol
        (foo [x] ...)
        (bar [x y] ...)))"
     [p & specs]
     (emit-extend-protocol p specs)))

#?(:cljs
   (core/defn ^{:private true}
   maybe-destructured
     [params body]
     (if (every? core/symbol? params)
       (cons params body)
       (core/loop [params params
                   new-params (with-meta [] (meta params))
                   lets []]
         (if params
           (if (core/symbol? (first params))
             (recur (next params) (conj new-params (first params)) lets)
             (core/let [gparam (gensym "p__")]
               (recur (next params) (conj new-params gparam)
                 (core/-> lets (conj (first params)) (conj gparam)))))
           `(~new-params
              (let ~lets
                ~@body)))))))

#?(:cljs
   (core/defmacro fn
     "params => positional-params* , or positional-params* & next-param
     positional-param => binding-form
     next-param => binding-form
     name => symbol

     Defines a function"
     {:forms '[(fn name? [params*] exprs*) (fn name? ([params*] exprs*) +)]}
     [& sigs]
     (core/let [name (if (core/symbol? (first sigs)) (first sigs) nil)
                sigs (if name (next sigs) sigs)
                sigs (if (vector? (first sigs))
                       (core/list sigs)
                       (if (seq? (first sigs))
                         sigs
                         ;; Assume single arity syntax
                         (throw (js/Error.
                                  (if (seq sigs)
                                    (core/str "Parameter declaration "
                                      (core/first sigs)
                                      " should be a vector")
                                    (core/str "Parameter declaration missing"))))))
                psig (fn* [sig]
                       ;; Ensure correct type before destructuring sig
                       (core/when (not (seq? sig))
                         (throw (js/Error.
                                  (core/str "Invalid signature " sig
                                    " should be a list"))))
                       (core/let [[params & body] sig
                                  _ (core/when (not (vector? params))
                                      (throw (js/Error.
                                               (if (seq? (first sigs))
                                                 (core/str "Parameter declaration " params
                                                   " should be a vector")
                                                 (core/str "Invalid signature " sig
                                                   " should be a list")))))
                                  conds (core/when (core/and (next body) (map? (first body)))
                                          (first body))
                                  body (if conds (next body) body)
                                  conds (core/or conds (meta params))
                                  pre (:pre conds)
                                  post (:post conds)
                                  body (if post
                                         `((let [~'% ~(if (core/< 1 (count body))
                                                        `(do ~@body)
                                                        (first body))]
                                             ~@(map (fn* [c] `(assert ~c)) post)
                                             ~'%))
                                         body)
                                  body (if pre
                                         (concat (map (fn* [c] `(assert ~c)) pre)
                                           body)
                                         body)]
                         (maybe-destructured params body)))
                new-sigs (map psig sigs)]
       (with-meta
         (if name
           (list* 'fn* name new-sigs)
           (cons 'fn* new-sigs))
         (meta &form)))))

#?(:cljs
   (core/defmacro defn-
     "same as defn, yielding non-public def"
     [name & decls]
     (list* `defn (with-meta name (assoc (meta name) :private true)) decls)))

#?(:cljs
   (core/defmacro if-let
     "bindings => binding-form test

     If test is true, evaluates then with binding-form bound to the value of
     test, if not, yields else"
     ([bindings then]
      `(if-let ~bindings ~then nil))
     ([bindings then else & oldform]
      (assert-args if-let
        (vector? bindings) "a vector for its binding"
        (empty? oldform) "1 or 2 forms after binding vector"
        (= 2 (count bindings)) "exactly 2 forms in binding vector")
      (core/let [form (bindings 0) tst (bindings 1)]
        `(let [temp# ~tst]
           (if temp#
             (let [~form temp#]
               ~then)
             ~else))))))

#?(:cljs
   (core/defmacro if-not
     "Evaluates test. If logical false, evaluates and returns then expr,
     otherwise else expr, if supplied, else nil."
     ([test then] `(if-not ~test ~then nil))
     ([test then else]
      `(if (not ~test) ~then ~else))))

#?(:cljs
   (core/defmacro letfn
     "fnspec ==> (fname [params*] exprs) or (fname ([params*] exprs)+)

     Takes a vector of function specs and a body, and generates a set of
     bindings of functions to their names. All of the names are available
     in all of the definitions of the functions, as well as the body."
     {:forms '[(letfn [fnspecs*] exprs*)],
      :special-form true, :url nil}
     [fnspecs & body]
     `(letfn* ~(vec (interleave (map first fnspecs)
                      (map #(cons `fn %) fnspecs)))
        ~@body)))

(core/defmacro memfn
  "Expands into code that creates a fn that expects to be passed an
  object and any args and calls the named instance method on the
  object passing the args. Use when you want to treat a JavaScript
  method as a first-class fn."
  [name & args]
  (core/let [t (with-meta (gensym "target")
                 (meta name))]
    `(fn [~t ~@args]
       (. ~t (~name ~@args)))))

#?(:cljs
   (core/defmacro when
     "Evaluates test. If logical true, evaluates body in an implicit do."
     [test & body]
     (core/list 'if test (cons 'do body))))

#?(:cljs
   (core/defmacro when-first
     "bindings => x xs

     Roughly the same as (when (seq xs) (let [x (first xs)] body)) but xs is evaluated only once"
     [bindings & body]
     (assert-args when-first
       (vector? bindings) "a vector for its binding"
       (= 2 (count bindings)) "exactly 2 forms in binding vector")
     (core/let [[x xs] bindings]
       `(when-let [xs# (seq ~xs)]
          (let [~x (first xs#)]
            ~@body)))))

#?(:cljs
   (core/defmacro when-let
     "bindings => binding-form test

     When test is true, evaluates body with binding-form bound to the value of test"
     [bindings & body]
     (assert-args when-let
       (vector? bindings) "a vector for its binding"
       (= 2 (count bindings)) "exactly 2 forms in binding vector")
     (core/let [form (bindings 0) tst (bindings 1)]
       `(let [temp# ~tst]
          (when temp#
            (let [~form temp#]
              ~@body))))))

#?(:cljs
   (core/defmacro when-not
     "Evaluates test. If logical false, evaluates body in an implicit do."
     [test & body]
     (core/list 'if test nil (cons 'do body))))

#?(:cljs
   (core/defmacro while
     "Repeatedly executes body while test expression is true. Presumes
     some side-effect will cause test to become false/nil. Returns nil"
     [test & body]
     `(loop []
        (when ~test
          ~@body
          (recur)))))

#?(:cljs
   (core/defmacro cond->
     "Takes an expression and a set of test/form pairs. Threads expr (via ->)
     through each form for which the corresponding test
     expression is true. Note that, unlike cond branching, cond-> threading does
     not short circuit after the first true test expression."
     [expr & clauses]
     (core/assert (even? (count clauses)))
     (core/let [g (gensym)
                steps (map (core/fn [[test step]] `(if ~test (-> ~g ~step) ~g))
                        (partition 2 clauses))]
       `(let [~g ~expr
              ~@(interleave (repeat g) (butlast steps))]
          ~(if (empty? steps)
             g
             (last steps))))))

#?(:cljs
   (core/defmacro cond->>
     "Takes an expression and a set of test/form pairs. Threads expr (via ->>)
     through each form for which the corresponding test expression
     is true.  Note that, unlike cond branching, cond->> threading does not short circuit
     after the first true test expression."
     [expr & clauses]
     (core/assert (even? (count clauses)))
     (core/let [g (gensym)
                steps (map (core/fn [[test step]] `(if ~test (->> ~g ~step) ~g))
                        (partition 2 clauses))]
       `(let [~g ~expr
              ~@(interleave (repeat g) (butlast steps))]
          ~(if (empty? steps)
             g
             (last steps))))))

#?(:cljs
   (core/defmacro as->
     "Binds name to expr, evaluates the first form in the lexical context
     of that binding, then binds name to that result, repeating for each
     successive form, returning the result of the last form."
     [expr name & forms]
     `(let [~name ~expr
            ~@(interleave (repeat name) (butlast forms))]
        ~(if (empty? forms)
           name
           (last forms)))))

#?(:cljs
   (core/defmacro some->
     "When expr is not nil, threads it into the first form (via ->),
     and when that result is not nil, through the next etc"
     [expr & forms]
     (core/let [g (gensym)
                steps (map (core/fn [step] `(if (nil? ~g) nil (-> ~g ~step)))
                        forms)]
       `(let [~g ~expr
              ~@(interleave (repeat g) (butlast steps))]
          ~(if (empty? steps)
             g
             (last steps))))))

#?(:cljs
   (core/defmacro some->>
     "When expr is not nil, threads it into the first form (via ->>),
     and when that result is not nil, through the next etc"
     [expr & forms]
     (core/let [g (gensym)
                steps (map (core/fn [step] `(if (nil? ~g) nil (->> ~g ~step)))
                        forms)]
       `(let [~g ~expr
              ~@(interleave (repeat g) (butlast steps))]
          ~(if (empty? steps)
             g
             (last steps))))))

#?(:cljs
   (core/defmacro if-some
     "bindings => binding-form test

      If test is not nil, evaluates then with binding-form bound to the
      value of test, if not, yields else"
     ([bindings then]
      `(if-some ~bindings ~then nil))
     ([bindings then else & oldform]
      (assert-args if-some
        (vector? bindings) "a vector for its binding"
        (empty? oldform) "1 or 2 forms after binding vector"
        (= 2 (count bindings)) "exactly 2 forms in binding vector")
      (core/let [form (bindings 0) tst (bindings 1)]
        `(let [temp# ~tst]
           (if (nil? temp#)
             ~else
             (let [~form temp#]
               ~then)))))))

#?(:cljs
   (core/defmacro when-some
     "bindings => binding-form test

      When test is not nil, evaluates body with binding-form bound to the
      value of test"
     [bindings & body]
     (assert-args when-some
       (vector? bindings) "a vector for its binding"
       (= 2 (count bindings)) "exactly 2 forms in binding vector")
     (core/let [form (bindings 0) tst (bindings 1)]
       `(let [temp# ~tst]
          (if (nil? temp#)
            nil
            (let [~form temp#]
              ~@body))))))

(core/defn- ^{:dynamic true} assert-valid-fdecl
  "A good fdecl looks like (([a] ...) ([a b] ...)) near the end of defn."
  [fdecl]
  (core/when (empty? fdecl)
    (throw
      #?(:clj  (IllegalArgumentException. "Parameter declaration missing")
         :cljs (js/Error. "Parameter declaration missing"))))
  (core/let [argdecls
             (map
               #(if (seq? %)
                 (first %)
                 (throw
                   #?(:clj (IllegalArgumentException.
                             (if (seq? (first fdecl))
                               (core/str "Invalid signature \""
                                 %
                                 "\" should be a list")
                               (core/str "Parameter declaration \""
                                 %
                                 "\" should be a vector")))
                      :cljs (js/Error.
                              (if (seq? (first fdecl))
                                (core/str "Invalid signature \""
                                  %
                                  "\" should be a list")
                                (core/str "Parameter declaration \""
                                  %
                                  "\" should be a vector"))))))
               fdecl)
             bad-args (seq (remove #(vector? %) argdecls))]
    (core/when bad-args
      (throw
        #?(:clj (IllegalArgumentException.
                  (core/str "Parameter declaration \"" (first bad-args)
                    "\" should be a vector"))
           :cljs (js/Error.
                   (core/str "Parameter declaration \"" (first bad-args)
                     "\" should be a vector")))))))

(def
  ^{:private true}
  sigs
  (core/fn [fdecl]
    (assert-valid-fdecl fdecl)
    (core/let [asig
               (core/fn [fdecl]
                 (core/let [arglist (first fdecl)
                            ;elide implicit macro args
                            arglist (if #?(:clj (clojure.lang.Util/equals '&form (first arglist))
                                           :cljs (= '&form (first arglist)))
                                      #?(:clj (clojure.lang.RT/subvec arglist 2 (clojure.lang.RT/count arglist))
                                         :cljs (subvec arglist 2 (count arglist)))
                                      arglist)
                            body (next fdecl)]
                   (if (map? (first body))
                     (if (next body)
                       (with-meta arglist (conj (if (meta arglist) (meta arglist) {}) (first body)))
                       arglist)
                     arglist)))]
      (if (seq? (first fdecl))
        (core/loop [ret [] fdecls fdecl]
          (if fdecls
            (recur (conj ret (asig (first fdecls))) (next fdecls))
            (seq ret)))
        (core/list (asig fdecl))))))

(core/defmacro defonce
  "defs name to have the root value of init iff the named var has no root value,
  else init is unevaluated"
  [x init]
  `(when-not (exists? ~x)
     (def ~x ~init)))

(core/defn destructure [bindings]
  (core/let [bents (partition 2 bindings)
             pb (core/fn pb [bvec b v]
                  (core/let [pvec
                             (core/fn [bvec b val]
                               (core/let [gvec (gensym "vec__")
                                          gseq (gensym "seq__")
                                          gfirst (gensym "first__")
                                          has-rest (some #{'&} b)]
                                 (core/loop [ret (core/let [ret (conj bvec gvec val)]
                                                   (if has-rest
                                                     (conj ret gseq (core/list `seq gvec))
                                                     ret))
                                             n 0
                                             bs b
                                             seen-rest? false]
                                   (if (seq bs)
                                     (core/let [firstb (first bs)]
                                       (core/cond
                                         (= firstb '&) (recur (pb ret (second bs) gseq)
                                                              n
                                                              (nnext bs)
                                                              true)
                                         (= firstb :as) (pb ret (second bs) gvec)
                                         :else (if seen-rest?
                                                 (throw #?(:clj (new Exception "Unsupported binding form, only :as can follow & parameter")
                                                           :cljs (new js/Error "Unsupported binding form, only :as can follow & parameter")))
                                                 (recur (pb (if has-rest
                                                              (conj ret
                                                                    gfirst `(first ~gseq)
                                                                    gseq `(next ~gseq))
                                                              ret)
                                                            firstb
                                                            (if has-rest
                                                              gfirst
                                                              (core/list `nth gvec n nil)))
                                                        (core/inc n)
                                                        (next bs)
                                                        seen-rest?))))
                                     ret))))
                             pmap
                             (core/fn [bvec b v]
                               (core/let [gmap (gensym "map__")
                                          defaults (:or b)]
                                 (core/loop [ret (core/-> bvec (conj gmap) (conj v)
                                                          (conj gmap) (conj `(if (implements? ISeq ~gmap) (apply cljs.core/hash-map ~gmap) ~gmap))
                                                     ((core/fn [ret]
                                                        (if (:as b)
                                                          (conj ret (:as b) gmap)
                                                          ret))))
                                             bes (core/let [transforms
                                                            (reduce
                                                              (core/fn [transforms mk]
                                                                (if (core/keyword? mk)
                                                                  (core/let [mkns (namespace mk)
                                                                        mkn (name mk)]
                                                                    (core/cond (= mkn "keys") (assoc transforms mk #(keyword (core/or mkns (namespace %)) (name %)))
                                                                               (= mkn "syms") (assoc transforms mk #(core/list `quote (symbol (core/or mkns (namespace %)) (name %))))
                                                                               (= mkn "strs") (assoc transforms mk core/str)
                                                                               :else transforms))
                                                                  transforms))
                                                              {}
                                                              (keys b))]
                                                   (reduce
                                                     (core/fn [bes entry]
                                                       (reduce #(assoc %1 %2 ((val entry) %2))
                                                         (dissoc bes (key entry))
                                                         ((key entry) bes)))
                                                     (dissoc b :as :or)
                                                     transforms))]
                                   (if (seq bes)
                                     (core/let [bb (key (first bes))
                                                bk (val (first bes))
                                                local (if #?(:clj  (core/instance? clojure.lang.Named bb)
                                                             :cljs (cljs.core/implements? INamed bb))
                                                          (with-meta (symbol nil (name bb)) (meta bb))
                                                        bb)
                                                bv (if (contains? defaults local)
                                                     (core/list 'cljs.core/get gmap bk (defaults local))
                                                     (core/list 'cljs.core/get gmap bk))]
                                       (recur
                                         (if (core/or (core/keyword? bb) (core/symbol? bb)) ;(ident? bb)
                                           (core/-> ret (conj local bv))
                                           (pb ret bb bv))
                                              (next bes)))
                                     ret))))]
                    (core/cond
                      (core/symbol? b) (core/-> bvec (conj (if (namespace b) (symbol (name b)) b)) (conj v))
                      (core/keyword? b) (core/-> bvec (conj (symbol (name b))) (conj v))
                      (vector? b) (pvec bvec b v)
                      (map? b) (pmap bvec b v)
                      :else (throw
                             #?(:clj (new Exception (core/str "Unsupported binding form: " b))
                                :cljs (new js/Error (core/str "Unsupported binding form: " b)))))))
             process-entry (core/fn [bvec b] (pb bvec (first b) (second b)))]
    (if (every? core/symbol? (map first bents))
      bindings
      (core/if-let [kwbs (seq (filter #(core/keyword? (first %)) bents))]
        (throw
          #?(:clj (new Exception (core/str "Unsupported binding key: " (ffirst kwbs)))
             :cljs (new js/Error (core/str "Unsupported binding key: " (ffirst kwbs)))))
        (reduce process-entry [] bents)))))

(core/defmacro ^:private return-first
  [& body]
  `(let [ret# ~(first body)]
     ~@(rest body)
     ret#))

(core/defmacro goog-define
  "Defines a var using `goog.define`. Passed default value must be
  string, number or boolean.

  Default value can be overridden at compile time using the
  compiler option `:closure-defines`.

  Example:
    (ns your-app.core)
    (goog-define DEBUG! false)
    ;; can be overridden with
    :closure-defines {\"your_app.core.DEBUG_BANG_\" true}
    or
    :closure-defines {'your-app.core/DEBUG! true}"
  [sym default]
  (assert-args goog-define
   (core/or (core/string? default)
            (core/number? default)
            (core/true? default)
            (core/false? default)) "a string, number or boolean as default value")
  (core/let [defname (comp/munge (core/str *ns* "/" sym))
             type    (core/cond
                       (core/string? default) "string"
                       (core/number? default) "number"
                       (core/or (core/true? default) (core/false? default)) "boolean")]
    `(~(if (:def-emits-var &env) `return-first `do)
       (declare ~(core/vary-meta sym
                   (core/fn [m]
                     (core/cond-> m
                       (core/not (core/contains? m :tag))
                       (core/assoc :tag (core/symbol type))
                       ))))
       (~'js* ~(core/str "/** @define {" type "} */"))
       (goog/define ~defname ~default))))

(core/defmacro let
  "binding => binding-form init-expr

  Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein."
  [bindings & body]
  (assert-args let
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  `(let* ~(destructure bindings) ~@body))

(core/defmacro loop
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target."
  [bindings & body]
  (assert-args loop
    (vector? bindings) "a vector for its binding"
    (even? (count bindings)) "an even number of forms in binding vector")
  (core/let [db (destructure bindings)]
    (if (= db bindings)
      `(loop* ~bindings ~@body)
      (core/let [vs (take-nth 2 (drop 1 bindings))
                 bs (take-nth 2 bindings)
                 gs (map (core/fn [b] (if (core/symbol? b) b (gensym))) bs)
                 bfs (reduce (core/fn [ret [b v g]]
                               (if (core/symbol? b)
                                 (conj ret g v)
                                 (conj ret g v b g)))
                       [] (map core/vector bs vs gs))]
        `(let ~bfs
           (loop* ~(vec (interleave gs gs))
             (let ~(vec (interleave bs gs))
               ~@body)))))))

(def fast-path-protocols
  "protocol fqn -> [partition number, bit]"
  (zipmap (map #(symbol "cljs.core" (core/str %))
               '[IFn ICounted IEmptyableCollection ICollection IIndexed ASeq ISeq INext
                 ILookup IAssociative IMap IMapEntry ISet IStack IVector IDeref
                 IDerefWithTimeout IMeta IWithMeta IReduce IKVReduce IEquiv IHash
                 ISeqable ISequential IList IRecord IReversible ISorted IPrintWithWriter IWriter
                 IPrintWithWriter IPending IWatchable IEditableCollection ITransientCollection
                 ITransientAssociative ITransientMap ITransientVector ITransientSet
                 IMultiFn IChunkedSeq IChunkedNext IComparable INamed ICloneable IAtom
                 IReset ISwap IIterable])
          (iterate (core/fn [[p b]]
                     (if (core/== 2147483648 b)
                       [(core/inc p) 1]
                       [p #?(:clj  (core/bit-shift-left b 1)
                             :cljs (core/* 2 b))]))
                   [0 1])))

(def fast-path-protocol-partitions-count
  "total number of partitions"
  (core/let [c (count fast-path-protocols)
             m (core/mod c 32)]
    (if (core/zero? m)
      (core/quot c 32)
      (core/inc (core/quot c 32)))))

(core/defn- compatible? [inferred-tag allowed-tags]
  (if (set? inferred-tag)
    (clojure.set/subset? inferred-tag allowed-tags)
    (contains? allowed-tags inferred-tag)))

(core/defn- typed-expr? [env form allowed-tags]
  (compatible? (cljs.analyzer/infer-tag env
                 (cljs.analyzer/no-warn (cljs.analyzer/analyze env form)))
    allowed-tags))

(core/defn- string-expr [e]
  (vary-meta e assoc :tag 'string))

(core/defmacro str
  ([] "")
  ([x]
   (if (typed-expr? &env x '#{string})
     x
     (string-expr (core/list 'js* "cljs.core.str.cljs$core$IFn$_invoke$arity$1(~{})" x))))
  ([x & ys]
   (core/let [interpolate (core/fn [x]
                            (if (typed-expr? &env x '#{string clj-nil})
                              "~{}"
                              "cljs.core.str.cljs$core$IFn$_invoke$arity$1(~{})"))
              strs        (core/->> (core/list* x ys)
                            (map interpolate)
                            (interpose ",")
                            (apply core/str))]
     (string-expr (list* 'js* (core/str "[" strs "].join('')") x ys)))))

(core/defn- bool-expr [e]
  (vary-meta e assoc :tag 'boolean))

(core/defn- simple-test-expr? [env ast]
  (core/and
    (#{:var :js-var :local :invoke :const :host-field :host-call :js :quote} (:op ast))
    ('#{boolean seq} (cljs.analyzer/infer-tag env ast))))

(core/defmacro and
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  ([] true)
  ([x] x)
  ([x & next]
   (core/let [forms (concat [x] next)]
     (if (every? #(simple-test-expr? &env %)
           (map #(cljs.analyzer/no-warn (cljs.analyzer/analyze &env %)) forms))
       (core/let [and-str (core/->> (repeat (count forms) "(~{})")
                            (interpose " && ")
                            (#(concat ["("] % [")"]))
                            (apply core/str))]
         (bool-expr `(~'js* ~and-str ~@forms)))
       `(let [and# ~x]
          (if and# (and ~@next) and#))))))

(core/defmacro or
  "Evaluates exprs one at a time, from left to right. If a form
  returns a logical true value, or returns that value and doesn't
  evaluate any of the other expressions, otherwise it returns the
  value of the last expression. (or) returns nil."
  ([] nil)
  ([x] x)
  ([x & next]
   (core/let [forms (concat [x] next)]
     (if (every? #(simple-test-expr? &env %)
           (map #(cljs.analyzer/no-warn (cljs.analyzer/analyze &env %)) forms))
       (core/let [or-str (core/->> (repeat (count forms) "(~{})")
                           (interpose " || ")
                           (#(concat ["("] % [")"]))
                           (apply core/str))]
         (bool-expr `(~'js* ~or-str ~@forms)))
       `(let [or# ~x]
          (if or# or# (or ~@next)))))))

(core/defmacro nil? [x]
  `(coercive-= ~x nil))

(core/defmacro some? [x]
  `(not (nil? ~x)))

(core/defmacro coercive-not [x]
  (bool-expr (core/list 'js* "(!~{})" x)))

(core/defmacro coercive-not= [x y]
  (bool-expr (core/list 'js* "(~{} != ~{})" x y)))

(core/defmacro coercive-= [x y]
  (bool-expr (core/list 'js* "(~{} == ~{})" x y)))

(core/defmacro coercive-boolean [x]
  (with-meta (core/list 'js* "~{}" x)
    {:tag 'boolean}))

;; internal - do not use.
(core/defmacro truth_ [x]
  (core/assert (core/symbol? x) "x is substituted twice")
  (core/list 'js* "(~{} != null && ~{} !== false)" x x))

(core/defmacro js-arguments []
  (core/list 'js* "arguments"))

(core/defmacro js-delete [obj key]
  (core/list 'js* "delete ~{}[~{}]" obj key))

(core/defmacro js-in [key obj]
  (core/list 'js* "~{} in ~{}" key obj))

(core/defmacro js-debugger
  "Emit JavaScript \"debugger;\" statement"
  []
  (core/list 'do
             (core/list 'js* "debugger")
             nil))

(core/defmacro js-comment
  "Emit a top-level JavaScript multi-line comment. New lines will create a
  new comment line. Comment block will be preceded and followed by a newline"
  [comment]
  (core/let [[x & ys] (string/split comment #"\n")]
    (core/list 'js*
      (core/str
        "\n/**\n"
        (core/str " * " x "\n")
        (core/->> ys
          (map #(core/str " * " (string/replace % #"^   " "") "\n"))
          (reduce core/str ""))
        " */\n"))))

(core/defmacro unsafe-cast
  "EXPERIMENTAL: Subject to change. Unsafely cast a value to a different type."
  [t x]
  (core/let [cast-expr (core/str "~{} = /** @type {" t "} */ (~{})")]
    (core/list 'js* cast-expr x x)))

(core/defmacro js-inline-comment
  "Emit an inline JavaScript comment."
  [comment]
  (core/list 'js* (core/str "/**" comment "*/")))

(core/defmacro true? [x]
  (bool-expr (core/list 'js* "~{} === true" x)))

(core/defmacro false? [x]
  (bool-expr (core/list 'js* "~{} === false" x)))

(core/defmacro string? [x]
  (bool-expr (core/list 'js* "typeof ~{} === 'string'" x)))

(core/defmacro exists?
  "Return true if argument exists, analogous to usage of typeof operator
   in JavaScript."
  [x]
  (if (core/symbol? x)
    (core/let [x     (core/cond-> (:name (cljs.analyzer/resolve-var &env x))
                       (= "js" (namespace x)) name)
               segs  (string/split (core/str (string/replace (core/str x) "/" ".")) #"\.")
               n     (count segs)
               syms  (map
                       #(vary-meta (symbol "js" (string/join "." %))
                          assoc :cljs.analyzer/no-resolve true)
                       (reverse (take n (iterate butlast segs))))
               js    (string/join " && " (repeat n "(typeof ~{} !== 'undefined')"))]
      (bool-expr (concat (core/list 'js* js) syms)))
    `(some? ~x)))

(core/defmacro undefined?
  "Return true if argument is identical to the JavaScript undefined value."
  [x]
  (bool-expr (core/list 'js* "(void 0 === ~{})" x)))

(core/defmacro identical? [a b]
  (bool-expr (core/list 'js* "(~{} === ~{})" a b)))

(core/defmacro instance? [c x]
  ;; Google Closure warns about some references to RegExp, so
  ;; (instance? RegExp ...) needs to be inlined, but the expansion
  ;; should preserve the order of argument evaluation.
  (bool-expr (if (clojure.core/symbol? c)
               (core/list 'js* "(~{} instanceof ~{})" x c)
               `(let [c# ~c x# ~x]
                  (~'js* "(~{} instanceof ~{})" x# c#)))))

(core/defmacro number? [x]
  (bool-expr (core/list 'js* "typeof ~{} === 'number'" x)))

(core/defmacro symbol? [x]
  (bool-expr `(instance? Symbol ~x)))

(core/defmacro keyword? [x]
  (bool-expr `(instance? Keyword ~x)))

(core/defmacro aget
  ([array idx]
   (core/case (ana/checked-arrays)
     :warn `(checked-aget ~array ~idx)
     :error `(checked-aget' ~array ~idx)
     (core/list 'js* "(~{}[~{}])" array idx)))
  ([array idx & idxs]
   (core/case (ana/checked-arrays)
     :warn `(checked-aget ~array ~idx ~@idxs)
     :error `(checked-aget' ~array ~idx ~@idxs)
     (core/let [astr (apply core/str (repeat (count idxs) "[~{}]"))]
       `(~'js* ~(core/str "(~{}[~{}]" astr ")") ~array ~idx ~@idxs)))))

(core/defmacro aset
  ([array idx val]
   (core/case (ana/checked-arrays)
     :warn `(checked-aset ~array ~idx ~val)
     :error `(checked-aset' ~array ~idx ~val)
     (core/list 'js* "(~{}[~{}] = ~{})" array idx val)))
  ([array idx idx2 & idxv]
   (core/case (ana/checked-arrays)
     :warn `(checked-aset ~array ~idx ~idx2 ~@idxv)
     :error `(checked-aset' ~array ~idx ~idx2 ~@idxv)
     (core/let [n    (core/dec (count idxv))
                astr (apply core/str (repeat n "[~{}]"))]
       `(~'js* ~(core/str "(~{}[~{}][~{}]" astr " = ~{})") ~array ~idx ~idx2 ~@idxv)))))

(core/defmacro unchecked-get
  "INTERNAL. Compiles to JavaScript property access using bracket notation. Does
  not distinguish between object and array types and not subject to compiler
  static analysis."
  [obj key]
  (core/list 'js* "(~{}[~{}])" obj key))

(core/defmacro unchecked-set
  "INTERNAL. Compiles to JavaScript property access using bracket notation. Does
  not distinguish between object and array types and not subject to compiler
  static analysis."
  [obj key val]
  (core/list 'js* "(~{}[~{}] = ~{})" obj key val))

(core/defmacro ^::ana/numeric +
  ([] 0)
  ([x] x)
  ([x y] (core/list 'js* "(~{} + ~{})" x y))
  ([x y & more] `(+ (+ ~x ~y) ~@more)))

(core/defmacro byte [x] x)
(core/defmacro short [x] x)
(core/defmacro float [x] x)
(core/defmacro double [x] x)

(core/defmacro unchecked-byte [x] x)
(core/defmacro unchecked-char [x] x)
(core/defmacro unchecked-short [x] x)
(core/defmacro unchecked-float [x] x)
(core/defmacro unchecked-double [x] x)

(core/defmacro ^::ana/numeric unchecked-add
  ([& xs] `(+ ~@xs)))

(core/defmacro ^::ana/numeric unchecked-add-int
  ([& xs] `(+ ~@xs)))

(core/defmacro ^::ana/numeric unchecked-dec
  ([x] `(dec ~x)))

(core/defmacro ^::ana/numeric unchecked-dec-int
  ([x] `(dec ~x)))

(core/defmacro ^::ana/numeric unchecked-divide-int
  ([& xs] `(/ ~@xs)))

(core/defmacro ^::ana/numeric unchecked-inc
  ([x] `(inc ~x)))

(core/defmacro ^::ana/numeric unchecked-inc-int
  ([x] `(inc ~x)))

(core/defmacro ^::ana/numeric unchecked-multiply
  ([& xs] `(* ~@xs)))

(core/defmacro ^::ana/numeric unchecked-multiply-int
  ([& xs] `(* ~@xs)))

(core/defmacro ^::ana/numeric unchecked-negate
  ([x] `(- ~x)))

(core/defmacro ^::ana/numeric unchecked-negate-int
  ([x] `(- ~x)))

(core/defmacro ^::ana/numeric unchecked-remainder-int
  ([x n] `(core/mod ~x ~n)))

(core/defmacro ^::ana/numeric unchecked-subtract
  ([& xs] `(- ~@xs)))

(core/defmacro ^::ana/numeric unchecked-subtract-int
  ([& xs] `(- ~@xs)))

(core/defmacro ^::ana/numeric -
  ([x] (core/list 'js* "(- ~{})" x))
  ([x y] (core/list 'js* "(~{} - ~{})" x y))
  ([x y & more] `(- (- ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric *
  ([] 1)
  ([x] x)
  ([x y] (core/list 'js* "(~{} * ~{})" x y))
  ([x y & more] `(* (* ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric /
  ([x] `(/ 1 ~x))
  ([x y] (core/list 'js* "(~{} / ~{})" x y))
  ([x y & more] `(/ (/ ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric divide
  ([x] `(/ 1 ~x))
  ([x y] (core/list 'js* "(~{} / ~{})" x y))
  ([x y & more] `(/ (/ ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric <
  ([x] true)
  ([x y] (bool-expr (core/list 'js* "(~{} < ~{})" x y)))
  ([x y & more] `(and (< ~x ~y) (< ~y ~@more))))

(core/defmacro ^::ana/numeric <=
  ([x] true)
  ([x y] (bool-expr (core/list 'js* "(~{} <= ~{})" x y)))
  ([x y & more] `(and (<= ~x ~y) (<= ~y ~@more))))

(core/defmacro ^::ana/numeric >
  ([x] true)
  ([x y] (bool-expr (core/list 'js* "(~{} > ~{})" x y)))
  ([x y & more] `(and (> ~x ~y) (> ~y ~@more))))

(core/defmacro ^::ana/numeric >=
  ([x] true)
  ([x y] (bool-expr (core/list 'js* "(~{} >= ~{})" x y)))
  ([x y & more] `(and (>= ~x ~y) (>= ~y ~@more))))

(core/defmacro ^::ana/numeric ==
  ([x] true)
  ([x y] (bool-expr (core/list 'js* "(~{} === ~{})" x y)))
  ([x y & more] `(and (== ~x ~y) (== ~y ~@more))))

(core/defmacro ^::ana/numeric dec [x]
  `(- ~x 1))

(core/defmacro ^::ana/numeric inc [x]
  `(+ ~x 1))

(core/defmacro ^::ana/numeric zero? [x]
  `(== ~x 0))

(core/defmacro ^::ana/numeric pos? [x]
  `(> ~x 0))

(core/defmacro ^::ana/numeric neg? [x]
  `(< ~x 0))

(core/defmacro ^::ana/numeric max
  ([x] x)
  ([x y] `(let [x# ~x, y# ~y]
            (~'js* "((~{} > ~{}) ? ~{} : ~{})" x# y# x# y#)))
  ([x y & more] `(max (max ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric min
  ([x] x)
  ([x y] `(let [x# ~x, y# ~y]
            (~'js* "((~{} < ~{}) ? ~{} : ~{})" x# y# x# y#)))
  ([x y & more] `(min (min ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric js-mod [num div]
  (core/list 'js* "(~{} % ~{})" num div))

(core/defmacro ^::ana/numeric bit-not [x]
  (core/list 'js* "(~ ~{})" x))

(core/defmacro ^::ana/numeric bit-and
  ([x y] (core/list 'js* "(~{} & ~{})" x y))
  ([x y & more] `(bit-and (bit-and ~x ~y) ~@more)))

;; internal do not use
(core/defmacro ^::ana/numeric unsafe-bit-and
  ([x y] (bool-expr (core/list 'js* "(~{} & ~{})" x y)))
  ([x y & more] `(unsafe-bit-and (unsafe-bit-and ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric bit-or
  ([x y] (core/list 'js* "(~{} | ~{})" x y))
  ([x y & more] `(bit-or (bit-or ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric int [x]
  `(bit-or ~x 0))

(core/defmacro ^::ana/numeric bit-xor
  ([x y] (core/list 'js* "(~{} ^ ~{})" x y))
  ([x y & more] `(bit-xor (bit-xor ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric bit-and-not
  ([x y] (core/list 'js* "(~{} & ~~{})" x y))
  ([x y & more] `(bit-and-not (bit-and-not ~x ~y) ~@more)))

(core/defmacro ^::ana/numeric bit-clear [x n]
  (core/list 'js* "(~{} & ~(1 << ~{}))" x n))

(core/defmacro ^::ana/numeric bit-flip [x n]
  (core/list 'js* "(~{} ^ (1 << ~{}))" x n))

(core/defmacro bit-test [x n]
  (bool-expr (core/list 'js* "((~{} & (1 << ~{})) != 0)" x n)))

(core/defmacro ^::ana/numeric bit-shift-left [x n]
  (core/list 'js* "(~{} << ~{})" x n))

(core/defmacro ^::ana/numeric bit-shift-right [x n]
  (core/list 'js* "(~{} >> ~{})" x n))

(core/defmacro ^::ana/numeric bit-shift-right-zero-fill [x n]
  (core/list 'js* "(~{} >>> ~{})" x n))

(core/defmacro ^::ana/numeric unsigned-bit-shift-right [x n]
  (core/list 'js* "(~{} >>> ~{})" x n))

(core/defmacro ^::ana/numeric bit-set [x n]
  (core/list 'js* "(~{} | (1 << ~{}))" x n))

;; internal
(core/defmacro mask [hash shift]
  (core/list 'js* "((~{} >>> ~{}) & 0x01f)" hash shift))

;; internal
(core/defmacro bitpos [hash shift]
  (core/list 'js* "(1 << ~{})" `(mask ~hash ~shift)))

;; internal
(core/defmacro caching-hash [coll hash-fn hash-key]
  (core/assert (clojure.core/symbol? hash-key) "hash-key is substituted twice")
  `(let [h# ~hash-key]
     (if-not (nil? h#)
       h#
       (let [h# (~hash-fn ~coll)]
         (set! ~hash-key h#)
         h#))))

;;; internal -- reducers-related macros

(core/defn- do-curried
  [name doc meta args body]
  (core/let [cargs (vec (butlast args))]
    `(defn ~name ~doc ~meta
       (~cargs (fn [x#] (~name ~@cargs x#)))
       (~args ~@body))))

(core/defmacro ^:private defcurried
  "Builds another arity of the fn that returns a fn awaiting the last
  param"
  [name doc meta args & body]
  (do-curried name doc meta args body))

(core/defn- do-rfn [f1 k fkv]
  `(fn
     ([] (~f1))
     ~(clojure.walk/postwalk
       #(if (sequential? %)
          ((if (vector? %) vec identity)
           (core/remove #{k} %))
          %)
       fkv)
     ~fkv))

(core/defmacro ^:private rfn
  "Builds 3-arity reducing fn given names of wrapped fn and key, and k/v impl."
  [[f1 k] fkv]
  (do-rfn f1 k fkv))

;;; end of reducers macros

(core/defn- protocol-prefix [psym]
  (core/str (core/-> (core/str psym)
              (.replace #?(:clj \. :cljs (js/RegExp. "\\." "g")) \$)
              (.replace \/ \$))
    "$"))

(def ^:private base-type
     {nil "null"
      'object "object"
      'string "string"
      'number "number"
      'array "array"
      'function "function"
      'boolean "boolean"
      'default "_"})

(def ^:private js-base-type
     {'js/Boolean "boolean"
      'js/String "string"
      'js/Array "array"
      'js/Object "object"
      'js/Number "number"
      'js/Function "function"})

(core/defmacro reify
  "reify is a macro with the following structure:

 (reify options* specs*)

  Currently there are no options.

  Each spec consists of the protocol name followed by zero
  or more method bodies:

  protocol
  (methodName [args+] body)*

  Methods should be supplied for all methods of the desired
  protocol(s). You can also define overrides for Object methods. Note that
  the first parameter must be supplied to correspond to the target object
  ('this' in JavaScript parlance). Note also that recur calls
  to the method head should *not* pass the target object, it will be supplied
  automatically and can not be substituted.

  recur works to method heads The method bodies of reify are lexical
  closures, and can refer to the surrounding local scope:

  (str (let [f \"foo\"]
       (reify Object
         (toString [this] f))))
  == \"foo\"

  (seq (let [f \"foo\"]
       (reify ISeqable
         (-seq [this] (seq f)))))
  == (\"f\" \"o\" \"o\"))

  reify always implements IMeta and IWithMeta and transfers meta
  data of the form to the created object.

  (meta ^{:k :v} (reify Object (toString [this] \"foo\")))
  == {:k :v}"
  [& impls]
  (core/let [t        (with-meta
                        (gensym
                          (core/str "t_"
                            (string/replace (core/str (munge ana/*cljs-ns*)) "." "$")))
                        {:anonymous true})
             meta-sym (gensym "meta")
             this-sym (gensym "_")
             locals   (keys (:locals &env))
             ns       (core/-> &env :ns :name)
             munge    comp/munge]
    `(do
       (when-not (exists? ~(symbol (core/str ns) (core/str t)))
         (deftype ~t [~@locals ~meta-sym]
           IWithMeta
           (~'-with-meta [~this-sym ~meta-sym]
             (new ~t ~@locals ~meta-sym))
           IMeta
           (~'-meta [~this-sym] ~meta-sym)
           ~@impls))
       (new ~t ~@locals ~(ana/elide-reader-meta (meta &form))))))

(core/defmacro specify!
  "Identical to reify but mutates its first argument."
  [expr & impls]
  (core/let [x (with-meta (gensym "x") {:extend :instance})]
    `(let [~x ~expr]
       (extend-type ~x ~@impls)
       ~x)))

(core/defmacro specify
  "Identical to specify! but does not mutate its first argument. The first
  argument must be an ICloneable instance."
  [expr & impls]
  `(cljs.core/specify! (cljs.core/clone ~expr)
     ~@impls))

(core/defmacro ^:private js-this []
  (core/list 'js* "this"))

(core/defmacro this-as
  "Defines a scope where JavaScript's implicit \"this\" is bound to the name provided."
  [name & body]
  `(let [~name (js-this)]
     ~@body))

(core/defn- to-property [sym]
  (symbol (core/str "-" sym)))

(core/defn- warn-and-update-protocol [p type env]
  (core/when-not (= 'Object p)
    (core/if-let [var (cljs.analyzer/resolve-existing-var (dissoc env :locals) p)]
      (do
        (core/when-not (:protocol-symbol var)
          (cljs.analyzer/warning :invalid-protocol-symbol env {:protocol p}))
        (core/when (core/and (:protocol-deprecated cljs.analyzer/*cljs-warnings*)
                (core/-> var :deprecated)
                (not (core/-> p meta :deprecation-nowarn)))
          (cljs.analyzer/warning :protocol-deprecated env {:protocol p}))
        (core/when (:protocol-symbol var)
          (swap! env/*compiler* update-in [:cljs.analyzer/namespaces]
            (core/fn [ns]
              (update-in ns [(:ns var) :defs (symbol (name p)) :impls]
                conj type)))))
      (core/when (:undeclared cljs.analyzer/*cljs-warnings*)
        (cljs.analyzer/warning :undeclared-protocol-symbol env {:protocol p})))))

(core/defn- resolve-var [env sym]
  (core/let [ret (:name (cljs.analyzer/resolve-var env sym))]
    (core/assert ret (core/str "Can't resolve: " sym))
    ret))

(core/defn- ->impl-map [impls]
  (core/loop [ret {} s impls]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
        (drop-while seq? (next s)))
      ret)))

(core/defn- base-assign-impls [env resolve tsym type [p sigs]]
  (warn-and-update-protocol p tsym env)
  (core/let [psym       (resolve p)
             pfn-prefix (subs (core/str psym) 0
                          (clojure.core/inc (.indexOf (core/str psym) "/")))]
    (cons `(goog.object/set ~psym ~type true)
      (map (core/fn [[f & meths :as form]]
             `(goog.object/set ~(symbol (core/str pfn-prefix f))
                ~type ~(with-meta `(fn ~@meths) (meta form))))
        sigs))))

(core/defmulti ^:private extend-prefix (core/fn [tsym sym] (core/-> tsym meta :extend)))

(core/defmethod extend-prefix :instance
  [tsym sym] `(.. ~tsym ~(to-property sym)))

(core/defmethod extend-prefix :default
  [tsym sym] `(.. ~tsym ~'-prototype ~(to-property sym)))

(core/defn- adapt-obj-params [type [[this & args :as sig] & body]]
  (core/list (vec args)
    (list* 'this-as (vary-meta this assoc :tag type) body)))

(core/defn- adapt-ifn-params [type [[this & args :as sig] & body]]
  (core/let [self-sym (with-meta 'self__ {:tag type})]
    `(~(vec (cons self-sym args))
       (this-as ~self-sym
         (let [~this ~self-sym]
           ~@body)))))

;; for IFn invoke implementations, we need to drop first arg
(core/defn- adapt-ifn-invoke-params [type [[this & args :as sig] & body]]
  `(~(vec args)
     (this-as ~(vary-meta this assoc :tag type)
       ~@body)))

(core/defn- adapt-proto-params [type [[this & args :as sig] & body]]
  (core/let [this' (vary-meta this assoc :tag type)]
    `(~(vec (cons this' args))
      (this-as ~this'
        ~@body))))

(core/defn- add-obj-methods [type type-sym sigs]
  (map (core/fn [[f & meths :as form]]
         (core/let [[f meths] (if (vector? (first meths))
                                [f [(rest form)]]
                                [f meths])]
           `(set! ~(extend-prefix type-sym f)
              ~(with-meta `(fn ~@(map #(adapt-obj-params type %) meths)) (meta form)))))
    sigs))

(core/defn- ifn-invoke-methods [type type-sym [f & meths :as form]]
  (map
    (core/fn [meth]
      (core/let [arity (count (first meth))]
        `(set! ~(extend-prefix type-sym (symbol (core/str "cljs$core$IFn$_invoke$arity$" arity)))
           ~(with-meta `(fn ~meth) (meta form)))))
    (map #(adapt-ifn-invoke-params type %) meths)))

(core/defn- add-ifn-methods [type type-sym [f & meths :as form]]
  (core/let [meths    (map #(adapt-ifn-params type %) meths)
             this-sym (with-meta 'self__ {:tag type})
             argsym   (gensym "args")]
    (concat
      [`(set! ~(extend-prefix type-sym 'call) ~(with-meta `(fn ~@meths) (meta form)))
       `(set! ~(extend-prefix type-sym 'apply)
          ~(with-meta
             `(fn ~[this-sym argsym]
                (this-as ~this-sym
                  (.apply (.-call ~this-sym) ~this-sym
                    (.concat (array ~this-sym) (cljs.core/aclone ~argsym)))))
             (meta form)))]
      (ifn-invoke-methods type type-sym form))))

(core/defn- add-proto-methods* [pprefix type type-sym [f & meths :as form]]
  (core/let [pf (core/str pprefix (munge (name f)))]
    (if (vector? (first meths))
      ;; single method case
      (core/let [meth meths]
        [`(set! ~(extend-prefix type-sym (core/str pf "$arity$" (count (first meth))))
            ~(with-meta `(fn ~@(adapt-proto-params type meth)) (meta form)))])
      (map (core/fn [[sig & body :as meth]]
             `(set! ~(extend-prefix type-sym (core/str pf "$arity$" (count sig)))
                ~(with-meta `(fn ~(adapt-proto-params type meth)) (meta form))))
        meths))))

(core/defn- proto-assign-impls [env resolve type-sym type [p sigs]]
  (warn-and-update-protocol p type env)
  (core/let [psym      (resolve p)
             pprefix   (protocol-prefix psym)
             skip-flag (set (core/-> type-sym meta :skip-protocol-flag))]
    (if (= p 'Object)
      (add-obj-methods type type-sym sigs)
      (concat
        (core/when-not (skip-flag psym)
          [`(set! ~(extend-prefix type-sym pprefix) cljs.core/PROTOCOL_SENTINEL)])
        (mapcat
          (core/fn [sig]
            (if (= psym 'cljs.core/IFn)
              (add-ifn-methods type type-sym sig)
              (add-proto-methods* pprefix type type-sym sig)))
          sigs)))))

(core/defn- validate-impl-sigs [env p method]
  (core/when-not (= p 'Object)
    (core/let [var (ana/resolve-var (dissoc env :locals) p)
               minfo (core/-> var :protocol-info :methods)
               method-name (first method)
               ->name (comp symbol name)
               [fname sigs] (if (core/vector? (second method))
                              [(->name method-name) [(second method)]]
                              [(->name method-name) (map first (rest method))])
               decmeths (core/get minfo fname ::not-found)]
      (core/when (= decmeths ::not-found)
        (ana/warning :protocol-invalid-method env {:protocol p :fname fname :no-such-method true}))
      (core/when (namespace method-name)
        (core/let [method-var (ana/resolve-var (dissoc env :locals) method-name
                                ana/confirm-var-exist-warning)]
          (core/when-not (= (:name var) (:protocol method-var))
            (ana/warning :protocol-invalid-method env
              {:protocol p :fname method-name :no-such-method true}))))
      (core/loop [sigs sigs seen #{}]
        (core/when (seq sigs)
          (core/let [sig (first sigs)
                     c   (count sig)]
            (core/when (contains? seen c)
              (ana/warning :protocol-duped-method env {:protocol p :fname fname}))
            (core/when (some '#{&} sig)
              (ana/warning :protocol-impl-with-variadic-method env {:protocol p :name fname}))
            (core/when (core/and (not= decmeths ::not-found) (not (some #{c} (map count decmeths))))
              (ana/warning :protocol-invalid-method env {:protocol p :fname fname :invalid-arity c}))
            (recur (next sigs) (conj seen c))))))))

(core/defn- validate-impls [env impls]
  (core/loop [protos #{} impls impls]
    (core/when (seq impls)
      (core/let [proto   (first impls)
                 methods (take-while seq? (next impls))
                 impls   (drop-while seq? (next impls))]
        (core/when (contains? protos proto)
          (ana/warning :protocol-multiple-impls env {:protocol proto}))
        (core/loop [seen #{} methods methods]
          (core/when (seq methods)
            (core/let [[fname :as method] (first methods)]
              (core/when (contains? seen fname)
                (ana/warning :extend-type-invalid-method-shape env
                  {:protocol proto :method fname}))
              (validate-impl-sigs env proto method)
              (recur (conj seen fname) (next methods)))))
        (recur (conj protos proto) impls)))))

(core/defn- type-hint-first-arg
  [type-sym argv]
  (assoc argv 0 (vary-meta (argv 0) assoc :tag type-sym)))

(core/defn- type-hint-single-arity-sig
  [type-sym sig]
  (list* (first sig) (type-hint-first-arg type-sym (second sig)) (nnext sig)))

(core/defn- type-hint-multi-arity-sig
  [type-sym sig]
  (list* (type-hint-first-arg type-sym (first sig)) (next sig)))

(core/defn- type-hint-multi-arity-sigs
  [type-sym sigs]
  (list* (first sigs) (map (partial type-hint-multi-arity-sig type-sym) (rest sigs))))

(core/defn- type-hint-sigs
  [type-sym sig]
  (if (vector? (second sig))
    (type-hint-single-arity-sig type-sym sig)
    (type-hint-multi-arity-sigs type-sym sig)))

(core/defn- type-hint-impl-map
  [type-sym impl-map]
  (reduce-kv (core/fn [m proto sigs]
               (assoc m proto (map (partial type-hint-sigs type-sym) sigs)))
    {} impl-map))

(core/defmacro extend-type
  "Extend a type to a series of protocols. Useful when you are
  supplying the definitions explicitly inline. Propagates the
  type as a type hint on the first argument of all fns.

  type-sym may be

   * default, meaning the definitions will apply for any value,
     unless an extend-type exists for one of the more specific
     cases below.
   * nil, meaning the definitions will apply for the nil value.
   * any of object, boolean, number, string, array, or function,
     indicating the definitions will apply for values of the
     associated base JavaScript types. Note that, for example,
     string should be used instead of js/String.
   * a JavaScript type not covered by the previous list, such
     as js/RegExp.
   * a type defined by deftype or defrecord.

  (extend-type MyType
    ICounted
    (-count [c] ...)
    Foo
    (bar [x y] ...)
    (baz ([x] ...) ([x y] ...) ...)"
  [type-sym & impls]
  (core/let [env &env
             _ (validate-impls env impls)
             resolve (partial resolve-var env)
             impl-map (->impl-map impls)
             impl-map (if ('#{boolean number} type-sym)
                        (type-hint-impl-map type-sym impl-map)
                        impl-map)
             [type assign-impls] (core/if-let [type (base-type type-sym)]
                                   [type base-assign-impls]
                                   [(resolve type-sym) proto-assign-impls])]
    (core/when (core/and (:extending-base-js-type cljs.analyzer/*cljs-warnings*)
            (js-base-type type-sym))
      (cljs.analyzer/warning :extending-base-js-type env
        {:current-symbol type-sym :suggested-symbol (js-base-type type-sym)}))
    `(do ~@(mapcat #(assign-impls env resolve type-sym type %) impl-map))))

(core/defn- prepare-protocol-masks [env impls]
  (core/let [resolve  (partial resolve-var env)
             impl-map (->impl-map impls)
             fpp-pbs  (seq
                        (keep fast-path-protocols
                          (map resolve
                            (keys impl-map))))]
    (if fpp-pbs
      (core/let [fpps  (into #{}
                         (filter (partial contains? fast-path-protocols)
                           (map resolve (keys impl-map))))
                 parts (core/as-> (group-by first fpp-pbs) parts
                         (into {}
                           (map (juxt key (comp (partial map peek) val))
                             parts))
                         (into {}
                           (map (juxt key (comp (partial reduce core/bit-or) val))
                             parts)))]
        [fpps (reduce (core/fn [ps p] (update-in ps [p] (core/fnil identity 0)))
                parts
                (range fast-path-protocol-partitions-count))]))))

(core/defn- annotate-specs [annots v [f sigs]]
  (conj v
    (vary-meta (cons f (map #(cons (second %) (nnext %)) sigs))
      merge annots)))

(core/defn dt->et
  ([type specs fields]
   (dt->et type specs fields false))
  ([type specs fields inline]
   (core/let [annots {:cljs.analyzer/type type
                      :cljs.analyzer/protocol-impl true
                      :cljs.analyzer/protocol-inline inline}]
     (core/loop [ret [] specs specs]
       (if (seq specs)
         (core/let [p     (first specs)
                    ret   (core/-> (conj ret p)
                            (into (reduce (partial annotate-specs annots) []
                                    (group-by first (take-while seq? (next specs))))))
                    specs (drop-while seq? (next specs))]
           (recur ret specs))
         ret)))))

(core/defn- collect-protocols [impls env]
  (core/->> impls
      (filter core/symbol?)
      (map #(:name (cljs.analyzer/resolve-var (dissoc env :locals) %)))
      (into #{})))

(core/defn- build-positional-factory
  [rsym rname fields]
  (core/let [fn-name (with-meta (symbol (core/str '-> rsym))
                       (assoc (meta rsym) :factory :positional))
             docstring (core/str "Positional factory function for " rname ".")
        field-values (if (core/-> rsym meta :internal-ctor) (conj fields nil nil nil) fields)]
    `(defn ~fn-name
       ~docstring
       [~@fields]
       (new ~rname ~@field-values))))

(core/defn- validate-fields
  [case name fields]
  (core/when-not (vector? fields)
    (throw
      #?(:clj (AssertionError. (core/str case " " name ", no fields vector given."))
         :cljs (js/Error. (core/str case " " name ", no fields vector given."))))))

(core/defmacro deftype
  "(deftype name [fields*]  options* specs*)

  Currently there are no options.

  Each spec consists of a protocol or interface name followed by zero
  or more method bodies:

  protocol-or-Object
  (methodName [args*] body)*

  The type will have the (by default, immutable) fields named by
  fields, which can have type hints. Protocols and methods
  are optional. The only methods that can be supplied are those
  declared in the protocols/interfaces.  Note that method bodies are
  not closures, the local environment includes only the named fields,
  and those fields can be accessed directly. Fields can be qualified
  with the metadata :mutable true at which point (set! afield aval) will be
  supported in method bodies. Note well that mutable fields are extremely
  difficult to use correctly, and are present only to facilitate the building
  of higherlevel constructs, such as ClojureScript's reference types, in
  ClojureScript itself. They are for experts only - if the semantics and
  implications of :mutable are not immediately apparent to you, you should not
  be using them.

  Method definitions take the form:

  (methodname [args*] body)

  The argument and return types can be hinted on the arg and
  methodname symbols. If not supplied, they will be inferred, so type
  hints should be reserved for disambiguation.

  Methods should be supplied for all methods of the desired
  protocol(s). You can also define overrides for methods of Object. Note that
  a parameter must be supplied to correspond to the target object
  ('this' in JavaScript parlance). Note also that recur calls to the method
  head should *not* pass the target object, it will be supplied
  automatically and can not be substituted.

  In the method bodies, the (unqualified) name can be used to name the
  class (for calls to new, instance? etc).

  One constructor will be defined, taking the designated fields.  Note
  that the field names __meta and __extmap are currently reserved and
  should not be used when defining your own types.

  Given (deftype TypeName ...), a factory function called ->TypeName
  will be defined, taking positional parameters for the fields"
  [t fields & impls]
  (validate-fields "deftype" t fields)
  (core/let [env &env
             r (:name (cljs.analyzer/resolve-var (dissoc env :locals) t))
             [fpps pmasks] (prepare-protocol-masks env impls)
             protocols (collect-protocols impls env)
             t (vary-meta t assoc
                 :protocols protocols
                 :skip-protocol-flag fpps) ]
    `(do
       (deftype* ~t ~fields ~pmasks
         ~(if (seq impls)
            `(extend-type ~t ~@(dt->et t impls fields))))
       (set! (.-getBasis ~t) (fn [] '[~@fields]))
       (set! (.-cljs$lang$type ~t) true)
       (set! (.-cljs$lang$ctorStr ~t) ~(core/str r))
       (set! (.-cljs$lang$ctorPrWriter ~t) (fn [this# writer# opt#] (-write writer# ~(core/str r))))

       ~(build-positional-factory t r fields)
       ~t)))

(core/defn- emit-defrecord
  "Do not use this directly - use defrecord"
  [env tagname rname fields impls]
  (core/let [hinted-fields fields
             fields (vec (map #(with-meta % nil) fields))
             base-fields fields
             pr-open (core/str "#" #?(:clj  (.getNamespace rname)
                                      :cljs (namespace rname))
                               "." #?(:clj  (.getName rname)
                                      :cljs (name rname))
                               "{")
             fields (conj fields '__meta '__extmap (with-meta '__hash {:mutable true}))]
    (core/let [gs (gensym)
               ksym (gensym "k")
               impls (concat
                       impls
                       ['IRecord
                        'ICloneable
                        `(~'-clone [this#] (new ~tagname ~@fields))
                        'IHash
                        `(~'-hash [this#]
                           (caching-hash this#
                             (fn [coll#]
                               (bit-xor
                                 ~(hash (core/-> rname comp/munge core/str))
                                 (hash-unordered-coll coll#)))
                             ~'__hash))
                        'IEquiv
                        (core/let [this (gensym 'this) other (gensym 'other)]
                          `(~'-equiv [~this ~other]
                             (and (some? ~other)
                                  (identical? (.-constructor ~this)
                                              (.-constructor ~other))
                                  ~@(map (core/fn [field]
                                           `(= (.. ~this ~(to-property field))
                                               (.. ~(with-meta other {:tag tagname}) ~(to-property field))))
                                         base-fields)
                                  (= (.-__extmap ~this)
                                     (.-__extmap ~(with-meta other {:tag tagname}))))))
                        'IMeta
                        `(~'-meta [this#] ~'__meta)
                        'IWithMeta
                        `(~'-with-meta [this# ~gs] (new ~tagname ~@(replace {'__meta gs} fields)))
                        'ILookup
                        `(~'-lookup [this# k#] (-lookup this# k# nil))
                        `(~'-lookup [this# ~ksym else#]
                           (case ~ksym
                             ~@(mapcat (core/fn [f] [(keyword f) f]) base-fields)
                             (cljs.core/get ~'__extmap ~ksym else#)))
                        'ICounted
                        `(~'-count [this#] (+ ~(count base-fields) (count ~'__extmap)))
                        'ICollection
                        `(~'-conj [this# entry#]
                           (if (vector? entry#)
                             (-assoc this# (-nth entry# 0) (-nth entry# 1))
                             (reduce -conj
                               this#
                               entry#)))
                        'IAssociative
                        `(~'-assoc [this# k# ~gs]
                           (condp keyword-identical? k#
                             ~@(mapcat (core/fn [fld]
                                         [(keyword fld) (list* `new tagname (replace {fld gs '__hash nil} fields))])
                                 base-fields)
                             (new ~tagname ~@(remove #{'__extmap '__hash} fields) (assoc ~'__extmap k# ~gs) nil)))
                        'IMap
                        `(~'-dissoc [this# k#] (if (contains? #{~@(map keyword base-fields)} k#)
                                                 (dissoc (-with-meta (into {} this#) ~'__meta) k#)
                                                 (new ~tagname ~@(remove #{'__extmap '__hash} fields)
                                                   (not-empty (dissoc ~'__extmap k#))
                                                   nil)))
                        'ISeqable
                        `(~'-seq [this#] (seq (concat [~@(map #(core/list 'cljs.core/MapEntry. (keyword %) % nil) base-fields)]
                                                ~'__extmap)))

                        'IIterable
                        `(~'-iterator [~gs]
                          (RecordIter. 0 ~gs ~(count base-fields) [~@(map keyword base-fields)] (if ~'__extmap
                                                                                                  (-iterator ~'__extmap)
                                                                                                  (core/nil-iter))))

                        'IPrintWithWriter
                        `(~'-pr-writer [this# writer# opts#]
                           (let [pr-pair# (fn [keyval#] (pr-sequential-writer writer# (~'js* "cljs.core.pr_writer") "" " " "" opts# keyval#))]
                             (pr-sequential-writer
                               writer# pr-pair# ~pr-open ", " "}" opts#
                               (concat [~@(map #(core/list `vector (keyword %) %) base-fields)]
                                 ~'__extmap))))
                        'IKVReduce
                        `(~'-kv-reduce [this# f# init#]
                           (reduce (fn [ret# [k# v#]] (f# ret# k# v#)) init# this#))
                        ])
               [fpps pmasks] (prepare-protocol-masks env impls)
               protocols (collect-protocols impls env)
               tagname (vary-meta tagname assoc
                         :protocols protocols
                         :skip-protocol-flag fpps)]
      `(do
         (~'defrecord* ~tagname ~hinted-fields ~pmasks
           (extend-type ~tagname ~@(dt->et tagname impls fields true)))))))

(core/defn- build-map-factory [rsym rname fields]
  (core/let [fn-name (with-meta (symbol (core/str 'map-> rsym))
                       (assoc (meta rsym) :factory :map))
             docstring (core/str "Factory function for " rname ", taking a map of keywords to field values.")
             ms (gensym)
             ks (map keyword fields)
             getters (map (core/fn [k] `(~k ~ms)) ks)]
    `(defn ~fn-name ~docstring [~ms]
       (let [extmap# (cond->> (dissoc ~ms ~@ks)
                        (record? ~ms) (into {}))]
         (new ~rname ~@getters nil (not-empty extmap#) nil)))))

(core/defmacro defrecord
  "(defrecord name [fields*]  options* specs*)

  Currently there are no options.

  Each spec consists of a protocol or interface name followed by zero
  or more method bodies:

  protocol-or-Object
  (methodName [args*] body)*

  The record will have the (immutable) fields named by
  fields, which can have type hints. Protocols and methods
  are optional. The only methods that can be supplied are those
  declared in the protocols.  Note that method bodies are
  not closures, the local environment includes only the named fields,
  and those fields can be accessed directly.

  Method definitions take the form:

  (methodname [args*] body)

  The argument and return types can be hinted on the arg and
  methodname symbols. If not supplied, they will be inferred, so type
  hints should be reserved for disambiguation.

  Methods should be supplied for all methods of the desired
  protocol(s). You can also define overrides for
  methods of Object. Note that a parameter must be supplied to
  correspond to the target object ('this' in JavaScript parlance). Note also
  that recur calls to the method head should *not* pass the target object, it
  will be supplied automatically and can not be substituted.

  In the method bodies, the (unqualified) name can be used to name the
  class (for calls to new, instance? etc).

  The type will have implementations of several ClojureScript
  protocol generated automatically: IMeta/IWithMeta (metadata support) and
  IMap, etc.

  In addition, defrecord will define type-and-value-based =,
  and will define ClojureScript IHash and IEquiv.

  Two constructors will be defined, one taking the designated fields
  followed by a metadata map (nil for none) and an extension field
  map (nil for none), and one taking only the fields (using nil for
  meta and extension fields). Note that the field names __meta
  and __extmap are currently reserved and should not be used when
  defining your own records.

  Given (defrecord TypeName ...), two factory functions will be
  defined: ->TypeName, taking positional parameters for the fields,
  and map->TypeName, taking a map of keywords to field values."
  [rsym fields & impls]
  (validate-fields "defrecord" rsym fields)
  (core/let [rsym (vary-meta rsym assoc :internal-ctor true)
             r    (vary-meta
                    (:name (cljs.analyzer/resolve-var (dissoc &env :locals) rsym))
                    assoc :internal-ctor true)]
    `(let []
       ~(emit-defrecord &env rsym r fields impls)
       (set! (.-getBasis ~r) (fn [] '[~@fields]))
       (set! (.-cljs$lang$type ~r) true)
       (set! (.-cljs$lang$ctorPrSeq ~r) (fn [this#] (cljs.core/list ~(core/str r))))
       (set! (.-cljs$lang$ctorPrWriter ~r) (fn [this# writer#] (-write writer# ~(core/str r))))
       ~(build-positional-factory rsym r fields)
       ~(build-map-factory rsym r fields)
       ~r)))

(core/defmacro defprotocol
  "A protocol is a named set of named methods and their signatures:

  (defprotocol AProtocolName
    ;optional doc string
    \"A doc string for AProtocol abstraction\"

  ;method signatures
    (bar [this a b] \"bar docs\")
    (baz [this a] [this a b] [this a b c] \"baz docs\"))

  No implementations are provided. Docs can be specified for the
  protocol overall and for each method. The above yields a set of
  polymorphic functions and a protocol object. All are
  namespace-qualified by the ns enclosing the definition The resulting
  functions dispatch on the type of their first argument, which is
  required and corresponds to the implicit target object ('this' in
  JavaScript parlance). defprotocol is dynamic, has no special compile-time
  effect, and defines no new types.

  (defprotocol P
    (foo [this])
    (bar-me [this] [this y]))

  (deftype Foo [a b c]
    P
    (foo [this] a)
    (bar-me [this] b)
    (bar-me [this y] (+ c y)))

  (bar-me (Foo. 1 2 3) 42)
  => 45

  (foo
    (let [x 42]
      (reify P
        (foo [this] 17)
        (bar-me [this] x)
        (bar-me [this y] x))))
  => 17"
  [psym & doc+methods]
  (core/let [p (:name (cljs.analyzer/resolve-var (dissoc &env :locals) psym))
             [opts methods]
             (core/loop [opts {:protocol-symbol true}
                         methods []
                         sigs doc+methods]
               (core/if-not (seq sigs)
                 [opts methods]
                 (core/let [[head & tail] sigs]
                   (core/cond
                     (core/string? head)
                     (recur (assoc opts :doc head) methods tail)
                     (core/keyword? head)
                     (recur (assoc opts head (first tail)) methods (rest tail))
                     (core/list? head)
                     (recur opts (conj methods head) tail)
                     :else
                     (throw #?(:clj  (Exception.
                                       (core/str "Invalid protocol, " psym " received unexpected argument"))
                               :cljs (js/Error.
                                       (core/str "Invalid protocol, " psym " received unexpected argument"))))
                     ))))
             psym (vary-meta psym merge opts)
             ns-name (core/-> &env :ns :name)
             fqn (core/fn [n] (symbol (core/str ns-name) (core/str n)))
             prefix (protocol-prefix p)
             _ (core/doseq [[mname & arities] methods]
                 (core/when (some #{0} (map count (filter vector? arities)))
                   (throw
                     #?(:clj (Exception.
                               (core/str "Invalid protocol, " psym
                                 " defines method " mname " with arity 0"))
                        :cljs (js/Error.
                                (core/str "Invalid protocol, " psym
                                  " defines method " mname " with arity 0"))))))
             expand-sig (core/fn [fname slot sig]
                          (core/let [sig (core/if-not (every? core/symbol? sig)
                                           (mapv (core/fn [arg]
                                                   (core/cond
                                                     (core/symbol? arg) arg
                                                     (core/and (map? arg) (core/some? (:as arg))) (:as arg)
                                                     :else (gensym))) sig)
                                           sig)

                                     fqn-fname (fqn fname)
                                     fsig (first sig)

                                     ;; construct protocol checks in reverse order
                                     ;; check the.protocol/fn["_"] for default impl last
                                     check
                                     `(let [m# (unchecked-get ~fqn-fname "_")]
                                        (if-not (nil? m#)
                                          (m# ~@sig)
                                          (throw
                                            (missing-protocol
                                              ~(core/str psym "." fname) ~fsig))))

                                     ;; then check protocol fn in metadata (only when protocol is marked with :extend-via-metadata true)
                                     check
                                     (core/if-not (:extend-via-metadata opts)
                                       check
                                       `(if-let [meta-impl# (-> ~fsig (core/meta) (core/get '~fqn-fname))]
                                          (meta-impl# ~@sig)
                                          ~check))

                                     ;; then check protocol on js string,function,array,object
                                     check
                                     `(let [x# (if (nil? ~fsig) nil ~fsig)
                                            m# (unchecked-get ~fqn-fname (goog/typeOf x#))]
                                        (if-not (nil? m#)
                                          (m# ~@sig)
                                          ~check))

                                     ;; then check protocol property on object (first check actually executed)
                                     check
                                     `(if (and (not (nil? ~fsig))
                                               (not (nil? (. ~fsig ~(symbol (core/str "-" slot)))))) ;; Property access needed here.
                                        (. ~fsig ~slot ~@sig)
                                        ~check)]
                            `(~sig ~check)))
             psym (core/-> psym
                    (vary-meta update-in [:jsdoc] conj
                      "@interface")
                    (vary-meta assoc-in [:protocol-info :methods]
                      (into {}
                        (map
                          (core/fn [[fname & sigs]]
                            (core/let [doc (core/as-> (last sigs) doc
                                             (core/when (core/string? doc) doc))
                                       sigs (take-while vector? sigs)]
                              [(vary-meta fname assoc :doc doc)
                               (vec sigs)]))
                          methods))))
             method (core/fn [[fname & sigs]]
                      (core/let [doc (core/as-> (last sigs) doc
                                       (core/when (core/string? doc) doc))
                                 sigs (take-while vector? sigs)
                                 amp (core/when (some #{'&} (apply concat sigs))
                                       (cljs.analyzer/warning
                                        :protocol-with-variadic-method
                                        &env {:protocol psym :name fname}))
                                 slot (symbol (core/str prefix (munge (name fname))))
                                 fname (vary-meta fname assoc
                                         :protocol p
                                         :doc doc)]
                        `(defn ~fname
                           ~@(map (core/fn [sig]
                                    (expand-sig fname
                                      (symbol (core/str slot "$arity$" (count sig)))
                                      sig))
                               sigs))))]
    `(do
       (set! ~'*unchecked-if* true)
       (def ~psym (~'js* "function(){}"))
       ~@(map method methods)
       (set! ~'*unchecked-if* false))))

(core/defmacro implements?
  "EXPERIMENTAL"
  [psym x]
  (core/let [p          (:name
                          (cljs.analyzer/resolve-var
                            (dissoc &env :locals) psym))
             prefix     (protocol-prefix p)
             xsym       (bool-expr (gensym))
             [part bit] (fast-path-protocols p)
             msym       (symbol
                          (core/str "-cljs$lang$protocol_mask$partition" part "$"))]
    (core/if-not (core/symbol? x)
      `(let [~xsym ~x]
         (if ~xsym
           (if (or ~(if bit `(unsafe-bit-and (. ~xsym ~msym) ~bit) false)
                    (identical? cljs.core/PROTOCOL_SENTINEL (. ~xsym ~(symbol (core/str "-" prefix)))))
             true
             false)
           false))
      `(if-not (nil? ~x)
         (if (or ~(if bit `(unsafe-bit-and (. ~x ~msym) ~bit) false)
                  (identical? cljs.core/PROTOCOL_SENTINEL (. ~x ~(symbol (core/str "-" prefix)))))
           true
           false)
         false))))

(core/defmacro satisfies?
  "Returns true if x satisfies the protocol"
  [psym x]
  (core/let [p          (:name
                          (cljs.analyzer/resolve-var
                            (dissoc &env :locals) psym))
             prefix     (protocol-prefix p)
             xsym       (bool-expr (gensym))
             [part bit] (fast-path-protocols p)
             msym       (symbol
                          (core/str "-cljs$lang$protocol_mask$partition" part "$"))]
    (core/if-not (core/symbol? x)
      `(let [~xsym ~x]
         (if-not (nil? ~xsym)
           (if (or ~(if bit `(unsafe-bit-and (. ~xsym ~msym) ~bit) false)
                    (identical? cljs.core/PROTOCOL_SENTINEL (. ~xsym ~(symbol (core/str "-" prefix)))))
             true
             (if (coercive-not (. ~xsym ~msym))
               (cljs.core/native-satisfies? ~psym ~xsym)
               false))
           (cljs.core/native-satisfies? ~psym ~xsym)))
      `(if-not (nil? ~x)
         (if (or ~(if bit `(unsafe-bit-and (. ~x ~msym) ~bit) false)
                  (identical? cljs.core/PROTOCOL_SENTINEL (. ~x ~(symbol (core/str "-" prefix)))))
           true
           (if (coercive-not (. ~x ~msym))
             (cljs.core/native-satisfies? ~psym ~x)
             false))
         (cljs.core/native-satisfies? ~psym ~x)))))

(core/defmacro lazy-seq
  "Takes a body of expressions that returns an ISeq or nil, and yields
  a ISeqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls."
  [& body]
  `(new cljs.core/LazySeq nil (fn [] ~@body) nil nil))

(core/defmacro delay
  "Takes a body of expressions and yields a Delay object that will
  invoke the body only the first time it is forced (with force or deref/@), and
  will cache the result and return it on all subsequent force
  calls."
  [& body]
  `(new cljs.core/Delay (fn [] ~@body) nil))

(core/defmacro with-redefs
  "binding => var-symbol temp-value-expr

  Temporarily redefines vars while executing the body.  The
  temp-value-exprs will be evaluated and each resulting value will
  replace in parallel the root value of its var.  After the body is
  executed, the root values of all the vars will be set back to their
  old values. Useful for mocking out functions during testing."
  [bindings & body]
  (core/let [names (take-nth 2 bindings)
             vals (take-nth 2 (drop 1 bindings))
             orig-val-syms (map (comp gensym #(core/str % "-orig-val__") name) names)
             temp-val-syms (map (comp gensym #(core/str % "-temp-val__") name) names)
             binds (map core/vector names temp-val-syms)
             resets (reverse (map core/vector names orig-val-syms))
             bind-value (core/fn [[k v]] (core/list 'set! k v))]
    `(let [~@(interleave orig-val-syms names)
           ~@(interleave temp-val-syms vals)]
       ~@(map bind-value binds)
       (try
         ~@body
         (finally
           ~@(map bind-value resets))))))

(core/defmacro binding
  "binding => var-symbol init-expr

  Creates new bindings for the (already-existing) vars, with the
  supplied initial values, executes the exprs in an implicit do, then
  re-establishes the bindings that existed before.  The new bindings
  are made in parallel (unlike let); all init-exprs are evaluated
  before the vars are bound to their new values."
  [bindings & body]
  (core/let [names (take-nth 2 bindings)]
    (cljs.analyzer/confirm-bindings &env names)
    `(with-redefs ~bindings ~@body)))

(core/defmacro condp
  "Takes a binary predicate, an expression, and a set of clauses.
  Each clause can take the form of either:

  test-expr result-expr

  test-expr :>> result-fn

  Note :>> is an ordinary keyword.

  For each clause, (pred test-expr expr) is evaluated. If it returns
  logical true, the clause is a match. If a binary clause matches, the
  result-expr is returned, if a ternary clause matches, its result-fn,
  which must be a unary function, is called with the result of the
  predicate as its argument, the result of that call being the return
  value of condp. A single default expression can follow the clauses,
  and its value will be returned if no clause matches. If no default
  expression is provided and no clause matches, an Error is thrown."
  {:added "1.0"}

  [pred expr & clauses]
  (core/let [gpred (gensym "pred__")
             gexpr (gensym "expr__")
             emit (core/fn emit [pred expr args]
                    (core/let [[[a b c :as clause] more]
                               (split-at (if (= :>> (second args)) 3 2) args)
                               n (count clause)]
                      (core/cond
                        (= 0 n) `(throw (js/Error. (cljs.core/str "No matching clause: " ~expr)))
                        (= 1 n) a
                        (= 2 n) `(if (~pred ~a ~expr)
                                   ~b
                                   ~(emit pred expr more))
                        :else `(if-let [p# (~pred ~a ~expr)]
                                 (~c p#)
                                 ~(emit pred expr more)))))
             gres (gensym "res__")]
    `(let [~gpred ~pred
           ~gexpr ~expr]
       ~(emit gpred gexpr clauses))))

(core/defn- assoc-test [m test expr env]
  (if (contains? m test)
    (throw
      #?(:clj (clojure.core/IllegalArgumentException.
                (core/str "Duplicate case test constant '"
                  test "'"
                  (core/when (:line env)
                    (core/str " on line " (:line env) " "
                      cljs.analyzer/*cljs-file*))))
         :cljs (js/Error.
                 (core/str "Duplicate case test constant '"
                   test "'"
                   (core/when (:line env)
                     (core/str " on line " (:line env) " "
                       cljs.analyzer/*cljs-file*))))))
    (assoc m test expr)))

(core/defn- const? [env x]
  (core/let [m (core/and (core/list? x)
                         (ana/resolve-var env (last x)))]
    (core/when m (core/get m :const))))

(core/defmacro case
  "Takes an expression, and a set of clauses.

  Each clause can take the form of either:

  test-constant result-expr

  (test-constant1 ... test-constantN)  result-expr

  The test-constants are not evaluated. They must be compile-time
  literals, and need not be quoted.  If the expression is equal to a
  test-constant, the corresponding result-expr is returned. A single
  default expression can follow the clauses, and its value will be
  returned if no clause matches. If no default expression is provided
  and no clause matches, an Error is thrown.

  Unlike cond and condp, case does a constant-time dispatch, the
  clauses are not considered sequentially.  All manner of constant
  expressions are acceptable in case, including numbers, strings,
  symbols, keywords, and (ClojureScript) composites thereof. Note that since
  lists are used to group multiple constants that map to the same
  expression, a vector can be used to match a list if needed. The
  test-constants need not be all of the same type."
  [e & clauses]
  (core/let [esym    (gensym)
             default (if (odd? (count clauses))
                       (last clauses)
                       `(throw
                          (js/Error.
                            (cljs.core/str "No matching clause: " ~esym))))
             env     &env
             pairs   (reduce
                       (core/fn [m [test expr]]
                         (core/cond
                           (seq? test)
                           (reduce
                             (core/fn [m test]
                               (core/let [test (if (core/symbol? test)
                                                 (core/list 'quote test)
                                                 test)]
                                 (assoc-test m test expr env)))
                             m test)
                           (core/symbol? test)
                           (assoc-test m (core/list 'quote test) expr env)
                           :else
                           (assoc-test m test expr env)))
                     {} (partition 2 clauses))
             tests   (keys pairs)]
    (core/cond
      (every? (some-fn core/number? core/string? #?(:clj core/char? :cljs (core/fnil core/char? :nonchar)) #(const? env %)) tests)
      (core/let [no-default (if (odd? (count clauses)) (butlast clauses) clauses)
                 tests      (mapv #(if (seq? %) (vec %) [%]) (take-nth 2 no-default))
                 thens      (vec (take-nth 2 (drop 1 no-default)))]
        `(let [~esym ~e] (case* ~esym ~tests ~thens ~default)))

      (every? core/keyword? tests)
      (core/let [no-default (if (odd? (count clauses)) (butlast clauses) clauses)
                 kw-str #(.substring (core/str %) 1)
                 tests (mapv #(if (seq? %) (mapv kw-str %) [(kw-str %)]) (take-nth 2 no-default))
                 thens (vec (take-nth 2 (drop 1 no-default)))]
        `(let [~esym ~e
               ~esym (if (keyword? ~esym) (.-fqn ~(vary-meta esym assoc :tag 'cljs.core/Keyword)) nil)]
           (case* ~esym ~tests ~thens ~default)))

      ;; equality
      :else
      `(let [~esym ~e]
         (cond
           ~@(mapcat (core/fn [[m c]] `((cljs.core/= ~m ~esym) ~c)) pairs)
           :else ~default)))))

(core/defmacro ^:private when-assert [x]
  (core/when *assert* x))

(core/defmacro assert
  "Evaluates expr and throws an exception if it does not evaluate to
  logical true."
  ([x]
     (core/when *assert*
       `(when-not ~x
          (throw (js/Error. ~(core/str "Assert failed: " (core/pr-str x)))))))
  ([x message]
     (core/when *assert*
       `(when-not ~x
          (throw (js/Error.
                  (cljs.core/str "Assert failed: " ~message "\n" ~(core/pr-str x))))))))

(core/defmacro for
  "List comprehension. Takes a vector of one or more
   binding-form/collection-expr pairs, each followed by zero or more
   modifiers, and yields a lazy sequence of evaluations of expr.
   Collections are iterated in a nested fashion, rightmost fastest,
   and nested coll-exprs can refer to bindings created in prior
   binding-forms.  Supported modifiers are: :let [binding-form expr ...],
   :while test, :when test.

  (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)]  [x y]))"
  [seq-exprs body-expr]
  (assert-args for
    (vector? seq-exprs) "a vector for its binding"
    (even? (count seq-exprs)) "an even number of forms in binding vector")
  (core/let [to-groups (core/fn [seq-exprs]
                         (reduce (core/fn [groups [k v]]
                                   (if (core/keyword? k)
                                     (conj (pop groups) (conj (peek groups) [k v]))
                                     (conj groups [k v])))
                           [] (partition 2 seq-exprs)))
             err (core/fn [& msg] (throw (ex-info (apply core/str msg) {})))
             emit-bind (core/fn emit-bind [[[bind expr & mod-pairs]
                                       & [[_ next-expr] :as next-groups]]]
                         (core/let [giter (gensym "iter__")
                                    gxs (gensym "s__")
                                    do-mod (core/fn do-mod [[[k v :as pair] & etc]]
                                             (core/cond
                                               (= k :let) `(let ~v ~(do-mod etc))
                                               (= k :while) `(when ~v ~(do-mod etc))
                                               (= k :when) `(if ~v
                                                              ~(do-mod etc)
                                                              (recur (rest ~gxs)))
                                               (core/keyword? k) (err "Invalid 'for' keyword " k)
                                               next-groups
                                               `(let [iterys# ~(emit-bind next-groups)
                                                      fs# (seq (iterys# ~next-expr))]
                                                  (if fs#
                                                    (concat fs# (~giter (rest ~gxs)))
                                                    (recur (rest ~gxs))))
                                               :else `(cons ~body-expr
                                                        (~giter (rest ~gxs)))))]
                           (if next-groups
                             #_ "not the inner-most loop"
                             `(fn ~giter [~gxs]
                                (lazy-seq
                                  (loop [~gxs ~gxs]
                                    (when-first [~bind ~gxs]
                                      ~(do-mod mod-pairs)))))
                             #_"inner-most loop"
                             (core/let [gi (gensym "i__")
                                        gb (gensym "b__")
                                        do-cmod (core/fn do-cmod [[[k v :as pair] & etc]]
                                                  (core/cond
                                                    (= k :let) `(let ~v ~(do-cmod etc))
                                                    (= k :while) `(when ~v ~(do-cmod etc))
                                                    (= k :when) `(if ~v
                                                                   ~(do-cmod etc)
                                                                   (recur
                                                                     (unchecked-inc ~gi)))
                                                    (core/keyword? k)
                                                    (err "Invalid 'for' keyword " k)
                                                    :else
                                                    `(do (chunk-append ~gb ~body-expr)
                                                         (recur (unchecked-inc ~gi)))))]
                               `(fn ~giter [~gxs]
                                  (lazy-seq
                                    (loop [~gxs ~gxs]
                                      (when-let [~gxs (seq ~gxs)]
                                        (if (chunked-seq? ~gxs)
                                          (let [c# ^not-native (chunk-first ~gxs)
                                                size# (count c#)
                                                ~gb (chunk-buffer size#)]
                                            (if (coercive-boolean
                                                  (loop [~gi 0]
                                                    (if (< ~gi size#)
                                                      (let [~bind (-nth c# ~gi)]
                                                        ~(do-cmod mod-pairs))
                                                      true)))
                                              (chunk-cons
                                                (chunk ~gb)
                                                (~giter (chunk-rest ~gxs)))
                                              (chunk-cons (chunk ~gb) nil)))
                                          (let [~bind (first ~gxs)]
                                            ~(do-mod mod-pairs)))))))))))]
    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
       (iter# ~(second seq-exprs)))))

(core/defmacro doseq
  "Repeatedly executes body (presumably for side-effects) with
  bindings and filtering as provided by \"for\".  Does not retain
  the head of the sequence. Returns nil."
  [seq-exprs & body]
  (assert-args doseq
    (vector? seq-exprs) "a vector for its binding"
    (even? (count seq-exprs)) "an even number of forms in binding vector")
  (core/let [err (core/fn [& msg] (throw (ex-info (apply core/str msg) {})))
             step (core/fn step [recform exprs]
                    (core/if-not exprs
                      [true `(do ~@body nil)]
                      (core/let [k (first exprs)
                                 v (second exprs)

                                 seqsym (gensym "seq__")
                                 recform (if (core/keyword? k) recform `(recur (next ~seqsym) nil 0 0))
                                 steppair (step recform (nnext exprs))
                                 needrec (steppair 0)
                                 subform (steppair 1)]
                        (core/cond
                          (= k :let) [needrec `(let ~v ~subform)]
                          (= k :while) [false `(when ~v
                                                 ~subform
                                                 ~@(core/when needrec [recform]))]
                          (= k :when) [false `(if ~v
                                                (do
                                                  ~subform
                                                  ~@(core/when needrec [recform]))
                                                ~recform)]
                          (core/keyword? k) (err "Invalid 'doseq' keyword" k)
                          :else (core/let [chunksym (with-meta (gensym "chunk__")
                                                      {:tag 'not-native})
                                           countsym (gensym "count__")
                                           isym     (gensym "i__")
                                           recform-chunk  `(recur ~seqsym ~chunksym ~countsym (unchecked-inc ~isym))
                                           steppair-chunk (step recform-chunk (nnext exprs))
                                           subform-chunk  (steppair-chunk 1)]
                                  [true `(loop [~seqsym   (seq ~v)
                                                ~chunksym nil
                                                ~countsym 0
                                                ~isym     0]
                                           (if (coercive-boolean (< ~isym ~countsym))
                                             (let [~k (-nth ~chunksym ~isym)]
                                               ~subform-chunk
                                               ~@(core/when needrec [recform-chunk]))
                                             (when-let [~seqsym (seq ~seqsym)]
                                               (if (chunked-seq? ~seqsym)
                                                 (let [c# (chunk-first ~seqsym)]
                                                   (recur (chunk-rest ~seqsym) c#
                                                     (count c#) 0))
                                                 (let [~k (first ~seqsym)]
                                                   ~subform
                                                   ~@(core/when needrec [recform]))))))])))))]
    (nth (step nil (seq seq-exprs)) 1)))

(core/defmacro array [& rest]
  (core/let [xs-str (core/->> (repeat "~{}")
                      (take (count rest))
                      (interpose ",")
                      (apply core/str))]
    (vary-meta
      (list* 'js* (core/str "[" xs-str "]") rest)
      assoc :tag 'array)))

(core/defmacro make-array
  ([size]
   (vary-meta
     (if (core/number? size)
       `(array ~@(take size (repeat nil)))
       `(js/Array. ~size))
     assoc :tag 'array))
  ([type size]
   `(cljs.core/make-array ~size))
  ([type size & more-sizes]
   (vary-meta
     `(let [dims#     (list ~@more-sizes)
            dimarray# (cljs.core/make-array ~size)]
        (dotimes [i# (alength dimarray#)]
          (aset dimarray# i# (apply cljs.core/make-array nil dims#)))
        dimarray#)
     assoc :tag 'array)))

(core/defmacro list
  ([]
   '(.-EMPTY cljs.core/List))
  ([x]
   `(cljs.core/List. nil ~x nil 1 nil))
  ([x & xs]
   (core/let [cnt (core/inc (count xs))]
     `(cljs.core/List. nil ~x (list ~@xs) ~cnt nil))))

(core/defmacro vector
  ([] '(.-EMPTY cljs.core/PersistentVector))
  ([& xs]
   (core/let [cnt (count xs)]
     (if (core/< cnt 32)
       `(cljs.core/PersistentVector. nil ~cnt 5
          (.-EMPTY-NODE cljs.core/PersistentVector) (array ~@xs) nil)
       (vary-meta
         `(.fromArray cljs.core/PersistentVector (array ~@xs) true)
         assoc :tag 'cljs.core/PersistentVector)))))

(core/defmacro array-map
  ([] '(.-EMPTY cljs.core/PersistentArrayMap))
  ([& kvs]
   (core/let [keys (map first (partition 2 kvs))]
     (if (core/and (every? #(= (:op (cljs.analyzer/unwrap-quote %)) :const)
                     (map #(cljs.analyzer/no-warn (cljs.analyzer/analyze &env %)) keys))
           (= (count (into #{} keys)) (count keys)))
       `(cljs.core/PersistentArrayMap. nil ~(clojure.core// (count kvs) 2) (array ~@kvs) nil)
       `(.createAsIfByAssoc cljs.core/PersistentArrayMap (array ~@kvs))))))

(core/defmacro hash-map
  ([] `(.-EMPTY cljs.core/PersistentHashMap))
  ([& kvs]
   (core/let [pairs (partition 2 kvs)
              ks    (map first pairs)
              vs    (map second pairs)]
     (vary-meta
       `(.fromArrays cljs.core/PersistentHashMap (array ~@ks) (array ~@vs))
       assoc :tag 'cljs.core/PersistentHashMap))))

(core/defmacro hash-set
  ([] `(.-EMPTY cljs.core/PersistentHashSet))
  ([& xs]
    (if (core/and (core/<= (count xs) 8)
                  (every? #(= (:op (cljs.analyzer/unwrap-quote %)) :const)
                    (map #(cljs.analyzer/no-warn (cljs.analyzer/analyze &env %)) xs))
                  (= (count (into #{} xs)) (count xs)))
      `(cljs.core/PersistentHashSet. nil
         (cljs.core/PersistentArrayMap. nil ~(count xs) (array ~@(interleave xs (repeat nil))) nil)
         nil)
      (vary-meta
        `(.createAsIfByAssoc cljs.core/PersistentHashSet (array ~@xs))
        assoc :tag 'cljs.core/PersistentHashSet))))

(core/defn- js-obj* [kvs]
  (core/let [kvs-str (core/->> (repeat "~{}:~{}")
                       (take (count kvs))
                       (interpose ",")
                       (apply core/str))]
    (vary-meta
      (list* 'js* (core/str "({" kvs-str "})") (apply concat kvs))
      assoc :tag 'object)))

(core/defmacro js-obj [& rest]
  (core/let [sym-or-str? (core/fn [x] (core/or (core/symbol? x) (core/string? x)))
             filter-on-keys (core/fn [f coll]
                              (core/->> coll
                                (filter (core/fn [[k _]] (f k)))
                                (into {})))
             kvs (into {} (map vec (partition 2 rest)))
             sym-pairs (filter-on-keys core/symbol? kvs)
             expr->local (zipmap
                           (filter (complement sym-or-str?) (keys kvs))
                           (repeatedly gensym))
             obj (gensym "obj")]
    (if (empty? rest)
      (js-obj* '())
      `(let [~@(apply concat (clojure.set/map-invert expr->local))
            ~obj ~(js-obj* (filter-on-keys core/string? kvs))]
        ~@(map (core/fn [[k v]] `(goog.object/set ~obj ~k ~v)) sym-pairs)
        ~@(map (core/fn [[k v]] `(goog.object/set ~obj ~v ~(core/get kvs k))) expr->local)
        ~obj))))

(core/defmacro alength [a]
  (vary-meta
    (core/list 'js* "~{}.length" a)
    assoc :tag 'number))

(core/defmacro amap
  "Maps an expression across an array a, using an index named idx, and
  return value named ret, initialized to a clone of a, then setting
  each element of ret to the evaluation of expr, returning the new
  array ret."
  [a idx ret expr]
  `(let [a# ~a
         l# (alength a#)
         ~ret (cljs.core/aclone a#)]
     (loop  [~idx 0]
       (if (< ~idx l#)
         (do
           (aset ~ret ~idx ~expr)
           (recur (inc ~idx)))
         ~ret))))

(core/defmacro areduce
  "Reduces an expression across an array a, using an index named idx,
  and return value named ret, initialized to init, setting ret to the
  evaluation of expr at each step, returning ret."
  [a idx ret init expr]
  `(let [a# ~a
         l# (alength a#)]
     (loop  [~idx 0 ~ret ~init]
       (if (< ~idx l#)
         (recur (inc ~idx) ~expr)
         ~ret))))

(core/defmacro dotimes
  "bindings => name n

  Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  [bindings & body]
  (core/let [i (first bindings)
             n (second bindings)]
    `(let [n# ~n]
       (loop [~i 0]
         (when (< ~i n#)
           ~@body
           (recur (inc ~i)))))))

(core/defn- check-valid-options
  "Throws an exception if the given option map contains keys not listed
  as valid, else returns nil."
  [options & valid-keys]
  (core/when (seq (apply disj (apply core/hash-set (keys options)) valid-keys))
    (throw
      (apply core/str "Only these options are valid: "
        (first valid-keys)
        (map #(core/str ", " %) (rest valid-keys))))))

(core/defmacro defmulti
  "Creates a new multimethod with the associated dispatch function.
  The docstring and attribute-map are optional.

  Options are key-value pairs and may be one of:
    :default    the default dispatch value, defaults to :default
    :hierarchy  the isa? hierarchy to use for dispatching
                defaults to the global hierarchy"
  {:arglists '([name docstring? attr-map? dispatch-fn & options])}
  [mm-name & options]
  (core/let [docstring   (if (core/string? (first options))
                           (first options)
                           nil)
             options     (if (core/string? (first options))
                           (next options)
                           options)
             m           (if (map? (first options))
                           (first options)
                           {})
             options     (if (map? (first options))
                           (next options)
                           options)
             dispatch-fn (first options)
             options     (next options)
             m           (if docstring
                           (assoc m :doc docstring)
                           m)
             m           (if (meta mm-name)
                           (conj (meta mm-name) m)
                           m)
             mm-ns (core/-> &env :ns :name core/str)]
    (core/when (= (count options) 1)
      (throw
        #?(:clj (Exception. "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)")
           :cljs (js/Error. "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)"))))
    (core/let [options (apply core/hash-map options)
               default (core/get options :default :default)]
      (check-valid-options options :default :hierarchy)
      `(defonce ~(with-meta mm-name m)
         (let [method-table# (atom {})
               prefer-table# (atom {})
               method-cache# (atom {})
               cached-hierarchy# (atom {})
               hierarchy# (cljs.core/get ~options :hierarchy ((~'js* "cljs.core.get_global_hierarchy")))]
           (cljs.core/MultiFn. (cljs.core/symbol ~mm-ns ~(name mm-name)) ~dispatch-fn ~default hierarchy#
             method-table# prefer-table# method-cache# cached-hierarchy#))))))

(core/defmacro defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  [multifn dispatch-val & fn-tail]
  `(-add-method ~(with-meta multifn {:tag 'cljs.core/MultiFn}) ~dispatch-val (fn ~@fn-tail)))

(core/defmacro time
  "Evaluates expr and prints the time it took. Returns the value of expr."
  [expr]
  `(let [start# (system-time)
         ret# ~expr]
     (prn (cljs.core/str "Elapsed time: "
            (.toFixed (- (system-time) start#) 6)
            " msecs"))
     ret#))

(core/defmacro simple-benchmark
  "Runs expr iterations times in the context of a let expression with
  the given bindings, then prints out the bindings and the expr
  followed by number of iterations and total time. The optional
  argument print-fn, defaulting to println, sets function used to
  print the result. expr's string representation will be produced
  using pr-str in any case."
  [bindings expr iterations & {:keys [print-fn] :or {print-fn 'println}}]
  (core/let [bs-str   (pr-str bindings)
             expr-str (pr-str expr)]
    `(let ~bindings
       (let [start#   (.getTime (js/Date.))
             ret#     (dotimes [_# ~iterations] ~expr)
             end#     (.getTime (js/Date.))
             elapsed# (- end# start#)]
         (~print-fn (str ~bs-str ", " ~expr-str ", "
                      ~iterations " runs, " elapsed# " msecs"))))))

(def ^:private cs (into [] (map (comp gensym core/str core/char) (range 97 118))))

(core/defn- gen-apply-to-helper
  ([] (gen-apply-to-helper 1))
  ([n]
   (if (core/<= n 20)
     `(let [~(cs (core/dec n)) (-first ~'args)
            ~'args (-rest ~'args)]
        (if (== ~'argc ~n)
          (~'f ~@(take n cs))
          ~(gen-apply-to-helper (core/inc n))))
     `(throw (js/Error. "Only up to 20 arguments supported on functions")))))

(core/defmacro gen-apply-to []
  `(do
     (set! ~'*unchecked-if* true)
     (defn ~'apply-to [~'f ~'argc ~'args]
       (let [~'args (seq ~'args)]
         (if (zero? ~'argc)
           (~'f)
           ~(gen-apply-to-helper))))
     (set! ~'*unchecked-if* false)))

(core/defn- gen-apply-to-simple-helper
  [f num-args args]
  (core/let [new-arg-sym (symbol (core/str "a" num-args))
             proto-name (core/str "cljs$core$IFn$_invoke$arity$" (core/inc num-args))
             proto-prop (symbol (core/str ".-" proto-name))
             proto-inv (symbol (core/str "." proto-name))
             next-sym (symbol (core/str "next_" num-args))
             all-args (mapv #(symbol (core/str "a" %)) (range (core/inc num-args)))]
    `(let [~new-arg-sym (cljs.core/-first ~args)
           ~next-sym (cljs.core/next ~args)]
       (if (nil? ~next-sym)
         (if (~proto-prop ~f)
           (~proto-inv ~f ~@all-args)
           (.call ~f ~f ~@all-args))
         ~(if (core/<= 19 num-args)
            ;; We've exhausted all protocols, fallback to .apply:
            `(let [arr# (cljs.core/array ~@all-args)]
               (loop [s# ~next-sym]
                 (when s#
                   (do (.push arr# (cljs.core/-first s#))
                       (recur (cljs.core/next s#)))))
               (.apply ~f ~f arr#))
            (gen-apply-to-simple-helper f (core/inc num-args) next-sym))))))

(core/defmacro gen-apply-to-simple
  [f num-args args]
  (gen-apply-to-simple-helper f num-args args))

(core/defmacro with-out-str
  "Evaluates exprs in a context in which *print-fn* is bound to .append
  on a fresh StringBuffer.  Returns the string created by any nested
  printing calls."
  [& body]
  `(let [sb# (goog.string/StringBuffer.)]
     (binding [cljs.core/*print-newline* true
               cljs.core/*print-fn* (fn [x#] (.append sb# x#))]
       ~@body)
     (cljs.core/str sb#)))

(core/defmacro lazy-cat
  "Expands to code which yields a lazy sequence of the concatenation
  of the supplied colls.  Each coll expr is not evaluated until it is
  needed.

  (lazy-cat xs ys zs) === (concat (lazy-seq xs) (lazy-seq ys) (lazy-seq zs))"
  [& colls]
  `(concat ~@(map #(core/list `lazy-seq %) colls)))

(core/defmacro js-str [s]
  (core/list 'js* "''+~{}" s))

(core/defmacro es6-iterable [ty]
  `(goog.object/set (.-prototype ~ty) cljs.core/ITER_SYMBOL
     (fn []
       (this-as this#
         (cljs.core/es6-iterator this#)))))

(core/defmacro ns-publics
  "Returns a map of the public intern mappings for the namespace."
  [quoted-ns]
  (core/assert (core/and (seq? quoted-ns)
                         (= (first quoted-ns) 'quote)
                         (core/symbol? (second quoted-ns)))
    "Argument to ns-publics must be a quoted symbol")
  (core/let [ns (second quoted-ns)]
    `(into {}
       [~@(map
            (core/fn [[sym _]]
              `[(symbol ~(name sym)) (var ~(symbol (name ns) (name sym)))])
            (filter (core/fn [[_ info]]
                      (not (core/-> info :meta :private)))
              (get-in @env/*compiler* [:cljs.analyzer/namespaces ns :defs])))])))

(core/defmacro ns-imports
  "Returns a map of the import mappings for the namespace."
  [quoted-ns]
  (core/assert (core/and (seq? quoted-ns)
                         (= (first quoted-ns) 'quote)
                         (core/symbol? (second quoted-ns)))
    "Argument to ns-imports must be a quoted symbol")
  (core/let [ns (second quoted-ns)]
    `(into {}
       [~@(map
            (core/fn [[ctor qualified-ctor]]
              `[(symbol ~(name ctor)) ~(symbol qualified-ctor)])
            (get-in @env/*compiler* [:cljs.analyzer/namespaces ns :imports]))])))

(core/defmacro ns-interns
  "Returns a map of the intern mappings for the namespace."
  [quoted-ns]
  (core/assert (core/and (seq? quoted-ns)
                 (= (first quoted-ns) 'quote)
                 (core/symbol? (second quoted-ns)))
    "Argument to ns-interns must be a quoted symbol")
  (core/let [ns (second quoted-ns)]
    `(into {}
       [~@(map
            (core/fn [[sym _]]
              `[(symbol ~(name sym)) (var ~(symbol (name ns) (name sym)))])
            (get-in @env/*compiler* [:cljs.analyzer/namespaces ns :defs]))])))

(core/defmacro ns-unmap
  "Removes the mappings for the symbol from the namespace."
  [quoted-ns quoted-sym]
  (core/assert
    (core/and (seq? quoted-ns) (= (first quoted-ns) 'quote) (core/symbol? (second quoted-ns))
              (seq? quoted-sym) (= (first quoted-sym) 'quote) (core/symbol? (second quoted-sym)))
    "Arguments to ns-unmap must be quoted symbols")
  (core/let [ns (second quoted-ns)
             sym (second quoted-sym)]
    (swap! env/*compiler* update-in [::ana/namespaces ns :defs] dissoc sym)
    `(js-delete ~(comp/munge ns) ~(comp/munge (core/str sym)))))

(core/defmacro vswap!
  "Non-atomically swaps the value of the volatile as if:
   (apply f current-value-of-vol args). Returns the value that
   was swapped in."
  [vol f & args]
  `(-vreset! ~vol (~f (-deref ~vol) ~@args)))

(core/defmacro locking
  [x & forms]
  `(do ~@forms))

;; An internal-use Var for defining specs on the ns special form
(core/defmacro ^:private ns-special-form [])

(core/defmacro require
  "Loads libs, skipping any that are already loaded. Each argument is
  either a libspec that identifies a lib or a flag that modifies how all the identified
  libs are loaded. Use :require in the ns macro in preference to calling this
  directly.

  Libs

  A 'lib' is a named set of resources in classpath whose contents define a
  library of ClojureScript code. Lib names are symbols and each lib is associated
  with a ClojureScript namespace. A lib's name also locates its root directory
  within classpath using Java's package name to classpath-relative path mapping.
  All resources in a lib should be contained in the directory structure under its
  root directory. All definitions a lib makes should be in its associated namespace.

  'require loads a lib by loading its root resource. The root resource path
  is derived from the lib name in the following manner:
  Consider a lib named by the symbol 'x.y.z; it has the root directory
  <classpath>/x/y/, and its root resource is <classpath>/x/y/z.clj. The root
  resource should contain code to create the lib's namespace (usually by using
  the ns macro) and load any additional lib resources.

  Libspecs

  A libspec is a lib name or a vector containing a lib name followed by
  options expressed as sequential keywords and arguments.

  Recognized options:
  :as takes a symbol as its argument and makes that symbol an alias to the
    lib's namespace in the current namespace.
  :refer takes a list of symbols to refer from the namespace.
  :refer-macros takes a list of macro symbols to refer from the namespace.
  :include-macros true causes macros from the namespace to be required.
  :rename specifies a map from referred var names to different
    symbols (and can be used to prevent clashes)


  Flags

  A flag is a keyword.
  Recognized flags: :reload, :reload-all, :verbose
  :reload forces loading of all the identified libs even if they are
    already loaded
  :reload-all implies :reload and also forces loading of all libs that the
    identified libs directly or indirectly load via require or use
  :verbose triggers printing information about each load, alias, and refer

  Example:

  The following would load the library clojure.string :as string.

  (require '[clojure.string :as string])"
  [& args]
  `(~'ns* ~(cons :require args)))

(core/defmacro require-macros
  "Similar to require but only for macros."
  [& args]
  `(~'ns* ~(cons :require-macros args)))

(core/defmacro use
  "Like require, but referring vars specified by the mandatory
  :only option.

  Example:

  The following would load the library clojure.set while referring
  the intersection var.

  (use '[clojure.set :only [intersection]])"
  [& args]
  `(~'ns* ~(cons :use args)))

(core/defmacro use-macros
  "Similar to use but only for macros."
  [& args]
  `(~'ns* ~(cons :use-macros args)))

(core/defmacro import
  "import-list => (closure-namespace constructor-name-symbols*)

  For each name in constructor-name-symbols, adds a mapping from name to the
  constructor named by closure-namespace to the current namespace. Use :import in the ns
  macro in preference to calling this directly."
  [& import-symbols-or-lists]
  `(~'ns* ~(cons :import import-symbols-or-lists)))

(core/defmacro refer-clojure
  "Refers to all the public vars of `cljs.core`, subject to
  filters.
  Filters can include at most one each of:

  :exclude list-of-symbols
  :rename map-of-fromsymbol-tosymbol

  Filters can be used to select a subset, via exclusion, or to provide a mapping
  to a symbol different from the var's name, in order to prevent clashes."
  [& args]
  `(~'ns* ~(cons :refer-clojure args)))

;; INTERNAL - do not use, only for Node.js
(core/defmacro load-file* [f]
  `(goog/nodeGlobalRequire ~f))

(core/defmacro macroexpand-1
  "If form represents a macro form, returns its expansion,
  else returns form."
  [quoted]
  (core/assert (core/= (core/first quoted) 'quote)
    "Argument to macroexpand-1 must be quoted")
  (core/let [form (second quoted)]
    (if (seq? form)
      `(quote ~(ana/macroexpand-1 &env form))
      form)))

(core/defmacro macroexpand
  "Repeatedly calls macroexpand-1 on form until it no longer
  represents a macro form, then returns it.  Note neither
  macroexpand-1 nor macroexpand expand macros in subforms."
  [quoted]
  (core/assert (core/= (core/first quoted) 'quote)
    "Argument to macroexpand must be quoted")
  (core/let [form (second quoted)
             env &env]
    (if (seq? form)
      (core/loop [form form form' (ana/macroexpand-1 env form)]
        (core/if-not (core/identical? form form')
          (recur form' (ana/macroexpand-1 env form'))
          `(quote ~form')))
      form)))

(core/defn- multi-arity-fn? [fdecl]
  (core/< 1 (count fdecl)))

(core/defn- variadic-fn? [fdecl]
  (core/and (= 1 (count fdecl))
            (some '#{&} (ffirst fdecl))))

(core/defn- variadic-fn*
  ([sym method]
   (variadic-fn* sym method true))
  ([sym [arglist & body :as method] solo]
   (core/let [sig (remove '#{&} arglist)
              restarg (gensym "seq")]
     (core/letfn [(get-delegate []
                    'cljs$core$IFn$_invoke$arity$variadic)
                  (get-delegate-prop []
                    (symbol (core/str "-" (get-delegate))))
                  (param-bind [param]
                    `[~param (^::ana/no-resolve first ~restarg)
                      ~restarg (^::ana/no-resolve next ~restarg)])
                  (apply-to []
                    (if (core/< 1 (count sig))
                      (core/let [params (repeatedly (core/dec (count sig)) gensym)]
                        `(fn
                           ([~restarg]
                            (let [~@(mapcat param-bind params)]
                              (this-as self#
                                (. self# (~(get-delegate) ~@params ~restarg)))))))
                      `(fn
                         ([~restarg]
                          (this-as self#
                            (. self# (~(get-delegate) (seq ~restarg))))))))]
       `(do
          (set! (. ~sym ~(get-delegate-prop))
            (fn (~(vec sig) ~@body)))
          ~@(core/when solo
              `[(set! (. ~sym ~'-cljs$lang$maxFixedArity)
                  ~(core/dec (count sig)))])
          (js-inline-comment " @this {Function} ")
          ;; dissoc :top-fn so this helper gets ignored in cljs.analyzer/parse 'set!
          (set! (. ~(vary-meta sym dissoc :top-fn) ~'-cljs$lang$applyTo)
            ~(apply-to)))))))

(core/defmacro copy-arguments [dest]
  `(let [len# (alength (js-arguments))]
     (loop [i# 0]
       (when (< i# len#)
         (.push ~dest (unchecked-get (js-arguments) i#))
         (recur (inc i#))))))

(core/defn- elide-implicit-macro-args [arglists]
  (core/map (core/fn [arglist]
              (if (core/vector? arglist)
                (core/subvec arglist 2)
                (core/drop 2 arglist)))
    arglists))

(core/defn- variadic-fn [name meta [[arglist & body :as method] :as fdecl] emit-var?]
  (core/letfn [(dest-args [c]
                 (map (core/fn [n] `(unchecked-get (js-arguments) ~n))
                   (range c)))]
    (core/let [rname (symbol (core/str ana/*cljs-ns*) (core/str name))
               sig   (remove '#{&} arglist)
               c-1   (core/dec (count sig))
               macro? (:macro meta)
               mfa   (core/cond-> c-1 macro? (core/- 2))
               meta  (assoc meta
                       :top-fn
                       {:variadic? true
                        :fixed-arity mfa
                        :max-fixed-arity mfa
                        :method-params (core/cond-> [sig] macro? elide-implicit-macro-args)
                        :arglists (core/cond-> (core/list arglist) macro? elide-implicit-macro-args)
                        :arglists-meta (doall (map meta [arglist]))})
               name  (with-meta name meta)]
      `(do
         (def ~name
           (fn [~'var_args]
             (let [args# (array)]
               (copy-arguments args#)
               (let [argseq# (when (< ~c-1 (alength args#))
                               (new ^::ana/no-resolve cljs.core/IndexedSeq
                                 (.slice args# ~c-1) 0 nil))]
                 (. ~rname (~'cljs$core$IFn$_invoke$arity$variadic ~@(dest-args c-1) argseq#))))))
         ~(variadic-fn* name method)
         ~(core/when emit-var? `(var ~name))))))

(core/comment
  (require '[clojure.pprint :as pp])
  (pp/pprint (variadic-fn 'foo {} '(([& xs]))))
  (pp/pprint (variadic-fn 'foo {} '(([a & xs] xs))))
  (pp/pprint (variadic-fn 'foo {} '(([a b & xs] xs))))
  (pp/pprint (variadic-fn 'foo {} '(([a [b & cs] & xs] xs))))
  )

(core/defn- multi-arity-fn [name meta fdecl emit-var?]
  (core/letfn [(dest-args [c]
                 (map (core/fn [n] `(unchecked-get (js-arguments) ~n))
                   (range c)))
               (fixed-arity [rname sig]
                 (core/let [c (count sig)]
                   [c `(. ~rname
                         (~(symbol
                             (core/str "cljs$core$IFn$_invoke$arity$" c))
                           ~@(dest-args c)))]))
               (fn-method [name [sig & body :as method]]
                 (if (some '#{&} sig)
                   (variadic-fn* name method false)
                   ;; fix up individual :fn-method meta for
                   ;; cljs.analyzer/parse 'set! :top-fn handling
                   `(set!
                      (. ~(vary-meta name update :top-fn merge
                            {:variadic? false :fixed-arity (count sig)})
                        ~(symbol (core/str "-cljs$core$IFn$_invoke$arity$"
                                   (count sig))))
                      (fn ~method))))]
    (core/let [rname    (symbol (core/str ana/*cljs-ns*) (core/str name))
               arglists (map first fdecl)
               varsig?  #(some '#{&} %)
               variadic (boolean (some varsig? arglists))
               sigs     (remove varsig? arglists)
               maxfa    (apply core/max
                          (concat
                            (map count sigs)
                            [(core/- (count (first (filter varsig? arglists))) 2)]))
               macro?   (:macro meta)
               mfa      (core/cond-> maxfa macro? (core/- 2))
               meta     (assoc meta
                          :top-fn
                          {:variadic? variadic
                           :fixed-arity mfa
                           :max-fixed-arity mfa
                           :method-params (core/cond-> sigs macro? elide-implicit-macro-args)
                           :arglists (core/cond-> arglists macro? elide-implicit-macro-args)
                           :arglists-meta (doall (map meta arglists))})
               args-sym (gensym "args")
               param-counts (map count arglists)
               name     (with-meta name meta)]
      (core/when (not= (distinct param-counts) param-counts)
        (ana/warning :overload-arity {} {:name name}))
      `(do
         (def ~name
           (fn [~'var_args]
             (case (alength (js-arguments))
               ~@(mapcat #(fixed-arity rname %) sigs)
               ~(if variadic
                  `(let [args-arr# (array)]
                     (copy-arguments args-arr#)
                     (let [argseq# (new ^::ana/no-resolve cljs.core/IndexedSeq
                                        (.slice args-arr# ~maxfa) 0 nil)]
                       (. ~rname
                          (~'cljs$core$IFn$_invoke$arity$variadic
                           ~@(dest-args maxfa)
                           argseq#))))
                  (if (:macro meta)
                    `(throw (js/Error.
                             (str "Invalid arity: " (- (alength (js-arguments)) 2))))
                    `(throw (js/Error.
                             (str "Invalid arity: " (alength (js-arguments))))))))))
         ~@(map #(fn-method name %) fdecl)
         ;; optimization properties
         (set! (. ~name ~'-cljs$lang$maxFixedArity) ~maxfa)
         ~(core/when emit-var? `(var ~name))))))

(core/comment
  (require '[clojure.pprint :as pp])
  (pp/pprint (multi-arity-fn 'foo {} '(([a]) ([a b]))))
  (pp/pprint (multi-arity-fn 'foo {} '(([a]) ([a & xs]))))
  (pp/pprint (multi-arity-fn 'foo {} '(([a]) ([a [b & cs] & xs]))))
  ;; CLJS-1216
  (pp/pprint (multi-arity-fn 'foo {} '(([a]) ([a b & xs]))))
  )

(def
  ^{:doc "Same as (def name (core/fn [params* ] exprs*)) or (def
    name (core/fn ([params* ] exprs*)+)) with any doc-string or attrs added
    to the var metadata. prepost-map defines a map with optional keys
    :pre and :post that contain collections of pre or post conditions."
    :arglists '([name doc-string? attr-map? [params*] prepost-map? body]
                 [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])
    :macro true}
  defn (core/fn defn [&form &env name & fdecl]
         ;; Note: Cannot delegate this check to def because of the call to (with-meta name ..)
         (if (core/instance? #?(:clj clojure.lang.Symbol :cljs Symbol) name)
           nil
           (throw
             #?(:clj (IllegalArgumentException. "First argument to defn must be a symbol")
                :cljs (js/Error. "First argument to defn must be a symbol"))))
         (core/let [m (if (core/string? (first fdecl))
                        {:doc (first fdecl)}
                        {})
                    fdecl (if (core/string? (first fdecl))
                            (next fdecl)
                            fdecl)
                    m (if (map? (first fdecl))
                        (conj m (first fdecl))
                        m)
                    fdecl (if (map? (first fdecl))
                            (next fdecl)
                            fdecl)
                    fdecl (if (vector? (first fdecl))
                            (core/list fdecl)
                            fdecl)
                    m (if (map? (last fdecl))
                        (conj m (last fdecl))
                        m)
                    fdecl (if (map? (last fdecl))
                            (butlast fdecl)
                            fdecl)
                    m (conj {:arglists (core/list 'quote (sigs fdecl))} m)
                    ;; no support for :inline
                    ;m (core/let [inline (:inline m)
                    ;             ifn (first inline)
                    ;             iname (second inline)]
                    ;    ;; same as: (if (and (= 'fn ifn) (not (symbol? iname))) ...)
                    ;    (if (if #?(:clj (clojure.lang.Util/equiv 'fn ifn)
                    ;               :cljs (= 'fn ifn))
                    ;          (if #?(:clj (core/instance? clojure.lang.Symbol iname)
                    ;                 :cljs (core/instance? Symbol iname)) false true))
                    ;      ;; inserts the same fn name to the inline fn if it does not have one
                    ;      (assoc m
                    ;        :inline (cons ifn
                    ;                  (cons (clojure.lang.Symbol/intern
                    ;                          (.concat (.getName ^clojure.lang.Symbol name) "__inliner"))
                    ;                    (next inline))))
                    ;      m))
                    m (conj (if (meta name) (meta name) {}) m)]
           (core/cond
             (multi-arity-fn? fdecl)
             (multi-arity-fn name
               (if (comp/checking-types?)
                 (update-in m [:jsdoc] conj "@param {...*} var_args")
                 m) fdecl (:def-emits-var &env))

             (variadic-fn? fdecl)
             (variadic-fn name
               (if (comp/checking-types?)
                 (update-in m [:jsdoc] conj "@param {...*} var_args")
                 m) fdecl (:def-emits-var &env))

             :else
             (core/list 'def (with-meta name m)
               ;;todo - restore propagation of fn name
               ;;must figure out how to convey primitive hints to self calls first
               (cons `fn fdecl))))))

#?(:clj  (. (var defn) (setMacro))
   :cljs (set! (. defn -cljs$lang$macro) true))

(core/defn defmacro
  "Like defn, but the resulting function name is declared as a
  macro and will be used as a macro by the compiler when it is
  called."
  {:arglists '([name doc-string? attr-map? [params*] body]
               [name doc-string? attr-map? ([params*] body)+ attr-map?])
   :macro true}
  [&form &env name & args]
  (core/let [prefix (core/loop [p (core/list (vary-meta name assoc :macro true)) args args]
                      (core/let [f (first args)]
                        (if (core/string? f)
                          (recur (cons f p) (next args))
                          (if (map? f)
                            (recur (cons f p) (next args))
                            p))))
             fdecl (core/loop [fd args]
                     (if (core/string? (first fd))
                       (recur (next fd))
                       (if (map? (first fd))
                         (recur (next fd))
                         fd)))
             fdecl (if (vector? (first fdecl))
                     (core/list fdecl)
                     fdecl)
             add-implicit-args (core/fn [fd]
                                 (core/let [args (first fd)]
                                   (cons (vec (cons '&form (cons '&env args))) (next fd))))
             add-args (core/fn [acc ds]
                        (if (core/nil? ds)
                          acc
                          (core/let [d (first ds)]
                            (if (map? d)
                              (conj acc d)
                              (recur (conj acc (add-implicit-args d)) (next ds))))))
             fdecl (seq (add-args [] fdecl))
             decl (core/loop [p prefix d fdecl]
                    (if p
                      (recur (next p) (cons (first p) d))
                      d))]
    `(let [ret# ~(cons `defn decl)]
       (set! (. ~name ~'-cljs$lang$macro) true)
       ret#)))

#?(:clj  (. (var defmacro) (setMacro))
   :cljs (set! (. defmacro -cljs$lang$macro) true))

(core/defmacro resolve
  "Returns the var to which a symbol will be resolved in the namespace else nil."
  [quoted-sym]
  (core/assert
    (core/and (seq? quoted-sym)
              (= 'quote (first quoted-sym)))
    "Argument to resolve must be a quoted symbol")
  (core/let [sym (second quoted-sym)
             env &env
             [var meta] (try
                          (core/let [var (ana/resolve-var env sym (ana/confirm-var-exists-throw)) ]
                            [var (ana/var-meta var)])
                          (catch #?@(:clj [Throwable t] :cljs [:default e])
                              [(ana/resolve-var env sym) nil]))
             resolved (vary-meta (:name var) assoc ::ana/no-resolve true)]
    `(when (exists? ~resolved)
       (cljs.core/Var. (fn [] ~resolved) '~resolved ~meta))))
