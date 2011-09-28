;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core
  (:refer-clojure :exclude [-> ->> .. amap and areduce assert binding bound-fn case comment cond condp
                            declare definline definterface defmethod defmulti defn defn- defonce
                            defprotocol defrecord defstruct deftype delay doseq dosync dotimes doto
                            extend-protocol extend-type fn for future gen-class gen-interface
                            if-let if-not import io! lazy-cat lazy-seq let letfn locking loop
                            memfn ns or proxy proxy-super pvalues refer-clojure reify sync time
                            when when-first when-let when-not while with-bindings with-in-str
                            with-loading-context with-local-vars with-open with-out-str with-precision with-redefs
                            satisfies?

                            aget aset
                            + - * / < <= > >= == zero? pos? neg? inc dec max min mod
                            bit-and bit-and-not bit-clear bit-flip bit-not bit-or bit-set 
                            bit-test bit-shift-left bit-shift-right bit-xor]))

(alias 'core 'clojure.core)

(defmacro import-macros [ns [& vars]]
  (core/let [ns (find-ns ns)
             vars (map #(ns-resolve ns %) vars)
             syms (map (core/fn [^clojure.lang.Var v] (core/-> v .sym (with-meta {:macro true}))) vars)
             defs (map (core/fn [sym var]
                                `(def ~sym (deref ~var))) syms vars)]
            `(do ~@defs
                 :imported)))

(import-macros clojure.core
 [-> ->> ..  and assert comment cond condp
  declare defn defn-
  doto
  extend-protocol fn for
  if-let if-not let letfn loop
  or
  when when-first when-let when-not while])

(defmacro aget [a i]
  (list 'js* "(~{}[~{}])" a i))

(defmacro aset [a i v]
  (list 'js* "(~{}[~{}] = ~{})" a i v))

(defmacro +
  ([] 0)
  ([x] x)
  ([x y] (list 'js* "(~{} + ~{})" x y))
  ([x y & more] `(+ (+ ~x ~y) ~@more)))

(defmacro -
  ([] 0)
  ([x] x)
  ([x y] (list 'js* "(~{} - ~{})" x y))
  ([x y & more] `(- (- ~x ~y) ~@more)))

(defmacro *
  ([] 1)
  ([x] x)
  ([x y] (list 'js* "(~{} * ~{})" x y))
  ([x y & more] `(* (* ~x ~y) ~@more)))

(defmacro /
  ([] 1)
  ([x] `(/ 1 x))
  ([x y] (list 'js* "(~{} / ~{})" x y))
  ([x y & more] `(/ (/ ~x ~y) ~@more)))

(defmacro <
  ([x] true)
  ([x y] (list 'js* "(~{} < ~{})" x y))
  ([x y & more] `(< (< ~x ~y) ~@more)))

(defmacro <=
  ([x] true)
  ([x y] (list 'js* "(~{} <= ~{})" x y))
  ([x y & more] `(<= (<= ~x ~y) ~@more)))

(defmacro >
  ([x] true)
  ([x y] (list 'js* "(~{} > ~{})" x y))
  ([x y & more] `(> (> ~x ~y) ~@more)))

(defmacro >=
  ([x] true)
  ([x y] (list 'js* "(~{} >= ~{})" x y))
  ([x y & more] `(>= (>= ~x ~y) ~@more)))

(defmacro ==
  ([x] true)
  ([x y] (list 'js* "(~{} === ~{})" x y))
  ([x y & more] `(== (== ~x ~y) ~@more)))

(defmacro dec [x]
  `(- ~x 1))

(defmacro inc [x]
  `(+ ~x 1))

(defmacro zero? [x]
  `(== ~x 0))

(defmacro pos? [x]
  `(> ~x 0))

(defmacro neg? [x]
  `(< ~x 0))

(defmacro max
  ([x] x)
  ([x y] (list 'js* "((~{} > ~{}) ? ~{} : ~{})" x y x y))
  ([x y & more] `(max (max ~x ~y) ~@more)))

(defmacro min
  ([x] x)
  ([x y] (list 'js* "((~{} < ~{}) ? ~{} : ~{})" x y x y))
  ([x y & more] `(min (min ~x ~y) ~@more)))

(defmacro mod [num div]
  (list 'js* "(~{} % ~{})" num div))

(defmacro bit-not [x]
  (list 'js* "(~ ~{})" x))

(defmacro bit-and
  ([x y] (list 'js* "(~{} & ~{})" x y))
  ([x y & more] `(bit-and (bit-and ~x ~y) ~@more)))

(defmacro bit-or
  ([x y] (list 'js* "(~{} | ~{})" x y))
  ([x y & more] `(bit-or (bit-or ~x ~y) ~@more)))

(defmacro bit-xor
  ([x y] (list 'js* "(~{} ^ ~{})" x y))
  ([x y & more] `(bit-xor (bit-xor ~x ~y) ~@more)))

(defmacro bit-and-not
  ([x y] (list 'js* "(~{} & ~~{})" x y))
  ([x y & more] `(bit-and-not (bit-and-not ~x ~y) ~@more)))

(defmacro bit-clear [x n]
  (list 'js* "(~{} & ~(1 << ~{}))" x n))

(defmacro bit-flip [x n]
  (list 'js* "(~{} ^ (1 << ~{}))" x n))

(defmacro bit-test [x n]
  (list 'js* "((~{} & (1 << ~{})) != 0)" x n))

(defmacro bit-shift-left [x n]
  (list 'js* "(~{} << ~{})" x n))

(defmacro bit-shift-right [x n]
  (list 'js* "(~{} >> ~{})" x n))

(defn- protocol-prefix [psym]
  (str (.replace (str psym) \. \$) "$"))

(def #^:private base-type
     {nil "null"
      'object "object"
      'string "string"
      'number "number"
      'array "array"
      'function "function"
      'boolean "boolean"
      'default "_"})

(defmacro reify [& impls]
  (let [t (gensym "t")
        locals (keys (:locals &env))]
   `(do
      (when (undefined? ~t)
        (deftype ~t [~@locals]
          ~@impls))
      (new ~t ~@locals))))

(defmacro extend-type [tsym & impls]
  (let [resolve #(let [ret (:name (cljs.compiler/resolve-var (dissoc &env :locals) %))]
                   (assert ret (str "Can't resolve: " %))
                   ret)
        impl-map (loop [ret {} s impls]
                   (if (seq s)
                     (recur (assoc ret (resolve (first s)) (take-while seq? (next s)))
                            (drop-while seq? (next s)))
                     ret))]
    (if (base-type tsym)
      (let [t (base-type tsym)
            assign-impls (fn [[psym sigs]]
                           (let [pfn-prefix (subs (str psym) 0 (clojure.core/inc (.lastIndexOf (str psym) ".")))]
                             (cons `(aset ~psym ~t true)
                                   (map (fn [[f & meths]]
                                          `(aset ~(symbol (str pfn-prefix f)) ~t (fn* ~@meths)))
                                        sigs))))]
        `(do ~@(mapcat assign-impls impl-map)))
      (let [t (resolve tsym)
            prototype-prefix (str t ".prototype.")
            
            assign-impls (fn [[psym sigs]]
                           (let [pprefix (protocol-prefix psym)]
                             (cons `(set! ~(symbol (str prototype-prefix pprefix)) true)
                                   (map (fn [[f & meths]]
                                          `(set! ~(symbol (str prototype-prefix pprefix f)) (fn* ~@meths)))
                                        sigs))))]
        `(do ~@(mapcat assign-impls impl-map))))))

(defmacro deftype [t fields & impls]
  (let [adorn-params (fn [sig]
                       (cons (vary-meta (second sig) assoc :cljs.compiler/fields fields)
                             (nnext sig)))
        ;;reshape for extend-type
        dt->et (fn [specs]
                 (loop [ret [] s specs]
                   (if (seq s)
                     (recur (-> ret
                                (conj (first s))
                                (into
                                 (reduce (fn [v [f sigs]]
                                           (conj v (cons f (map adorn-params sigs))))
                                         []
                                         (group-by first (take-while seq? (next s))))))
                            (drop-while seq? (next s)))
                     ret)))]
    (if (seq impls)
      `(do
         (deftype* ~t ~fields)
         (extend-type ~t ~@(dt->et impls)))
      `(deftype* ~t ~fields))))

(defn- emit-defrecord
   "Do not use this directly - use defrecord"
  [tagname rname fields impls]
  (let [hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        base-fields fields
	fields (conj fields '__meta '__extmap)
	adorn-params (fn [sig]
                       (cons (vary-meta (second sig) assoc :cljs.compiler/fields fields)
                             (nnext sig)))
        ;;reshape for extend-type
        dt->et (fn [specs]
                 (loop [ret [] s specs]
                   (if (seq s)
                     (recur (-> ret
                                (conj (first s))
                                (into
                                 (reduce (fn [v [f sigs]]
                                           (conj v (cons f (map adorn-params sigs))))
                                         []
                                         (group-by first (take-while seq? (next s))))))
                            (drop-while seq? (next s)))
                     ret)))]
    (let [gs (gensym)
	  impls (concat
		 impls
		 ['IRecord
		  'IHash
		  `(~'-hash [this#] (hash-coll this#))
		  'IEquiv
		  `(~'-equiv [this# other#] (equiv-map this# other#))
		  'IMeta
		  `(~'-meta [this#] ~'__meta)
		  'IWithMeta
		  `(~'-with-meta [this# ~gs] (new ~tagname ~@(replace {'__meta gs} fields)))
		  'ILookup
		  `(~'-lookup [this# k#] (-lookup this# k# nil))
		  `(~'-lookup [this# k# else#]
			      (get (merge (hash-map ~@(mapcat (fn [fld] [(keyword fld) fld]) 
							      base-fields))
					  ~'__extmap)
				   k# else#))
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
                     (condp identical? k#
                       ~@(mapcat (fn [fld]
                                   [(keyword fld) (list* `new tagname (replace {fld gs} fields))])
                                 base-fields)
                       (new ~tagname ~@(remove #{'__extmap} fields) (assoc ~'__extmap k# ~gs))))
		  'IMap
		  `(~'-dissoc [this# k#] (if (contains? #{~@(map keyword base-fields)} k#)
                                            (dissoc (with-meta (into {} this#) ~'__meta) k#)
                                            (new ~tagname ~@(remove #{'__extmap} fields) 
                                                 (not-empty (dissoc ~'__extmap k#)))))
		  'ISeqable
		  `(~'-seq [this#] (seq (concat [~@(map #(list `vector (keyword %) %) base-fields)] 
                                              ~'__extmap)))
		  'IPrintable
		  `(~'-pr-seq [this# opts#]
			      (let [pr-pair# (fn [keyval#] (pr-sequential pr-seq "" " " "" opts# keyval#))]
				(pr-sequential
				 pr-pair# (str "#" ~(name rname) "{") ", " "}" opts#
				 (concat [~@(map #(list `vector (keyword %) %) base-fields)] 
					 ~'__extmap))))
		  ])]
      `(do
	 (~'defrecord* ~tagname ~hinted-fields)
	 (extend-type ~tagname ~@(dt->et impls))))))

(defn- build-positional-factory
  [rsym rname fields]
  (let [fn-name (symbol (str '-> rsym))]
    `(defn ~fn-name
       [~@fields]
       (new ~rname ~@fields))))

(defn- build-map-factory
  [rsym rname fields]
  (let [fn-name (symbol (str 'map-> rsym))
	ms (gensym)
	ks (map keyword fields)
	getters (map (fn [k] `(~k ~ms)) ks)]
    `(defn ~fn-name
       [~ms]
       (new ~rname ~@getters nil (dissoc ~ms ~@ks)))))

(defmacro defrecord [rsym fields & impls]
  (let [r (:name (cljs.compiler/resolve-var (dissoc &env :locals) rsym))]
    `(let []
       ~(emit-defrecord rsym r fields impls)
       ~(build-positional-factory rsym r fields)
       ~(build-map-factory rsym r fields))))

(defmacro defprotocol [psym & doc+methods]
  (let [p (:name (cljs.compiler/resolve-var (dissoc &env :locals) psym))
        ns-name (-> &env :ns :name)
        fqn (fn [n] (symbol (str ns-name "." n)))
        prefix (protocol-prefix p)
        methods (if (string? (first doc+methods)) (next doc+methods) doc+methods)
        expand-sig (fn [fname slot sig]
                     `(~sig
                       (if (and ~(first sig) (. ~(first sig) ~slot))
                         (. ~(first sig) ~slot ~@sig)
                         ((or
                           (aget ~(fqn fname) (goog.typeOf ~(first sig)))
                           (aget ~(fqn fname) "_")
                           (throw (missing-protocol
                                    ~(str psym "." fname) ~(first sig))))
                          ~@sig))))
        method (fn [[fname & sigs]]
                 (let [sigs (take-while vector? sigs)
                       slot (symbol (str prefix (name fname)))]
                   `(defn ~fname ~@(map #(expand-sig fname slot %) sigs))))]
    `(do
       (def ~psym (~'js* "{}"))
       ~@(map method methods))))

(defmacro satisfies?
  "Returns true if x satisfies the protocol"
  [psym x]
  (let [p (:name (cljs.compiler/resolve-var (dissoc &env :locals) psym))
        prefix (protocol-prefix p)]
    `(let [x# ~x]
       (if (and x# (. x# ~(symbol prefix)) (not (. x# (~'hasOwnProperty ~prefix))))
	 true
	 (cljs.core/type_satisfies_ ~psym x#)))))

(defmacro lazy-seq [& body]
  `(new cljs.core.LazySeq nil false (fn [] ~@body)))

(defmacro binding
  "binding => var-symbol init-expr

  Creates new bindings for the (already-existing) vars, with the
  supplied initial values, executes the exprs in an implicit do, then
  re-establishes the bindings that existed before.  The new bindings
  are made in parallel (unlike let); all init-exprs are evaluated
  before the vars are bound to their new values."
  [bindings & body]
  (let [names (take-nth 2 bindings)
        vals (take-nth 2 (drop 1 bindings))
        tempnames (map gensym names)
        binds (map vector names vals)
        resets (reverse (map vector names tempnames))]
    `(let [~@(interleave tempnames names)]
       (try
        ~@(map
           (fn [[k v]] (list 'set! k v))
           binds)
        ~@body
        (finally
         ~@(map
            (fn [[k v]] (list 'set! k v))
            resets))))))

(defmacro try
  "(try expr* catch-clause* finally-clause?)

   Special Form

   catch-clause => (catch protoname name expr*)
   finally-clause => (finally expr*)

  Catches and handles JavaScript exceptions."
  [& forms]
  (let [catch? #(and (list? %) (= (first %) 'catch))
        [body catches] (split-with (complement catch?) forms)
        [catches fin] (split-with catch? catches)
        e (gensym "e")]
    (assert (every? #(clojure.core/> (count %) 2) catches) "catch block must specify a prototype and a name")
    (if (seq catches)
      `(~'try*
        ~@body
        (catch ~e
            (cond
             ~@(mapcat
                (fn [[_ type name & cb]]
                  `[(instance? ~type ~e) (let [~name ~e] ~@cb)])
                catches)
             :else (throw ~e)))
        ~@fin)
      `(~'try*
        ~@body
        ~@fin))))

(defmacro assert
  "Evaluates expr and throws an exception if it does not evaluate to
  logical true."
  ([x]
     (when *assert*
       `(when-not ~x
          (throw (cljs.core/str
                   "Assert failed: " (cljs.core/pr-str '~x))))))
  ([x message]
     (when *assert*
       `(when-not ~x
          (throw (cljs.core/str
                  "Assert failed: " ~message "\n" (cljs.core/pr-str '~x)))))))

(defmacro ^{:private true} assert-args [fnname & pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  ~(str fnname " requires " (second pairs)))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args fnname more)))))

(defmacro for
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
  (let [to-groups (fn [seq-exprs]
                    (reduce (fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (fn [& msg] (throw (apply str msg)))
        emit-bind (fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (let [giter (gensym "iter__")
                          gxs (gensym "s__")
                          do-mod (fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     next-groups
                                      `(let [iterys# ~(emit-bind next-groups)
                                             fs# (seq (iterys# ~next-expr))]
                                         (if fs#
                                           (concat fs# (~giter (rest ~gxs)))
                                           (recur (rest ~gxs))))
                                     :else `(cons ~body-expr
                                                  (~giter (rest ~gxs)))))]
                      `(fn ~giter [~gxs]
                         (lazy-seq
                           (loop [~gxs ~gxs]
                             (when-first [~bind ~gxs]
                               ~(do-mod mod-pairs)))))))]
    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
       (iter# ~(second seq-exprs)))))

(defmacro doseq
  "Repeatedly executes body (presumably for side-effects) with
  bindings and filtering as provided by \"for\".  Does not retain
  the head of the sequence. Returns nil."
  [seq-exprs & body]
  (assert-args doseq
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [step (fn step [recform exprs]
               (if-not exprs
                 [true `(do ~@body)]
                 (let [k (first exprs)
                       v (second exprs)
                       
                       seqsym (when-not (keyword? k) (gensym))
                       recform (if (keyword? k) recform `(recur (first ~seqsym) ~seqsym))
                       steppair (step recform (nnext exprs))
                       needrec (steppair 0)
                       subform (steppair 1)]
                   (cond
                     (= k :let) [needrec `(let ~v ~subform)]
                     (= k :while) [false `(when ~v
                                            ~subform
                                            ~@(when needrec [recform]))]
                     (= k :when) [false `(if ~v
                                           (do
                                             ~subform
                                             ~@(when needrec [recform]))
                                           ~recform)]
                     :else [true `(let [~seqsym (seq ~v)]
                                    (when ~seqsym
                                      (loop [~k (first ~seqsym) ~seqsym ~seqsym]
                                       ~subform
                                       (when-let [~seqsym (next ~seqsym)]
                                        ~@(when needrec [recform])))))]))))]
    (nth (step nil (seq seq-exprs)) 1)))

(defmacro amap
  "Maps an expression across an array a, using an index named idx, and
  return value named ret, initialized to a clone of a, then setting 
  each element of ret to the evaluation of expr, returning the new 
  array ret."
  [a idx ret expr]
  `(let [a# ~a
         ~ret (aclone a#)]
     (loop  [~idx 0]
       (if (< ~idx  (alength a#))
         (do
           (aset ~ret ~idx ~expr)
           (recur (inc ~idx)))
         ~ret))))

(defmacro areduce
  "Reduces an expression across an array a, using an index named idx,
  and return value named ret, initialized to init, setting ret to the 
  evaluation of expr at each step, returning ret."
  [a idx ret init expr]
  `(let [a# ~a]
     (loop  [~idx 0 ~ret ~init]
       (if (< ~idx  (alength a#))
         (recur (inc ~idx) ~expr)
         ~ret))))

(defmacro dotimes
  "bindings => name n

  Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  [bindings & body]
  (let [i (first bindings)
        n (second bindings)]
    `(let [n# ~n]
       (loop [~i 0]
         (when (< ~i n#)
           ~@body
           (recur (inc ~i)))))))

(defn ^:private check-valid-options
  "Throws an exception if the given option map contains keys not listed
  as valid, else returns nil."
  [options & valid-keys]
  (when (seq (apply disj (apply hash-set (keys options)) valid-keys))
    (throw
     (apply str "Only these options are valid: "
	    (first valid-keys)
	    (map #(str ", " %) (rest valid-keys))))))

(defmacro defmulti
  "Creates a new multimethod with the associated dispatch function.
  The docstring and attribute-map are optional.

  Options are key-value pairs and may be one of:
    :default    the default dispatch value, defaults to :default
    :hierarchy  the isa? hierarchy to use for dispatching
                defaults to the global hierarchy"
  [mm-name & options]
  (let [docstring   (if (string? (first options))
                      (first options)
                      nil)
        options     (if (string? (first options))
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
                      m)]
    (when (= (count options) 1)
      (throw "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)"))
    (let [options   (apply hash-map options)
          default   (get options :default :default)
          ;; hierarchy (get options :hierarchy #'cljs.core.global-hierarchy)
	  ]
      (check-valid-options options :default :hierarchy)
      `(def ~(with-meta mm-name m)
	 (let [method-table# (atom {})
	       prefer-table# (atom {})
	       method-cache# (atom {})
	       cached-hierarchy# (atom {})
	       hierarchy# (get ~options :hierarchy cljs.core/global-hierarchy)
	       ]
	   (cljs.core.MultiFn. ~(name mm-name) ~dispatch-fn ~default hierarchy#
			       method-table# prefer-table# method-cache# cached-hierarchy#))))))

(defmacro defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  [multifn dispatch-val & fn-tail]
  `(-add-method ~(with-meta multifn {:tag 'cljs.core.MultiFn}) ~dispatch-val (fn ~@fn-tail)))

(defmacro time
  "Evaluates expr and prints the time it took. Returns the value of expr."
  [expr]
  `(let [start# (.getTime (js/Date.) ())
         ret# ~expr]
     (prn (str "Elapsed time: " (- (.getTime (js/Date.) ()) start#) " msecs"))
     ret#))
