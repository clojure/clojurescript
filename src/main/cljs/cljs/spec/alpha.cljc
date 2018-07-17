;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.spec.alpha
  (:refer-clojure :exclude [+ * and or cat def keys merge resolve assert])
  (:require [cljs.core :as c]
            [cljs.analyzer :as ana]
            [cljs.env :as env]
            [cljs.analyzer.api :refer [resolve]]
            [clojure.walk :as walk]
            [cljs.spec.gen.alpha :as gen]
            [clojure.string :as str]))

(defonce registry-ref (atom {}))

(defn- ->sym
  "Returns a symbol from a symbol or var"
  [x]
  (if (map? x)
    (:name x)
    x))

(defn- unfn [expr]
  (if (clojure.core/and (seq? expr)
             (symbol? (first expr))
             (= "fn*" (name (first expr))))
    (let [[[s] & form] (rest expr)]
      (conj (walk/postwalk-replace {s '%} form) '[%] 'cljs.core/fn))
    expr))

(defn- res [env form]
  (cond
    (keyword? form) form
    (symbol? form) #?(:clj  (clojure.core/or (->> form (resolve env) ->sym) form)
                      :cljs (let [resolved (clojure.core/or (->> form (resolve env) ->sym) form)
                                  ns-name (namespace resolved)]
                              (symbol
                                (if (clojure.core/and ns-name (str/ends-with? ns-name "$macros"))
                                  (subs ns-name 0 (- (count ns-name) 7))
                                  ns-name)
                                (name resolved))))
    (sequential? form) (walk/postwalk #(if (symbol? %) (res env %) %) (unfn form))
    :else form))

(defmacro ^:private mres
  "a compile time res, for use in cljs/spec/alpha.cljs"
  [form]
  (res &env form))

(defn- ns-qualify
  "Qualify symbol s by resolving it or using the current *ns*."
  [env s]
  (if (namespace s)
    (->sym (binding [ana/*private-var-access-nowarn* true]
             (ana/resolve-var env s)))
    (symbol (str ana/*cljs-ns*) (str s))))

(defmacro def
  "Given a namespace-qualified keyword or resolveable symbol k, and a spec,
   spec-name, predicate or regex-op makes an entry in the registry mapping k to
   the spec"
  [k spec-form]
  (let [k    (if (symbol? k) (ns-qualify &env k) k)
        form (res &env spec-form)]
    (swap! registry-ref assoc k form)
    `(def-impl '~k '~form ~spec-form)))

(defmacro spec
  "Takes a single predicate form, e.g. can be the name of a predicate,
  like even?, or a fn literal like #(< % 42). Note that it is not
  generally necessary to wrap predicates in spec when using the rest
  of the spec macros, only to attach a unique generator

  Can also be passed the result of one of the regex ops -
  cat, alt, *, +, ?, in which case it will return a regex-conforming
  spec, useful when nesting an independent regex.
  ---

  Optionally takes :gen generator-fn, which must be a fn of no args that
  returns a test.check generator.

  Returns a spec."
  [form & {:keys [gen]}]
  (when form
    `(spec-impl '~(res &env form) ~form ~gen nil)))

(defmacro multi-spec
  "Takes the name of a spec/predicate-returning multimethod and a
  tag-restoring keyword or fn (retag).  Returns a spec that when
  conforming or explaining data will pass it to the multimethod to get
  an appropriate spec. You can e.g. use multi-spec to dynamically and
  extensibly associate specs with 'tagged' data (i.e. data where one
  of the fields indicates the shape of the rest of the structure).

  (defmulti mspec :tag)

  The methods should ignore their argument and return a predicate/spec:
  (defmethod mspec :int [_] (s/keys :req-un [::tag ::i]))

  retag is used during generation to retag generated values with
  matching tags. retag can either be a keyword, at which key the
  dispatch-tag will be assoc'ed, or a fn of generated value and
  dispatch-tag that should return an appropriately retagged value.

  Note that because the tags themselves comprise an open set,
  the tag key spec cannot enumerate the values, but can e.g.
  test for keyword?.

  Note also that the dispatch values of the multimethod will be
  included in the path, i.e. in reporting and gen overrides, even
  though those values are not evident in the spec.
"
  [mm retag]
  `(multi-spec-impl '~(res &env mm) (var ~mm) ~retag))

(defmacro keys
  "Creates and returns a map validating spec. :req and :opt are both
  vectors of namespaced-qualified keywords. The validator will ensure
  the :req keys are present. The :opt keys serve as documentation and
  may be used by the generator.

  The :req key vector supports 'and' and 'or' for key groups:

  (s/keys :req [::x ::y (or ::secret (and ::user ::pwd))] :opt [::z])

  There are also -un versions of :req and :opt. These allow
  you to connect unqualified keys to specs.  In each case, fully
  qualfied keywords are passed, which name the specs, but unqualified
  keys (with the same name component) are expected and checked at
  conform-time, and generated during gen:

  (s/keys :req-un [:my.ns/x :my.ns/y])

  The above says keys :x and :y are required, and will be validated
  and generated by specs (if they exist) named :my.ns/x :my.ns/y
  respectively.

  In addition, the values of *all* namespace-qualified keys will be validated
  (and possibly destructured) by any registered specs. Note: there is
  no support for inline value specification, by design.

  Optionally takes :gen generator-fn, which must be a fn of no args that
  returns a test.check generator."
  [& {:keys [req req-un opt opt-un gen]}]
  (let [unk #(-> % name keyword)
        req-keys (filterv keyword? (flatten req))
        req-un-specs (filterv keyword? (flatten req-un))
        _ (clojure.core/assert (every? #(clojure.core/and (keyword? %) (namespace %)) (concat req-keys req-un-specs opt opt-un))
                  "all keys must be namespace-qualified keywords")
        req-specs (into req-keys req-un-specs)
        req-keys (into req-keys (map unk req-un-specs))
        opt-keys (into (vec opt) (map unk opt-un))
        opt-specs (into (vec opt) opt-un)
        gx (gensym)
        parse-req (fn [rk f]
                    (map (fn [x]
                           (if (keyword? x)
                             `(contains? ~gx ~(f x))
                             (walk/postwalk
                               (fn [y] (if (keyword? y) `(contains? ~gx ~(f y)) y))
                               x)))
                         rk))
        pred-exprs [`(map? ~gx)]
        pred-exprs (into pred-exprs (parse-req req identity))
        pred-exprs (into pred-exprs (parse-req req-un unk))
        keys-pred `(fn* [~gx] (c/and ~@pred-exprs))
        pred-exprs (mapv (fn [e] `(fn* [~gx] ~e)) pred-exprs)
        pred-forms (walk/postwalk #(res &env %) pred-exprs)]
    ;; `(map-spec-impl ~req-keys '~req ~opt '~pred-forms ~pred-exprs ~gen)
    `(map-spec-impl {:req '~req :opt '~opt :req-un '~req-un :opt-un '~opt-un
                     :req-keys '~req-keys :req-specs '~req-specs
                     :opt-keys '~opt-keys :opt-specs '~opt-specs
                     :pred-forms '~pred-forms
                     :pred-exprs ~pred-exprs
                     :keys-pred ~keys-pred
                     :gfn ~gen})))

(defmacro or
  "Takes key+pred pairs, e.g.

  (s/or :even even? :small #(< % 42))

  Returns a destructuring spec that returns a map entry containing the
  key of the first matching pred and the corresponding value. Thus the
  'key' and 'val' functions can be used to refer generically to the
  components of the tagged return."
  [& key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        pred-forms (mapv second pairs)
        pf (mapv #(res &env %) pred-forms)]
    (clojure.core/assert (clojure.core/and (even? (count key-pred-forms)) (every? keyword? keys)) "spec/or expects k1 p1 k2 p2..., where ks are keywords")
    `(or-spec-impl ~keys '~pf ~pred-forms nil)))

(defmacro and
  "Takes predicate/spec-forms, e.g.

  (s/and even? #(< % 42))

  Returns a spec that returns the conformed value. Successive
  conformed values propagate through rest of predicates."
  [& pred-forms]
  `(and-spec-impl '~(mapv #(res &env %) pred-forms) ~(vec pred-forms) nil))

(defn- res-kind
  [env opts]
  (let [{kind :kind :as mopts} opts]
    (->>
      (if kind
        (assoc mopts :kind `~(res env kind))
        mopts)
      (mapcat identity))))

(defmacro every
  "takes a pred and validates collection elements against that pred.

  Note that 'every' does not do exhaustive checking, rather it samples
  *coll-check-limit* elements. Nor (as a result) does it do any
  conforming of elements. 'explain' will report at most *coll-error-limit*
  problems.  Thus 'every' should be suitable for potentially large
  collections.

  Takes several kwargs options that further constrain the collection:

  :kind - a pred/spec that the collection type must satisfy, e.g. vector?
          (default nil) Note that if :kind is specified and :into is
          not, this pred must generate in order for every to generate.
  :count - specifies coll has exactly this count (default nil)
  :min-count, :max-count - coll has count (<= min-count count max-count) (defaults nil)
  :distinct - all the elements are distinct (default nil)

  And additional args that control gen

  :gen-max - the maximum coll size to generate (default 20)
  :into - one of [], (), {}, #{} - the default collection to generate into
      (default same as :kind if supplied, else []

  Optionally takes :gen generator-fn, which must be a fn of no args that
  returns a test.check generator

  See also - coll-of, every-kv
"
  [pred & {:keys [into kind count max-count min-count distinct gen-max gen-into gen] :as opts}]
  (let [desc (::describe opts)
        nopts (-> opts
                (dissoc :gen ::describe)
                (assoc ::kind-form `'~(res &env (:kind opts))
                       ::describe (clojure.core/or desc `'(every ~(res &env pred) ~@(res-kind &env opts)))))
        gx (gensym)
        cpreds (cond-> [(list (clojure.core/or kind `coll?) gx)]
                 count (conj `(= ~count (c/bounded-count ~count ~gx)))

                 (clojure.core/or min-count max-count)
                 (conj `(<= (c/or ~min-count 0)
                          (c/bounded-count (if ~max-count (inc ~max-count) ~min-count) ~gx)
                          (c/or ~max-count MAX_INT)))

                 distinct
                 (conj `(c/or (empty? ~gx) (apply distinct? ~gx))))]
    `(every-impl '~pred ~pred ~(assoc nopts ::cpred `(fn* [~gx] (c/and ~@cpreds))) ~gen)))

(defmacro every-kv
  "like 'every' but takes separate key and val preds and works on associative collections.

  Same options as 'every', :into defaults to {}

  See also - map-of"

  [kpred vpred & opts]
  (let [desc `(every-kv ~(res &env kpred) ~(res &env vpred) ~@(res-kind &env opts))]
    `(every (tuple ~kpred ~vpred) ::kfn (fn [i# v#] (nth v# 0)) :into {} ::describe '~desc ~@opts)))

(defmacro coll-of
  "Returns a spec for a collection of items satisfying pred. Unlike
  generator will fill an empty init-coll.

  Same options as 'every'. conform will produce a collection
  corresponding to :into if supplied, else will match the input collection,
  avoiding rebuilding when possible.

  Same options as 'every'.

  See also - every, map-of"
  [pred & opts]
  (let [desc `(coll-of ~(res &env pred) ~@(res-kind &env opts))]
    `(every ~pred ::conform-all true ::describe '~desc ~@opts)))

(defmacro map-of
  "Returns a spec for a map whose keys satisfy kpred and vals satisfy
  vpred. Unlike 'every-kv', map-of will exhaustively conform every
  value.

  Same options as 'every', :kind defaults to map?, with the addition of:

  :conform-keys - conform keys as well as values (default false)

  See also - every-kv"
  [kpred vpred & opts]
  (let [desc `(map-of ~(res &env kpred) ~(res &env vpred) ~@(res-kind &env opts))]
    `(every-kv ~kpred ~vpred ::conform-all true :kind map? ::describe '~desc ~@opts)))

(defmacro *
  "Returns a regex op that matches zero or more values matching
  pred. Produces a vector of matches iff there is at least one match"
  [pred-form]
  `(rep-impl '~(res &env pred-form) ~pred-form))

(defmacro +
  "Returns a regex op that matches one or more values matching
  pred. Produces a vector of matches"
  [pred-form]
  `(rep+impl '~(res &env pred-form) ~pred-form))

(defmacro ?
  "Returns a regex op that matches zero or one value matching
  pred. Produces a single value (not a collection) if matched."
  [pred-form]
  `(maybe-impl ~pred-form '~(res &env pred-form)))

(defmacro alt
  "Takes key+pred pairs, e.g.

  (s/alt :even even? :small #(< % 42))

  Returns a regex op that returns a map entry containing the key of the
  first matching pred and the corresponding value. Thus the
  'key' and 'val' functions can be used to refer generically to the
  components of the tagged return."
  [& key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        pred-forms (mapv second pairs)
        pf (mapv #(res &env %) pred-forms)]
    (clojure.core/assert (clojure.core/and (even? (count key-pred-forms)) (every? keyword? keys)) "alt expects k1 p1 k2 p2..., where ks are keywords")
    `(alt-impl ~keys ~pred-forms '~pf)))

(defmacro cat
  "Takes key+pred pairs, e.g.

  (s/cat :e even? :o odd?)

  Returns a regex op that matches (all) values in sequence, returning a map
  containing the keys of each pred and the corresponding value."
  [& key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        pred-forms (mapv second pairs)
        pf (mapv #(res &env %) pred-forms)]
    ;;(prn key-pred-forms)
    (clojure.core/assert (clojure.core/and (even? (count key-pred-forms)) (every? keyword? keys)) "cat expects k1 p1 k2 p2..., where ks are keywords")
    `(cat-impl ~keys ~pred-forms '~pf)))

(defmacro &
  "takes a regex op re, and predicates. Returns a regex-op that consumes
  input as per re but subjects the resulting value to the
  conjunction of the predicates, and any conforming they might perform."
  [re & preds]
  (let [pv (vec preds)]
    `(amp-impl ~re ~pv '~(mapv #(res &env %) pv))))

(defmacro conformer
  "takes a predicate function with the semantics of conform i.e. it should return either a
  (possibly converted) value or :cljs.spec.alpha/invalid, and returns a
  spec that uses it as a predicate/conformer. Optionally takes a
  second fn that does unform of result of first"
  ([f] `(spec-impl '(conformer ~(res &env f)) ~f nil true))
  ([f unf] `(spec-impl '(conformer ~(res &env f) ~(res &env unf)) ~f nil true ~unf)))

(defmacro fspec
  "takes :args :ret and (optional) :fn kwargs whose values are preds
  and returns a spec whose conform/explain take a fn and validates it
  using generative testing. The conformed value is always the fn itself.

  See 'fdef' for a single operation that creates an fspec and
  registers it, as well as a full description of :args, :ret and :fn

  fspecs can generate functions that validate the arguments and
  fabricate a return value compliant with the :ret spec, ignoring
  the :fn spec if present.

  Optionally takes :gen generator-fn, which must be a fn of no args
  that returns a test.check generator."
  [& {:keys [args ret fn gen] :or {ret `cljs.core/any?}}]
  (let [env &env]
    `(fspec-impl (spec ~args) '~(res env args)
                           (spec ~ret) '~(res env ret)
                           (spec ~fn) '~(res env fn) ~gen)))

(defmacro tuple
  "takes one or more preds and returns a spec for a tuple, a vector
  where each element conforms to the corresponding pred. Each element
  will be referred to in paths using its ordinal."
  [& preds]
  (clojure.core/assert (not (empty? preds)))
  `(tuple-impl '~(mapv #(res &env %) preds) ~(vec preds)))

(def ^:private _speced_vars (atom #{}))

(defn speced-vars []
  @_speced_vars)

(defmacro fdef
  "Takes a symbol naming a function, and one or more of the following:

  :args A regex spec for the function arguments as they were a list to be
    passed to apply - in this way, a single spec can handle functions with
    multiple arities
  :ret A spec for the function's return value
  :fn A spec of the relationship between args and ret - the
    value passed is {:args conformed-args :ret conformed-ret} and is
    expected to contain predicates that relate those values

  Qualifies fn-sym with resolve, or using *ns* if no resolution found.
  Registers an fspec in the global registry, where it can be retrieved
  by calling get-spec with the var or full-qualified symbol.

  Once registered, function specs are included in doc, checked by
  instrument, tested by the runner cljs.spec.test.alpha/run-tests, and (if
  a macro) used to explain errors during macroexpansion.

  Note that :fn specs require the presence of :args and :ret specs to
  conform values, and so :fn specs will be ignored if :args or :ret
  are missing.

  Returns the qualified fn-sym.

  For example, to register function specs for the symbol function:

  (s/fdef cljs.core/symbol
    :args (s/alt :separate (s/cat :ns string? :n string?)
                 :str string?
                 :sym symbol?)
    :ret symbol?)"
  [fn-sym & specs]
  (swap! _speced_vars conj
    (vary-meta (ns-qualify &env fn-sym)
      assoc :fdef-ns (-> &env :ns :name)))
  `(cljs.spec.alpha/def ~fn-sym (fspec ~@specs)))

(defmacro keys*
  "takes the same arguments as spec/keys and returns a regex op that matches sequences of key/values,
  converts them into a map, and conforms that map with a corresponding
  spec/keys call:

  user=> (s/conform (s/keys :req-un [::a ::c]) {:a 1 :c 2})
  {:a 1, :c 2}
  user=> (s/conform (s/keys* :req-un [::a ::c]) [:a 1 :c 2])
  {:a 1, :c 2}

  the resulting regex op can be composed into a larger regex:

  user=> (s/conform (s/cat :i1 integer? :m (s/keys* :req-un [::a ::c]) :i2 integer?) [42 :a 1 :c 2 :d 4 99])
  {:i1 42, :m {:a 1, :c 2, :d 4}, :i2 99}"
  [& kspecs]
  `(let [mspec# (keys ~@kspecs)]
     (with-gen (cljs.spec.alpha/& (* (cat ::k keyword? ::v cljs.core/any?)) ::kvs->map mspec#)
       (fn [] (gen/fmap (fn [m#] (apply concat m#)) (gen mspec#))))))

(defmacro nilable
  "returns a spec that accepts nil and values satisfiying pred"
  [pred]
  (let [pf (res &env pred)]
    `(nilable-impl '~pf ~pred nil)))

(defmacro inst-in
  "Returns a spec that validates insts in the range from start
  (inclusive) to end (exclusive)."
  [start end]
  `(let [st# (cljs.core/inst-ms ~start)
         et# (cljs.core/inst-ms ~end)
         mkdate# (fn [d#] (js/Date. d#))]
     (spec (and cljs.core/inst? #(inst-in-range? ~start ~end %))
       :gen (fn []
              (gen/fmap mkdate#
                (gen/large-integer* {:min st# :max et#}))))))

(defmacro int-in
  "Returns a spec that validates fixed precision integers in the
  range from start (inclusive) to end (exclusive)."
  [start end]
  `(spec (and c/int? #(int-in-range? ~start ~end %))
     :gen #(gen/large-integer* {:min ~start :max (dec ~end)})))

(defmacro double-in
  "Specs a 64-bit floating point number. Options:

    :infinite? - whether +/- infinity allowed (default true)
    :NaN?      - whether NaN allowed (default true)
    :min       - minimum value (inclusive, default none)
    :max       - maximum value (inclusive, default none)"
  [& {:keys [infinite? NaN? min max]
      :or {infinite? true NaN? true}
      :as m}]
  `(spec (and c/double?
           ~@(when-not infinite? '[#(not (infinite? %))])
           ~@(when-not NaN? '[#(not (js/isNaN %))])
           ~@(when max `[#(<= % ~max)])
           ~@(when min `[#(<= ~min %)]))
     :gen #(gen/double* ~m)))

(defmacro merge
  "Takes map-validating specs (e.g. 'keys' specs) and
  returns a spec that returns a conformed map satisfying all of the
  specs.  Successive conformed values propagate through rest of
  predicates. Unlike 'and', merge can generate maps satisfying the
  union of the predicates."
  [& pred-forms]
  `(merge-spec-impl '~(mapv #(res &env %) pred-forms) ~(vec pred-forms) nil))

(defmacro exercise-fn
  "exercises the fn named by sym (a symbol) by applying it to
  n (default 10) generated samples of its args spec. When fspec is
  supplied its arg spec is used, and sym-or-f can be a fn.  Returns a
  sequence of tuples of [args ret]. "
  ([sym]
   `(exercise-fn ~sym 10))
  ([sym n]
   `(exercise-fn ~sym ~n nil))
  ([sym n fspec]
   (let [sym (cond-> sym
               (clojure.core/and (sequential? sym)
                                 (= (first sym) 'quote))
               second)]
     `(let [fspec# ~(if-not fspec
                      `(get-spec '~(:name (resolve &env sym)))
                      fspec)
            f#     ~sym]
        (if-let [arg-spec# (c/and fspec# (:args fspec#))]
          (for [args# (gen/sample (gen arg-spec#) ~n)]
            [args# (apply f# args#)])
          (throw (js/Error. "No :args spec found, can't generate")))))))

(defmacro ^:private init-compile-asserts []
  (let [compile-asserts (not (-> env/*compiler* deref :options :elide-asserts))]
    compile-asserts))

(defmacro assert
  "spec-checking assert expression. Returns x if x is valid? according
to spec, else throws an error with explain-data plus ::failure of
:assertion-failed.
Can be disabled at either compile time or runtime:
If *compile-asserts* is false at compile time, compiles to x. Defaults
to the negation value of the ':elide-asserts' compiler option, or true if
not set.
If (check-asserts?) is false at runtime, always returns x. Defaults to
value of 'cljs.spec.alpha/*runtime-asserts*', or false if not set. You can
toggle check-asserts? with (check-asserts bool)."
  [spec x]
  `(if *compile-asserts*
     (if @#'*runtime-asserts*
       (assert* ~spec ~x)
       ~x)
    ~x))
