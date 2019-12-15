;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.spec.alpha
  (:refer-clojure :exclude [+ * and or cat def keys merge])
  (:require-macros [cljs.core :as c]
                   [cljs.spec.alpha :as s])
  (:require [goog.object :as gobj]
            [cljs.core :as c]
            [clojure.walk :as walk]
            [cljs.spec.gen.alpha :as gen]
            [clojure.string :as str]))

(def ^:const MAX_INT 9007199254740991)

(def ^:dynamic *recursion-limit*
  "A soft limit on how many times a branching spec (or/alt/*/opt-keys/multi-spec)
  can be recursed through during generation. After this a
  non-recursive branch will be chosen."
  4)

(def ^:dynamic *fspec-iterations*
  "The number of times an anonymous fn specified by fspec will be (generatively) tested during conform"
  21)

(def ^:dynamic *coll-check-limit*
  "The number of items validated in a collection spec'ed with 'every'"
  101)

(def ^:dynamic *coll-error-limit*
  "The number of errors reported by explain in a collection spec'ed with 'every'"
  20)

(defprotocol Spec
  (conform* [spec x])
  (unform* [spec y])
  (explain* [spec path via in x])
  (gen* [spec overrides path rmap])
  (with-gen* [spec gfn])
  (describe* [spec]))

(defonce ^:private registry-ref (atom {}))

(defn- deep-resolve [reg k]
  (loop [spec k]
    (if (ident? spec)
      (recur (get reg spec))
      spec)))

(defn- reg-resolve
  "returns the spec/regex at end of alias chain starting with k, nil if not found, k if k not ident"
  [k]
  (if (ident? k)
    (let [reg @registry-ref
          spec (get reg k)]
      (if-not (ident? spec)
        spec
        (deep-resolve reg spec)))
    k))

(defn- reg-resolve!
  "returns the spec/regex at end of alias chain starting with k, throws if not found, k if k not ident"
  [k]
  (if (ident? k)
    (c/or (reg-resolve k)
          (throw (js/Error. (str "Unable to resolve spec: " k))))
    k))

(defn spec?
  "returns x if x is a spec object, else logical false"
  [x]
  (when (implements? Spec x)
    x))

(defn regex?
  "returns x if x is a (cljs.spec.alpha) regex op, else logical false"
  [x]
  (c/and (::op x) x))

(defn- with-name [spec name]
  (cond
   (ident? spec) spec
   (regex? spec) (assoc spec ::name name)

   (implements? IMeta spec)
   (with-meta spec (assoc (meta spec) ::name name))))

(defn- spec-name [spec]
  (cond
   (ident? spec) spec

   (regex? spec) (::name spec)

   (implements? IMeta spec)
   (-> (meta spec) ::name)))

(declare ^{:arglists '([form pred gfn cpred?] [form pred gfn cpred? unc])} spec-impl)
(declare ^{:arglists '([re gfn])} regex-spec-impl)

(defn- maybe-spec
  "spec-or-k must be a spec, regex or resolvable kw/sym, else returns nil."
  [spec-or-k]
  (let [s (c/or (c/and (ident? spec-or-k) (reg-resolve spec-or-k))
                (spec? spec-or-k)
                (regex? spec-or-k)
                nil)]
    (if (regex? s)
      (with-name (regex-spec-impl s nil) (spec-name s))
      s)))

(defn- the-spec
  "spec-or-k must be a spec, regex or kw/sym, else returns nil. Throws if unresolvable kw/sym"
  [spec-or-k]
  (c/or (maybe-spec spec-or-k)
        (when (ident? spec-or-k)
          (throw (js/Error. (str "Unable to resolve spec: " spec-or-k))))))

(defn- fn-sym [f-n]
  (when-not (str/blank? f-n)
    (let [xs (map demunge (str/split f-n "$"))]
      (when (c/and (<= 2 (count xs))
                   (every? #(not (str/blank? %)) xs))
        (let [[xs y] ((juxt butlast last) xs)]
          (symbol (str (str/join "." xs) "/" y)))))))

(defprotocol Specize
  (specize* [_] [_ form]))

(extend-protocol Specize
  Keyword
  (specize* ([k] (specize* (reg-resolve! k)))
            ([k _] (specize* (reg-resolve! k))))

  Symbol
  (specize* ([s] (specize* (reg-resolve! s)))
            ([s _] (specize* (reg-resolve! s))))

  PersistentHashSet
  (specize* ([s] (spec-impl s s nil nil))
            ([s form] (spec-impl form s nil nil)))

  PersistentTreeSet
  (specize* ([s] (spec-impl s s nil nil))
            ([s form] (spec-impl form s nil nil)))

  default
  (specize*
    ([o]
     (if-let [f-n (c/and (fn? o) (fn-sym (.-name o)))]
       (spec-impl f-n o nil nil)
       (spec-impl ::unknown o nil nil)))
    ([o form] (spec-impl form o nil nil))))

(defn- specize
  ([s] (c/or (spec? s) (specize* s)))
  ([s form] (c/or (spec? s) (specize* s form))))

(defn invalid?
  "tests the validity of a conform return value"
  [ret]
  (keyword-identical? ::invalid ret))

(defn conform
  "Given a spec and a value, returns :cljs.spec.alpha/invalid if value does
  not match spec, else the (possibly destructured) value."
  [spec x]
  (conform* (specize spec) x))

(defn unform
  "Given a spec and a value created by or compliant with a call to
  'conform' with the same spec, returns a value with all conform
   destructuring undone."
  [spec x]
  (unform* (specize spec) x))

(defn form
  "returns the spec as data"
  [spec]
  ;;TODO - incorporate gens
  (describe* (specize spec)))

(defn abbrev [form]
  (cond
    (seq? form)
    (walk/postwalk (fn [form]
                     (cond
                       (c/and (symbol? form) (namespace form))
                       (-> form name symbol)

                       (c/and (seq? form) (= 'fn (first form)) (= '[%] (second form)))
                       (last form)

                       :else form))
                   form)

    (c/and (symbol? form) (namespace form))
    (-> form name symbol)

    :else form))

(defn describe
  "returns an abbreviated description of the spec as data"
  [spec]
  (abbrev (form spec)))

(defn with-gen
  "Takes a spec and a no-arg, generator-returning fn and returns a version of that spec that uses that generator"
  [spec gen-fn]
  (let [spec (reg-resolve spec)]
    (if (regex? spec)
      (assoc spec ::gfn gen-fn)
      (with-gen* (specize spec) gen-fn))))

(defn explain-data* [spec path via in x]
  (when-let [probs (explain* (specize spec) path via in x)]
    (when-not (empty? probs)
      {::problems probs
       ::spec spec
       ::value x})))

(defn explain-data
  "Given a spec and a value x which ought to conform, returns nil if x
  conforms, else a map with at least the key ::problems whose value is
  a collection of problem-maps, where problem-map has at least :path :pred and :val
  keys describing the predicate and the value that failed at that
  path."
  [spec x]
  (explain-data* spec [] (if-let [name (spec-name spec)] [name] []) [] x))

(defn explain-printer
  "Default printer for explain-data. nil indicates a successful validation."
  [ed]
  (if ed
    (let [problems (->> (::problems ed)
                     (sort-by #(- (count (:in %))))
                     (sort-by #(- (count (:path %)))))]
      (print
        (with-out-str
          ;;(prn {:ed ed})
          (doseq [{:keys [path pred val reason via in] :as prob} problems]
            (pr val)
            (print " - failed: ")
            (if reason (print reason) (pr (abbrev pred)))
            (when-not (empty? in)
              (print (str " in: " (pr-str in))))
            (when-not (empty? path)
              (print (str " at: " (pr-str path))))
            (when-not (empty? via)
              (print (str " spec: " (pr-str (last via)))))
            (doseq [[k v] prob]
              (when-not (#{:path :pred :val :reason :via :in} k)
                (print "\n\t" (pr-str k) " ")
                (pr v)))
            (newline)))))
    (println "Success!")))

(def ^:dynamic *explain-out* explain-printer)

(defn explain-out
  "Prints explanation data (per 'explain-data') to *out* using the printer in *explain-out*,
    by default explain-printer."
  [ed]
  (*explain-out* ed))

(defn explain
  "Given a spec and a value that fails to conform, prints an explanation to *out*."
  [spec x]
  (explain-out (explain-data spec x)))

(defn explain-str
  "Given a spec and a value that fails to conform, returns an explanation as a string."
  [spec x]
  (with-out-str (explain spec x)))

(declare ^{:arglists '([spec x] [spec x form])} valid?)

(defn- gensub
  [spec overrides path rmap form]
  ;;(prn {:spec spec :over overrides :path path :form form})
  (let [spec (specize spec)]
    (if-let [g (c/or (when-let [gfn (c/or (get overrides (c/or (spec-name spec) spec))
                                          (get overrides path))]
                       (gfn))
                 (gen* spec overrides path rmap))]
      (gen/such-that #(valid? spec %) g 100)
      (throw (js/Error. (str "Unable to construct gen at: " path " for: " (abbrev form)))))))

(defn gen
  "Given a spec, returns the generator for it, or throws if none can
  be constructed. Optionally an overrides map can be provided which
  should map spec names or paths (vectors of keywords) to no-arg
  generator-creating fns. These will be used instead of the generators at those
  names/paths. Note that parent generator (in the spec or overrides
  map) will supersede those of any subtrees. A generator for a regex
  op must always return a sequential collection (i.e. a generator for
  s/? should return either an empty sequence/vector or a
  sequence/vector with one item in it)"
  ([spec] (gen spec nil))
  ([spec overrides] (gensub spec overrides [] {::recursion-limit *recursion-limit*} spec)))

(defn ^:skip-wiki def-impl
  "Do not call this directly, use 'def'"
  [k form spec]
  (assert (c/and (ident? k) (namespace k)) "k must be namespaced keyword or resolveable symbol")
  (if (nil? spec)
    (swap! registry-ref dissoc k)
    (let [spec (if (c/or (spec? spec) (regex? spec) (get @registry-ref spec))
                 spec
                 (spec-impl form spec nil nil))]
      (swap! registry-ref assoc k (with-name spec k))))
  k)

(defn registry
  "returns the registry map, prefer 'get-spec' to lookup a spec by name"
  []
  @registry-ref)

(defn- ->sym
  "Returns a symbol from a symbol or var"
  [x]
  (if (var? x)
    (.-sym x)
    x))

(defn get-spec
  "Returns spec registered for keyword/symbol/var k, or nil."
  [k]
  (get (registry) (if (keyword? k) k (->sym k))))

(declare map-spec)

(defn- macroexpand-check
  [v args]
  (let [specs (get-spec v)]
    (when-let [arg-spec (:args specs)]
      (when (invalid? (conform arg-spec args))
        (let [ed (assoc (explain-data* arg-spec []
                          (if-let [name (spec-name arg-spec)] [name] []) [] args)
                   ::args args)]
          (throw (ex-info
                   (str
                     "Call to " (->sym v) " did not conform to spec.")
                   ed)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; impl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- recur-limit? [rmap id path k]
  (c/and (> (get rmap id) (::recursion-limit rmap))
         (contains? (set path) k)))

(defn- inck [m k]
  (assoc m k (inc (c/or (get m k) 0))))

(defn- dt
  ([pred x form] (dt pred x form nil))
  ([pred x form cpred?]
   (if pred
     (if-let [spec (the-spec pred)]
       (conform spec x)
       (if (ifn? pred)
         (if cpred?
           (pred x)
           (if (pred x) x ::invalid))
         (throw (js/Error. (str (pr-str form) " is not a fn, expected predicate fn")))))
     x)))

(defn valid?
  "Helper function that returns true when x is valid for spec."
  ([spec x]
   (let [spec (specize spec)]
     (not (invalid? (conform* spec x)))))
  ([spec x form]
   (let [spec (specize spec form)]
     (not (invalid? (conform* spec x))))))

(defn- pvalid?
  "internal helper function that returns true when x is valid for spec."
  ([pred x]
   (not (invalid? (dt pred x ::unknown))))
  ([pred x form]
   (not (invalid? (dt pred x form)))))

(defn- explain-1 [form pred path via in v]
  ;;(prn {:form form :pred pred :path path :in in :v v})
  (let [pred (maybe-spec pred)]
    (if (spec? pred)
      (explain* pred path (if-let [name (spec-name pred)] (conj via name) via) in v)
      [{:path path :pred form :val v :via via :in in}])))

(declare ^{:arglists '([s] [min-count s])} or-k-gen
         ^{:arglists '([s])} and-k-gen)

(defn- k-gen
  "returns a generator for form f, which can be a keyword or a list
  starting with 'or or 'and."
  [f]
  (cond
    (keyword? f)       (gen/return f)
    (= 'or  (first f)) (or-k-gen 1 (rest f))
    (= 'and (first f)) (and-k-gen (rest f))))

(defn- or-k-gen
  "returns a tuple generator made up of generators for a random subset
  of min-count (default 0) to all elements in s."
  ([s] (or-k-gen 0 s))
  ([min-count s]
   (gen/bind (gen/tuple
               (gen/choose min-count (count s))
               (gen/shuffle (map k-gen s)))
     (fn [[n gens]]
       (apply gen/tuple (take n gens))))))

(defn- and-k-gen
  "returns a tuple generator made up of generators for every element
  in s."
  [s]
  (apply gen/tuple (map k-gen s)))

(defn ^:skip-wiki map-spec-impl
  "Do not call this directly, use 'spec' with a map argument"
  [{:keys [req-un opt-un keys-pred pred-exprs opt-keys req-specs req req-keys opt-specs pred-forms opt gfn]
    :as argm}]
  (let [k->s (zipmap (concat req-keys opt-keys) (concat req-specs opt-specs))
        keys->specnames #(c/or (k->s %) %)
        id (random-uuid)]
    (reify
      Specize
      (specize* [s] s)
      (specize* [s _] s)

      Spec
      (conform* [_ m]
        (if (keys-pred m)
          (let [reg (registry)]
            (loop [ret m, [[k v] & ks :as keys] m]
              (if keys
                (let [sname (keys->specnames k)]
                  (if-let [s (get reg sname)]
                    (let [cv (conform s v)]
                      (if (invalid? cv)
                        ::invalid
                        (recur (if (identical? cv v) ret (assoc ret k cv))
                          ks)))
                    (recur ret ks)))
                ret)))
          ::invalid))
      (unform* [_ m]
        (let [reg (registry)]
          (loop [ret m, [k & ks :as keys] (c/keys m)]
            (if keys
              (if (contains? reg (keys->specnames k))
                (let [cv (get m k)
                      v (unform (keys->specnames k) cv)]
                  (recur (if (identical? cv v) ret (assoc ret k v))
                    ks))
                (recur ret ks))
              ret))))
      (explain* [_ path via in x]
        (if-not (map? x)
          [{:path path :pred `map? :val x :via via :in in}]
          (let [reg (registry)]
            (apply concat
                   (when-let [probs (->> (map (fn [pred form] (when-not (pred x) form))
                                              pred-exprs pred-forms)
                                         (keep identity)
                                         seq)]
                     (map
                       #(identity {:path path :pred % :val x :via via :in in})
                       probs))
                   (map (fn [[k v]]
                          (when-not (c/or (not (contains? reg (keys->specnames k)))
                                      (pvalid? (keys->specnames k) v k))
                            (explain-1 (keys->specnames k) (keys->specnames k) (conj path k) via (conj in k) v)))
                     (seq x))))))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (let [rmap (inck rmap id)
                rgen (fn [k s] [k (gensub s overrides (conj path k) rmap k)])
                ogen (fn [k s]
                       (when-not (recur-limit? rmap id path k)
                         [k (gen/delay (gensub s overrides (conj path k) rmap k))]))
                reqs (map rgen req-keys req-specs)
                opts (remove nil? (map ogen opt-keys opt-specs))]
            (when (every? identity (concat (map second reqs) (map second opts)))
              (gen/bind
                (gen/tuple
                  (and-k-gen req)
                  (or-k-gen opt)
                  (and-k-gen req-un)
                  (or-k-gen opt-un))
                (fn [[req-ks opt-ks req-un-ks opt-un-ks]]
                  (let [qks (flatten (concat req-ks opt-ks))
                        unqks (map (comp keyword name) (flatten (concat req-un-ks opt-un-ks)))]
                    (->> (into reqs opts)
                      (filter #((set (concat qks unqks)) (first %)))
                      (apply concat)
                      (apply gen/hash-map)))))))))
      (with-gen* [_ gfn] (map-spec-impl (assoc argm :gfn gfn)))
      (describe* [_] (cons `keys
                           (cond-> []
                                   req (conj :req req)
                                   opt (conj :opt opt)
                                   req-un (conj :req-un req-un)
                                   opt-un (conj :opt-un opt-un)))))))

(defn ^:skip-wiki spec-impl
  "Do not call this directly, use 'spec'"
  ([form pred gfn cpred?] (spec-impl form pred gfn cpred? nil))
  ([form pred gfn cpred? unc]
   (cond
     (spec? pred) (cond-> pred gfn (with-gen gfn))
     (regex? pred) (regex-spec-impl pred gfn)
     (ident? pred) (cond-> (the-spec pred) gfn (with-gen gfn))
     :else
     (reify
       Specize
       (specize* [s] s)
       (specize* [s _] s)

       Spec
       (conform* [_ x] (let [ret (pred x)]
                         (if cpred?
                           ret
                           (if ret x ::invalid))))
       (unform* [_ x] (if cpred?
                        (if unc
                          (unc x)
                          (throw (js/Error. "no unform fn for conformer")))
                        x))
       (explain* [_ path via in x]
         (when (invalid? (dt pred x form cpred?))
           [{:path path :pred form :val x :via via :in in}]))
       (gen* [_ _ _ _] (if gfn
                         (gfn)
                         (gen/gen-for-pred pred)))
       (with-gen* [_ gfn] (spec-impl form pred gfn cpred? unc))
       (describe* [_] form)))))

(defn ^:skip-wiki multi-spec-impl
  "Do not call this directly, use 'multi-spec'"
  ([form mmvar retag] (multi-spec-impl form mmvar retag nil))
  ([form mmvar retag gfn]
   (let [id (random-uuid)
         predx #(let [mm @mmvar]
                 (c/and (-get-method mm ((-dispatch-fn mm) %))
                        (mm %)))
         dval #((-dispatch-fn @mmvar) %)
         tag (if (keyword? retag)
               #(assoc %1 retag %2)
               retag)]
     (reify
       Specize
       (specize* [s] s)
       (specize* [s _] s)

       Spec
       (conform* [_ x] (if-let [pred (predx x)]
                         (dt pred x form)
                         ::invalid))
       (unform* [_ x] (if-let [pred (predx x)]
                        (unform pred x)
                        (throw (js/Error. (str "No method of: " form " for dispatch value: " (dval x))))))
       (explain* [_ path via in x]
         (let [dv (dval x)
               path (conj path dv)]
           (if-let [pred (predx x)]
             (explain-1 form pred path via in x)
             [{:path path :pred form :val x :reason "no method" :via via :in in}])))
       (gen* [_ overrides path rmap]
         (if gfn
           (gfn)
           (let [gen (fn [[k f]]
                       (let [p (f nil)]
                         (let [rmap (inck rmap id)]
                           (when-not (recur-limit? rmap id path k)
                             (gen/delay
                               (gen/fmap
                                 #(tag % k)
                                 (gensub p overrides (conj path k) rmap (list 'method form k))))))))
                 gs (->> (methods @mmvar)
                         (remove (fn [[k]] (invalid? k)))
                         (map gen)
                         (remove nil?))]
             (when (every? identity gs)
               (gen/one-of gs)))))
       (with-gen* [_ gfn] (multi-spec-impl form mmvar retag gfn))
       (describe* [_] `(multi-spec ~form ~retag))))))

(defn ^:skip-wiki tuple-impl
  "Do not call this directly, use 'tuple'"
  ([forms preds] (tuple-impl forms preds nil))
  ([forms preds gfn]
   (let [specs (delay (mapv specize preds forms))
         cnt (count preds)]
     (reify
       Specize
       (specize* [s] s)
       (specize* [s _] s)

       Spec
       (conform* [_ x]
         (let [specs @specs]
           (if-not (c/and (vector? x)
                     (= (count x) cnt))
             ::invalid
             (loop [ret x, i 0]
               (if (= i cnt)
                 ret
                 (let [v (x i)
                       cv (conform* (specs i) v)]
                   (if (invalid? cv)
                     ::invalid
                     (recur (if (identical? cv v) ret (assoc ret i cv))
                            (inc i)))))))))
       (unform* [_ x]
         (assert (c/and (vector? x)
                   (= (count x) (count preds))))
         (loop [ret x, i 0]
           (if (= i (count x))
             ret
             (let [cv (x i)
                   v (unform (preds i) cv)]
               (recur (if (identical? cv v) ret (assoc ret i v))
                 (inc i))))))
       (explain* [_ path via in x]
         (cond
           (not (vector? x))
           [{:path path :pred `vector? :val x :via via :in in}]

           (not= (count x) (count preds))
           [{:path path :pred `(= (count ~'%) ~(count preds)) :val x :via via :in in}]

           :else
           (apply concat
             (map (fn [i form pred]
                    (let [v (x i)]
                      (when-not (pvalid? pred v)
                        (explain-1 form pred (conj path i) via (conj in i) v))))
               (range (count preds)) forms preds))))
       (gen* [_ overrides path rmap]
         (if gfn
           (gfn)
           (let [gen (fn [i p f]
                       (gensub p overrides (conj path i) rmap f))
                 gs (map gen (range (count preds)) preds forms)]
             (when (every? identity gs)
               (apply gen/tuple gs)))))
       (with-gen* [_ gfn] (tuple-impl forms preds gfn))
       (describe* [_] `(tuple ~@forms))))))

(defn- tagged-ret [tag ret]
  (MapEntry. tag ret nil))

(defn ^:skip-wiki or-spec-impl
  "Do not call this directly, use 'or'"
  [keys forms preds gfn]
  (let [id (random-uuid)
        kps (zipmap keys preds)
        specs (delay (mapv specize preds forms))
        cform (case (count preds)
                2 (fn [x]
                    (let [specs @specs
                          ret (conform* (specs 0) x)]
                      (if (invalid? ret)
                        (let [ret (conform* (specs 1) x)]
                          (if (invalid? ret)
                            ::invalid
                            (tagged-ret (keys 1) ret)))
                        (tagged-ret (keys 0) ret))))
                3 (fn [x]
                    (let [specs @specs
                          ret (conform* (specs 0) x)]
                      (if (invalid? ret)
                        (let [ret (conform* (specs 1) x)]
                          (if (invalid? ret)
                            (let [ret (conform* (specs 2) x)]
                              (if (invalid? ret)
                                ::invalid
                                (tagged-ret (keys 2) ret)))
                            (tagged-ret (keys 1) ret)))
                        (tagged-ret (keys 0) ret))))
                (fn [x]
                  (let [specs @specs]
                    (loop [i 0]
                      (if (< i (count specs))
                        (let [spec (specs i)]
                          (let [ret (conform* spec x)]
                            (if (invalid? ret)
                              (recur (inc i))
                              (tagged-ret (keys i) ret))))
                        ::invalid)))))]
    (reify
      Specize
      (specize* [s] s)
      (specize* [s _] s)

      Spec
      (conform* [_ x] (cform x))
      (unform* [_ [k x]] (unform (kps k) x))
      (explain* [this path via in x]
        (when-not (pvalid? this x)
          (apply concat
                 (map (fn [k form pred]
                        (when-not (pvalid? pred x)
                          (explain-1 form pred (conj path k) via in x)))
                      keys forms preds))))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (let [gen (fn [k p f]
                      (let [rmap (inck rmap id)]
                        (when-not (recur-limit? rmap id path k)
                          (gen/delay
                            (gensub p overrides (conj path k) rmap f)))))
                gs (remove nil? (map gen keys preds forms))]
            (when-not (empty? gs)
              (gen/one-of gs)))))
      (with-gen* [_ gfn] (or-spec-impl keys forms preds gfn))
      (describe* [_] `(or ~@(mapcat vector keys forms))))))

(defn- and-preds [x preds forms]
  (loop [ret x
         [pred & preds] preds
         [form & forms] forms]
    (if pred
      (let [nret (dt pred ret form)]
        (if (invalid? nret)
          ::invalid
          ;;propagate conformed values
          (recur nret preds forms)))
      ret)))

(defn- explain-pred-list
  [forms preds path via in x]
  (loop [ret x
         [form & forms] forms
         [pred & preds] preds]
    (when pred
      (let [nret (dt pred ret form)]
        (if (invalid? nret)
          (explain-1 form pred path via in ret)
          (recur nret forms preds))))))

(defn ^:skip-wiki and-spec-impl
  "Do not call this directly, use 'and'"
  [forms preds gfn]
  (let [specs (delay (mapv specize preds forms))
        cform
        (case (count preds)
          2 (fn [x]
              (let [specs @specs
                    ret (conform* (specs 0) x)]
                (if (invalid? ret)
                  ::invalid
                  (conform* (specs 1) ret))))
          3 (fn [x]
              (let [specs @specs
                    ret (conform* (specs 0) x)]
                (if (invalid? ret)
                  ::invalid
                  (let [ret (conform* (specs 1) ret)]
                    (if (invalid? ret)
                      ::invalid
                      (conform* (specs 2) ret))))))
          (fn [x]
            (let [specs @specs]
              (loop [ret x i 0]
                (if (< i (count specs))
                  (let [nret (conform* (specs i) ret)]
                    (if (invalid? nret)
                      ::invalid
                      ;;propagate conformed values
                      (recur nret (inc i))))
                  ret)))))]
    (reify
      Specize
      (specize* [s] s)
      (specize* [s _] s)

      Spec
      (conform* [_ x] (cform x))
      (unform* [_ x] (reduce #(unform %2 %1) x (reverse preds)))
      (explain* [_ path via in x] (explain-pred-list forms preds path via in x))
      (gen* [_ overrides path rmap] (if gfn (gfn) (gensub (first preds) overrides path rmap (first forms))))
      (with-gen* [_ gfn] (and-spec-impl forms preds gfn))
      (describe* [_] `(and ~@forms)))))

(defn- coll-prob [x kfn kform distinct count min-count max-count
                  path via in]
  (let [pred (c/or kfn coll?)
        kform (c/or kform `coll?)]
    (cond
     (not (pvalid? pred x))
     (explain-1 kform pred path via in x)

     (c/and count (not= count (bounded-count count x)))
     [{:path path :pred `(= ~count (c/count ~'%)) :val x :via via :in in}]

     (c/and (c/or min-count max-count)
       (not (<= (c/or min-count 0)
              (bounded-count (if max-count (inc max-count) min-count) x)
              (c/or max-count MAX_INT))))
     [{:path path :pred `(<= ~(c/or min-count 0) (c/count ~'%) ~(c/or max-count MAX_INT)) :val x :via via :in in}]

     (c/and distinct (not (empty? x)) (not (apply distinct? x)))
     [{:path path :pred 'distinct? :val x :via via :in in}])))

(defn ^:skip-wiki merge-spec-impl
  "Do not call this directly, use 'merge'"
  [forms preds gfn]
  (reify
    Specize
    (specize* [s] s)
    (specize* [s _] s)

    Spec
    (conform* [_ x] (let [ms (map #(dt %1 x %2) preds forms)]
                      (if (some invalid? ms)
                        ::invalid
                        (apply c/merge ms))))
    (unform* [_ x] (apply c/merge (map #(unform % x) (reverse preds))))
    (explain* [_ path via in x]
      (apply concat
        (map #(explain-1 %1 %2 path via in x)
          forms preds)))
    (gen* [_ overrides path rmap]
      (if gfn
        (gfn)
        (gen/fmap
          #(apply c/merge %)
          (apply gen/tuple (map #(gensub %1 overrides path rmap %2)
                             preds forms)))))
    (with-gen* [_ gfn] (merge-spec-impl forms preds gfn))
    (describe* [_] `(merge ~@forms))))

(def ^:private empty-coll {`vector? [], `set? #{}, `list? (), `map? {}})

(defn ^:skip-wiki every-impl
  "Do not call this directly, use 'every', 'every-kv', 'coll-of' or 'map-of'"
  ([form pred opts] (every-impl form pred opts nil))
  ([form pred {conform-into :into
               describe-form ::describe
               :keys [kind ::kind-form count max-count min-count distinct gen-max ::kfn ::cpred
                      conform-keys ::conform-all]
               :or {gen-max 20}
               :as opts}
    gfn]
   (let [gen-into (if conform-into (empty conform-into) (get empty-coll kind-form))
         spec (delay (specize pred))
         check? #(valid? @spec %)
         kfn (c/or kfn (fn [i v] i))
         addcv (fn [ret i v cv] (conj ret cv))
         cfns (fn [x]
                ;;returns a tuple of [init add complete] fns
                (cond
                  (c/and (vector? x) (c/or (not conform-into) (vector? conform-into)))
                  [identity
                   (fn [ret i v cv]
                     (if (identical? v cv)
                       ret
                       (assoc ret i cv)))
                   identity]

                  (c/and (map? x) (c/or (c/and kind (not conform-into)) (map? conform-into)))
                  [(if conform-keys empty identity)
                   (fn [ret i v cv]
                     (if (c/and (identical? v cv) (not conform-keys))
                       ret
                       (assoc ret (nth (if conform-keys cv v) 0) (nth cv 1))))
                   identity]

                  (c/or (list? conform-into) (seq? conform-into) (c/and (not conform-into) (c/or (list? x) (seq? x))))
                  [empty addcv reverse]

                  :else [#(empty (c/or conform-into %)) addcv identity]))]
     (reify
       Specize
       (specize* [s] s)
       (specize* [s _] s)

       Spec
       (conform* [_ x]
         (let [spec @spec]
           (cond
             (not (cpred x)) ::invalid

             conform-all
             (let [[init add complete] (cfns x)]
               (loop [ret (init x), i 0, [v & vs :as vseq] (seq x)]
                 (if vseq
                   (let [cv (conform* spec v)]
                     (if (invalid? cv)
                       ::invalid
                       (recur (add ret i v cv) (inc i) vs)))
                   (complete ret))))

             :else
             (if (indexed? x)
               (let [step (max 1 (long (/ (c/count x) *coll-check-limit*)))]
                 (loop [i 0]
                   (if (>= i (c/count x))
                     x
                     (if (valid? spec (nth x i))
                       (recur (c/+ i step))
                       ::invalid))))
               (let [limit *coll-check-limit*]
                 (loop [i 0 [v & vs :as vseq] (seq x)]
                   (cond
                     (c/or (nil? vseq) (= i limit)) x
                     (valid? spec v) (recur (inc i) vs)
                     :else ::invalid)))))))
       (unform* [_ x]
         (if conform-all
           (let [spec @spec
                 [init add complete] (cfns x)]
             (loop [ret (init x), i 0, [v & vs :as vseq] (seq x)]
               (if (>= i (c/count x))
                 (complete ret)
                 (recur (add ret i v (unform* spec v)) (inc i) vs))))
           x))
       (explain* [_ path via in x]
         (c/or (coll-prob x kind kind-form distinct count min-count max-count
                          path via in)
           (apply concat
             ((if conform-all identity (partial take *coll-error-limit*))
               (keep identity
                 (map (fn [i v]
                        (let [k (kfn i v)]
                          (when-not (check? v)
                            (let [prob (explain-1 form pred path via (conj in k) v)]
                              prob))))
                   (range) x))))))
       (gen* [_ overrides path rmap]
         (if gfn
           (gfn)
           (let [pgen (gensub pred overrides path rmap form)]
             (gen/bind
               (cond
                 gen-into (gen/return gen-into)
                 kind (gen/fmap #(if (empty? %) % (empty %))
                        (gensub kind overrides path rmap form))
                 :else (gen/return []))
               (fn [init]
                 (gen/fmap
                   #(if (vector? init) % (into init %))
                   (cond
                     distinct
                     (if count
                       (gen/vector-distinct pgen {:num-elements count :max-tries 100})
                       (gen/vector-distinct pgen {:min-elements (c/or min-count 0)
                                                  :max-elements (c/or max-count (max gen-max (c/* 2 (c/or min-count 0))))
                                                  :max-tries 100}))

                     count
                     (gen/vector pgen count)

                     (c/or min-count max-count)
                     (gen/vector pgen (c/or min-count 0) (c/or max-count (max gen-max (c/* 2 (c/or min-count 0)))))

                     :else
                     (gen/vector pgen 0 gen-max))))))))

       (with-gen* [_ gfn] (every-impl form pred opts gfn))
       (describe* [_] (c/or describe-form `(every ~(s/mres form) ~@(mapcat identity opts))))))))

;;;;;;;;;;;;;;;;;;;;;;; regex ;;;;;;;;;;;;;;;;;;;
;;See:
;; http://matt.might.net/articles/implementation-of-regular-expression-matching-in-scheme-with-derivatives/
;; http://www.ccs.neu.edu/home/turon/re-deriv.pdf

;;ctors
(defn- accept [x] {::op ::accept :ret x})

(defn- accept? [{:keys [::op]}]
  (= ::accept op))

(defn- pcat* [{[p1 & pr :as ps] :ps,  [k1 & kr :as ks] :ks, [f1 & fr :as forms] :forms, ret :ret, rep+ :rep+}]
  (when (every? identity ps)
    (if (accept? p1)
      (let [rp (:ret p1)
            ret (conj ret (if ks {k1 rp} rp))]
        (if pr
          (pcat* {:ps pr :ks kr :forms fr :ret ret})
          (accept ret)))
      {::op ::pcat, :ps ps, :ret ret, :ks ks, :forms forms :rep+ rep+})))

(defn- pcat [& ps] (pcat* {:ps ps :ret []}))

(defn ^:skip-wiki cat-impl
  "Do not call this directly, use 'cat'"
  [ks ps forms]
  (pcat* {:ks ks, :ps ps, :forms forms, :ret {}}))

(defn- rep* [p1 p2 ret splice form]
  (when p1
    (let [r {::op ::rep, :p2 p2, :splice splice, :forms form :id (random-uuid)}]
      (if (accept? p1)
        (assoc r :p1 p2 :ret (conj ret (:ret p1)))
        (assoc r :p1 p1, :ret ret)))))

(defn ^:skip-wiki rep-impl
  "Do not call this directly, use '*'"
  [form p] (rep* p p [] false form))

(defn ^:skip-wiki rep+impl
  "Do not call this directly, use '+'"
  [form p]
  (pcat* {:ps [p (rep* p p [] true form)] :forms `[~form (* ~form)] :ret [] :rep+ form}))

(defn ^:skip-wiki amp-impl
  "Do not call this directly, use '&'"
  [re re-form preds pred-forms]
  {::op ::amp :p1 re :amp re-form :ps preds :forms pred-forms})

(defn- filter-alt [ps ks forms f]
  (if (c/or ks forms)
    (let [pks (->> (map vector ps
                        (c/or (seq ks) (repeat nil))
                        (c/or (seq forms) (repeat nil)))
                   (filter #(-> % first f)))]
      [(seq (map first pks)) (when ks (seq (map second pks))) (when forms (seq (map #(nth % 2) pks)))])
    [(seq (filter f ps)) ks forms]))

(defn- alt* [ps ks forms]
  (let [[[p1 & pr :as ps] [k1 :as ks] forms] (filter-alt ps ks forms identity)]
    (when ps
      (let [ret {::op ::alt, :ps ps, :ks ks :forms forms}]
        (if (nil? pr)
          (if k1
            (if (accept? p1)
              (accept (tagged-ret k1 (:ret p1)))
              ret)
            p1)
          ret)))))

(defn- alts [& ps] (alt* ps nil nil))
(defn- alt2 [p1 p2] (if (c/and p1 p2) (alts p1 p2) (c/or p1 p2)))

(defn ^:skip-wiki alt-impl
  "Do not call this directly, use 'alt'"
  [ks ps forms] (assoc (alt* ps ks forms) :id (random-uuid)))

(defn ^:skip-wiki maybe-impl
  "Do not call this directly, use '?'"
  [p form] (assoc (alt* [p (accept ::nil)] nil [form ::nil]) :maybe form))

(defn- noret? [p1 pret]
  (c/or (= pret ::nil)
        (c/and (#{::rep ::pcat} (::op (reg-resolve! p1))) ;;hrm, shouldn't know these
               (empty? pret))
        nil))

(declare ^{:arglists '([p])} preturn)

(defn- accept-nil? [p]
  (let [{:keys [::op ps p1 p2 forms] :as p} (reg-resolve! p)]
    (case op
      ::accept true
      nil nil
      ::amp (c/and (accept-nil? p1)
                   (let [ret (-> (preturn p1) (and-preds ps (next forms)))]
                     (not (invalid? ret))))
      ::rep (c/or (identical? p1 p2) (accept-nil? p1))
      ::pcat (every? accept-nil? ps)
      ::alt (c/some accept-nil? ps))))

(declare ^{:arglists '([p r k])} add-ret)

(defn- preturn [p]
  (let [{[p0 & pr :as ps] :ps, [k :as ks] :ks, :keys [::op p1 ret forms] :as p} (reg-resolve! p)]
    (case op
      ::accept ret
      nil nil
      ::amp (let [pret (preturn p1)]
              (if (noret? p1 pret)
                ::nil
                (and-preds pret ps forms)))
      ::rep (add-ret p1 ret k)
      ::pcat (add-ret p0 ret k)
      ::alt (let [[[p0] [k0]] (filter-alt ps ks forms accept-nil?)
                  r (if (nil? p0) ::nil (preturn p0))]
              (if k0 (tagged-ret k0 r) r)))))

(defn- op-unform [p x]
  ;;(prn {:p p :x x})
  (let [{[p0 & pr :as ps] :ps, [k :as ks] :ks, :keys [::op p1 ret forms rep+ maybe] :as p} (reg-resolve! p)
        kps (zipmap ks ps)]
    (case op
      ::accept [ret]
      nil [(unform p x)]
      ::amp (let [px (reduce #(unform %2 %1) x (reverse ps))]
              (op-unform p1 px))
      ::rep (mapcat #(op-unform p1 %) x)
      ::pcat (if rep+
               (mapcat #(op-unform p0 %) x)
               (mapcat (fn [k]
                         (when (contains? x k)
                           (op-unform (kps k) (get x k))))
                 ks))
      ::alt (if maybe
              [(unform p0 x)]
              (let [[k v] x]
                (op-unform (kps k) v))))))

(defn- add-ret [p r k]
  (let [{:keys [::op ps splice] :as p} (reg-resolve! p)
        prop #(let [ret (preturn p)]
               (if (empty? ret) r ((if splice into conj) r (if k {k ret} ret))))]
    (case op
      nil r
      (::alt ::accept ::amp)
      (let [ret (preturn p)]
        ;;(prn {:ret ret})
        (if (= ret ::nil) r (conj r (if k {k ret} ret))))

      (::rep ::pcat) (prop))))

(defn- deriv
  [p x]
  (let [{[p0 & pr :as ps] :ps, [k0 & kr :as ks] :ks, :keys [::op p1 p2 ret splice forms amp] :as p} (reg-resolve! p)]
    (when p
      (case op
        ::accept nil
        nil (let [ret (dt p x p)]
              (when-not (invalid? ret) (accept ret)))
        ::amp (when-let [p1 (deriv p1 x)]
                (if (= ::accept (::op p1))
                  (let [ret (-> (preturn p1) (and-preds ps (next forms)))]
                    (when-not (invalid? ret)
                      (accept ret)))
                  (amp-impl p1 amp ps forms)))
        ::pcat (alt2 (pcat* {:ps (cons (deriv p0 x) pr), :ks ks, :forms forms, :ret ret})
                     (when (accept-nil? p0) (deriv (pcat* {:ps pr, :ks kr, :forms (next forms), :ret (add-ret p0 ret k0)}) x)))
        ::alt (alt* (map #(deriv % x) ps) ks forms)
        ::rep (alt2 (rep* (deriv p1 x) p2 ret splice forms)
                    (when (accept-nil? p1) (deriv (rep* p2 p2 (add-ret p1 ret nil) splice forms) x)))))))

(defn- op-describe [p]
  (let [{:keys [::op ps ks forms splice p1 rep+ maybe amp] :as p} (reg-resolve! p)]
    ;;(prn {:op op :ks ks :forms forms :p p})
    (when p
      (case op
        ::accept nil
        nil p
        ::amp (list* 'cljs.spec.alpha/& amp forms)
        ::pcat (if rep+
                 (list `+ rep+)
                 (cons `cat (mapcat vector (c/or (seq ks) (repeat :_)) forms)))
        ::alt (if maybe
                (list `? maybe)
                (cons `alt (mapcat vector ks forms)))
        ::rep (list (if splice `+ `*) forms)))))

(defn- op-explain [form p path via in input]
  ;;(prn {:form form :p p :path path :input input})
  (let [[x :as input] input
        {:keys [::op ps ks forms splice p1 p2] :as p} (reg-resolve! p)
        via (if-let [name (spec-name p)] (conj via name) via)
        insufficient (fn [path form]
                       [{:path path
                         :reason "Insufficient input"
                         :pred form
                         :val ()
                         :via via
                         :in in}])]
    (when p
      (case op
        ::accept nil
        nil (if (empty? input)
              (insufficient path form)
              (explain-1 form p path via in x))
        ::amp (if (empty? input)
                (if (accept-nil? p1)
                  (explain-pred-list forms ps path via in (preturn p1))
                  (insufficient path (:amp p)))
                (if-let [p1 (deriv p1 x)]
                  (explain-pred-list forms ps path via in (preturn p1))
                  (op-explain (:amp p) p1 path via in input)))
        ::pcat (let [pkfs (map vector
                               ps
                               (c/or (seq ks) (repeat nil))
                               (c/or (seq forms) (repeat nil)))
                     [pred k form] (if (= 1 (count pkfs))
                                     (first pkfs)
                                     (first (remove (fn [[p]] (accept-nil? p)) pkfs)))
                     path (if k (conj path k) path)
                     form (c/or form (op-describe pred))]
                 (if (c/and (empty? input) (not pred))
                   (insufficient path form)
                   (op-explain form pred path via in input)))
        ::alt (if (empty? input)
                (insufficient path (op-describe p))
                (apply concat
                       (map (fn [k form pred]
                              (op-explain (c/or form (op-describe pred))
                                          pred
                                          (if k (conj path k) path)
                                          via
                                          in
                                          input))
                            (c/or (seq ks) (repeat nil))
                            (c/or (seq forms) (repeat nil))
                            ps)))
        ::rep (op-explain (if (identical? p1 p2)
                            forms
                            (op-describe p1))
                          p1 path via in input)))))

(defn- re-gen [p overrides path rmap f]
  ;;(prn {:op op :ks ks :forms forms})
  (let [origp p
        {:keys [::op ps ks p1 p2 forms splice ret id ::gfn] :as p} (reg-resolve! p)
        rmap (if id (inck rmap id) rmap)
        ggens (fn [ps ks forms]
                (let [gen (fn [p k f]
                            ;;(prn {:k k :path path :rmap rmap :op op :id id})
                            (when-not (c/and rmap id k (recur-limit? rmap id path k))
                              (if id
                                (gen/delay (re-gen p overrides (if k (conj path k) path) rmap (c/or f p)))
                                (re-gen p overrides (if k (conj path k) path) rmap (c/or f p)))))]
                  (map gen ps (c/or (seq ks) (repeat nil)) (c/or (seq forms) (repeat nil)))))]
    (c/or (when-let [gfn (c/or (get overrides (spec-name origp))
                               (get overrides (spec-name p))
                               (get overrides path))]
            (case op
              (:accept nil) (gen/fmap vector (gfn))
              (gfn)))
          (when gfn
            (gfn))
          (when p
            (case op
              ::accept (if (= ret ::nil)
                         (gen/return [])
                         (gen/return [ret]))
              nil (when-let [g (gensub p overrides path rmap f)]
                    (gen/fmap vector g))
              ::amp (re-gen p1 overrides path rmap (op-describe p1))
              ::pcat (let [gens (ggens ps ks forms)]
                       (when (every? identity gens)
                         (apply gen/cat gens)))
              ::alt (let [gens (remove nil? (ggens ps ks forms))]
                      (when-not (empty? gens)
                        (gen/one-of gens)))
              ::rep (if (recur-limit? rmap id [id] id)
                      (gen/return [])
                      (when-let [g (re-gen p2 overrides path rmap forms)]
                        (gen/fmap #(apply concat %)
                                  (gen/vector g)))))))))

(defn- re-conform [p [x & xs :as data]]
  ;;(prn {:p p :x x :xs xs})
  (if (empty? data)
    (if (accept-nil? p)
      (let [ret (preturn p)]
        (if (= ret ::nil)
          nil
          ret))
      ::invalid)
    (if-let [dp (deriv p x)]
      (recur dp xs)
      ::invalid)))

(defn- re-explain [path via in re input]
  (loop [p re [x & xs :as data] input i 0]
    ;;(prn {:p p :x x :xs xs :re re}) (prn)
    (if (empty? data)
      (if (accept-nil? p)
        nil ;;success
        (op-explain (op-describe p) p path via in nil))
      (if-let [dp (deriv p x)]
        (recur dp xs (inc i))
        (if (accept? p)
          (if (= (::op p) ::pcat)
            (op-explain (op-describe p) p path via (conj in i) (seq data))
            [{:path path
              :reason "Extra input"
              :pred (op-describe re)
              :val data
              :via via
              :in (conj in i)}])
          (c/or (op-explain (op-describe p) p path via (conj in i) (seq data))
                [{:path path
                  :reason "Extra input"
                  :pred (op-describe p)
                  :val data
                  :via via
                  :in (conj in i)}]))))))

(defn ^:skip-wiki regex-spec-impl
  "Do not call this directly, use 'spec' with a regex op argument"
  [re gfn]
  (reify
    Specize
    (specize* [s] s)
    (specize* [s _] s)

    Spec
    (conform* [_ x]
      (if (c/or (nil? x) (sequential? x))
        (re-conform re (seq x))
        ::invalid))
    (unform* [_ x] (op-unform re x))
    (explain* [_ path via in x]
      (if (c/or (nil? x) (sequential? x))
        (re-explain path via in re (seq x))
        [{:path path :pred `(fn [~'%] (c/or (nil? ~'%) (sequential? ~'%))) :val x :via via :in in}]))
    (gen* [_ overrides path rmap]
      (if gfn
        (gfn)
        (re-gen re overrides path rmap (op-describe re))))
    (with-gen* [_ gfn] (regex-spec-impl re gfn))
    (describe* [_] (op-describe re))))

;;;;;;;;;;;;;;;;; HOFs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- call-valid?
  [f specs args]
  (let [cargs (conform (:args specs) args)]
    (when-not (invalid? cargs)
      (let [ret (apply f args)
            cret (conform (:ret specs) ret)]
        (c/and (not (invalid? cret))
               (if (:fn specs)
                 (pvalid? (:fn specs) {:args cargs :ret cret})
                 true))))))

(defn- validate-fn
  "returns f if valid, else smallest"
  [f specs iters]
  (let [g (gen (:args specs))
        prop (gen/for-all* [g] #(call-valid? f specs %))]
    (let [ret (gen/quick-check iters prop)]
      (if-let [[smallest] (-> ret :shrunk :smallest)]
        smallest
        f))))

(defn ^:skip-wiki fspec-impl
  "Do not call this directly, use 'fspec'"
  [argspec aform retspec rform fnspec fform gfn]
  (let [specs {:args argspec :ret retspec :fn fnspec}]
    (reify
      ILookup
      (-lookup [this k] (get specs k))
      (-lookup [_ k not-found] (get specs k not-found))

      Specize
      (specize* [s] s)
      (specize* [s _] s)

      Spec
      (conform* [_ f] (if (ifn? f)
                        (if (identical? f (validate-fn f specs *fspec-iterations*)) f ::invalid)
                        ::invalid))
      (unform* [_ f] f)
      (explain* [_ path via in f]
        (if (ifn? f)
          (let [args (validate-fn f specs 100)]
            (if (identical? f args) ;;hrm, we might not be able to reproduce
              nil
              (let [ret (try (apply f args) (catch js/Error t t))]
                (if (instance? js/Error ret)
                  ;;TODO add exception data
                  [{:path path :pred '(apply fn) :val args :reason (.-message ret) :via via :in in}]

                  (let [cret (dt retspec ret rform)]
                    (if (invalid? cret)
                      (explain-1 rform retspec (conj path :ret) via in ret)
                      (when fnspec
                        (let [cargs (conform argspec args)]
                          (explain-1 fform fnspec (conj path :fn) via in {:args cargs :ret cret})))))))))
          [{:path path :pred 'ifn? :val f :via via :in in}]))
      (gen* [_ overrides _ _] (if gfn
                        (gfn)
                        (gen/return
                          (fn [& args]
                            (assert (pvalid? argspec args) (with-out-str (explain argspec args)))
                            (gen/generate (gen retspec overrides))))))
      (with-gen* [_ gfn] (fspec-impl argspec aform retspec rform fnspec fform gfn))
      (describe* [_] `(fspec :args ~aform :ret ~rform :fn ~fform)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; non-primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cljs.spec.alpha/def ::kvs->map (cljs.spec.alpha/conformer #(zipmap (map ::k %) (map ::v %)) #(map (fn [[k v]] {::k k ::v v}) %)))

(defn nonconforming
  "takes a spec and returns a spec that has the same properties except
  'conform' returns the original (not the conformed) value. Note, will specize regex ops."
  [spec]
  (let [spec (delay (specize spec))]
    (reify
     Specize
     (specize* [s] s)
     (specize* [s _] s)

     Spec
     (conform* [_ x] (let [ret (conform* @spec x)]
                       (if (invalid? ret)
                         ::invalid
                         x)))
     (unform* [_ x] (unform* @spec x))
     (explain* [_ path via in x] (explain* @spec path via in x))
     (gen* [_ overrides path rmap] (gen* @spec overrides path rmap))
     (with-gen* [_ gfn] (nonconforming (with-gen* @spec gfn)))
     (describe* [_] `(nonconforming ~(describe* @spec))))))

(defn ^:skip-wiki nilable-impl
  "Do not call this directly, use 'nilable'"
  [form pred gfn]
  (let [spec (delay (specize pred form))]
    (reify
      Specize
      (specize* [s] s)
      (specize* [s _] s)

      Spec
      (conform* [_ x] (if (nil? x) nil (conform* @spec x)))
      (unform* [_ x] (if (nil? x) nil (unform* @spec x)))
      (explain* [_ path via in x]
        (when-not (c/or (pvalid? @spec x) (nil? x))
          (conj
            (explain-1 form pred (conj path ::pred) via in x)
            {:path (conj path ::nil) :pred 'nil? :val x :via via :in in})))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (gen/frequency
            [[1 (gen/delay (gen/return nil))]
             [9 (gen/delay (gensub pred overrides (conj path ::pred) rmap form))]])))
      (with-gen* [_ gfn] (nilable-impl form pred gfn))
      (describe* [_] `(nilable ~(s/mres form))))))

(defn exercise
  "generates a number (default 10) of values compatible with spec and maps conform over them,
  returning a sequence of [val conformed-val] tuples. Optionally takes
  a generator overrides map as per gen"
  ([spec] (exercise spec 10))
  ([spec n] (exercise spec n nil))
  ([spec n overrides]
   (map #(vector % (conform spec %)) (gen/sample (gen spec overrides) n))))

(defn inst-in-range?
  "Return true if inst at or after start and before end"
  [start end inst]
  (c/and (inst? inst)
         (let [t (inst-ms inst)]
           (c/and (<= (inst-ms start) t) (< t (inst-ms end))))))

(defn int-in-range?
  "Return true if start <= val, val < end and val is a fixed
  precision integer."
  [start end val]
  (cond
    (integer? val) (c/and (<= start val) (< val end))

    (instance? goog.math.Long val)
    (c/and (.lessThanOrEqual start val)
           (.lessThan val end))

    (instance? goog.math.Integer val)
    (c/and (.lessThanOrEqual start val)
           (.lessThan val end))

    :else false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; assert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defonce
  ^{:dynamic true
    :doc "If true, compiler will enable spec asserts, which are then
subject to runtime control via check-asserts? If false, compiler
will eliminate all spec assert overhead. See 'assert'.
Initially set to the negation of the ':elide-asserts' compiler option.
Defaults to true."}
  *compile-asserts*
  (s/init-compile-asserts))

(defonce ^{:private true
           :dynamic true}
  *runtime-asserts*
  false)

(defn ^boolean check-asserts?
  "Returns the value set by check-asserts."
  []
  *runtime-asserts*)

(defn check-asserts
  "Enable or disable spec asserts that have been compiled
with '*compile-asserts*' true.  See 'assert'.
Initially set to boolean value of cljs.spec.alpha/*runtime-asserts*.
Defaults to false."
  [^boolean flag]
  (set! *runtime-asserts* flag))

(defn assert*
  "Do not call this directly, use 'assert'."
  [spec x]
  (if (valid? spec x)
    x
    (let [ed (c/merge (assoc (explain-data* spec [] [] [] x)
                        ::failure :assertion-failed))]
      (throw (js/Error.
              (str "Spec assertion failed\n" (with-out-str (explain-out ed))))))))
