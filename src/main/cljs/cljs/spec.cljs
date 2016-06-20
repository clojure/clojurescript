;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.spec
  (:refer-clojure :exclude [+ * and or cat def keys])
  (:require-macros [cljs.core :as c]
                   [cljs.spec :as s])
  (:require [goog.object :as gobj]
            [cljs.core :as c]
            [clojure.walk :as walk]
            [cljs.spec.impl.gen :as gen]
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

(def ^:private ^:dynamic *instrument-enabled*
  "if false, instrumented fns call straight through"
  true)

(defprotocol Spec
  (conform* [spec x])
  (unform* [spec y])
  (explain* [spec path via in x])
  (gen* [spec overrides path rmap])
  (with-gen* [spec gfn])
  (describe* [spec]))

(defonce ^:private registry-ref (atom {}))

(defn- named? [x] (implements? INamed x))

(defn- with-name [spec name]
  (with-meta spec (assoc (meta spec) ::name name)))

(defn- spec-name [spec]
  (cond
    (keyword? spec) spec

    (implements? IMeta spec)
    (-> (meta spec) ::name)))

(defn- reg-resolve
  "returns the spec/regex at end of alias chain starting with k, nil if not found, k if k not Named"
  [k]
  (if (named? k)
    (let [reg @registry-ref]
      (loop [spec k]
        (if (named? spec)
          (recur (get reg spec))
          (when spec
            (with-name spec k)))))
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
  (c/and (implements? Spec x) x))

(defn regex?
  "returns x if x is a (clojure.spec) regex op, else logical false"
  [x]
  (c/and (::op x) x))

(declare spec-impl)
(declare regex-spec-impl)

(defn- maybe-spec
  "spec-or-k must be a spec, regex or resolvable kw/sym, else returns nil."
  [spec-or-k]
  (let [s (c/or (spec? spec-or-k)
                (regex? spec-or-k)
                (c/and (named? spec-or-k) (reg-resolve spec-or-k))
                nil)]
    (if (regex? s)
      (with-name (regex-spec-impl s nil) (spec-name s))
      s)))

(defn- the-spec
  "spec-or-k must be a spec, regex or kw/sym, else returns nil. Throws if unresolvable kw/sym"
  [spec-or-k]
  (c/or (maybe-spec spec-or-k)
        (when (named? spec-or-k)
          (throw (js/Error. (str "Unable to resolve spec: " spec-or-k))))))

(defn- specize [s]
  (c/or (the-spec s) (spec-impl ::unknown s nil nil)))

(defn conform
  "Given a spec and a value, returns :clojure.spec/invalid if value does not match spec,
  else the (possibly destructured) value."
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
  (with-gen* (specize spec) gen-fn))

(defn explain-data* [spec path via in x]
  (when-let [probs (explain* (specize spec) path via in x)]
    {::problems probs}))

(defn explain-data
  "Given a spec and a value x which ought to conform, returns nil if x
  conforms, else a map with at least the key ::problems whose value is
  a path->problem-map, where problem-map has at least :pred and :val
  keys describing the predicate and the value that failed at that
  path."
  [spec x]
  (explain-data* spec [] (if-let [name (spec-name spec)] [name] []) [] x))

(defn explain-out
  "prints an explanation to *out*."
  [ed]
  (if ed
    (print
      (with-out-str
        ;;(prn {:ed ed})
        (doseq [[path {:keys [pred val reason via in] :as prob}] (::problems ed)]
          (when-not (empty? in)
            (print "In:" (pr-str in) ""))
          (print "val: ")
          (pr val)
          (print " fails")
          (when-not (empty? via)
            (print " spec:" (pr-str (last via))))
          (when-not (empty? path)
            (print " at:" (pr-str path)))
          (print " predicate: ")
          (pr pred)
          (when reason (print ", " reason))
          (doseq [[k v] prob]
            (when-not (#{:pred :val :reason :via :in} k)
              (print "\n\t" (pr-str k) " ")
              (pr v)))
          (newline))
        (doseq [[k v] ed]
          (when-not (#{::problems} k)
            (print (pr-str k) " ")
            (pr v)
            (newline)))))
    (println "Success!")))

(defn explain
  "Given a spec and a value that fails to conform, prints an explanation to *out*."
  [spec x]
  (explain-out (explain-data spec x)))

(defn explain-str
  "Given a spec and a value that fails to conform, returns an explanation as a string."
  [spec x]
  (with-out-str (explain spec x)))

(declare valid?)

(defn- gensub
  [spec overrides path rmap form]
  ;;(prn {:spec spec :over overrides :path path :form form})
  (let [spec (c/or (get overrides spec) spec)
        spec (specize spec)]
    (if-let [g (c/or (get overrides path) (gen* spec overrides path rmap))]
      (gen/such-that #(valid? spec %) g 100)
      (throw (js/Error. (str "Unable to construct gen at: " path " for: " (abbrev form)))))))

(defn gen
  "Given a spec, returns the generator for it, or throws if none can
  be constructed. Optionally an overrides map can be provided which
  should map spec names or paths (vectors of keywords) to
  generators. These will be used instead of the generators at those
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
  (assert (c/and (named? k) (namespace k)) "k must be namespaced keyword or resolveable symbol")
  (let [spec (if (c/or (spec? spec) (regex? spec) (get @registry-ref spec))
               spec
               (spec-impl form spec nil nil))]
    (swap! registry-ref assoc k spec)
    k))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; instrument ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- expect
  "Returns nil if v conforms to spec, else throws ex-info with explain-data."
  [spec v]
  )

(defn- fn-spec?
  "Fn-spec must include at least :args or :ret specs."
  [m]
  (c/or (:args m) (:ret m)))

(defn- spec-checking-fn
  [v f]
  (let [conform! (fn [v role spec data args]
                   (let [conformed (conform spec data)]
                     (if (= ::invalid conformed)
                       (let [ed (assoc (explain-data* spec [role] [] [] data)
                                  ::args args)]
                         (throw (ex-info
                                  (str "Call to " (pr-str v) " did not conform to spec:\n" (with-out-str (explain-out ed)))
                                  ed)))
                       conformed)))]
    (cond->
      (c/fn
        [& args]
        (if *instrument-enabled*
          (s/with-instrument-disabled
            (let [specs (get-spec v)]
              (when (:args specs) (conform! v :args (:args specs) args args))
              (binding [*instrument-enabled* true]
                (apply f args))))
          (apply f args)))
      (not (instance? MultiFn f)) (doto (gobj/extend f)))))

(defn- macroexpand-check
  [v args]
  (let [specs (get-spec v)]
    (when-let [arg-spec (:args specs)]
      (when (= ::invalid (conform arg-spec args))
        (let [ed (assoc (explain-data* arg-spec [:args]
                          (if-let [name (spec-name arg-spec)] [name] []) [] args)
                   ::args args)]
          (throw (js/Error.
                   (str
                     "Call to " (->sym v) " did not conform to spec:\n"
                     (with-out-str (explain-out ed))))))))))

(defn- no-fn-spec
  [v specs]
  (ex-info (str "Fn at " (pr-str v) " is not spec'ed.")
           {:var v :specs specs}))

(def ^:private instrumented-vars
  "Map for instrumented vars to :raw/:wrapped fns"
  (atom {}))

(defn instrument*
  [v]
  (let [spec (get-spec v)]
    (if (fn-spec? spec)
      (locking instrumented-vars
               (let [{:keys [raw wrapped]} (get @instrumented-vars v)
                     current @v]
                 (when-not (= wrapped current)
                   (let [checked (spec-checking-fn v current)]
                     (swap! instrumented-vars assoc v {:raw current :wrapped checked})
                     checked))))
      (throw (no-fn-spec v spec)))))

(defn unstrument*
  [v]
  (locking instrumented-vars
           (when-let [{:keys [raw wrapped]} (get @instrumented-vars v)]
             (let [current @v]
               (when (= wrapped current)
                 (swap! instrumented-vars dissoc v)
                 raw)))))

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
   (not= ::invalid (dt spec x ::unknown)))
  ([spec x form]
   (not= ::invalid (dt spec x form))))

(defn- explain-1 [form pred path via in v]
  ;;(prn {:form form :pred pred :path path :in in :v v})
  (let [pred (maybe-spec pred)]
    (if (spec? pred)
      (explain* pred path (if-let [name (spec-name pred)] (conj via name) via) in v)
      {path {:pred (abbrev form) :val v :via via :in in}})))

(defn ^:skip-wiki map-spec-impl
  "Do not call this directly, use 'spec' with a map argument"
  [{:keys [req-un opt-un pred-exprs opt-keys req-specs req req-keys opt-specs pred-forms opt gfn]
    :as argm}]
  (let [keys-pred (apply every-pred pred-exprs)
        k->s (zipmap (concat req-keys opt-keys) (concat req-specs opt-specs))
        keys->specs #(c/or (k->s %) %)
        id (random-uuid)]
    (reify
      Spec
      (conform* [_ m]
        (if (keys-pred m)
          (let [reg (registry)]
            (loop [ret m, [k & ks :as keys] (c/keys m)]
              (if keys
                (if (contains? reg (keys->specs k))
                  (let [v (get m k)
                        cv (conform (keys->specs k) v)]
                    (if (= cv ::invalid)
                      ::invalid
                      (recur (if (identical? cv v) ret (assoc ret k cv))
                             ks)))
                  (recur ret ks))
                ret)))
          ::invalid))
      (unform* [_ m]
        (let [reg (registry)]
          (loop [ret m, [k & ks :as keys] (c/keys m)]
            (if keys
              (if (contains? reg (keys->specs k))
                (let [cv (get m k)
                      v (unform (keys->specs k) cv)]
                  (recur (if (identical? cv v) ret (assoc ret k v))
                    ks))
                (recur ret ks))
              ret))))
      (explain* [_ path via in x]
        (if-not (map? x)
          {path {:pred 'map? :val x :via via :in in}}
          (let [reg (registry)]
            (apply merge
                   (when-let [probs (->> (map (fn [pred form] (when-not (pred x) (abbrev form)))
                                              pred-exprs pred-forms)
                                         (keep identity)
                                         seq)]
                     {path {:pred (vec probs) :val x :via via :in in}})
                   (map (fn [[k v]]
                          (when-not (c/or (not (contains? reg (keys->specs k)))
                                          (valid? (keys->specs k) v k))
                            (explain-1 (keys->specs k) (keys->specs k) (conj path k) via (conj in k) v)))
                        (seq x))))))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (let [rmap (inck rmap id)
                gen (fn [k s] (gensub s overrides (conj path k) rmap k))
                ogen (fn [k s]
                       (when-not (recur-limit? rmap id path k)
                         [k (gen/delay (gensub s overrides (conj path k) rmap k))]))
                req-gens (map gen req-keys req-specs)
                opt-gens (remove nil? (map ogen opt-keys opt-specs))]
            (when (every? identity (concat req-gens opt-gens))
              (let [reqs (zipmap req-keys req-gens)
                    opts (into {} opt-gens)]
                (gen/bind (gen/choose 0 (count opts))
                          #(let [args (concat (seq reqs) (when (seq opts) (shuffle (seq opts))))]
                            (->> args
                                 (take (c/+ % (count reqs)))
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
     (named? pred) (cond-> (the-spec pred) gfn (with-gen gfn))
     :else
     (reify
       Spec
       (conform* [_ x] (dt pred x form cpred?))
       (unform* [_ x] (if cpred?
                        (if unc
                          (unc x)
                          (throw (js/Error. "no unform fn for conformer")))
                        x))
       (explain* [_ path via in x]
         (when (= ::invalid (dt pred x form cpred?))
           {path {:pred (abbrev form) :val x :via via :in in}}))
       (gen* [_ _ _ _] (if gfn
                         (gfn)
                         (gen/gen-for-pred pred)))
       (with-gen* [_ gfn] (spec-impl form pred gfn cpred?))
       (describe* [_] form)))))

(defn ^:skip-wiki multi-spec-impl
  "Do not call this directly, use 'multi-spec'"
  ([form mmvar retag] (multi-spec-impl form mmvar retag nil))
  ([form mmvar retag gfn]
   (let [id (random-uuid)
         predx #(let [mm @mmvar]
                 (c/and (contains? (methods mm)
                                   ((-dispatch-fn mm) %))
                        (mm %)))
         dval #((-dispatch-fn @mmvar) %)
         tag (if (keyword? retag)
               #(assoc %1 retag %2)
               retag)]
     (reify
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
             {path {:pred form :val x :reason "no method" :via via :in in}})))
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
                         (remove (fn [[k]] (= k ::invalid)))
                         (map gen)
                         (remove nil?))]
             (when (every? identity gs)
               (gen/one-of gs)))))
       (with-gen* [_ gfn] (multi-spec-impl form mmvar retag gfn))
       (describe* [_] `(multi-spec ~form))))))

(defn ^:skip-wiki tuple-impl
  "Do not call this directly, use 'tuple'"
  ([forms preds] (tuple-impl forms preds nil))
  ([forms preds gfn]
   (reify
     Spec
     (conform* [_ x]
       (if-not (c/and (vector? x)
                      (= (count x) (count preds)))
         ::invalid
         (loop [ret x, i 0]
           (if (= i (count x))
             ret
             (let [v (x i)
                   cv (dt (preds i) v (forms i))]
               (if (= ::invalid cv)
                 ::invalid
                 (recur (if (identical? cv v) ret (assoc ret i cv))
                        (inc i))))))))
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
         {path {:pred 'vector? :val x :via via :in in}}

         (not= (count x) (count preds))
         {path {:pred `(= (count ~'%) ~(count preds)) :val x :via via :in in}}

         :else
         (apply merge
                (map (fn [i form pred]
                       (let [v (x i)]
                         (when-not (valid? pred v)
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
     (describe* [_] `(tuple ~@forms)))))

(defn- tagged-ret [v]
  (specify! v
    IMapEntry
    (-key [_] (-nth v 0))
    (-val [_] (-nth v 1))))

(defn ^:skip-wiki or-spec-impl
  "Do not call this directly, use 'or'"
  [keys forms preds gfn]
  (let [id (random-uuid)
        kps (zipmap keys preds)
        cform (fn [x]
                (loop [i 0]
                  (if (< i (count preds))
                    (let [pred (preds i)]
                      (let [ret (dt pred x (nth forms i))]
                        (if (= ::invalid ret)
                          (recur (inc i))
                          (tagged-ret [(keys i) ret]))))
                    ::invalid)))]
    (reify
      Spec
      (conform* [_ x] (cform x))
      (unform* [_ [k x]] (unform (kps k) x))
      (explain* [this path via in x]
        (when-not (valid? this x)
          (apply merge
                 (map (fn [k form pred]
                        (when-not (valid? pred x)
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
        (if (= ::invalid nret)
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
        (if (not= ::invalid nret)
          (recur nret forms preds)
          (explain-1 form pred path via in ret))))))

(defn ^:skip-wiki and-spec-impl
  "Do not call this directly, use 'and'"
  [forms preds gfn]
  (reify
    Spec
    (conform* [_ x] (and-preds x preds forms))
    (unform* [_ x] (reduce #(unform %2 %1) x (reverse preds)))
    (explain* [_ path via in x] (explain-pred-list forms preds path via in x))
    (gen* [_ overrides path rmap] (if gfn (gfn) (gensub (first preds) overrides path rmap (first forms))))
    (with-gen* [_ gfn] (and-spec-impl forms preds gfn))
    (describe* [_] `(and ~@forms))))

(defn ^:skip-wiki every-impl
  "Do not call this directly, use 'every'"
  ([form pred opts] (every-impl form pred opts nil))
  ([form pred {:keys [count max-count min-count distinct gen-max gen-into ::kfn]
               :or {gen-max 20, gen-into []}
               :as opts}
    gfn]
   (let [check? #(valid? pred %)
         kfn (c/or kfn (fn [i v] i))]
     (reify
       Spec
       (conform* [_ x]
         (cond
           (c/or (not (seqable? x))
             (c/and distinct (not (empty? x)) (not (apply distinct? x)))
             (c/and count (not= count (bounded-count (inc count) x)))
             (c/and (c/or min-count max-count)
               (not (<= (c/or min-count 0)
                      (bounded-count (if max-count (inc max-count) min-count) x)
                      (c/or max-count MAX_INT)))))
           ::invalid

           :else
           (if (indexed? x)
             (let [step (max 1 (long (/ (c/count x) *coll-check-limit*)))]
               (loop [i 0]
                 (if (>= i (c/count x))
                   x
                   (if (check? (nth x i))
                     (recur (c/+ i step))
                     ::invalid))))
             (c/or (c/and (every? check? (take *coll-check-limit* x)) x)
               ::invalid))))
       (unform* [_ x] x)
       (explain* [_ path via in x]
         (cond
           (not (seqable? x))
           {path {:pred 'seqable? :val x :via via :in in}}

           (c/and distinct (not (empty? x)) (not (apply distinct? x)))
           {path {:pred 'distinct? :val x :via via :in in}}

           (c/and count (not= count (bounded-count count x)))
           {path {:pred `(= ~count (c/count %)) :val x :via via :in in}}

           (c/and (c/or min-count max-count)
             (not (<= (c/or min-count 0)
                    (bounded-count (if max-count (inc max-count) min-count) x)
                    (c/or max-count MAX_INT))))
           {path {:pred `(<= ~(c/or min-count 0) (c/count %) ~(c/or max-count 'js/Number.MAX_SAFE_INTEGER)) :val x :via via :in in}}

           :else
           (apply merge
             (take *coll-error-limit*
               (keep identity
                 (map (fn [i v]
                        (let [k (kfn i v)]
                          (when-not (check? v)
                            (let [prob (explain-1 form pred (conj path k) via (conj in k) v)]
                              prob))))
                   (range) x))))))
       (gen* [_ overrides path rmap]
         (if gfn
           (gfn)
           (let [init (empty gen-into)
                 pgen (gensub pred overrides path rmap form)]
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
                 (gen/vector pgen 0 gen-max))))))

       (with-gen* [_ gfn] (every-impl form pred opts gfn))
       (describe* [_] `(every ~form ~@(mapcat identity opts)))))))

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
  [re preds pred-forms]
  {::op ::amp :p1 re :ps preds :forms pred-forms})

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
              (accept (tagged-ret [k1 (:ret p1)]))
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

(declare preturn)

(defn- accept-nil? [p]
  (let [{:keys [::op ps p1 p2 forms] :as p} (reg-resolve! p)]
    (case op
      ::accept true
      nil nil
      ::amp (c/and (accept-nil? p1)
                   (c/or (noret? p1 (preturn p1))
                         (let [ret (-> (preturn p1) (and-preds ps (next forms)))]
                           (not= ret ::invalid))))
      ::rep (c/or (identical? p1 p2) (accept-nil? p1))
      ::pcat (every? accept-nil? ps)
      ::alt (c/some accept-nil? ps))))

(declare add-ret)

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
              (if k0 (tagged-ret [k0 r]) r)))))

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
  (let [{[p0 & pr :as ps] :ps, [k0 & kr :as ks] :ks, :keys [::op p1 p2 ret splice forms] :as p} (reg-resolve! p)]
    (when p
      (case op
        ::accept nil
        nil (let [ret (dt p x p)]
              (when-not (= ::invalid ret) (accept ret)))
        ::amp (when-let [p1 (deriv p1 x)]
                (if (= ::accept (::op p1))
                  (let [ret (-> (preturn p1) (and-preds ps (next forms)))]
                    (when-not (= ret ::invalid)
                      (accept ret)))
                  (amp-impl p1 ps forms)))
        ::pcat (alt2 (pcat* {:ps (cons (deriv p0 x) pr), :ks ks, :forms forms, :ret ret})
                     (when (accept-nil? p0) (deriv (pcat* {:ps pr, :ks kr, :forms (next forms), :ret (add-ret p0 ret k0)}) x)))
        ::alt (alt* (map #(deriv % x) ps) ks forms)
        ::rep (alt2 (rep* (deriv p1 x) p2 ret splice forms)
                    (when (accept-nil? p1) (deriv (rep* p2 p2 (add-ret p1 ret nil) splice forms) x)))))))

(defn- op-describe [p]
  (let [{:keys [::op ps ks forms splice p1 rep+ maybe] :as p} (reg-resolve! p)]
    ;;(prn {:op op :ks ks :forms forms :p p})
    (when p
      (case op
        ::accept nil
        nil p
        ::amp (list* 'clojure.spec/& (op-describe p1) forms)
        ::pcat (if rep+
                 (list `+ rep+)
                 (cons `cat (mapcat vector (c/or (seq ks) (repeat :_)) (c/or (seq forms) (repeat nil)))))
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
                       {path {:reason "Insufficient input"
                              :pred (abbrev form)
                              :val ()
                              :via via
                              :in in}})]
    (when p
      (case op
        ::accept nil
        nil (if (empty? input)
              (insufficient path form)
              (explain-1 form p path via in x))
        ::amp (if (empty? input)
                (if (accept-nil? p1)
                  (explain-pred-list forms ps path via in (preturn p1))
                  (insufficient path (op-describe p1)))
                (if-let [p1 (deriv p1 x)]
                  (explain-pred-list forms ps path via in (preturn p1))
                  (op-explain (op-describe p1) p1 path via in input)))
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
                (apply merge
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
  (let [{:keys [::op ps ks p1 p2 forms splice ret id] :as p} (reg-resolve! p)
        rmap (if id (inck rmap id) rmap)
        ggens (fn [ps ks forms]
                (let [gen (fn [p k f]
                            ;;(prn {:k k :path path :rmap rmap :op op :id id})
                            (when-not (c/and rmap id k (recur-limit? rmap id path k))
                              (if id
                                (gen/delay (re-gen p overrides (if k (conj path k) path) rmap (c/or f p)))
                                (re-gen p overrides (if k (conj path k) path) rmap (c/or f p)))))]
                  (map gen ps (c/or (seq ks) (repeat nil)) (c/or (seq forms) (repeat nil)))))]
    (c/or (when-let [g (get overrides path)]
            (case op
              (:accept nil) (gen/fmap vector g)
              g))
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
            {path {:reason "Extra input"
                   :pred (abbrev (op-describe re))
                   :val data
                   :via via
                   :in (conj in i)}})
          (c/or (op-explain (op-describe p) p path via (conj in i) (seq data))
                {path {:reason "Extra input"
                       :pred (abbrev (op-describe p))
                       :val data
                       :via via
                       :in (conj in i)}}))))))

(defn ^:skip-wiki regex-spec-impl
  "Do not call this directly, use 'spec' with a regex op argument"
  [re gfn]
  (reify
    Spec
    (conform* [_ x]
      (if (c/or (nil? x) (coll? x))
        (re-conform re (seq x))
        ::invalid))
    (unform* [_ x] (op-unform re x))
    (explain* [_ path via in x]
      (if (c/or (nil? x) (coll? x))
        (re-explain path via in re (seq x))
        {path {:pred (abbrev (op-describe re)) :val x :via via :in in}}))
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
    (when-not (= cargs ::invalid)
      (let [ret (apply f args)
            cret (conform (:ret specs) ret)]
        (c/and (not= cret ::invalid)
               (if (:fn specs)
                 (valid? (:fn specs) {:args cargs :ret cret})
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
                  {path {:pred '(apply fn) :val args :reason (.-message ret) :via via :in in}}

                  (let [cret (dt retspec ret rform)]
                    (if (= ::invalid cret)
                      (explain-1 rform retspec (conj path :ret) via in ret)
                      (when fnspec
                        (let [cargs (conform argspec args)]
                          (explain-1 fform fnspec (conj path :fn) via in {:args cargs :ret cret})))))))))
          {path {:pred 'ifn? :val f :via via :in in}}))
      (gen* [_ _ _ _] (if gfn
                        (gfn)
                        (gen/return
                          (fn [& args]
                            (assert (valid? argspec args) (with-out-str (explain argspec args)))
                            (gen/generate (gen retspec))))))
      (with-gen* [_ gfn] (fspec-impl argspec aform retspec rform fnspec fform gfn))
      (describe* [_] `(fspec :args ~aform :ret ~rform :fn ~fform)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; non-primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cljs.spec/def ::any (cljs.spec/spec (constantly true) :gen gen/any))
(cljs.spec/def ::kvs->map (cljs.spec/conformer #(zipmap (map ::k %) (map ::v %)) #(map (fn [[k v]] {::k k ::v v}) %)))

(defn exercise
  "generates a number (default 10) of values compatible with spec and maps conform over them,
  returning a sequence of [val conformed-val] tuples. Optionally takes
  a generator overrides map as per gen"
  ([spec] (exercise spec 10))
  ([spec n] (exercise spec n nil))
  ([spec n overrides]
   (map #(vector % (conform spec %)) (gen/sample (gen spec overrides) n))))

(defn coll-checker
  "returns a predicate function that checks *coll-check-limit* items in a collection with pred"
  [pred]
  (let [check? #(valid? pred %)]
    (fn [coll]
      (c/or (nil? coll)
            (c/and
              (coll? coll)
              (every? check? (take *coll-check-limit* coll)))))))

(defn coll-gen
  "returns a function of no args that returns a generator of
  collections of items conforming to pred, with the same shape as
  init-coll"
  [pred init-coll]
  (let [init (empty init-coll)]
    (fn []
      (gen/fmap
        #(if (vector? init) % (into init %))
        (gen/vector (gen pred))))))

(defn inst-in-range?
  "Return true if inst at or after start and before end"
  [start end inst]
  (c/and (inst? inst)
         (let [t (inst-ms inst)]
           (c/and (<= (inst-ms start) t) (< t (inst-ms end))))))

(defn int-in-range?
  "Return true if start <= val and val < end"
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