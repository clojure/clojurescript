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
                            with-loading-context with-local-vars with-open with-out-str with-precision with-redefs]))

(alias 'core 'clojure.core)

(defmacro import-macros [ns [& vars]]
  (core/let [ns (find-ns ns)
        vars (map #(ns-resolve ns %) vars)
        syms (map #(core/-> % .sym (with-meta {:macro true})) vars)
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

(defn- protocol-prefix [psym]
  (str (.replace (str psym) \. \$) "$"))

(defmacro extend-type [tsym & impls]
  (let [resolve #(let [ret (:name (cljs.compiler/resolve-var (dissoc &env :locals) %))]
                   (assert ret (str "Can't resolve: " %))
                   ret)
        t (resolve tsym)
        prototype-prefix (str t ".prototype.")
        impl-map (loop [ret {} s impls]
                   (if (seq s)
                     (recur (assoc ret (resolve (first s)) (take-while seq? (next s)))
                            (drop-while seq? (next s)))
                     ret))
        assign-impls (fn [[psym sigs]]
                       (let [pprefix (protocol-prefix psym)]
                         (map (fn [[f & meths]]
                                `(set! ~(symbol (str prototype-prefix pprefix f)) (fn* ~@meths)))
                              sigs)))]
    `(do ~@(mapcat assign-impls impl-map))))

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
                     ret)))
        ]
    (if (seq impls)
      `(do
         (deftype* ~t ~fields)
         (extend-type ~t ~@(dt->et impls)))
      `(deftype* ~t ~fields))))

(defmacro defprotocol [psym & doc+methods]
  (let [p (:name (cljs.compiler/resolve-var (dissoc &env :locals) psym))
        prefix (protocol-prefix p)
        methods (if (string? (first doc+methods)) (next doc+methods) doc+methods)
        expand-sig (fn [slot sig]
                     `(~sig (. ~(first sig) ~slot ~@sig)))
        method (fn [[fname & sigs]]
                 (let [sigs (take-while vector? sigs)
                       slot (symbol (str prefix (name fname)))]
                   `(defn ~fname ~@(map #(expand-sig slot %) sigs))))]
    `(do
       (def ~p ~prefix)
       ~@(map method methods))))

(defprotocol ICounted
  (icount [coll] "constant time count"))

#_(defprotocol IEmptyableCollection
  (iempty [coll]))

(defprotocol ICollection
  (iconj [coll o]))

#_(defprotocol IOrdinal
    (iindex [coll]))

(defprotocol IIndexed
  (inth [coll n])
  (inth [coll n not-found]))

(defprotocol ISeq
  (ifirst [coll])
  (irest [coll]))

(defprotocol ILookup
  (ilookup [o k])
  (ilookup [o k not-found]))

(defprotocol IAssociative
  #_(icontains-key? [coll k])
  #_(ientry-at [coll k])
  (iassoc [coll k v]))

(defprotocol IMap
  #_(iassoc-ex [coll k v])
  (iwithout [coll k]))

(defprotocol ISet
  (icontains? [coll v])
  (idisjoin [coll v])
  (iget [coll v]))

(defprotocol IStack
  (ipeek [coll])
  (ipop [coll]))

(defprotocol IVector
  (iassoc-n [coll n val]))

(defprotocol IDeref
  (ideref [o]))

(defprotocol IDerefWithTimeout
  (ideref-with-timeout [o msec timeout-val]))

(defprotocol IMeta
  (imeta [o]))

(defprotocol IWithMeta
  (iwith-meta [o meta]))
