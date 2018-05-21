;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.spec.gen.alpha
  (:refer-clojure :exclude [boolean cat hash-map list map not-empty set vector
                            char double int keyword symbol string uuid delay])
  (:require-macros [cljs.core :as c]
                   [cljs.spec.gen.alpha :as gen :refer [dynaload lazy-combinators lazy-prims]])
  (:require [cljs.core :as c])
  (:import (goog Uri)))

(deftype LazyVar [f ^:mutable cached]
  IDeref
  (-deref [this]
    (if-not (nil? cached)
      cached
      (let [x (f)]
        (when-not (nil? x)
          (set! cached x))
        x))))

(def ^:private quick-check-ref
  (dynaload 'clojure.test.check/quick-check))

(defn quick-check
  [& args]
  (apply @quick-check-ref args))

(def ^:private for-all*-ref
  (dynaload 'clojure.test.check.properties/for-all*))

(defn for-all*
  "Dynamically loaded clojure.test.check.properties/for-all*."
  [& args]
  (apply @for-all*-ref args))

(let [g? (dynaload 'clojure.test.check.generators/generator?)
      g (dynaload 'clojure.test.check.generators/generate)
      mkg (dynaload 'clojure.test.check.generators/->Generator)]
  (defn- generator?
    [x]
    (@g? x))
  (defn- generator
    [gfn]
    (@mkg gfn))
  (defn generate
    "Generate a single value using generator."
    [generator]
    (@g generator)))

(defn ^:skip-wiki delay-impl
  [gfnd]
  ;;N.B. depends on test.check impl details
  (generator (fn [rnd size]
               ((:gen @gfnd) rnd size))))

;(defn gen-for-name
;  "Dynamically loads test.check generator named s."
;  [s]
;  (let [g (dynaload s)]
;    (if (generator? g)
;      g
;      (throw (js/Error. (str "Var " s " is not a generator"))))))

(lazy-combinators hash-map list map not-empty set vector vector-distinct fmap elements
  bind choose one-of such-that tuple sample return
  large-integer* double* frequency)

(lazy-prims any any-printable boolean char char-alpha char-alphanumeric char-ascii double
  int keyword keyword-ns large-integer ratio simple-type simple-type-printable
  string string-ascii string-alphanumeric symbol symbol-ns uuid)

(defn cat
  "Returns a generator of a sequence catenated from results of
gens, each of which should generate something sequential."
  [& gens]
  (fmap #(apply concat %)
    (apply tuple gens)))

(defn- ^boolean qualified? [ident] (not (nil? (namespace ident))))

(def ^:private
gen-builtins
  (c/delay
    (let [simple (simple-type-printable)]
      {any? (one-of [(return nil) (any-printable)])
       number? (one-of [(large-integer) (double)])
       integer? (large-integer)
       int? (large-integer)
       pos-int? (large-integer* {:min 1})
       neg-int? (large-integer* {:max -1})
       nat-int? (large-integer* {:min 0})
       float? (double)
       double? (double)
       string? (string-alphanumeric)
       ident? (one-of [(keyword-ns) (symbol-ns)])
       simple-ident? (one-of [(keyword) (symbol)])
       qualified-ident? (such-that qualified? (one-of [(keyword-ns) (symbol-ns)]))
       keyword? (keyword-ns)
       simple-keyword? (keyword)
       qualified-keyword? (such-that qualified? (keyword-ns))
       symbol? (symbol-ns)
       simple-symbol? (symbol)
       qualified-symbol? (such-that qualified? (symbol-ns))
       uuid? (uuid)
       uri? (fmap #(Uri. (str "http://" % ".com")) (uuid))
       inst? (fmap #(js/Date. %)
                    (large-integer))
       seqable? (one-of [(return nil)
                         (list simple)
                         (vector simple)
                         (map simple simple)
                         (set simple)
                         (string-alphanumeric)])
       indexed? (vector simple)
       map? (map simple simple)
       vector? (vector simple)
       list? (list simple)
       seq? (list simple)
       char? (char)
       set? (set simple)
       nil? (return nil)
       false? (return false)
       true? (return true)
       boolean? (boolean)
       zero? (return 0)
       ;rational? (one-of [(large-integer) (ratio)])
       coll? (one-of [(map simple simple)
                      (list simple)
                      (vector simple)
                      (set simple)])
       empty? (elements [nil '() [] {} #{}])
       associative? (one-of [(map simple simple) (vector simple)])
       sequential? (one-of [(list simple) (vector simple)])
       ;ratio? (such-that ratio? (ratio))
       })))

(defn gen-for-pred
  "Given a predicate, returns a built-in generator if one exists."
  [pred]
  (if (set? pred)
    (elements pred)
    (get @gen-builtins pred)))

(comment
  (require 'clojure.test.check)
  (require 'clojure.test.check.properties)
  (require 'cljs.spec.gen)
  (in-ns 'cljs.spec.gen)

  ;; combinators, see call to lazy-combinators above for complete list
  (generate (one-of [(gen-for-pred integer?) (gen-for-pred string?)]))
  (generate (such-that #(< 10000 %) (gen-for-pred integer?)))
  (let [reqs {:a (gen-for-pred number?)
              :b (gen-for-pred keyword?)}
        opts {:c (gen-for-pred string?)}]
    (generate (bind (choose 0 (count opts))
                #(let [args (concat (seq reqs) (shuffle (seq opts)))]
                  (->> args
                    (take (+ % (count reqs)))
                    (mapcat identity)
                    (apply hash-map))))))
  (generate (cat (list (gen-for-pred string?))
              (list (gen-for-pred integer?))))

  ;; load your own generator
  ;(gen-for-name 'clojure.test.check.generators/int)

  ;; failure modes
  ;(gen-for-name 'unqualified)
  ;(gen-for-name 'clojure.core/+)
  ;(gen-for-name 'clojure.core/name-does-not-exist)
  ;(gen-for-name 'ns.does.not.exist/f)

  )


