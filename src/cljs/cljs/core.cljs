;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core
  (:require [goog.string :as gstring]
            [goog.string.StringBuffer :as gstringbuf]
            [goog.object :as gobject]
            [goog.array :as garray]))

(defn truth_
  "Internal - do not use!"
  [x]
  (js* "(~{x} != null && ~{x} !== false)"))

(defn type_satisfies_
  "Internal - do not use!"
  [p x]
  (or
   (aget p (goog.typeOf x))
   (aget p "_")
   false))

(def
  ^{:doc "When compiled for a command-line target, whatever
  function *main-fn* is set to will be called with the command-line
  argv as arguments"}
  *main-cli-fn* nil)

(defn missing-protocol [proto obj]
  (js/Error (js* "~{}+~{}+~{}+~{}+~{}+~{}"
                 "No protocol method " proto
                 " defined for type " (goog/typeOf obj) ": " obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arrays ;;;;;;;;;;;;;;;;

(defn aclone
  "Returns a javascript array, cloned from the passed in array"
  [array-like]
  #_(goog.array.clone array-like)
  (js* "Array.prototype.slice.call(~{array-like})"))

(defn array
  "Creates a new javascript array.
@param {...*} var_args" ;;array is a special case, don't emulate this doc string
  [var-args]            ;; [& items]
  (js* "Array.prototype.slice.call(arguments)"))

(defn aget
  "Returns the value at the index."
  [array i]
  (js* "~{array}[~{i}]"))

(defn aset
  "Sets the value at the index."
  [array i val]
  (js* "(~{array}[~{i}] = ~{val})"))

(defn alength
  "Returns the length of the Java array. Works on arrays of all types."
  [array]
  (js* "~{array}.length"))

;;;;;;;;;;;;;;;;;;;;;;;;;;; core protocols ;;;;;;;;;;;;;
(defprotocol ICounted
  (-count [coll] "constant time count"))

(defprotocol IEmptyableCollection
  (-empty [coll]))

(defprotocol ICollection
  (-conj [coll o]))

#_(defprotocol IOrdinal
    (-index [coll]))

(defprotocol IIndexed
  (-nth [coll n] [coll n not-found]))

(defprotocol ISeq
  (-first [coll])
  (-rest [coll]))

(defprotocol ILookup
  (-lookup [o k] [o k not-found]))

(defprotocol IAssociative
  (-contains-key? [coll k])
  #_(-entry-at [coll k])
  (-assoc [coll k v]))

(defprotocol IMap
  #_(-assoc-ex [coll k v])
  (-dissoc [coll k]))

(defprotocol ISet
  (-disjoin [coll v]))

(defprotocol IStack
  (-peek [coll])
  (-pop [coll]))

(defprotocol IVector
  (-assoc-n [coll n val]))

(defprotocol IDeref
 (-deref [o]))

(defprotocol IDerefWithTimeout
  (-deref-with-timeout [o msec timeout-val]))

(defprotocol IMeta
  (-meta [o]))

(defprotocol IWithMeta
  (-with-meta [o meta]))

(defprotocol IReduce
  (-reduce [coll f] [coll f start]))

(defprotocol IEquiv
  (-equiv [o other]))

(defprotocol IHash
  (-hash [o]))

(defprotocol ISeqable
  (-seq [o]))

(defprotocol ISequential
  "Marker interface indicating a persistent collection of sequential items")

(defprotocol IRecord
  "Marker interface indicating a record object")

(defprotocol IPrintable
  (-pr-seq [o opts]))

(defprotocol IPending
  (-realized? [d]))

(defprotocol IWatchable
  (-notify-watches [this oldval newval])
  (-add-watch [this key f])
  (-remove-watch [this key]))

;;;;;;;;;;;;;;;;;;; fundamentals ;;;;;;;;;;;;;;;
(defn identical? [x y]
  "Tests if 2 arguments are the same object"
  (js* "(~{x} === ~{y})"))

(defn = [x y]
  (-equiv x y))

(defn nil? [x]
  "Returns true if x is nil, false otherwise."
  (identical? x nil))

;;;;;;;;;;;;;;;;;;; protocols on primitives ;;;;;;;;
(declare hash-map list equiv-sequential)

(extend-type nil
  IEquiv
  (-equiv [_ o] (nil? o))

  ICounted
  (-count [_] 0)

  IEmptyableCollection
  (-empty [_] nil)

  ICollection
  (-conj [_ o] (list o))

  IIndexed
  (-nth
   ([_ n] nil)
   ([_ n not-found] not-found))

  ISeq
  (-first [_] nil)
  (-rest [_] (list))

  ILookup
  (-lookup
   ([o k] nil)
   ([o k not-found] not-found))

  IAssociative
  (-assoc [_ k v] (hash-map k v))

  IMap
  (-dissoc [_ k] nil)

  ISet
  (-disjoin [_ v] nil)

  IStack
  (-peek [_] nil)
  (-pop [_] nil)

  IMeta
  (-meta [_] nil)

  IWithMeta
  (-with-meta [_ meta] nil)

  IReduce
  (-reduce
    ([_ f] (f))
    ([_ f start] start))

  IHash
  (-hash [o] 0))

(extend-type js/Date
  IEquiv
  (-equiv [o other] (identical? (. o (toString)) (. other (toString)))))

(extend-type number
  IEquiv
  (-equiv [x o] (identical? x o))

  IHash
  (-hash [o] o))

(extend-type function
  IHash
  (-hash [o] (goog.getUid o)))

;;this is primitive because & emits call to array-seq
(defn inc
  "Returns a number one greater than num."
  [x] (js* "(~{x} + 1)"))

(defn- ci-reduce
  "Accepts any collection which satisfies the ICount and IIndexed protocols and
reduces them without incurring seq initialization"
  ([cicoll f]
     (if (= 0 (-count cicoll))
       (f)
       (loop [val (-nth cicoll 0), n 1]
         (if (< n (-count cicoll))
           (recur (f val (-nth cicoll n)) (inc n))
           val))))
  ([cicoll f val]
     (loop [val val, n 0]
         (if (< n (-count cicoll))
           (recur (f val (-nth cicoll n)) (inc n))
           val)))
  ([cicoll f val idx]
     (loop [val val, n idx]
         (if (< n (-count cicoll))
           (recur (f val (-nth cicoll n)) (inc n))
           val))))

(deftype IndexedSeq [a i]
  ISeqable
  (-seq [this] this)
  ISeq
  (-first [_] (aget a i))
  (-rest [_] (if (< (inc i) (.length a))
               (IndexedSeq. a (inc i))
               (list)))
  ICounted
  (-count [_] (.length a))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IReduce
  (-reduce [coll f]
    (ci-reduce coll f (aget a i) (inc i)))
  (-reduce [coll f start]
    (ci-reduce coll f start i))

  IHash
  (-hash [coll] (hash-coll coll)))

(defn prim-seq [prim i]
  (when-not (= 0 (.length prim))
    (IndexedSeq. prim i)))

(defn array-seq [array i]
  (prim-seq array i))

(extend-type array
  ISeqable
  (-seq [array] (array-seq array 0))

  ICounted
  (-count [a] (.length a))

  IIndexed
  (-nth
    ([array n]
       (if (< n (.length array)) (aget array n)))
    ([array n not-found]
       (if (< n (.length array)) (aget array n)
           not-found)))

  ILookup
  (-lookup
    ([array k]
       (aget array k))
    ([array k not-found]
       (-nth array k not-found)))

  IReduce
  (-reduce
    ([array f]
       (ci-reduce array f))
    ([array f start]
       (ci-reduce array f start))))

(defn seq
  "Returns a seq on the collection. If the collection is
  empty, returns nil.  (seq nil) returns nil. seq also works on
  Strings."
  [coll]
  (when coll
    (-seq coll)))

(defn first
  "Returns the first item in the collection. Calls seq on its
  argument. If coll is nil, returns nil."
  [coll]
  (when-let [s (seq coll)]
    (-first s)))

(defn rest
  "Returns a possibly empty seq of the items after the first. Calls seq on its
  argument."
  [coll]
  (-rest (seq coll)))

(defn next
  "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil"
  [coll]
  (when coll
    (seq (rest coll))))

(defn second
  "Same as (first (next x))"
  [coll]
  (first (next coll)))

(defn ffirst
  "Same as (first (first x))"
  [coll]
  (first (first coll)))

(defn nfirst
  "Same as (next (first x))"
  [coll]
  (next (first coll)))

(defn fnext
  "Same as (first (next x))"
  [coll]
  (first (next coll)))

(defn nnext
  "Same as (next (next x))"
  [coll]
  (next (next coll)))

(defn last
  "Return the last item in coll, in linear time"
  [s]
  (if (next s)
    (recur (next s))
    (first s)))

(extend-type default
  IEquiv
  (-equiv [x o] (identical? x o))

  ICounted
  (-count [x]
    (loop [s (seq x) n 0]
      (if s
	(recur (next s) (inc n))
	n))))

(defn not
  "Returns true if x is logical false, false otherwise."
  [x] (if x false true))

(defn conj
  "conj[oin]. Returns a new collection with the xs
  'added'. (conj nil item) returns (item).  The 'addition' may
  happen at different 'places' depending on the concrete type."
  ([coll x]
     (-conj coll x))
  ([coll x & xs]
     (if xs
       (recur (conj coll x) (first xs) (next xs))
       (conj coll x))))

(defn empty
  "Returns an empty collection of the same category as coll, or nil"
  [coll]
  (-empty coll))

(defn count
  "Returns the number of items in the collection. (count nil) returns
  0.  Also works on strings, arrays, and Maps"
  [coll]
  (-count coll))

(defn nth
  "Returns the value at the index. get returns nil if index out of
  bounds, nth throws an exception unless not-found is supplied.  nth
  also works for strings, arrays, regex Matchers and Lists, and,
  in O(n) time, for sequences."
  ([coll n]
     (-nth coll n))
  ([coll n not-found]
     (-nth coll n not-found)))

(defn get
  "Returns the value mapped to key, not-found or nil if key not present."
  ([o k]
     (-lookup o k))
  ([o k not-found]
     (-lookup o k not-found)))

(defn assoc
  "assoc[iate]. When applied to a map, returns a new map of the
   same (hashed/sorted) type, that contains the mapping of key(s) to
   val(s). When applied to a vector, returns a new vector that
   contains val at index."
  ([coll k v]
     (-assoc coll k v))
  ([coll k v & kvs]
     (let [ret (assoc coll k v)]
       (if kvs
         (recur ret (first kvs) (second kvs) (nnext kvs))
         ret))))

(defn dissoc
  "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
  that does not contain a mapping for key(s)."
  ([coll] coll)
  ([coll k]
     (-dissoc coll k))
  ([coll k & ks]
     (let [ret (dissoc coll k)]
       (if ks
         (recur ret (first ks) (next ks))
         ret))))

(defn with-meta
  "Returns an object of the same type and value as obj, with
  map m as its metadata."
  [o meta]
  (-with-meta o meta))

(defn meta
  "Returns the metadata of obj, returns nil if there is no metadata."
  [o]
  (when (satisfies? IMeta o)
    (-meta o)))

(defn peek
  "For a list or queue, same as first, for a vector, same as, but much
  more efficient than, last. If the collection is empty, returns nil."
  [coll]
  (-peek coll))

(defn pop
  "For a list or queue, returns a new list/queue without the first
  item, for a vector, returns a new vector without the last item.
  Note - not the same as next/butlast."
  [coll]
  (-pop coll))

(defn disj
  "disj[oin]. Returns a new set of the same (hashed/sorted) type, that
  does not contain key(s)."
  ([coll] coll)
  ([coll k]
     (-disjoin coll k))
  ([coll k & ks]
     (let [ret (disj coll k)]
       (if ks
         (recur ret (first ks) (next ks))
         ret))))

(defn hash [o]
  (-hash o))

(defn empty?
  "Returns true if coll has no items - same as (not (seq coll)).
  Please use the idiom (seq x) rather than (not (empty? x))"
  [coll] (not (seq coll)))

(defn coll?
  "Returns true if x satisfies ICollection"
  [x]
  (if (nil? x)
    false
    (satisfies? ICollection x)))

(defn set?
  "Returns true if x satisfies ISet"
  [x]
  (if (nil? x)
    false
    (satisfies? ISet x)))

(defn associative?
 "Returns true if coll implements Associative"
  [x] (satisfies? IAssociative x))

(defn sequential?
  "Returns true if coll satisfies ISequential"
  [x] (satisfies? ISequential x))

(defn counted?
  "Returns true if coll implements count in constant time"
  [x] (satisfies? ICounted x))

(defn map?
  "Return true if x satisfies IMap"
  [x]
  (if (nil? x)
    false
    (satisfies? IMap x)))

(defn vector?
  "Return true if x satisfies IVector"
  [x] (satisfies? IVector x))

;;;;;;;;;;;;;;;;;;;; js primitives ;;;;;;;;;;;;
(defn js-obj []
  (js* "{}"))

(defn js-keys [obj]
  (let [keys (array)]
    (goog.object/forEach obj (fn [val key obj] (.push keys key)))
    keys))

(defn js-delete [obj key]
  (js* "delete ~{obj}[~{key}]"))

;;;;;;;;;;;;;;;; preds ;;;;;;;;;;;;;;;;;;

(def ^:private lookup-sentinel (js-obj))

(defn false?
  "Returns true if x is the value false, false otherwise."
  [x] (js* "~{x} === false"))

(defn true?
  "Returns true if x is the value true, false otherwise."
  [x] (js* "~{x} === true"))

(defn undefined? [x]
  (js* "(void 0 === ~{x})"))

(defn instance? [t o]
  (js* "(~{o} instanceof ~{t})"))

(defn seq?
  "Return true if s satisfies ISeq"
  [s]
  (if (nil? s)
    false
    (satisfies? ISeq s)))

(defn boolean [x]
  (if x true false))

(defn string? [x]
  (and (goog/isString x)
       (not (or (= (.charAt x 0) \uFDD0)
                (= (.charAt x 0) \uFDD1)))))

(defn keyword? [x]
  (and (goog/isString x)
       (= (.charAt x 0) \uFDD0)))

(defn symbol? [x]
  (and (goog/isString x)
       (= (.charAt x 0) \uFDD1)))

(defn number? [n]
  (goog/isNumber n))

(defn fn? [f]
  (goog/isFunction f))

(defn integer?
  "Returns true if n is an integer.  Warning: returns true on underflow condition."
  [n]
  (and (number? n)
       (js* "(~{n} == ~{n}.toFixed())")))

(defn contains?
  "Returns true if key is present in the given collection, otherwise
  returns false.  Note that for numerically indexed collections like
  vectors and arrays, this tests if the numeric key is within the
  range of indexes. 'contains?' operates constant or logarithmic time;
  it will not perform a linear search for a value.  See also 'some'."
  [coll v]
  (if (identical? (-lookup coll v lookup-sentinel) lookup-sentinel)
    false
    true))

(defn find
  "Returns the map entry for key, or nil if key not present."
  [coll k]
  (when (and coll
             (associative? coll)
             (contains? coll k))
    [k (-lookup coll k)]))

(defn distinct?
  "Returns true if no two of the arguments are ="
  ([x] true)
  ([x y] (not (= x y)))
  ([x y & more]
     (if (not (= x y))
     (loop [s #{x y} xs more]
       (let [x (first xs)
             etc (next xs)]
         (if xs
           (if (contains? s x)
             false
             (recur (conj s x) etc))
           true)))
     false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Seq fns ;;;;;;;;;;;;;;;;

(defn compare
  "Comparator. Returns a negative number, zero, or a positive number
  when x is logically 'less than', 'equal to', or 'greater than'
  y. Uses google.array.defaultCompare."
  [x y] (garray/defaultCompare x y))

(defn ^:private fn->comparator
  "Given a fn that might be boolean valued or a comparator,
   return a fn that is a comparator."
  [f]
  (if (= f compare)
    compare
    (fn [x y]
      (let [r (f x y)]
        (if (number? r)
          r
          (if r
            -1
            (if (f y x) 1 0)))))))

(declare to-array)
(defn sort
  "Returns a sorted sequence of the items in coll. Comp can be
   boolean-valued comparison funcion, or a -/0/+ valued comparator.
   Comp defaults to compare."
  ([coll]
   (sort compare coll))
  ([comp coll]
   (if (seq coll)
     (let [a (to-array coll)]
       ;; matching Clojure's stable sort, though docs don't promise it
       (garray/stableSort a (fn->comparator comp))
       (seq a))
     ())))

(defn sort-by
  "Returns a sorted sequence of the items in coll, where the sort
   order is determined by comparing (keyfn item).  Comp can be
   boolean-valued comparison funcion, or a -/0/+ valued comparator.
   Comp defaults to compare."
  ([keyfn coll]
   (sort-by keyfn compare coll))
  ([keyfn comp coll]
     (sort (fn [x y] ((fn->comparator comp) (keyfn x) (keyfn y))) coll)))

(defn reduce
  "f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called."
  ([f coll]
     (-reduce coll f))
  ([f val coll]
     (-reduce coll f val)))

; simple reduce based on seqs, used as default
(defn- seq-reduce
  ([f coll]
    (if-let [s (seq coll)]
      (reduce f (first s) (next s))
      (f)))
  ([f val coll]
    (loop [val val, coll (seq coll)]
      (if coll
        (recur (f val (first coll)) (next coll))
        val))))

(extend-type default
  IReduce
  (-reduce
    ([coll f]
       (seq-reduce f coll))
    ([coll f start]
       (seq-reduce f start coll))))

;;; Math - variadic forms will not work until the following implemented:
;;; first, next, reduce

(defn +
  "Returns the sum of nums. (+) returns 0."
  ([] 0)
  ([x] x)
  ([x y] (js* "(~{x} + ~{y})"))
  ([x y & more] (reduce + (+ x y) more)))

(defn -
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result."
  ([x] (js* "(- ~{x})"))
  ([x y] (js* "(~{x} - ~{y})"))
  ([x y & more] (reduce - (- x y) more)))

(defn *
  "Returns the product of nums. (*) returns 1."
  ([] 1)
  ([x] x)
  ([x y] (js* "(~{x} * ~{y})"))
  ([x y & more] (reduce * (* x y) more)))

(defn /
  "If no denominators are supplied, returns 1/numerator,
  else returns numerator divided by all of the denominators."  
  ([x] (js* "(1 / ~{x})"))
  ([x y] (js* "(~{x} / ~{y})"))
  ([x y & more] (reduce / (/ x y) more)))

(defn <
  "Returns non-nil if nums are in monotonically increasing order,
  otherwise false."
  ([x] true)
  ([x y] (js* "(~{x} < ~{y})"))
  ([x y & more]
     (if (< x y)
       (if (next more)
         (recur y (first more) (next more))
         (< y (first more)))
       false)))

(defn <=
  "Returns non-nil if nums are in monotonically non-decreasing order,
  otherwise false."
  ([x] true)
  ([x y] (js* "(~{x} <= ~{y})"))
  ([x y & more]
   (if (<= x y)
     (if (next more)
       (recur y (first more) (next more))
       (<= y (first more)))
     false)))

(defn >
  "Returns non-nil if nums are in monotonically decreasing order,
  otherwise false."
  ([x] true)
  ([x y] (js* "(~{x} > ~{y})"))
  ([x y & more]
   (if (> x y)
     (if (next more)
       (recur y (first more) (next more))
       (> y (first more)))
     false)))

(defn >=
  "Returns non-nil if nums are in monotonically non-increasing order,
  otherwise false."
  ([x] true)
  ([x y] (js* "(~{x} >= ~{y})"))
  ([x y & more]
   (if (>= x y)
     (if (next more)
       (recur y (first more) (next more))
       (>= y (first more)))
     false)))

(defn dec
  "Returns a number one less than num."
  [x] (- x 1))

(defn max
  "Returns the greatest of the nums."
  ([x] x)
  ([x y] (js* "((~{x} > ~{y}) ? x : y)"))
  ([x y & more]
   (reduce max (max x y) more)))

(defn min
  "Returns the least of the nums."
  ([x] x)
  ([x y] (js* "((~{x} < ~{y}) ? x : y)"))
  ([x y & more]
   (reduce min (min x y) more)))

(defn- fix [q]
  (if (>= q 0)
    (Math/floor q)
    (Math/ceil q)))

(defn mod
  "Modulus of num and div. Truncates toward negative infinity."
  [n d]
  (js* "(~{n} % ~{d})"))

(defn quot
  "quot[ient] of dividing numerator by denominator."
  [n d]
  (let [rem (mod n d)]
    (fix (js* "((~{n} - ~{rem}) / ~{d})"))))

(defn rem
  "remainder of dividing numerator by denominator."
  [n d]
  (let [q (quot n d)]
    (js* "(~{n} - (~{d} * ~{q}))")))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and n (default 1) (exclusive)."
  ([]  (Math/random))
  ([n] (* n (rand))))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive)."
  [n] (fix (rand n)))

(defn bit-xor
  "Bitwise exclusive or"
  [x y] (js* "(~{x} ^ ~{y})"))

(defn bit-and
  "Bitwise and"
  [x y] (js* "(~{x} & ~{y})"))

(defn bit-or
  "Bitwise or"
  [x y] (js* "(~{x} | ~{y})"))

(defn bit-and-not
  "Bitwise and"
  [x y] (js* "(~{x} & ~~{y})"))

(defn bit-clear
  "Clear bit at index n"
  [x n]
  (js* "(~{x} & ~(1 << ~{n}))"))

(defn bit-flip
  "Flip bit at index n"
  [x n]
  (js* "(~{x} ^ (1 << ~{n}))"))

(defn bit-not
  "Bitwise complement"
  [x] (js* "(~~{x})"))

(defn bit-set
  "Set bit at index n"
  [x n]
  (js* "(~{x} | (1 << ~{n}))"))

(defn bit-test
  "Test bit at index n"
  [x n]
  (js* "((~{x} & (1 << ~{n})) != 0)"))


(defn bit-shift-left
  "Bitwise shift left"
  [x n] (js* "(~{x} << ~{n})"))

(defn bit-shift-right
  "Bitwise shift right"
  [x n] (js* "(~{x} >> ~{n})"))

(defn ==
  "Returns non-nil if nums all have the equivalent
  value (type-independent), otherwise false"
  ([x] true)
  ([x y] (-equiv x y))
  ([x y & more]
   (if (== x y)
     (if (next more)
       (recur y (first more) (next more))
       (== y (first more)))
     false)))

(defn pos?
  "Returns true if num is greater than zero, else false"
  [n] (< 0 n))

(defn zero? [n]
  (== 0 n))

(defn neg?
  "Returns true if num is less than zero, else false"
  [x] (js* "(~{x} < 0)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; protocols for host types ;;;;;;



(defn nthnext
  "Returns the nth next of coll, (seq coll) when n is 0."
  [coll n]
  (loop [n n xs (seq coll)]
    (if (and xs (pos? n))
      (recur (dec n) (next xs))
      xs)))

(extend-type default
  IIndexed
  (-nth
   ([coll n]
      (if-let [xs (nthnext coll n)]
        (first xs)
        (throw "Index out of bounds")))
   ([coll n not-found]
      (if-let [xs (nthnext coll n)]
        (first xs)
        not-found))))


;;;;;;;;;;;;;;;;;;;;;;;;;; basics ;;;;;;;;;;;;;;;;;;

(defn str
  "With no args, returns the empty string. With one arg x, returns
  x.toString().  (str nil) returns the empty string. With more than
  one arg, returns the concatenation of the str values of the args."
  ([] "")
  ([x] (if (nil? x) "" (. x (toString))))
  ([x & ys]
     ((fn [sb more]
        (if more
          (recur (. sb  (append (str (first more)))) (next more))
          (str sb)))
      (gstring/StringBuffer. (str x)) ys)))

(defn subs
  "Returns the substring of s beginning at start inclusive, and ending
  at end (defaults to length of string), exclusive."
  ([s start] (.substring s start))
  ([s start end] (.substring s start end)))

(defn symbol
  "Returns a Symbol with the given namespace and name."
  ([name] (cond (symbol? name) name
                (keyword? name) (str "\uFDD1" "'" (subs name 2))
                :else (str "\uFDD1" "'" name)))
  ([ns name] (symbol (str ns "/" name))))

(defn keyword
  "Returns a Keyword with the given namespace and name.  Do not use :
  in the keyword strings, it will be added automatically."
  ([name] (cond (keyword? name) name
                (symbol? name) (str "\uFDD0" "'" (subs name 2))
                :else (str "\uFDD0" "'" name)))
  ([ns name] (keyword (str ns "/" name))))



(defn- equiv-sequential
  "Assumes x is sequential. Returns true if x equals y, otherwise
  returns false."
  [x y]
  (boolean
   (when (sequential? y)
     (loop [xs (seq x) ys (seq y)]
       (cond (nil? xs) (nil? ys)
             (nil? ys) false
             (= (first xs) (first ys)) (recur (next xs) (next ys))
             :else false)))))

(defn hash-combine [seed hash]
  ; a la boost
  (bit-xor seed (+ hash 0x9e3779b9
                   (bit-shift-left seed 6)
                   (bit-shift-right seed 2))))

(defn- hash-coll [coll]
  (reduce #(hash-combine %1 (hash %2)) (hash (first coll)) (next coll)))


;;;;;;;;;;;;;;;; cons ;;;;;;;;;;;;;;;;
(deftype List [meta first rest count]
  IWithMeta
  (-with-meta [coll meta] (List. meta first rest count))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] first)
  (-rest [coll] rest)

  IStack
  (-peek [coll] first)
  (-pop [coll] (-rest coll))

  ICollection
  (-conj [coll o] (List. meta o coll (inc count)))

  IEmptyableCollection
  (-empty [coll] cljs.core.List/EMPTY)

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (hash-coll coll))

  ISeqable
  (-seq [coll] coll)

  ICounted
  (-count [coll] count))

(deftype EmptyList [meta]
  IWithMeta
  (-with-meta [coll meta] (EmptyList. meta))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] nil)
  (-rest [coll] nil)

  IStack
  (-peek [coll] nil)
  (-pop [coll] #_(throw "Can't pop empty list"))

  ICollection
  (-conj [coll o] (List. meta o nil 1))

  IEmptyableCollection
  (-empty [coll] coll)

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (hash-coll coll))

  ISeqable
  (-seq [coll] nil)

  ICounted
  (-count [coll] 0))

(set! cljs.core.List/EMPTY (EmptyList. nil))

(defn reverse
  "Returns a seq of the items in coll in reverse order. Not lazy."
  [coll]
  (reduce conj () coll))

(defn list [& items]
  (reduce conj () (reverse items)))

(deftype Cons [meta first rest]
  IWithMeta
  (-with-meta [coll meta] (Cons. meta first rest))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] first)
  (-rest [coll] (if (nil? rest) () rest))

  ICollection
  (-conj [coll o] (Cons. nil o coll))

  IEmptyableCollection
  (-empty [coll] (with-meta cljs.core.List/EMPTY meta))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (hash-coll coll))

  ISeqable
  (-seq [coll] coll))

(defn cons
  "Returns a new seq where x is the first element and seq is the rest."
  [x seq]
  (Cons. nil x seq))

(declare hash-map)

(extend-type string
  IHash
  (-hash [o] (goog.string/hashCode o))

  ISeqable
  (-seq [string] (prim-seq string 0))
  
  ICounted
  (-count [s] (.length s))

  IIndexed
  (-nth
    ([string n]
       (if (< n (-count string)) (.charAt string n)))
    ([string n not-found]
       (if (< n (-count string)) (.charAt string n)
           not-found)))

  ILookup
  (-lookup
    ([string k]
       (-nth string k))
    ([string k not_found]
       (-nth string k not_found)))

  IReduce
  (-reduce
    ([string f]
       (ci-reduce string f))
    ([string f start]
       (ci-reduce string f start))))

;;hrm
(set! js/String.prototype.call
      (fn
        ([_ coll]
           (get coll (js* "this.toString()")))
        ([_ coll not-found]
           (get coll (js* "this.toString()") not-found))))

(set! js/String.prototype.apply
      (fn
        [s args]
        (if (< (count args) 2)
          (get (aget args 0) s)
          (get (aget args 0) s (aget args 1)))))

; could use reify
;;; LazySeq ;;;

(defn- lazy-seq-value [lazy-seq]
  (let [x (.x lazy-seq)]
    (if (.realized lazy-seq)
      x
      (do
        (set! (.x lazy-seq) (x))
        (set! (.realized lazy-seq) true)
        (.x lazy-seq)))))

(deftype LazySeq [meta realized x]
  IWithMeta
  (-with-meta [coll meta] (LazySeq. meta realized x))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] (first (lazy-seq-value coll)))
  (-rest [coll] (rest (lazy-seq-value coll)))

  ICollection
  (-conj [coll o] (cons o coll))

  IEmptyableCollection
  (-empty [coll] (with-meta cljs.core.List/EMPTY meta))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (hash-coll coll))

  ISeqable
  (-seq [coll] (seq (lazy-seq-value coll))))

;;;;;;;;;;;;;;;;

(defn to-array
  "Naive impl of to-array as a start."
  [s]
  (let [ary (array)]
    (loop [s s]
      (if (seq s)
        (do (. ary push (first s))
            (recur (next s)))
        ary))))

(defn- bounded-count [s n]
  (loop [s s i n sum 0]
    (if (and (pos? i)
             (seq s))
      (recur (next s)
             (dec i)
             (inc sum))
      sum)))

(defn spread
  [arglist]
  (cond
   (nil? arglist) nil
   (nil? (next arglist)) (seq (first arglist))
   :else (cons (first arglist)
               (spread (next arglist)))))

(defn concat
  "Returns a lazy seq representing the concatenation of the elements in the supplied colls."
  ([] (lazy-seq nil))
  ([x] (lazy-seq x))
  ([x y]
    (lazy-seq
      (let [s (seq x)]
        (if s
          (cons (first s) (concat (rest s) y))
          y))))
  ([x y & zs]
     (let [cat (fn cat [xys zs]
                 (lazy-seq
                   (let [xys (seq xys)]
                     (if xys
                       (cons (first xys) (cat (rest xys) zs))
                       (when zs
                         (cat (first zs) (next zs)))))))]
       (cat (concat x y) zs))))

(defn list*
  "Creates a new list containing the items prepended to the rest, the
  last of which will be treated as a sequence."
  ([args] (seq args))
  ([a args] (cons a args))
  ([a b args] (cons a (cons b args)))
  ([a b c args] (cons a (cons b (cons c args))))
  ([a b c d & more]
     (cons a (cons b (cons c (cons d (spread more)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; apply ;;;;;;;;;;;;;;;;

(defn apply
  "Applies fn f to the argument list formed by prepending intervening arguments to args.
  First cut.  Not lazy.  Needs to use emitted toApply."
  ([f args]
     (let [fixed-arity (. f cljs$lang$maxFixedArity)]
       (if (. f cljs$lang$applyTo)
         (if (<= (bounded-count args (inc fixed-arity))
                 fixed-arity)
           (. f apply f (to-array args))
           (. f cljs$lang$applyTo args))
         (. f apply f (to-array args)))))
  ([f x args]
     (let [arglist (list* x args)
           fixed-arity (. f cljs$lang$maxFixedArity)]
       (if (. f cljs$lang$applyTo)
         (if (<= (bounded-count arglist fixed-arity)
                 fixed-arity)
           (. f apply f (to-array arglist))
           (. f cljs$lang$applyTo arglist))
         (. f apply f (to-array arglist)))))
  ([f x y args]
     (let [arglist (list* x y args)
           fixed-arity (. f cljs$lang$maxFixedArity)]
       (if (. f cljs$lang$applyTo)
         (if (<= (bounded-count arglist fixed-arity)
                 fixed-arity)
           (. f apply f (to-array arglist))
           (. f cljs$lang$applyTo arglist))
         (. f apply f (to-array arglist)))))
  ([f x y z args]
     (let [arglist (list* x y z args)
           fixed-arity (. f cljs$lang$maxFixedArity)]
       (if (. f cljs$lang$applyTo)
         (if (<= (bounded-count arglist fixed-arity)
                 fixed-arity)
           (. f apply f (to-array arglist))
           (. f cljs$lang$applyTo arglist))
         (. f apply f (to-array arglist)))))
  ([f a b c d & args]
     (let [arglist (cons a (cons b (cons c (cons d (spread args)))))
           fixed-arity (. f cljs$lang$maxFixedArity)]
       (if (. f cljs$lang$applyTo)
         (if (<= (bounded-count arglist fixed-arity)
                 fixed-arity)
           (. f apply f (to-array arglist))
           (. f cljs$lang$applyTo arglist))
         (. f apply f (to-array arglist))))))

(defn vary-meta
 "Returns an object of the same type and value as obj, with
  (apply f (meta obj) args) as its metadata."
 [obj f & args]
 (with-meta obj (apply f (meta obj) args)))

(defn not=
  "Same as (not (= obj1 obj2))"
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more]
   (not (apply = x y more))))

(defn not-empty
  "If coll is empty, returns nil, else coll"
  [coll] (when (seq coll) coll))

(defn every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false."
  [pred coll]
  (cond
   (nil? (seq coll)) true
   (pred (first coll)) (recur pred (next coll))
   :else false))

(defn not-every?
  "Returns false if (pred x) is logical true for every x in
  coll, else true."
  [pred coll] (not (every? pred coll)))

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil.  One common idiom is to use a set as pred, for example
  this will return :fred if :fred is in the sequence, otherwise nil:
  (some #{:fred} coll)"
  [pred coll]
    (when (seq coll)
      (or (pred (first coll)) (recur pred (next coll)))))

(defn not-any?
  "Returns false if (pred x) is logical true for any x in coll,
  else true."
  [pred coll] (not (some pred coll)))

(defn even?
  "Returns true if n is even, throws an exception if n is not an integer"
   [n] (if (integer? n)
        (zero? (bit-and n 1))
        (throw (str "Argument must be an integer: " n))))

(defn odd?
  "Returns true if n is odd, throws an exception if n is not an integer"
  [n] (not (even? n)))

(defn identity [x] x)

(defn complement
  "Takes a fn f and returns a fn that takes the same arguments as f,
  has the same effects, if any, and returns the opposite truth value."
  [f] 
  (fn 
    ([] (not (f)))
    ([x] (not (f x)))
    ([x y] (not (f x y)))
    ([x y & zs] (not (apply f x y zs)))))

(defn constantly
  "Returns a function that takes any number of arguments and returns x."
  [x] (fn [& args] x))

(defn comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc.

  TODO: Implement apply"
  ([] identity)
  ([f] f)
  ([f g] 
     (fn 
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
  ([f g h] 
     (fn 
       ([] (f (g (h))))
       ([x] (f (g (h x))))
       ([x y] (f (g (h x y))))
       ([x y z] (f (g (h x y z))))
       ([x y z & args] (f (g (apply h x y z args))))))
  ([f1 f2 f3 & fs]
    (let [fs (reverse (list* f1 f2 f3 fs))]
      (fn [& args]
        (loop [ret (apply (first fs) args) fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defn partial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with args + additional args.

  TODO: Implement apply"
  ([f arg1]
   (fn [& args] (apply f arg1 args)))
  ([f arg1 arg2]
   (fn [& args] (apply f arg1 arg2 args)))
  ([f arg1 arg2 arg3]
   (fn [& args] (apply f arg1 arg2 arg3 args)))
  ([f arg1 arg2 arg3 & more]
   (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))

(defn fnil
  "Takes a function f, and returns a function that calls f, replacing
  a nil first argument to f with the supplied value x. Higher arity
  versions can replace arguments in the second and third
  positions (y, z). Note that the function f can take any number of
  arguments, not just the one(s) being nil-patched."
  ([f x]
   (fn
     ([a] (f (if (nil? a) x a)))
     ([a b] (f (if (nil? a) x a) b))
     ([a b c] (f (if (nil? a) x a) b c))
     ([a b c & ds] (apply f (if (nil? a) x a) b c ds))))
  ([f x y]
   (fn
     ([a b] (f (if (nil? a) x a) (if (nil? b) y b)))
     ([a b c] (f (if (nil? a) x a) (if (nil? b) y b) c))
     ([a b c & ds] (apply f (if (nil? a) x a) (if (nil? b) y b) c ds))))
  ([f x y z]
   (fn
     ([a b] (f (if (nil? a) x a) (if (nil? b) y b)))
     ([a b c] (f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c)))
     ([a b c & ds] (apply f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c) ds)))))

(defn map-indexed
  "Returns a lazy sequence consisting of the result of applying f to 0
  and the first item of coll, followed by applying f to 1 and the second
  item in coll, etc, until coll is exhausted. Thus function f should
  accept 2 arguments, index and item."
  [f coll]
  (let [mapi (fn mpi [idx coll]
               (lazy-seq
                (when-let [s (seq coll)]
                  (cons (f idx (first s))
                        (mpi (inc idx) (rest s))))))]
    (mapi 0 coll)))

(defn keep
  "Returns a lazy sequence of the non-nil results of (f item). Note,
  this means false return values will be included.  f must be free of
  side-effects."
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [x (f (first s))]
        (if (nil? x)
          (keep f (rest s))
          (cons x (keep f (rest s)))))))))

(defn keep-indexed
  "Returns a lazy sequence of the non-nil results of (f index item). Note,
  this means false return values will be included.  f must be free of
  side-effects."
  ([f coll]
     (let [keepi (fn kpi [idx coll]
                   (lazy-seq
                    (when-let [s (seq coll)]
                      (let [x (f idx (first s))]
                        (if (nil? x)
                          (kpi (inc idx) (rest s))
                          (cons x (kpi (inc idx) (rest s))))))))]
       (keepi 0 coll))))

(defn every-pred
  "Takes a set of predicates and returns a function f that returns true if all of its
  composing predicates return a logical true value against all of its arguments, else it returns
  false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical false result against the original predicates."
  ([p]
     (fn ep1
       ([] true)
       ([x] (boolean (p x)))
       ([x y] (boolean (and (p x) (p y))))
       ([x y z] (boolean (and (p x) (p y) (p z))))
       ([x y z & args] (boolean (and (ep1 x y z)
                                     (every? p args))))))
  ([p1 p2]
     (fn ep2
       ([] true)
       ([x] (boolean (and (p1 x) (p2 x))))
       ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y))))
       ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
       ([x y z & args] (boolean (and (ep2 x y z)
                                     (every? #(and (p1 %) (p2 %)) args))))))
  ([p1 p2 p3]
     (fn ep3
       ([] true)
       ([x] (boolean (and (p1 x) (p2 x) (p3 x))))
       ([x y] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y))))
       ([x y z] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z))))
       ([x y z & args] (boolean (and (ep3 x y z)
                                     (every? #(and (p1 %) (p2 %) (p3 %)) args))))))
  ([p1 p2 p3 & ps]
     (let [ps (list* p1 p2 p3 ps)]
       (fn epn
         ([] true)
         ([x] (every? #(% x) ps))
         ([x y] (every? #(and (% x) (% y)) ps))
         ([x y z] (every? #(and (% x) (% y) (% z)) ps))
         ([x y z & args] (boolean (and (epn x y z)
                                       (every? #(every? % args) ps))))))))

(defn some-fn
  "Takes a set of predicates and returns a function f that returns the first logical true value
  returned by one of its composing predicates against any of its arguments, else it returns
  logical false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical true result against the original predicates."
  ([p]
     (fn sp1
       ([] nil)
       ([x] (p x))
       ([x y] (or (p x) (p y)))
       ([x y z] (or (p x) (p y) (p z)))
       ([x y z & args] (or (sp1 x y z)
                           (some p args)))))
  ([p1 p2]
     (fn sp2
       ([] nil)
       ([x] (or (p1 x) (p2 x)))
       ([x y] (or (p1 x) (p1 y) (p2 x) (p2 y)))
       ([x y z] (or (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z)))
       ([x y z & args] (or (sp2 x y z)
                           (some #(or (p1 %) (p2 %)) args)))))
  ([p1 p2 p3]
     (fn sp3
       ([] nil)
       ([x] (or (p1 x) (p2 x) (p3 x)))
       ([x y] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y)))
       ([x y z] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z)))
       ([x y z & args] (or (sp3 x y z)
                           (some #(or (p1 %) (p2 %) (p3 %)) args)))))
  ([p1 p2 p3 & ps]
     (let [ps (list* p1 p2 p3 ps)]
       (fn spn
         ([] nil)
         ([x] (some #(% x) ps))
         ([x y] (some #(or (% x) (% y)) ps))
         ([x y z] (some #(or (% x) (% y) (% z)) ps))
         ([x y z & args] (or (spn x y z)
                             (some #(some % args) ps)))))))

(defn map
  "Returns a lazy sequence consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments."
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (cons (f (first s)) (map f (rest s))))))
  ([f c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (when (and s1 s2)
        (cons (f (first s1) (first s2))
              (map f (rest s1) (rest s2)))))))
  ([f c1 c2 c3]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
      (when (and  s1 s2 s3)
        (cons (f (first s1) (first s2) (first s3))
              (map f (rest s1) (rest s2) (rest s3)))))))
  ([f c1 c2 c3 & colls]
   (let [step (fn step [cs]
                 (lazy-seq
                  (let [ss (map seq cs)]
                    (when (every? identity ss)
                      (cons (map first ss) (step (map rest ss)))))))]
     (map #(apply f %) (step (conj colls c3 c2 c1))))))

(defn take
  "Returns a lazy sequence of the first n items in coll, or all items if
  there are fewer than n."
  [n coll]
  (lazy-seq
   (when (pos? n)
     (when-let [s (seq coll)]
      (cons (first s) (take (dec n) (rest s)))))))

(defn drop
  "Returns a lazy sequence of all but the first n items in coll."
  [n coll]
  (let [step (fn [n coll]
               (let [s (seq coll)]
                 (if (and (pos? n) s)
                   (recur (dec n) (rest s))
                   s)))]
    (lazy-seq (step n coll))))

(defn drop-last
  "Return a lazy sequence of all but the last n (default 1) items in coll"
  ([s] (drop-last 1 s))
  ([n s] (map (fn [x _] x) s (drop n s))))

(defn take-last
  "Returns a seq of the last n items in coll.  Depending on the type
  of coll may be no better than linear time.  For vectors, see also subvec."
  [n coll]
  (loop [s (seq coll), lead (seq (drop n coll))]
    (if lead
      (recur (next s) (next lead))
      s)))

(defn drop-while
  "Returns a lazy sequence of the items in coll starting from the first
  item for which (pred item) returns nil."
  [pred coll]
  (let [step (fn [pred coll]
               (let [s (seq coll)]
                 (if (and s (pred (first s)))
                   (recur pred (rest s))
                   s)))]
    (lazy-seq (step pred coll))))

(defn cycle
  "Returns a lazy (infinite!) sequence of repetitions of the items in coll."
  [coll] (lazy-seq 
          (when-let [s (seq coll)] 
            (concat s (cycle s)))))

(defn split-at
  "Returns a vector of [(take n coll) (drop n coll)]"
  [n coll]
  [(take n coll) (drop n coll)])

(defn repeat
  "Returns a lazy (infinite!, or length n if supplied) sequence of xs."
  ([x] (lazy-seq (cons x (repeat x))))
  ([n x] (take n (repeat x))))

(defn replicate
  "Returns a lazy seq of n xs."
  [n x] (take n (repeat x)))

(defn repeatedly
  "Takes a function of no args, presumably with side effects, and
  returns an infinite (or length n if supplied) lazy sequence of calls
  to it"
  ([f] (lazy-seq (cons (f) (repeatedly f))))
  ([n f] (take n (repeatedly f))))

(defn iterate
  "Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects"
  {:added "1.0"}
  [f x] (cons x (lazy-seq (iterate f (f x)))))

(defn interleave
  "Returns a lazy seq of the first item in each coll, then the second etc."
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (when (and s1 s2)
          (cons (first s1) (cons (first s2)
                                 (interleave (rest s1) (rest s2))))))))
  ([c1 c2 & colls]
     (lazy-seq
      (let [ss (map seq (conj colls c2 c1))]
        (when (every? identity ss)
          (concat (map first ss) (apply interleave (map rest ss))))))))

(defn interpose
  "Returns a lazy seq of the elements of coll separated by sep"
  [sep coll] (drop 1 (interleave (repeat sep) coll)))



(defn- flatten1
  "Take a collection of collections, and return a lazy seq
  of items from the inner collection"
  [colls]
  (let [cat (fn cat [coll colls]
              (lazy-seq
                (if-let [coll (seq coll)]
                  (cons (first coll) (cat (rest coll) colls))
                  (when (seq colls)
                    (cat (first colls) (rest colls))))))]
    (cat nil colls)))

(defn mapcat
  "Returns the result of applying concat to the result of applying map
  to f and colls.  Thus function f should return a collection."
  ([f coll]
    (flatten1 (map f coll)))
  ([f coll & colls]
    (flatten1 (apply map f coll colls))))

(defn filter
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns true. pred must be free of side-effects."
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [f (first s) r (rest s)]
        (if (pred f)
          (cons f (filter pred r))
          (filter pred r)))))))

(defn remove
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns false. pred must be free of side-effects."
  [pred coll]
  (filter (complement pred) coll))

(defn tree-seq
  "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree."
   [branch? children root]
   (let [walk (fn walk [node]
                (lazy-seq
                 (cons node
                  (when (branch? node)
                    (mapcat walk (children node))))))]
     (walk root)))

(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat sequence.
  (flatten nil) returns nil."
  [x]
  (filter #(not (sequential? %))
          (rest (tree-seq sequential? seq x))))

(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined."
  [to from]
  (reduce -conj to from))

(defn partition
  "Returns a lazy sequence of lists of n items each, at offsets step
  apart. If step is not supplied, defaults to n, i.e. the partitions
  do not overlap. If a pad collection is supplied, use its elements as
  necessary to complete last partition upto n items. In case there are
  not enough padding elements, return a partition with less than n items."
  ([n coll]
     (partition n n coll))
  ([n step coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (let [p (take n s)]
           (when (= n (count p))
             (cons p (partition n step (drop step s))))))))
  ([n step pad coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (let [p (take n s)]
           (if (= n (count p))
             (cons p (partition n step pad (drop step s)))
             (list (take n (concat p pad)))))))))

(defn get-in
  "Returns the value in a nested associative structure,
  where ks is a sequence of ke(ys. Returns nil if the key is not present,
  or the not-found value if supplied."
  {:added "1.2"
   :static true}
  ([m ks]
     (reduce get m ks))
  ([m ks not-found]
     (loop [sentinel lookup-sentinel
            m m
            ks (seq ks)]
       (if ks
         (let [m (get m (first ks) sentinel)]
           (if (identical? sentinel m)
             not-found
             (recur sentinel m (next ks))))
         m))))

(defn assoc-in
  "Associates a value in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  [m [k & ks] v]
  (if ks
    (assoc m k (assoc-in (get m k) ks v))
    (assoc m k v)))

(defn update-in
  "'Updates' a value in a nested associative structure, where ks is a
  sequence of keys and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  nested structure.  If any levels do not exist, hash-maps will be
  created."
  ([m [k & ks] f & args]
   (if ks
     (assoc m k (apply update-in (get m k) ks f args))
     (assoc m k (apply f (get m k) args)))))


;;; Vector

(deftype Vector [meta array]
  IWithMeta
  (-with-meta [coll meta] (Vector. meta array))

  IMeta
  (-meta [coll] meta)

  IStack
  (-peek [coll]
    (let [count (.length array)]
      (when (> count 0)
        (aget array (dec count)))))
  (-pop [coll]
    (if (> (.length array) 0)
      (let [new-array (aclone array)]
        (. new-array (pop))
        (Vector. meta new-array))
      (throw "Can't pop empty vector")))

  ICollection
  (-conj [coll o]
    (let [new-array (aclone array)]
      (.push new-array o)
      (Vector. meta new-array)))

  IEmptyableCollection
  (-empty [coll] (with-meta cljs.core.Vector/EMPTY meta))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (hash-coll coll))

  ISeqable
  (-seq [coll]
    (when (> (.length array) 0)
      (let [vector-seq
             (fn vector-seq [i]
               (lazy-seq
                 (when (< i (.length array))
                   (cons (aget array i) (vector-seq (inc i))))))]
        (vector-seq 0))))

  ICounted
  (-count [coll] (.length array))

  IIndexed
  (-nth [coll n]
    (if (and (<= 0 n) (< n (.length array)))
      (aget array n)
      #_(throw (str "No item " n " in vector of length " (.length array)))))
  (-nth [coll n not-found]
    (if (and (<= 0 n) (< n (.length array)))
      (aget array n)
      not-found))

  ILookup
  (-lookup [coll k] (-nth coll k nil))
  (-lookup [coll k not-found] (-nth coll k not-found))

  IAssociative
  (-assoc [coll k v]
    (let [new-array (aclone array)]
      (aset new-array k v)
      (Vector. meta new-array)))

  IVector
  (-assoc-n [coll n val] (-assoc coll n val))

  IReduce
  (-reduce [v f]
	   (ci-reduce array f))
  (-reduce [v f start]
	   (ci-reduce array f start)))

(set! cljs.core.Vector/EMPTY (Vector. nil (array)))

(set! cljs.core.Vector/fromArray (fn [xs] (Vector. nil xs)))

(set! cljs.core.Vector.prototype.call
      (fn
        ([_ k] (-lookup (js* "this") k))
        ([_ k not-found] (-lookup (js* "this") k not-found))))

(defn vec [coll]
  (reduce conj cljs.core.Vector/EMPTY coll)) ; using [] here causes infinite recursion

(defn vector [& args] (vec args))

(deftype NeverEquiv []
  IEquiv
  (-equiv [o other] false))

(def ^:private never-equiv (NeverEquiv.))

(defn- equiv-map
  "Assumes y is a map. Returns true if x equals y, otherwise returns
  false."
  [x y]
  (boolean
    (when (map? y)
      ; assume all maps are counted
      (when (= (count x) (count y))
        (every? identity
                (map (fn [xkv] (= (get y (first xkv) never-equiv)
                                  (second xkv)))
                     x))))))


(defn- scan-array [incr k array]
  (let [len (.length array)]
    (loop [i 0]
      (when (< i len)
        (if (= k (aget array i))
          i
          (recur (+ i incr)))))))

; The keys field is an array of all keys of this map, in no particular
; order. Any string, keyword, or symbol key is used as a property name
; to store the value in strobj.  If a key is assoc'ed when that same
; key already exists in strobj, the old value is overwritten. If a
; non-string key is assoc'ed, return a HashMap object instead.

(defn- obj-map-contains-key?
  ([k strobj]
     (obj-map-contains-key? k strobj true false))
  ([k strobj true-val false-val]
     (if (and (goog/isString k) (.hasOwnProperty strobj k))
       true-val
       false-val)))

(declare hash-map)
(deftype ObjMap [meta keys strobj]
  IWithMeta
  (-with-meta [coll meta] (ObjMap. meta keys strobj))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj
              coll
              entry)))

  IEmptyableCollection
  (-empty [coll] (with-meta cljs.core.ObjMap/EMPTY meta))

  IEquiv
  (-equiv [coll other] (equiv-map coll other))

  IHash
  (-hash [coll] (hash-coll coll))

  ISeqable
  (-seq [coll]
    (when (pos? (.length keys))
      (map #(vector % (aget strobj %)) keys)))

  ICounted
  (-count [coll] (.length keys))

  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found]
    (obj-map-contains-key? k strobj (aget strobj k) not-found))

  IAssociative
  (-assoc [coll k v]
    (if (goog/isString k)
      (let [new-strobj (goog.object/clone strobj)
            overwrite? (.hasOwnProperty new-strobj k)]
        (aset new-strobj k v)
        (if overwrite?
          (ObjMap. meta keys new-strobj)     ; overwrite
          (let [new-keys (aclone keys)] ; append
            (.push new-keys k)
            (ObjMap. meta new-keys new-strobj))))
      ; non-string key. game over.
      (with-meta (into (hash-map k v) (seq coll)) meta)))
  (-contains-key? [coll k]
    (obj-map-contains-key? k strobj))

  IMap
  (-dissoc [coll k]
    (if (and (goog/isString k) (.hasOwnProperty strobj k))
      (let [new-keys (aclone keys)
            new-strobj (goog.object/clone strobj)]
        (.splice new-keys (scan-array 1 k new-keys) 1)
        (js-delete new-strobj k)
        (ObjMap. meta new-keys new-strobj))
      coll))) ; key not found, return coll unchanged

(set! cljs.core.ObjMap/EMPTY (ObjMap. nil (array) (js-obj)))

(set! cljs.core.ObjMap/fromObject (fn [ks obj] (ObjMap. nil ks obj)))

(set! cljs.core.ObjMap.prototype.call
      (fn
        ([_ k] (-lookup (js* "this") k))
        ([_ k not-found] (-lookup (js* "this") k not-found))))

; The keys field is an array of all keys of this map, in no particular
; order. Each key is hashed and the result used as a property name of
; hashobj. Each values in hashobj is actually a bucket in order to handle hash
; collisions. A bucket is an array of alternating keys (not their hashes) and
; vals.
(deftype HashMap [meta count hashobj]
  IWithMeta
  (-with-meta [coll meta] (HashMap. meta count hashobj))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj
              coll
              entry)))

  IEmptyableCollection
  (-empty [coll] (with-meta cljs.core.HashMap/EMPTY meta))

  IEquiv
  (-equiv [coll other] (equiv-map coll other))

  IHash
  (-hash [coll] (hash-coll coll))

  ISeqable
  (-seq [coll]
    (when (pos? count)
      (let [hashes (js-keys hashobj)]
        (mapcat #(map vec (partition 2 (aget hashobj %)))
                hashes))))

  ICounted
  (-count [coll] count)

  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found]
    (let [bucket (aget hashobj (hash k))
          i (when bucket (scan-array 2 k bucket))]
      (if i
        (aget bucket (inc i))
        not-found)))

  IAssociative
  (-assoc [coll k v]
    (let [h (hash k)
          bucket (aget hashobj h)]
      (if bucket
        (let [new-bucket (aclone bucket)
              new-hashobj (goog.object/clone hashobj)]
          (aset new-hashobj h new-bucket)
          (if-let [i (scan-array 2 k new-bucket)]
            (do                         ; found key, replace
              (aset new-bucket (inc i) v)
              (HashMap. meta count new-hashobj))
            (do                         ; did not find key, append
              (.push new-bucket k v)
              (HashMap. meta (inc count) new-hashobj))))
        (let [new-hashobj (goog.object/clone hashobj)] ; did not find bucket
          (aset new-hashobj h (array k v))
          (HashMap. meta (inc count) new-hashobj)))))
  (-contains-key? [coll k]
    (let [bucket (aget hashobj (hash k))
          i (when bucket (scan-array 2 k bucket))]
      (if i
        true
        false)))

  IMap
  (-dissoc [coll k]
    (let [h (hash k)
          bucket (aget hashobj h)
          i (when bucket (scan-array 2 k bucket))]
      (if (not i)
        coll ; key not found, return coll unchanged
        (let [new-hashobj (goog.object/clone hashobj)]
          (if (> 3 (.length bucket))
            (js-delete new-hashobj h)
            (let [new-bucket (aclone bucket)]
              (.splice new-bucket i 2)
              (aset new-hashobj h new-bucket)))
          (HashMap. meta (dec count) new-hashobj))))))

(set! cljs.core.HashMap/EMPTY (HashMap. nil 0 (js-obj)))

(set! cljs.core.HashMap/fromArrays (fn [ks vs]
  (let [len (.length ks)]
    (loop [i 0, out cljs.core.HashMap/EMPTY]
      (if (< i len)
        (recur (inc i) (assoc out (aget ks i) (aget vs i)))
        out)))))

(set! cljs.core.HashMap.prototype.call
      (fn
        ([_ k] (-lookup (js* "this") k))
        ([_ k not-found] (-lookup (js* "this") k not-found))))

(defn hash-map
  "keyval => key val
  Returns a new hash map with supplied mappings."
  [& keyvals]
  (loop [in (seq keyvals), out cljs.core.HashMap/EMPTY]
    (if in
      (recur (nnext in) (assoc out (first in) (second in)))
      out)))

(defn keys
  "Returns a sequence of the map's keys."
  [hash-map]
  (seq (map first hash-map)))

(defn vals
  "Returns a sequence of the map's values."
  [hash-map]
  (seq (map second hash-map)))

(defn merge
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping from
  the latter (left-to-right) will be the mapping in the result."
  [& maps]
  (when (some identity maps)
    (reduce #(conj (or %1 {}) %2) maps)))

(defn merge-with
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter)."
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (first e) v (second e)]
                          (if (contains? m k)
                            (assoc m k (f (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defn select-keys
  "Returns a map containing only those entries in map whose key is in keys"
  [map keyseq]
    (loop [ret {} keys (seq keyseq)]
      (if keys
        (let [key   (first keys)
              entry (get map key)]
          (recur
           (if entry
             (assoc ret key entry)
             ret)
           (next keys)))
        ret)))

;;; Set

(deftype Set [meta hash-map]
  IWithMeta
  (-with-meta [coll meta] (Set. meta hash-map))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll o]
    (Set. meta (assoc hash-map o nil)))

  IEmptyableCollection
  (-empty [coll] (with-meta cljs.core.Set/EMPTY meta))

  IEquiv
  (-equiv [coll other]
    (and
     (set? other)
     (= (count coll) (count other))
     (every? #(contains? coll %)
             other)))

  IHash
  (-hash [coll] (hash-coll coll))

  ISeqable
  (-seq [coll] (keys hash-map))

  ICounted
  (-count [coll] (count (seq coll)))

  ILookup
  (-lookup [coll v]
    (-lookup coll v nil))
  (-lookup [coll v not-found]
    (if (-contains-key? hash-map v)
      v
      not-found))

  ISet
  (-disjoin [coll v]
    (Set. meta (dissoc hash-map v))))

(set! cljs.core.Set/EMPTY (Set. nil (hash-map)))

(set! cljs.core.Set.prototype.call
      (fn
        ([_ k] (-lookup (js* "this") k))
        ([_ k not-found] (-lookup (js* "this") k not-found))))

(defn set
  "Returns a set of the distinct elements of coll."
  [coll]
  (loop [in (seq coll)
         out cljs.core.Set/EMPTY]
    (if-not (empty? in)
      (recur (rest in) (conj out (first in)))
      out)))

(defn replace
  "Given a map of replacement pairs and a vector/collection, returns a
  vector/seq with any elements = a key in smap replaced with the
  corresponding val in smap"
  [smap coll]
  (if (vector? coll)
    (let [n (count coll)]
      (reduce (fn [v i]
                (if-let [e (find smap (nth v i))]
                  (assoc v i (second e))
                  v))
              coll (take n (iterate inc 0))))
    (map #(if-let [e (find smap %)] (second e) %) coll)))

(defn distinct
  "Returns a lazy sequence of the elements of coll with duplicates removed"
  [coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[f :as xs] seen]
                   (when-let [s (seq xs)]
                     (if (contains? seen f)
                       (recur (rest s) seen)
                       (cons f (step (rest s) (conj seen f))))))
                 xs seen)))]
    (step coll #{})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn butlast [s]
  (loop [ret [] s s]
    (if (next s)
      (recur (conj ret (first s)) (next s))
      (seq ret))))

(defn name
  "Returns the name String of a string, symbol or keyword."
  [x]
  (cond
    (string? x) x
    (or (keyword? x) (symbol? x))
      (let [i (.lastIndexOf x "/")]
        (if (< i 0)
          (subs x 2)
          (subs x (inc i))))
    :else nil #_(throw (str "Doesn't support name: " x))))

(defn namespace
  "Returns the namespace String of a symbol or keyword, or nil if not present."
  [x]
  (if (or (keyword? x) (symbol? x))
    (let [i (.lastIndexOf x "/")]
      (when (> i -1)
        (subs x 2 i)))
    nil #_(throw (str "Doesn't support namespace: " x))))

(defn zipmap
  "Returns a map with the keys mapped to the corresponding vals."
  [keys vals]
    (loop [map {}
           ks (seq keys)
           vs (seq vals)]
      (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
               (next ks)
               (next vs))
        map)))

(defn max-key
  "Returns the x for which (k x), a number, is greatest."
  ([k x] x)
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (reduce #(max-key k %1 %2) (max-key k x y) more)))

(defn min-key
  "Returns the x for which (k x), a number, is least."
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
     (reduce #(min-key k %1 %2) (min-key k x y) more)))

(defn partition-all
  "Returns a lazy sequence of lists like partition, but may include
  partitions with fewer than n items at the end."
  ([n coll]
     (partition-all n n coll))
  ([n step coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (cons (take n s) (partition-all n step (drop step s)))))))

(defn take-while
  "Returns a lazy sequence of successive items from coll while
  (pred item) returns true. pred must be free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (when (pred (first s))
       (cons (first s) (take-while pred (rest s)))))))

(deftype Range [meta start end step]
  IWithMeta
  (-with-meta [rng meta] (Range. meta start end step))

  IMeta
  (-meta [rng] meta)

  ISeq
  (-first [rng] start)

  (-rest [rng]
    (if (-seq rng)
      (Range. meta (+ start step) end step)
      (list)))

  ICollection
  (-conj [rng o] (cons o rng))

  IEmptyableCollection
  (-empty [rng] (with-meta cljs.core.List/EMPTY meta))

  ISequential
  IEquiv
  (-equiv [rng other] (equiv-sequential rng other))

  IHash
  (-hash [rng] (hash-coll rng))

  ICounted
  (-count [rng]
    (if-not (-seq rng)
      0
      (js/Math.ceil (/ (- end start) step))))

  IIndexed
  (-nth [rng n]
    (if (< n (-count rng))
      (+ start (* n step))
      (if (and (> start end) (= step 0))
        start
        (throw "Index out of bounds"))))
  (-nth [rng n not-found]
    (if (< n (-count rng))
      (+ start (* n step))
      (if (and (> start end) (= step 0))
        start
        not-found)))

  ISeqable
  (-seq [rng]
    (let [comp (if (pos? step) < >)]
      (when (comp start end)
        rng)))

  IReduce
  (-reduce [rng f] (ci-reduce rng f))
  (-reduce [rng f s] (ci-reduce rng f s)))

(defn range
  "Returns a lazy seq of nums from start (inclusive) to end
   (exclusive), by step, where start defaults to 0, step to 1,
   and end to infinity."
  ([] (range 0 js/Number.MAX_VALUE 1))
  ([end] (range 0 end 1))
  ([start end] (range start end 1))
  ([start end step] (Range. nil start end step)))

(defn take-nth
  "Returns a lazy seq of every nth item in coll."
  [n coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (cons (first s) (take-nth n (drop n s))))))

(defn split-with
  "Returns a vector of [(take-while pred coll) (drop-while pred coll)]"
  [pred coll]
  [(take-while pred coll) (drop-while pred coll)])

(defn partition-by
  "Applies f to each value in coll, splitting it each time f returns
   a new value.  Returns a lazy seq of partitions."
  [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           fv (f fst)
           run (cons fst (take-while #(= fv (f %)) (next s)))]
       (cons run (partition-by f (seq (drop (count run) s))))))))

(defn frequencies
  "Returns a map from distinct items in coll to the number of times
  they appear."
  [coll]
  (reduce
   (fn [counts x]
     (assoc counts x (inc (get counts x 0))))
   {}
   coll))

(defn reductions
  "Returns a lazy seq of the intermediate values of the reduction (as
  per reduce) of coll by f, starting with init."
  ([f coll]
     (lazy-seq
      (if-let [s (seq coll)]
        (reductions f (first s) (rest s))
        (list (f)))))
  ([f init coll]
     (cons init
           (lazy-seq
            (when-let [s (seq coll)]
              (reductions f (f init (first s)) (rest s)))))))

(defn juxt
  "Takes a set of functions and returns a fn that is the juxtaposition
  of those fns.  The returned fn takes a variable number of args, and
  returns a vector containing the result of applying each fn to the
  args (left-to-right).
  ((juxt a b c) x) => [(a x) (b x) (c x)]

  TODO: Implement apply"
  ([f]
     (fn
       ([] (vector (f)))
       ([x] (vector (f x)))
       ([x y] (vector (f x y)))
       ([x y z] (vector (f x y z)))
       ([x y z & args] (vector (apply f x y z args)))))
  ([f g]
     (fn
       ([] (vector (f) (g)))
       ([x] (vector (f x) (g x)))
       ([x y] (vector (f x y) (g x y)))
       ([x y z] (vector (f x y z) (g x y z)))
       ([x y z & args] (vector (apply f x y z args) (apply g x y z args)))))
  ([f g h]
     (fn
       ([] (vector (f) (g) (h)))
       ([x] (vector (f x) (g x) (h x)))
       ([x y] (vector (f x y) (g x y) (h x y)))
       ([x y z] (vector (f x y z) (g x y z) (h x y z)))
       ([x y z & args] (vector (apply f x y z args) (apply g x y z args) (apply h x y z args)))))
  ([f g h & fs]
     (let [fs (list* f g h fs)]
       (fn
         ([] (reduce #(conj %1 (%2)) [] fs))
         ([x] (reduce #(conj %1 (%2 x)) [] fs))
         ([x y] (reduce #(conj %1 (%2 x y)) [] fs))
         ([x y z] (reduce #(conj %1 (%2 x y z)) [] fs))
         ([x y z & args] (reduce #(conj %1 (apply %2 x y z args)) [] fs))))))

(defn dorun
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. dorun can
  be used to force any effects. Walks through the successive nexts of
  the seq, does not retain the head and returns nil."
  ([coll]
   (when (seq coll)
     (recur (next coll))))
  ([n coll]
   (when (and (seq coll) (pos? n))
     (recur (dec n) (next coll)))))

(defn doall
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. doall can
  be used to force any effects. Walks through the successive nexts of
  the seq, retains the head and returns it, thus causing the entire
  seq to reside in memory at one time."
  ([coll]
   (dorun coll)
   coll)
  ([n coll]
   (dorun n coll)
   coll))

;;;;;;;;;;;;;;;;;;;;;;;;; Regular Expressions ;;;;;;;;;;

(defn re-matches
  "Returns the result of (re-find re s) if re fully matches s."
  [re s]
  (let [matches (.exec re s)]
    (when (= (first matches) s)
      (if (= (count matches) 1)
        (first matches)
        (vec matches)))))

(defn re-find
  "Returns the first regex match, if any, of s to re, using
  re.exec(s). Returns a vector, containing first the matching
  substring, then any capturing groups if the regular expression contains
  capturing groups."
  [re s]
  (let [matches (.exec re s)]
    (when-not (nil? matches)
      (if (= (count matches) 1)
        (first matches)
        (vec matches)))))

(defn re-seq
  "Returns a lazy sequence of successive matches of re in s."
  [re s]
  (let [match-data (re-find re s)
        match-idx (.search s re)
        match-str (if (coll? match-data) (first match-data) match-data)
        post-match (subs s (+ match-idx (count match-str)))]
    (when match-data (lazy-seq (cons match-data (re-seq re post-match))))))

(defn re-pattern
  "Returns an instance of RegExp which has compiled the provided string."
  [s]
  (js/RegExp. s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Printing ;;;;;;;;;;;;;;;;

(defn pr-sequential [print-one begin sep end opts coll]
  (concat [begin]
          (flatten1
            (interpose [sep] (map #(print-one % opts) coll)))
          [end]))

; This should be different in different runtime environments. For example
; when in the browser, could use console.debug instead of print.
(defn string-print [x]
  (js/print x)
  nil)

(defn flush [] ;stub
  nil)

(defn- pr-seq [obj opts]
  (cond
    (nil? obj) (list "nil")
    (undefined? obj) (list "#<undefined>")
    :else (concat
            (when (and (get opts :meta)
                       (satisfies? IMeta obj)
                       (meta obj))
              (concat ["^"] (pr-seq (meta obj) opts) [" "]))
            (if (satisfies? IPrintable obj)
              (-pr-seq obj opts)
              (list "#<" (str obj) ">")))))

(defn pr-str-with-opts
  "Prints a sequence of objects to a string, observing all the
  options given in opts"
  [objs opts]
  (let [first-obj (first objs)
        sb (gstring/StringBuffer.)]
    (doseq [obj objs]
      (when-not (identical? obj first-obj)
        (.append sb " "))
      (doseq [string (pr-seq obj opts)]
        (.append sb string)))
    (str sb)))

(defn pr-with-opts
  "Prints a sequence of objects using string-print, observing all
  the options given in opts"
  [objs opts]
  (let [first-obj (first objs)]
    (doseq [obj objs]
      (when-not (identical? obj first-obj)
        (string-print " "))
      (doseq [string (pr-seq obj opts)]
        (string-print string)))))

(defn newline [opts]
  (string-print "\n")
  (when (get opts :flush-on-newline)
    (flush)))

(def *flush-on-newline* true)
(def *print-readably* true)
(def *print-meta* false)
(def *print-dup* false)

(defn- pr-opts []
  {:flush-on-newline *flush-on-newline*
   :readably *print-readably*
   :meta *print-meta*
   :dup *print-dup*})

(defn pr-str
  "pr to a string, returning it. Fundamental entrypoint to IPrintable."
  [& objs]
  (pr-str-with-opts objs (pr-opts)))

(defn pr
  "Prints the object(s) using string-print.  Prints the
  object(s), separated by spaces if there is more than one.
  By default, pr and prn print in a way that objects can be
  read by the reader"
  [& objs]
  (pr-with-opts objs (pr-opts)))

(def ^{:doc
  "Prints the object(s) using string-print.
  print and println produce output for human consumption."}
  print
  (fn cljs-core-print [& objs]
    (pr-with-opts objs (assoc (pr-opts) :readably false))))

(defn println
  "Same as print followed by (newline)"
  [& objs]
  (pr-with-opts objs (assoc (pr-opts) :readably false))
  (newline (pr-opts)))

(defn prn
  "Same as pr followed by (newline)."
  [& objs]
  (pr-with-opts objs (pr-opts))
  (newline (pr-opts)))

(extend-protocol IPrintable
  boolean
  (-pr-seq [bool opts] (list (str bool)))

  number
  (-pr-seq [n opts] (list (str n)))

  array
  (-pr-seq [a opts]
    (pr-sequential pr-seq "#<Array [" ", " "]>" opts a))

  string
  (-pr-seq [obj opts]
    (cond
     (keyword? obj)
     (list (str ":"
                (when-let [nspc (namespace obj)]
                  (str nspc "/"))
                (name obj)))
     (symbol? obj)
     (list (str (when-let [nspc (namespace obj)]
                  (str nspc "/"))
                (name obj)))
     :else (list (if (:readably opts)
                   (goog.string.quote obj)
                   obj))))

  LazySeq
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  IndexedSeq
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  List
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  Cons
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  EmptyList
  (-pr-seq [coll opts] (list "()"))

  Vector
  (-pr-seq [coll opts] (pr-sequential pr-seq "[" " " "]" opts coll))

  ObjMap
  (-pr-seq [coll opts]
    (let [pr-pair (fn [keyval] (pr-sequential pr-seq "" " " "" opts keyval))]
      (pr-sequential pr-pair "{" ", " "}" opts coll)))

  HashMap
  (-pr-seq [coll opts]
    (let [pr-pair (fn [keyval] (pr-sequential pr-seq "" " " "" opts keyval))]
      (pr-sequential pr-pair "{" ", " "}" opts coll)))

  Set
  (-pr-seq [coll opts] (pr-sequential pr-seq "#{" " " "}" opts coll))

  Range
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reference Types ;;;;;;;;;;;;;;;;

(deftype Atom [state meta validator watches]
  IEquiv
  (-equiv [o other] (identical? o other))

  IDeref
  (-deref [_] state)

  IMeta
  (-meta [_] meta)

  IPrintable
  (-pr-seq [a opts]
    (concat  ["#<Atom: "] (-pr-seq state opts) ">"))

  IWatchable
  (-notify-watches [this oldval newval]
    (doseq [[key f] watches]
      (f key this oldval newval)))
  (-add-watch [this key f]
    (set! (.watches this) (assoc watches key f)))
  (-remove-watch [this key]
    (set! (.watches this) (dissoc watches key))))

(defn atom
  "Creates and returns an Atom with an initial value of x and zero or
  more options (in any order):

  :meta metadata-map

  :validator validate-fn

  If metadata-map is supplied, it will be come the metadata on the
  atom. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an Error.  If either of these error conditions
  occur, then the value of the atom will not change."
  ([x] (Atom. x nil nil nil))
  ([x & {:keys [meta validator]}] (Atom. x meta validator nil)))

(defn reset!
  "Sets the value of atom to newval without regard for the
  current value. Returns newval."
  [a new-value]
  (when-let [validate (.validator a)]
    (assert (validate new-value) "Validator rejected reference state"))
  (let [old-value (.state a)]
    (set! (.state a) new-value)
    (-notify-watches a old-value new-value))
  new-value)

(defn swap!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args). Note that f may be called
  multiple times, and thus should be free of side effects.  Returns
  the value that was swapped in."
  ([a f]
     (reset! a (f (.state a))))
  ([a f x]
     (reset! a (f (.state a) x)))
  ([a f x y]
     (reset! a (f (.state a) x y)))
  ([a f x y z]
     (reset! a (f (.state a) x y z)))
  ([a f x y z & more]
     (reset! a (apply f (.state a) x y z more))))

(defn compare-and-set!
  "Atomically sets the value of atom to newval if and only if the
  current value of the atom is identical to oldval. Returns true if
  set happened, else false."
  [a oldval newval]
  (if (= a.state oldval)
    (do (reset! a newval) true)
    false))

;; generic to all refs
;; (but currently hard-coded to atom!)

(defn deref
  [o]
  (-deref o))

(defn set-validator!
  "Sets the validator-fn for an atom. validator-fn must be nil or a
  side-effect-free fn of one argument, which will be passed the intended
  new state on any state change. If the new state is unacceptable, the
  validator-fn should return false or throw an Error. If the current state
  is not acceptable to the new validator, an Error will be thrown and the
  validator will not be changed."
  [iref val]
  (set! (.validator iref) val))

(defn get-validator
  "Gets the validator-fn for a var/ref/agent/atom."
  [iref]
  (.validator iref))

(defn alter-meta!
  "Atomically sets the metadata for a namespace/var/ref/agent/atom to be:

  (apply f its-current-meta args)

  f must be free of side-effects"
  [iref f & args]
  (set! (.meta iref) (apply f (.meta iref) args)))

(defn reset-meta!
  "Atomically resets the metadata for an atom"
  [iref m]
  (set! (.meta iref) m))

(defn add-watch
  "Alpha - subject to change.

  Adds a watch function to an atom reference. The watch fn must be a
  fn of 4 args: a key, the reference, its old-state, its
  new-state. Whenever the reference's state might have been changed,
  any registered watches will have their functions called. The watch
  fn will be called synchronously. Note that an atom's state
  may have changed again prior to the fn call, so use old/new-state
  rather than derefing the reference. Keys must be unique per
  reference, and can be used to remove the watch with remove-watch,
  but are otherwise considered opaque by the watch mechanism.  Bear in
  mind that regardless of the result or action of the watch fns the
  atom's value will change.  Example:

      (def a (atom 0)) 
      (add-watch a :inc (fn [k r o n] (assert (== 0 n))))
      (swap! a inc)
      ;; Assertion Error
      (deref a)
      ;=> 1"
  [iref key f]
  (-add-watch iref key f))

(defn remove-watch
  "Alpha - subject to change.

  Removes a watch (set by add-watch) from a reference"
  [iref key]
  (-remove-watch iref key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gensym ;;;;;;;;;;;;;;;;
;; Internal - do not use!
(def gensym_counter nil)

(defn gensym
  "Returns a new symbol with a unique name. If a prefix string is
  supplied, the name is prefix# where # is some unique number. If
  prefix is not supplied, the prefix is 'G__'."
  ([] (gensym "G__"))
  ([prefix-string]
     (when (nil? gensym_counter)
       (set! gensym_counter (atom 0)))
     (symbol (str prefix-string (swap! gensym_counter inc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fixtures ;;;;;;;;;;;;;;;;

(def fixture1 1)
(def fixture2 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Delay ;;;;;;;;;;;;;;;;;;;;

(deftype Delay [f state]

  IDeref
  (-deref [_]
    (when-not @state
      (swap! state f))
    @state)

  IPending
  (-realized? [d]
    (not (nil? @state))))

(defn delay
  "Takes a body of expressions and yields a Delay object that will
  invoke the body only the first time it is forced (with force or deref/@), and
  will cache the result and return it on all subsequent force
  calls."
  [& body]
  (Delay. (fn [] (apply identity body)) (atom nil)))

(defn delay?
  "returns true if x is a Delay created with delay"
  [x] (instance? cljs.core.Delay x))

(defn force
  "If x is a Delay, returns the (possibly cached) value of its expression, else returns x"
  [x]
  (if (delay? x)
    (deref x)
    x))

(defn realized?
  "Returns true if a value has been produced for a promise, delay, future or lazy sequence."
  [d]
  (-realized? d))

(defn js->clj
  "Recursively transforms JavaScript arrays into ClojureScript
  vectors, and JavaScript objects into ClojureScript maps.  With
  option ':keywordize-keys true' will convert object fields from
  strings to keywords."
  [x & options]
  (let [{:keys [keywordize-keys]} options
        keyfn (if keywordize-keys keyword str)
        f (fn thisfn [x]
            (cond
             (seq? x) (doall (map thisfn x))
             (coll? x) (into (empty x) (map thisfn x))
             (goog.isArray x) (vec (map thisfn x))
             (goog.isObject x) (into {} (for [k (js-keys x)]
                                          [(keyfn k)
                                           (thisfn (aget x k))]))
             :else x))]
    (f x)))

(defn memoize
  "Returns a memoized version of a referentially transparent function. The
  memoized version of the function keeps a cache of the mapping from arguments
  to results and, when calls with the same arguments are repeated often, has
  higher performance at the expense of higher memory use."
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [v (get @mem args)]
        v
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(defn trampoline
  "trampoline can be used to convert algorithms requiring mutual
  recursion without stack consumption. Calls f with supplied args, if
  any. If f returns a fn, calls that fn with no arguments, and
  continues to repeat, until the return value is not a fn, then
  returns that non-fn value. Note that if you want to return a fn as a
  final value, you must wrap it in some data structure and unpack it
  after trampoline returns."
  ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
  ([f & args]
     (trampoline #(apply f args))))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive)."
  ([] (rand 1))
  ([n] (js* "Math.random() * ~{n}")))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive)."
  [n] (js* "Math.floor(Math.random() * ~{n})"))

(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection."
  [coll]
  (nth coll (rand-int (count coll))))

(defn group-by
  "Returns a map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll."
  [f coll]
  (reduce
   (fn [ret x]
     (let [k (f x)]
       (assoc ret k (conj (get ret k []) x))))
   {} coll))

(defn make-hierarchy
  "Creates a hierarchy object for use with derive, isa? etc."
  [] {:parents {} :descendants {} :ancestors {}})

(def
  ^{:private true}
  global-hierarchy (atom (make-hierarchy)))

(defn isa?
  "Returns true if (= child parent), or child is directly or indirectly derived from
  parent, either via a Java type inheritance relationship or a
  relationship established via derive. h must be a hierarchy obtained
  from make-hierarchy, if not supplied defaults to the global
  hierarchy"
  ([child parent] (isa? @global-hierarchy child parent))
  ([h child parent]
     (or (= child parent)
         ;; (and (class? parent) (class? child)
         ;;    (. ^Class parent isAssignableFrom child))
         (contains? ((:ancestors h) child) parent)
         ;;(and (class? child) (some #(contains? ((:ancestors h) %) parent) (supers child)))
         (and (vector? parent) (vector? child)
              (= (count parent) (count child))
              (loop [ret true i 0]
                (if (or (not ret) (= i (count parent)))
                  ret
                  (recur (isa? h (child i) (parent i)) (inc i))))))))

(defn parents
  "Returns the immediate parents of tag, either via a Java type
  inheritance relationship or a relationship established via derive. h
  must be a hierarchy obtained from make-hierarchy, if not supplied
  defaults to the global hierarchy"
  ([tag] (parents @global-hierarchy tag))
  ([h tag] (not-empty (get (:parents h) tag))))

(defn ancestors
  "Returns the immediate and indirect parents of tag, either via a Java type
  inheritance relationship or a relationship established via derive. h
  must be a hierarchy obtained from make-hierarchy, if not supplied
  defaults to the global hierarchy"
  ([tag] (ancestors @global-hierarchy tag))
  ([h tag] (not-empty (get (:ancestors h) tag))))

(defn descendants
  "Returns the immediate and indirect children of tag, through a
  relationship established via derive. h must be a hierarchy obtained
  from make-hierarchy, if not supplied defaults to the global
  hierarchy. Note: does not work on Java type inheritance
  relationships."
  ([tag] (descendants @global-hierarchy tag))
  ([h tag] (not-empty (get (:descendants h) tag))))

(defn derive
  "Establishes a parent/child relationship between parent and
  tag. Parent must be a namespace-qualified symbol or keyword and
  child can be either a namespace-qualified symbol or keyword or a
  class. h must be a hierarchy obtained from make-hierarchy, if not
  supplied defaults to, and modifies, the global hierarchy."
  ([tag parent]
   (assert (namespace parent))
   ;; (assert (or (class? tag) (and (instance? cljs.core.Named tag) (namespace tag))))
   (swap! global-hierarchy derive tag parent) nil)
  ([h tag parent]
   (assert (not= tag parent))
   ;; (assert (or (class? tag) (instance? clojure.lang.Named tag)))
   ;; (assert (instance? clojure.lang.INamed tag))
   ;; (assert (instance? clojure.lang.INamed parent))
   (let [tp (:parents h)
         td (:descendants h)
         ta (:ancestors h)
         tf (fn [m source sources target targets]
              (reduce (fn [ret k]
                        (assoc ret k
                               (reduce conj (get targets k #{}) (cons target (targets target)))))
                      m (cons source (sources source))))]
     (or
      (when-not (contains? (tp tag) parent)
        (when (contains? (ta tag) parent)
          (throw (str tag "already has" parent "as ancestor")))
        (when (contains? (ta parent) tag)
          (throw (str "Cyclic derivation:" parent "has" tag "as ancestor")))
        {:parents (assoc (:parents h) tag (conj (get tp tag #{}) parent))
         :ancestors (tf (:ancestors h) tag td parent ta)
         :descendants (tf (:descendants h) parent ta tag td)})
      h))))

(defn underive
  "Removes a parent/child relationship between parent and
  tag. h must be a hierarchy obtained from make-hierarchy, if not
  supplied defaults to, and modifies, the global hierarchy."
  ([tag parent]
     ;; (alter-var-root #'global-hierarchy underive tag parent)
     (swap! global-hierarchy underive tag parent) nil)
  ([h tag parent]
    (let [parentMap (:parents h)
          childsParents (if (parentMap tag)
                          (disj (parentMap tag) parent) #{})
          newParents (if (not-empty childsParents)
                      (assoc parentMap tag childsParents)
                      (dissoc parentMap tag))
          deriv-seq (flatten (map #(cons (first %) (interpose (first %) (second %)))
                                  (seq newParents)))]
      (if (contains? (parentMap tag) parent)
        (reduce #(apply derive %1 %2) (make-hierarchy)
                (partition 2 deriv-seq))
        h))))

(defn- reset-cache
  [method-cache method-table cached-hierarchy hierarchy]
  (swap! method-cache (fn [_] (deref method-table)))
  (swap! cached-hierarchy (fn [_] (deref hierarchy))))

(defn- prefers*
  [x y prefer-table]
  (let [xprefs (@prefer-table x)]
    (or
     (when (and xprefs (xprefs y))
       true)
     (loop [ps (parents y)]
       (when (pos? (count ps))
         (when (prefers* x (first ps) prefer-table)
           true)
         (recur (rest ps))))
     (loop [ps (parents x)]
       (when (pos? (count ps))
         (when (prefers* (first ps) y prefer-table)
           true)
         (recur (rest ps))))
     false)))

(defn- dominates
  [x y prefer-table]
  (or (prefers* x y prefer-table) (isa? x y)))

(defn- find-and-cache-best-method
  [name dispatch-val hierarchy method-table prefer-table method-cache cached-hierarchy]
  (let [best-entry (reduce (fn [be [k _ :as e]]
                             (when (isa? dispatch-val k)
                               (let [be2 (if (or (nil? be) (dominates k (first be) prefer-table))
                                           e
                                           be)]
                                 (when-not (dominates (first be2) k prefer-table)
                                   (throw (str "Multiple methods in multimethod '" name
                                               "' match dispatch value: " dispatch-val " -> " k
                                               " and " (first be2) ", and neither is preferred")))
                                 be2)))
                           nil @method-table)]
    (when best-entry
      (if (= @cached-hierarchy @hierarchy)
        (do
          (swap! method-cache assoc dispatch-val (second best-entry))
          (second best-entry))
        (do
          (reset-cache method-cache method-table cached-hierarchy hierarchy)
          (find-and-cache-best-method name dispatch-val hierarchy method-table prefer-table
                                      method-cache cached-hierarchy))))))

(defprotocol IMultiFn
  (-reset [mf])
  (-add-method [mf dispatch-val method])
  (-remove-method [mf dispatch-val])
  (-prefer-method [mf dispatch-val dispatch-val-y])
  (-get-method [mf dispatch-val])
  (-methods [mf])
  (-prefers [mf])
  (-invoke [mf args]))

(defn- do-invoke
  [mf dispatch-fn args]
  (let [dispatch-val (apply dispatch-fn args)
        target-fn (-get-method mf dispatch-val)]
    (when-not target-fn
      (throw (str "No method in multimethod '" name "' for dispatch value: " dispatch-val)))
    (apply target-fn args)))

(deftype MultiFn [name dispatch-fn default-dispatch-val hierarchy
                  method-table prefer-table method-cache cached-hierarchy]
  IMultiFn
  (-reset [mf]
    (swap! method-table (fn [mf] {}))
    (swap! method-cache (fn [mf] {}))
    (swap! prefer-table (fn [mf] {}))
    (swap! cached-hierarchy (fn [mf] nil))
    mf)

  (-add-method [mf dispatch-val method]
    (swap! method-table assoc dispatch-val method)
    (reset-cache method-cache method-table cached-hierarchy hierarchy)
    mf)

  (-remove-method [mf dispatch-val]
    (swap! method-table dissoc dispatch-val)
    (reset-cache method-cache method-table cached-hierarchy hierarchy)
    mf)

  (-get-method [mf dispatch-val]
    (when-not (= @cached-hierarchy @hierarchy)
      (reset-cache method-cache method-table cached-hierarchy hierarchy))
    (if-let [target-fn (@method-cache dispatch-val)]
      target-fn
      (if-let [target-fn (find-and-cache-best-method name dispatch-val hierarchy method-table
                                                     prefer-table method-cache cached-hierarchy)]
        target-fn
        (@method-table default-dispatch-val))))

  (-prefer-method [mf dispatch-val-x dispatch-val-y]
    (when (prefers* dispatch-val-x dispatch-val-y prefer-table)
      (throw (str "Preference conflict in multimethod '" name "': " dispatch-val-y
                  " is already preferred to " dispatch-val-x)))
    (swap! prefer-table
           (fn [old]
             (assoc old dispatch-val-x
                    (conj (get old dispatch-val-x #{})
                          dispatch-val-y))))
    (reset-cache method-cache method-table cached-hierarchy hierarchy))

  (-methods [mf] @method-table)
  (-prefers [mf] @prefer-table)

  (-invoke [mf args] (do-invoke mf dispatch-fn args)))

(set! cljs.core.MultiFn.prototype.call
      (fn [_ & args] (-invoke (js* "this") args)))

(defn remove-all-methods
  "Removes all of the methods of multimethod."
 [multifn]
 (-reset multifn))

(defn remove-method
  "Removes the method of multimethod associated with dispatch-value."
 [multifn dispatch-val]
 (-remove-method multifn dispatch-val))

(defn prefer-method
  "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y
   when there is a conflict"
  [multifn dispatch-val-x dispatch-val-y]
  (-prefer-method multifn dispatch-val-x dispatch-val-y))

(defn methods
  "Given a multimethod, returns a map of dispatch values -> dispatch fns"
  [multifn] (-methods multifn))

(defn get-method
  "Given a multimethod and a dispatch value, returns the dispatch fn
  that would apply to that value, or nil if none apply and no default"
  [multifn dispatch-val] (-get-method multifn dispatch-val))

(defn prefers
  "Given a multimethod, returns a map of preferred value -> set of other values"
  [multifn] (-prefers multifn))
