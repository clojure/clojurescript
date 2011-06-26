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
            [goog.object :as gobject]))

(defn truth_
  "Internal - do not use!"
  [x]
  (js* "(~{x} != null && ~{x} !== false)"))

(defn fn_of_
  "Internal - do not use!"
  [f]
  (js* "(~{f} instanceof Function?~{f}:~{f}.cljs$core$Fn$invoke);"))

(defn type_satisfies_
  "Internal - do not use!"
  [p x]
  (or
   (aget p (goog.typeOf x))
   (aget p "_")
   false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arrays ;;;;;;;;;;;;;;;;

(defn- array-clone [array-like]
  #_(goog.array.clone array-like)
  (js* "Array.prototype.slice.call(~{array-like})"))

(defn array [var-args];; [& items]
  (js* "Array.prototype.slice.call(arguments)"))

(defn aget [array i]
  (js* "~{array}[~{i}]"))

(defn aset [array i val]
  (js* "(~{array}[~{i}] = ~{val})"))

(defn alength [array]
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
  #_(-contains-key? [coll k])
  #_(-entry-at [coll k])
  (-assoc [coll k v]))

(defprotocol IMap
  #_(-assoc-ex [coll k v])
  (-dissoc [coll k]))

(defprotocol ISet
  (-contains? [coll v])
  (-disjoin [coll v])
  #_(-get [coll v]))

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

(defprotocol IPrintable
  (-pr-seq [o opts]))



;;;;;;;;;;;;;;;;;;; fundamentals ;;;;;;;;;;;;;;;
(defn identical? [x y]
  (js* "(~{x} === ~{y})"))

(defn = [x y]
  (-equiv x y))

(defn nil? [x]
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
  (-contains? [_ v] false)
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
    ([_ f start] start)))

(extend-type goog.global.Date
  IEquiv
  (-equiv [o other] (identical? (.toString o) (.toString other))))

(extend-type number
  IEquiv
  (-equiv [x o] (identical? x o))
  
  IHash
  (-hash [o] o))

;;this is primitive because & emits call to array-seq
(defn inc
  "Returns a number one greater than num."
  [x] (js* "(~{x} + 1)"))

(defn- lt- [x y]
  (js* "(~{x} < ~{y})"))

(defn- ci-reduce
  "Accepts any collection which satisfies the ICount and IIndexed protocols and
reduces them without incurring seq initialization"
  ([cicoll f]
     (if (= 0 (-count cicoll))
       (f)
       (loop [val (-nth cicoll 0), n 1]
         (if (lt- n (-count cicoll))
           (recur (f val (-nth cicoll n)) (inc n))
           val))))
  ([cicoll f val]
     (loop [val val, n 0]
         (if (lt- n (-count cicoll))
           (recur (f val (-nth cicoll n)) (inc n))
           val)))
  ([cicoll f val idx]
     (loop [val val, n idx]
         (if (lt- n (-count cicoll))
           (recur (f val (-nth cicoll n)) (inc n))
           val))))

(deftype IndexedSeq [a i]
  ISeqable
  (-seq [this] this)
  ISeq
  (-first [_] (aget a i))
  (-rest [_] (when (lt- (inc i) (-count a))
               (IndexedSeq. a (inc i))))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IReduce
  (-reduce [_ f]
    (ci-reduce a f (aget a i) (inc i)))
  (-reduce [_ f start]
    (ci-reduce a f start i)))

(defn prim-seq [prim i]
  (when-not (= 0 (-count prim))
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
       (if (lt- n (.length array)) (aget array n)))
    ([array n not-found]
       (if (lt- n (.length array)) (aget array n)
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
       (ci-reduce array f start)))
  )

(defn seq
  "Returns a seq on the collection. If the collection is
  empty, returns nil.  (seq nil) returns nil. seq also works on
  Strings, native Java arrays (of reference types) and any objects
  that implement Iterable."
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
  (when coll
    (-rest (seq coll))))

(defn next
  "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil"
  [coll]
  (when coll
    (seq (rest coll))))

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
   contains val at index. Note - index must be <= (count vector)."
  [coll k v]
  (-assoc coll k v))

(defn dissoc
  "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
  that does not contain a mapping for key(s)."
  [coll k]
  (-dissoc coll k))

(defn with-meta
  "Returns an object of the same type and value as obj, with
  map m as its metadata."
  [o meta]
  (-with-meta o meta))

(defn meta
  "Returns the metadata of obj, returns nil if there is no metadata."
  [o]
  (-meta o))

(defn peek
  "For a list or queue, same as first, for a vector, same as, but much
  more efficient than, last. If the collection is empty, returns nil."
  [coll]
  (-peek coll))

(defn pop
  "For a list or queue, returns a new list/queue without the first
  item, for a vector, returns a new vector without the last item. If
  the collection is empty, throws an exception.  Note - not the same
  as next/butlast."
  [coll]
  (-pop coll))

(defn contains?
  "Returns true if key is present in the given collection, otherwise
  returns false.  Note that for numerically indexed collections like
  vectors and arrays, this tests if the numeric key is within the
  range of indexes. 'contains?' operates constant or logarithmic time;
  it will not perform a linear search for a value.  See also 'some'."
  [coll v]
  (-contains? coll v))

(defn disj
  "disj[oin]. Returns a new set of the same (hashed/sorted) type, that
  does not contain key(s)."
  [coll v]
  (-disjoin coll v))

(defn hash [o]
  (-hash o))

(defn empty?
  "Returns true if coll has no items - same as (not (seq coll)).
  Please use the idiom (seq x) rather than (not (empty? x))"
  [coll] (not (seq coll)))

(defn coll?
  "Returns true if x satisfies ICollection"
  [x] (satisfies? ICollection x))

(defn set?
  "Returns true if x satisfies ISet"
  [x] (satisfies? ISet x))

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
  [x] (satisfies? IMap x))

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

(defn seq? [s]
  (satisfies? ISeq s))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Seq fns ;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; protocols for host types ;;;;;;




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
  (bit-xor (+ hash 0x9e3779b9
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
  (-count [coll] count)
  )

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
  (-count [coll] 0)

  )

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
  (-seq [coll] coll)

  )

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

; could use reify
;;; LazySeq ;;;

(defn- lazy-seq-value [lazy-seq]
  (let [x (.x lazy-seq)]
    (if (.realized lazy-seq)
      x
      (do
        (set! lazy-seq.x (x))
        (set! lazy-seq.realized true)
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
  (-seq [coll] (seq (lazy-seq-value coll)))

  )

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
       (if (. f applyTo)
         (if (<= (bounded-count args fixed-arity)
                 fixed-arity)
           (. f apply f (to-array args))
           (. f apply f (to-array args))) ;; applyTo
         (. f apply f (to-array args)))))
  ([f x args]
     (let [args (list* x args)
           fixed-arity (. f cljs$lang$maxFixedArity)]
       (if (. f applyTo)
         (if (<= (bounded-count args fixed-arity)
                 fixed-arity)
           (. f apply f (to-array args))
           (. f apply f (to-array args))) ;; applyTo
         (. f apply f (to-array args)))))
  ([f x y args]
     (let [args (list* x y args)
           fixed-arity (. f cljs$lang$maxFixedArity)]
       (if (. f applyTo)
         (if (<= (bounded-count args fixed-arity)
                 fixed-arity)
           (. f apply f (to-array args))
           (. f apply f (to-array args))) ;; applyTo
         (. f apply f (to-array args)))))
  ([f x y z args]
     (let [args (list* x y z args)
           fixed-arity (. f cljs$lang$maxFixedArity)]
       (if (. f applyTo)
         (if (<= (bounded-count args fixed-arity)
                 fixed-arity)
           (. f apply f (to-array args))
           (. f apply f (to-array args))) ;; applyTo
         (. f apply f (to-array args)))))
  ([f a b c d & args]
     (let [args (cons a (cons b (cons c (cons d (spread args)))))
           fixed-arity (. f cljs$lang$maxFixedArity)]
       (if (. f applyTo)
         (if (<= (bounded-count args fixed-arity)
                 fixed-arity)
           (. f apply f (to-array args))
           (. f apply f (to-array args))) ;; applyTo
         (. f apply f (to-array args))))))

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

(defn repeat
  "Returns a lazy (infinite!, or length n if supplied) sequence of xs."
  ([x] (lazy-seq (cons x (repeat x))))
  ([n x] (take n (repeat x))))

(defn replicate
  "Returns a lazy seq of n xs."
  [n x] (take n (repeat x)))

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
      (let [new-array (array-clone array)]
        (. new-array (pop))
        (Vector. meta new-array))
      #_(throw "Can't pop empty vector")))

  ICollection
  (-conj [coll o]
    (let [new-array (array-clone array)]
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
    (let [new-array (array-clone array)]
      (aset new-array k v)
      (Vector. meta new-array)))

  IVector
  (-assoc-n [coll n val] (-assoc coll n val))

  IReduce
  (-reduce [v f]
    (ci-reduce array f))
  (-reduce [v f start]
    (ci-reduce array start))
  )

(set! cljs.core.Vector/EMPTY (Vector. nil (array)))

(set! cljs.core.Vector/fromArray (fn [xs] (Vector. nil xs)))

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

(declare hash-map)
(deftype ObjMap [meta keys strobj]
  IWithMeta
  (-with-meta [coll meta] (ObjMap. meta keys strobj))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry] (-assoc coll (-nth entry 0) (-nth entry 1)))

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
    (if (and (goog/isString k) (.hasOwnProperty strobj k))
      (aget strobj k)
      not-found))

  IAssociative
  (-assoc [coll k v]
    (if (goog/isString k)
      (let [new-strobj (goog.object/clone strobj)
            overwrite? (.hasOwnProperty new-strobj k)]
        (aset new-strobj k v)
        (if overwrite?
          (ObjMap. meta keys new-strobj)     ; overwrite
          (let [new-keys (array-clone keys)] ; append
            (.push new-keys k)
            (ObjMap. meta new-keys new-strobj))))
      ; non-string key. game over.
      (with-meta (into (hash-map k v) (seq coll)) meta)))

  IMap
  (-dissoc [coll k]
    (if (and (goog/isString k) (.hasOwnProperty strobj k))
      (let [new-keys (array-clone keys)
            new-strobj (goog.object/clone strobj)]
        (.splice new-keys (scan-array 1 k new-keys) 1)
        (js-delete new-strobj k)
        (ObjMap. meta new-keys new-strobj))
      coll)) ; key not found, return coll unchanged

  )

(set! cljs.core.ObjMap/EMPTY (ObjMap. nil (array) (js-obj)))

(set! cljs.core.ObjMap/fromObject (fn [ks obj] (ObjMap. nil ks obj)))

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
  (-conj [coll entry] (-assoc coll (-nth entry 0) (-nth entry 1)))

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
    (let [h (hash k)
          bucket (aget hashobj h)
          i (when bucket (scan-array 2 k bucket))]
      (if i
        (aget bucket (inc i))
        not-found)))

  IAssociative
  (-assoc [coll k v]
    (let [h (hash k)
          bucket (aget hashobj h)]
      (if bucket
        (let [new-bucket (array-clone bucket)
              new-hashobj (goog.object/clone hashobj)]
          (aset new-hashobj h new-bucket)
          (if-let [i (scan-array 2 k new-bucket)]
            (do ; found key, replace
              (aset new-bucket (inc i) v)
              (HashMap. meta count new-hashobj))
            (do ; did not find key, append
              (.push new-bucket k v)
              (HashMap. meta (inc count) new-hashobj))))
        (let [new-hashobj (goog.object/clone hashobj)] ; did not find bucket
          (aset new-hashobj h (array k v))
          (HashMap. meta (inc count) new-hashobj)))))
  
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
            (let [new-bucket (array-clone bucket)]
              (.splice new-bucket i 2)
              (aset new-hashobj h new-bucket)))
          (HashMap. meta (dec count) new-hashobj)))))

  )

(set! cljs.core.HashMap/EMPTY (HashMap. nil 0 (js-obj)))

(set! cljs.core.HashMap/fromArrays (fn [ks vs]
  (let [len (.length ks)]
    (loop [i 0, out cljs.core.HashMap/EMPTY]
      (if (< i len)
        (recur (inc i) (assoc out (aget ks i) (aget vs i)))
        out)))))

(defn hash-map
  "keyval => key val
  Returns a new hash map with supplied mappings."
  [& keyvals]
  (loop [in (seq keyvals), out cljs.core.HashMap/EMPTY]
    (if in
      (recur (nnext in) (assoc out (first in) (second in)))
      out)))

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
    :else nil #_(throw (str "Doesn't support namespace: " x))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Printing ;;;;;;;;;;;;;;;;

(defn pr-sequential [print-one begin sep end opts coll]
  (concat [begin]
          (flatten1
            (interpose [sep] (map #(print-one % opts) coll)))
          [end]))

; This should be different in different runtime environments. For example
; when in the browser, could use console.debug instead of print.
(defn string-print [x]
  (goog.global/print x)
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
  "Prints a single object to a string, observing all the
  options given in opts"
  [obj opts]
  (let [sb (gstring/StringBuffer.)]
    (loop [coll (seq (pr-seq obj opts))]
      (when coll
        (.append sb (first coll))
        (recur (next coll))))
    (str sb)))

(defn pr-with-opts
  "Prints a single object using string-print, observing all
  the options given in opts"
  [obj opts]
  (loop [coll (seq (pr-seq obj opts))]
    (when coll
      (string-print (first coll))
      (recur (next coll)))))

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

; These should all be variadic.  Where oh where has my apply gone?
(defn pr-str
  "pr to a string, returning it. Fundamental entrypoint to IPrintable."
  [obj]
  (pr-str-with-opts obj (pr-opts)))

(defn pr
  "Prints the object(s) using string-print.  Prints the
  object(s), separated by spaces if there is more than one.
  By default, pr and prn print in a way that objects can be
  read by the reader"
  [obj]
  (pr-with-opts obj (pr-opts)))

(defn println
  "Prints the object(s) using string-print.
  print and println produce output for human consumption."
  [obj]
  (pr-with-opts obj (assoc (pr-opts) :readably false))
  (newline (pr-opts)))

(defn prn
  "Same as pr followed by (newline)."
  [obj]
  (pr-with-opts obj (pr-opts))
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
     (get opts :readably)
     (list (goog.string.quote obj))
     :else (list obj)))
 
  LazySeq
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
      (pr-sequential pr-pair "{" ", " "}" opts coll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reference Types ;;;;;;;;;;;;;;;;

(deftype Atom [state meta validator]
  IEquiv
  (-equiv [o other] (identical? o other))

  IDeref
  (-deref [_] state)

  IMeta
  (-meta [_] meta))

(defn atom
  "Creates and returns an Atom with an initial value of x and zero or
  more options (in any order):

  :meta metadata-map

  :validator validate-fn

  If metadata-map is supplied, it will be come the metadata on the
  atom. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an exception."
  ([x] (Atom. x nil nil))
  ([x & {:keys [meta validator]}] (Atom. x meta validator)))

(defn reset!
  "Sets the value of atom to newval without regard for the
  current value. Returns newval."
  [a newval]
  (when-let [v (.validator a)]
    (when-not (v newval)
      (throw "Validator rejected reference state")))
  (set! a.state newval))

(defn swap!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args). Note that f may be called
  multiple times, and thus should be free of side effects.  Returns
  the value that was swapped in."
  ([a f]
     (reset! a (f (.state a))))
  ([a f & args]
     (reset! a (apply f (.state a) args))))

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
  "Sets the validator-fn for a var/ref/agent/atom. validator-fn must be nil or a
  side-effect-free fn of one argument, which will be passed the intended
  new state on any state change. If the new state is unacceptable, the
  validator-fn should return false or throw an exception. If the current state (root
  value if var) is not acceptable to the new validator, an exception
  will be thrown and the validator will not be changed."
  [iref val]
  (set! iref.validator val))

(defn get-validator
  "Gets the validator-fn for a var/ref/agent/atom."
  [iref]
  (.validator iref))

(defn alter-meta!
  "Atomically sets the metadata for a namespace/var/ref/agent/atom to be:

  (apply f its-current-meta args)

  f must be free of side-effects"
  [iref f & args]
  (set! iref.meta (apply f (.meta iref) args)))

(defn reset-meta!
  "Atomically resets the metadata for a namespace/var/ref/agent/atom"
  [iref m]
  (set! iref.meta m))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;

(defn test-stuff []
  (assert (= "baz" (name 'foo/bar/baz)))
  (assert (= "foo/bar" (namespace 'foo/bar/baz)))
  (assert (= "baz" (name :foo/bar/baz)))
  ;(assert (= "foo/bar" (namespace :foo/bar/baz)))

  (assert (= {:a :b} (get {[1 2 3] {:a :b}, 4 5} [1 2 3])))
  (assert (= :a (nth [:a :b :c :d] 0)))
  (assert (not (= {:a :b :c nil} {:a :b :d nil})))
  (assert (= (list 3 2 1) [3 2 1]))
  (assert (= [3 2 1] (seq (array 3 2 1))))
  (assert (= {"x" "y"} (meta ^{"x" "y"} [])))
  (assert (= {:a :b} (dissoc {:a :b :c :d} :c)))
  (assert (= (hash-map :foo 5)
             (assoc (cljs.core.ObjMap. nil (array) (js-obj)) :foo 5)))

  (assert (= "[1 true {:a 2, :b 42} #<Array [3, 4]>]"
             (pr-str [1 true {:a 2 :b 42} (array 3 4)])))
  (assert (= "symbol\"'string"
             (pr-str (str 'symbol \" \' "string"))))
  (assert (not (= "one" "two")))
  (assert (= 3 (-count "abc")))
  (assert (= 4 (-count (array 1 2 3 4))))
  (assert (= "c" (-nth "abc" 2)))
  (assert (= "quux" (-nth "abc" 3 "quux")))
  (assert (= 1 (-nth (array 1 2 3 4) 0)))
  (assert (= "val" (-nth (array 1 2 3 4) 4 "val")))
  (assert (= "b" (-lookup "abc" 1)))
  (assert (= "harriet" (-lookup "abcd" 4 "harriet")))
  (assert (= 4 (-lookup (array 1 2 3 4) 3)))
  (assert (= "zot" (-lookup (array 1 2 3 4) 4 "zot")))
  (assert (= 10 (-reduce (array 1 2 3 4) +)))
  (assert (= 20 (-reduce (array 1 2 3 4) + 10)))
  (assert (= "cabd" (let
                        [jumble (fn [a b] (str (apply str (reverse (str a))) b))]
                      (-reduce "abcd" jumble))))
  (assert (= "cafrogbd" (let
                            [jumble (fn [a b] (str (apply str (reverse (str a))) b))]
                          (-reduce "abcd" jumble "frog"))))
  (assert (= [0 0 1 0 1]
               [(bit-and 1 0)
                (bit-and 0 0)
                (bit-and 1 1)
                (bit-and 42 1)
                (bit-and 41 1)]))
  (assert (= [1 0 1 43 41]
               [(bit-or 1 0)
                (bit-or 0 0)
                (bit-or 1 1)
                (bit-or 42 1)
                (bit-or 41 1)]))
  (assert (= [1 0 0 42 40]
               [(bit-and-not 1 0)
                (bit-and-not 0 0)
                (bit-and-not 1 1)
                (bit-and-not 42 1)
                (bit-and-not 41 1)]))
  (assert (= [0 2 968 16649 0]
               [(bit-clear 1 0)
                (bit-clear 2 0)
                (bit-clear 1000 5)
                (bit-clear 16713 6)
                (bit-clear 1024 10)]))
  (assert (= [0 0 992 18761 0]
               [(bit-flip 1 0)
                (bit-flip 2 1)
                (bit-flip 1000 3)
                (bit-flip 16713 11)
                (bit-flip 1024 10)]))
  (assert (= [-2 -3 999 -16714 -1025]
               [(bit-not 1)
                (bit-not 2)
                (bit-not -1000)
                (bit-not 16713)
                (bit-not 1024)]))
  (assert (= [1 2 1000 18761 1024]
               [(bit-set 1 0)
                (bit-set 2 1)
                (bit-set 1000 3)
                (bit-set 16713 11)
                (bit-set 1024 10)]))
  (assert (= [true true true false true]
               [(bit-test 1 0)
                (bit-test 2 1)
                (bit-test 1000 3)
                (bit-test 16713 11)
                (bit-test 1024 10)]))
  (assert (= [true false true false false false]
             [(true? true)
              (true? false)
              (false? false)
              (false? true)
              (true? goog.global.undefined)
              (false? goog.global.undefined)]))
  (let [a (atom 0)]
    (assert (= 0 (deref a)))
    (assert (= 1 (swap! a inc)))
    (assert (= false (compare-and-set! a 0 42)))
    (assert (= true (compare-and-set! a 1 7)))
    (assert (nil? (meta a)))
    (assert (nil? (get-validator a))))
  (let [a (atom [1] :validator coll? :meta {:a 1})]
    (assert (= coll? (get-validator a)))
    (assert (= {:a 1} (meta a)))
    (alter-meta! a assoc :b 2)
    (assert (= {:a 1 :b 2} (meta a))))
  (assert (nil? (empty nil)))
  (let [e-lazy-seq (empty (with-meta (lazy-seq (cons :a nil)) {:b :c}))]
    (assert (seq? e-lazy-seq))
    (assert (empty? e-lazy-seq))
    (assert (= {:b :c} (meta e-lazy-seq))))
  (let [e-list (empty '^{:b :c} (1 2 3))]
    (assert (seq? e-list))
    (assert (empty? e-list)))
  (let [e-elist (empty '^{:b :c} ())]
    (assert (seq? e-elist))
    (assert (empty? e-elist))
    (assert (= :c (get (meta e-elist) :b))))
  (let [e-cons (empty (with-meta (cons :a nil) {:b :c}))]
    (assert (seq? e-cons))
    (assert (empty? e-cons))
    (assert (= {:b :c} (meta e-cons))))
  (let [e-vec (empty ^{:b :c} [:a :d :g])]
    (assert (vector? e-vec))
    (assert (empty? e-vec))
    (assert (= {:b :c} (meta e-vec))))
  (let [e-omap (empty ^{:b :c} {:a :d :g :h})]
    (assert (map? e-omap))
    (assert (empty? e-omap))
    (assert (= {:b :c} (meta e-omap))))
  (let [e-hmap (empty ^{:b :c} {[1 2] :d :g :h})]
    (assert (map? e-hmap))
    (assert (empty? e-hmap))
    (assert (= {:b :c} (meta e-hmap))))
  (let [a (atom nil)]
    (assert (= 1 (try* 1)))
    (assert (= 1 (try* 1 (finally 2))))
    (assert (= 2 (try* 1 (throw 3) (catch e 2))))
    (assert (= 3 (try* 1 (throw 3) (catch e e))))
    (assert (= 3 (try* 1 (throw 3) (catch e e) (finally 4))))
    (assert (= 2 (try* 1 (throw 3) (catch e e 1 2))))
    (assert (= 1 (try* 1 (finally (reset! a 42)))))
    (assert (= 42 (deref a))))
  (let [a (atom nil)]
    (assert (= 1 (try 1)))
    (assert (= 1 (try 1 (finally 2))))
    (assert (= 2 (try 1 (throw (goog.global.Error.)) (catch goog.global.Error e 2))))
    (assert (= 2 (try 1 (throw (goog.global.Error.)) (catch goog.global.Error e 1 2))))
    (assert (= 1 (try 1 (finally (reset! a 42)))))
    (assert (= 42 (deref a))))
  :ok
  )

#_(goog.global/print (assoc {} :a 1))