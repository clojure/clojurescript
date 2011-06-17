;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core)

(defprotocol ICounted
  (-count [coll] "constant time count"))

#_(defprotocol IEmptyableCollection
    (-empty [coll]))

(defprotocol ICollection
  (-conj [coll o]))

#_(defprotocol IOrdinal
    (-index [coll]))

(defprotocol IIndexed
  (-nth [coll n] [coll n not-found]) )

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
  #_(-get [coll v])
  )

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

(defn js-obj []
  (js* "return {}"))

(defn js-delete [obj key]
  (js* "delete ~{obj}[~{key}]"))

(defn identical? [x y]
  (js* "return ~{x} === ~{y}"))

(defn nil? [x]
  (identical? x nil))

(defn instance? [t o]
  (js* "return ~{o} instanceof ~{t};"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Seq fns ;;;;;;;;;;;;;;;;

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
  (when-let [s (seq coll)]
    (-rest s)))

(defn next
  "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil"
  [coll]
  (seq (rest coll)))

(defn second
  "Same as (first (next x))"
  [coll]
  (first (rest coll)))

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
     (if-let [s (seq coll)]
       (reduce f (first s) (next s))
       (f)))
  ([f val coll]
     (let [s (seq coll)]
       (-reduce s f val))))

; simple reduce, to be removed when IReduce is working
(defn reduce
  ([f coll]
    (if-let [s (seq coll)]
      (reduce f (first s) (next s))
      (f)))
  ([f val coll]
    (loop [val val, coll (seq coll)]
      (if coll
        (recur (f val (first coll)) (next coll))
        val))))

(defn reverse
  "Returns a seq of the items in coll in reverse order. Not lazy."
  [coll]
  (reduce conj () coll))

(defn butlast [s]
  (loop [ret [] s s]
    (if (next s)
      (recur (conj ret (first s)) (next s))
      (seq ret))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arrays ;;;;;;;;;;;;;;;;

(defn- array-clone [array-like]
  #_(goog.array.clone array-like)
  (js* "return Array.prototype.slice.call(~{array-like});"))

(defn array [& items]
  (array-clone items))

(defn aget [array i]
  (js* "return ~{array}[~{i}]"))

(defn aset [array i val]
  (js* "return ~{array}[~{i}] = ~{val}"))

(defn alength [array]
  (js* "return ~{array}.length"))

(extend-protocol IEquiv
  goog.global.String
  (-equiv [o other] (identical? o other))

  goog.global.Number
  (-equiv [o other] (identical? o other))

  goog.global.Date ; treat dates as values
  (-equiv [o other] (identical? (.toString o ()) (.toString other ()))))

(defn = [x y]
  (-equiv x y))

(defn- lazy-seq-value [lazy-seq]
  (let [x lazy-seq.x]
    (if lazy-seq.realized
      x
      (do
        (set! lazy-seq.x (x))
        (set! lazy-seq.realized true)
        lazy-seq.x))))

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

; IEmptyableCollection
; (iempty [coll] coll)

  ISeqable
  (-seq [coll] (seq (lazy-seq-value coll))))

(defn array-seq [array i]
  (prim-seq array i))

(defn prim-seq [prim i]
  (lazy-seq
    (when (< i (-count prim))
      (cons (-nth prim i) (prim-seq prim (inc i))))))

(extend-protocol ISeqable
  goog.global.String
  (-seq [string] (prim-seq string 0))
  
  goog.global.Array
  (-seq [array] (array-seq array 0)))


(deftype List [meta first rest count]
  IWithMeta
  (-with-meta [coll meta] (List. meta first rest count))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] first)
  (-rest [coll] (if (nil? rest) (EmptyList. meta) rest))

  IStack
  (-peek [coll] first)
  (-pop [coll] (irest coll))

  ICollection
  (-conj [coll o] (List. meta o coll (inc count)))

; IEmptyableCollection
; (iempty [coll] coll)

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

; IEmptyableCollection
; (iempty [coll] coll)

  ISeqable
  (-seq [coll] nil)

  ICounted
  (-count [coll] 0))

(set! cljs.core.List.EMPTY (EmptyList. nil))

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

; IEmptyableCollection
; (iempty [coll] List.EMPTY)

  ISeqable
  (-seq [coll] coll))

(extend-protocol ICounted
  goog.global.String
  (-count [s] (.length s))

  goog.global.Array
  (-count [a] (.length a)))

(extend-protocol IIndexed
  goog.global.String
  (-nth
    ([string n]
       (if (< n (-count string)) (.charAt string n)))
    ([string n not-found]
       (if (< n (-count string)) (.charAt string n)
           not_found)))

  goog.global.Array
  (-nth
    ([array n]
       (if (< n (-count array)) (aget array n)))
    ([array n not_found]
       (if (< n (-count array)) (aget array n)
           not_found))))

(extend-protocol ILookup
  goog.global.String
  (-lookup
    ([string k]
       (-nth string k))
    ([string k not_found]
       (-nth string k not_found)))
  
  goog.global.Array
  (-lookup
    ([array k]
       (-nth array k))
    ([array k not-found]
       (-nth array k not-found))))

(defn- ci-reduce
  "Accepts any collection which satisfies the ICount and IIndexed protocols and
reduces them without incurring seq initialization"
  ([cicoll f val n]
     (loop [val val, n n]
         (if (< n (-count cicoll))
           (recur (f val (-nth cicoll n)) (inc n))
           val))))

(extend-protocol IReduce
  goog.global.String
  (-reduce
    ([string f]
       (ci-reduce string f (-nth string 0) 1))
    ([string f start]
       (ci-reduce string f start 0)))
  goog.global.Array
  (-reduce
    ([array f]
       (ci-reduce array f (-nth array 0) 1))
    ([array f start]
       (ci-reduce array f start 0))))

(defn cons
  "Returns a new seq where x is the first element and seq is the rest."
  [first rest]
  (Cons. nil first rest))

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

; should use: count, nth
(defn- vector-seq [vector i]
  (lazy-seq
    (when (< i (-count vector))
      (cons (-nth vector i) (vector-seq vector (inc i))))))

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

; IEmptyableCollection
; (iempty [coll] coll)

  ISeqable
  (-seq [coll]
    (when (> (.length array) 0)
      (vector-seq coll 0)))

  ICounted
  (-count [coll] (.length array))

  IIndexed
  ; Must also check lower bound, (<= 0 n)
  (-nth [coll n]
    (if (< n (.length array))
      (aget array n)
      #_(throw (str "No item " n " in vector of length " (.length array)))))
  (-nth [coll n not-found]
    (if (< n (.length array))
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
  (-assoc-n [coll n val] (-assoc coll n val)))

(set! cljs.core.Vector.EMPTY (Vector. nil (array)))

(defn vec [coll]
  (reduce conj cljs.core.Vector.EMPTY coll)) ; using [] here causes infinite recursion

(defn vector [& args] (vec args))

(extend-protocol IHash
  goog.global.Number (-hash [o] o))

(defn hash [o]
  (when o (-hash o)))

(defn- scan-array [incr k array]
  (let [len (.length array)]
    (loop [i 0]
      (when (< i len)
        (if (= k (aget array i))
          i
          (recur (+ i incr)))))))

; Keys is an array of all keys of this map, in no particular order.
; Any string key is stored along with its value in strobj. If a string key is
; assoc'ed when that same key already exists in strobj, the old value is
; overwritten.
; Any non-string key is hashed and the result used as a property name of
; hashobj. Each values in hashobj is actually a bucket in order to handle hash
; collisions. A bucket is an array of alternating keys (not their hashes) and
; vals.
(deftype HashMap [meta keys strobj hashobj]
  IWithMeta
  (-with-meta [coll meta] (HashMap. meta keys strobj hashobj))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry] (-assoc coll (-nth entry 0) (-nth entry 1)))

; IEmptyableCollection
; (iempty [coll] coll)

  ISeqable
  (-seq [coll]
    (when (> (.length keys) 0)
      (let [hash-map-seq
             (fn hash-map-seq [i]
               (lazy-seq
                 (when (< i (.length keys))
                   (cons
                     (vector (aget keys i) (-lookup coll (aget keys i)))
                     (hash-map-seq (inc i))))))]
        (hash-map-seq 0))))

  ICounted
  (-count [coll] (.length keys))

  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found]
    (if (goog.isString k)
      (if (.hasOwnProperty strobj k)
        (aget strobj k)
        not-found)
      ; non-string key
      (let [h (hash k)
            bucket (aget hashobj h)
            i (when bucket (scan-array 2 k bucket))]
        (if i
          (aget bucket (inc i))
          not-found))))

  IAssociative
  (-assoc [coll k v]
    (if (goog.isString k)
      (let [new-strobj (goog.cloneObject strobj) ; should use goog.object.clone
            overwrite? (.hasOwnProperty new-strobj k)]
        (aset new-strobj k v)
        (if overwrite?
          (HashMap. meta keys new-strobj hashobj)
          (let [new-keys (array-clone keys)] ; append
            (.push new-keys k)
            (HashMap. meta new-keys new-strobj hashobj))))
      ; non-string key
      (let [h (hash k)
            bucket (aget hashobj h)]
        (if bucket
          (let [new-bucket (array-clone bucket)
                new-hashobj (goog.cloneObject hashobj)]
            (aset new-hashobj h new-bucket)
            (if-let [i (scan-array 2 k new-bucket)]
              (do
                (aset new-bucket (inc i) v) ; found key, replace
                (HashMap. meta keys strobj new-hashobj))
              (let [new-keys (array-clone keys)] ; did not find key, append
                (.push new-keys k)
                (.push new-bucket k v)
                (HashMap. meta new-keys strobj new-hashobj))))
          (let [new-keys (array-clone keys)
                new-hashobj (goog.cloneObject hashobj)]
            (.push new-keys k)
            (aset new-hashobj h (array k v))
            (HashMap. meta new-keys strobj new-hashobj))))))
  
  IMap
  (-dissoc [coll k]
    (if (goog.isString k)
      (if (not (.hasOwnProperty strobj k))
        coll ; key not found, return coll unchanged
        (let [new-keys (array-clone keys)
              new-strobj (goog.cloneObject strobj)
              new-count (dec (.length keys))]
          (.splice new-keys (scan-array 1 k new-keys) 1)
          (js-delete new-strobj k)
          (HashMap. meta new-keys new-strobj hashobj)))
      ; non-string key
      (let [h (hash k)
            bucket (aget hashobj h)
            i (when bucket (scan-array 2 k bucket))]
        (if (not i)
          coll ; key not found, return coll unchanged
          (let [new-keys (array-clone keys)
                new-hashobj (goog.cloneObject hashobj)]
            (if (> 3 (.length bucket))
              (js-delete new-hashobj h)
              (let [new-bucket (array-clone bucket)]
                (.splice new-bucket i 2)
                (aset new-hashobj h new-bucket)))
            (.splice new-keys (scan-array 1 k new-keys) 1)
            (HashMap. meta new-keys strobj new-hashobj)))))))

(set! cljs.core.HashMap.EMPTY (HashMap. nil (array) (js-obj) (js-obj)))

(defn hash-map [& keyvals]
  (loop [in (seq keyvals), out cljs.core.HashMap.EMPTY]
    (if in
      (recur (nnext in) (-assoc out (first in) (second in)))
      out)))

(defn conj
  "conj[oin]. Returns a new collection with the xs
  'added'. (conj nil item) returns (item).  The 'addition' may
  happen at different 'places' depending on the concrete type."
  ([coll x]
     (if coll (-conj coll x) (cons x nil)))
  ([coll x & xs]
     (if xs
       (recur (conj coll x) (first xs) (next xs))
       (conj coll x))))

;;; Math - variadic forms will not work until the following implemented:
;;; first, next, reduce

(defn +
  "Returns the sum of nums. (+) returns 0."
  ([] 0)
  ([x] x)
  ([x y] (js* "return ~{x} + ~{y};"))
  ([x y & more] (reduce + (+ x y) more)))

(defn -
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result."
  ([x] (js* "return - ~{x};"))
  ([x y] (js* "return ~{x} - ~{y};"))
  ([x y & more] (reduce - (- x y) more)))

(defn *
  "Returns the product of nums. (*) returns 1."
  ([] 1)
  ([x] x)
  ([x y] (js* "return ~{x} * ~{y};"))
  ([x y & more] (reduce * (* x y) more)))

(defn /
  "If no denominators are supplied, returns 1/numerator,
  else returns numerator divided by all of the denominators."  
  ([x] (js* "return 1 / ~{x};"))
  ([x y] (js* "return ~{x} / ~{y};"))
  ([x y & more] (reduce / (/ x y) more)))

(defn <
  "Returns non-nil if nums are in monotonically increasing order,
  otherwise false."
  ([x] true)
  ([x y] (js* "return ~{x} < ~{y};"))
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
  ([x y] (js* "return ~{x} <= ~{y};"))
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
  ([x y] (js* "return ~{x} > ~{y};"))
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
  ([x y] (js* "return ~{x} >= ~{y};"))
  ([x y & more]
   (if (>= x y)
     (if (next more)
       (recur y (first more) (next more))
       (>= y (first more)))
     false)))

(defn inc
  "Returns a number one greater than num."
  [x] (+ x 1))

(defn dec
  "Returns a number one less than num."
  [x] (- x 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; predicates and logic ;;;;;;;;;;;;;;;;

(defn not
  "Returns true if x is logical false, false otherwise."
  [x] (if x false true))

(defn pos? [n]
  (< 0 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fn stuff ;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fun seq fns ;;;;;;;;;;;;;;;;

(defn drop
  "Returns a lazy sequence of all but the first n items in coll."
  [n coll]
  (let [step (fn [n coll]
               (let [s (seq coll)]
                 (if (and (pos? n) s)
                   (recur (dec n) (rest s))
                   s)))]
    (lazy-seq (step n coll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sequence fns ;;;;;;;;;;;;;;

(defn count
  [coll]
  (if coll
    (if (satisfies? ICounted coll)
      (-count coll)
      (loop [s (seq coll) n 0]
	(if (first s)
	  (recur (rest s) (inc n))
	  n)
	#_(count (seq coll))))
    0))

(defn nth
  ([coll n]
     (when coll
       (-nth coll n)))
  ([coll n not-found]
     (when coll
       (-nth coll n not-found))))

(defn get
  ([o k]
     (when o
       (-lookup o k)))
  ([o k not-found]
     (when o
       (-lookup o k not-found))))

(defn assoc
  [coll k v]
  (when coll
    (-assoc coll k v)))

(defn dissoc
  [coll k]
  (when coll
    (-dissoc coll k)))

(defn with-meta
  [o meta]
  (when o (-with-meta o meta)))

(defn meta
  [o]
  (when o (-meta o)))

(defn peek
  [coll]
  (when coll (-peek coll)))

(defn pop
  [coll]
  (when coll (-pop coll)))

(defn contains?
  [coll v]
  (when coll (-contains? coll v)))

(defn disj
  [coll v]
  (when coll (-disjoin coll v)))

