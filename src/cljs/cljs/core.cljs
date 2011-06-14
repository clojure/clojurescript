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
  (-nth [coll n])
  (-nth [coll n not-found]))

(defprotocol ISeq
  (-first [coll])
  (-rest [coll]))

(defprotocol ILookup
  (-lookup [o k])
  (-lookup [o k not-found]))

(defprotocol IAssociative
  #_(-contains-key? [coll k])
  #_(-entry-at [coll k])
  (-assoc [coll k v]))

(defprotocol IMap
  #_(-assoc-ex [coll k v])
  (-without [coll k]))

(defprotocol ISet
  (-contains? [coll v])
  (-disjoin [coll v])
  (-get [coll v]))

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
  (-reduce [seq f start]))

(defprotocol IEquiv
  (-equiv [o other]))

(defprotocol ISeqable
  (-seq [o]))

(defn nil? [x]
  (js* "return ~{x} === null"))

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
  (when coll
    (-first (seq coll))))

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

(defn reverse
  "Returns a seq of the items in coll in reverse order. Not lazy."
  [coll]
  ; when we have reduce: (reduce conj () coll)
  (loop [in coll, out ()]
    (if (seq in)
      (recur (rest in) (conj out (first in)))
      out)))

(defn- array-clone [array-like]
  #_(goog.array.clone array-like)
  (js* "return Array.prototype.slice.call(~{array-like});"))

(defn array [& items]
  (array-clone items))

(defn aget [array i]
  (js* "return ~{array}[~{i}]"))

(defn aset [array i val]
  (js* "return ~{array}[~{i}] = ~{val}"))

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
  (-with-meta [coll meta] (new LazySeq meta realized x))

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
  (lazy-seq
    (when (< i (.length array))
      (cons (aget array i) (array-seq array (inc i))))))

(extend-type goog.global.Array
  ISeqable
  (-seq [array] (array-seq array 0)))

(deftype List [meta first rest count]
  IWithMeta
  (-with-meta [coll meta] (new List meta first rest count))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] first)
  (-rest [coll] (if (nil? rest) (new EmptyList meta) rest))

  IStack
  (-peek [coll] first)
  (-pop [coll] (irest coll))

  ICollection
  (-conj [coll o] (new List meta o coll (inc count)))

; IEmptyableCollection
; (iempty [coll] coll)

  ISeqable
  (-seq [coll] coll)

  ICounted
  (-count [coll] count))

(deftype EmptyList [meta]
  IWithMeta
  (-with-meta [coll meta] (new EmptyList meta))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] nil)
  (-rest [coll] nil)

  IStack
  (-peek [coll] nil)
  (-pop [coll] #_(throw "Can't pop empty list"))

  ICollection
  (-conj [coll o] (new List meta o nil 1))

; IEmptyableCollection
; (iempty [coll] coll)

  ISeqable
  (-seq [coll] nil)

  ICounted
  (-count [coll] 0))

(set! cljs.core.List.EMPTY (new EmptyList nil))

(defn list [& items]
  ; when we have reduce: (reduce conj () (reverse items))
  (reverse (reverse items)))

(deftype Cons [meta first rest]
  IWithMeta
  (-with-meta [coll meta] (new Cons meta first rest))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] first)
  (-rest [coll] (if (nil? rest) () rest))

  ICollection
  (-conj [coll o] (new Cons nil o coll))

; IEmptyableCollection
; (iempty [coll] List.EMPTY)

  ISeqable
  (-seq [coll] coll))

(defn cons
  "Returns a new seq where x is the first element and seq is the rest."
  [first rest]
  (new Cons nil first rest))

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
  (-lookup [coll k] (inth coll k))
  (-lookup [coll k not-found] (-nth coll k not-found))

  IAssociative
  (-assoc [coll k v]
    (let [new-array (array-clone array)]
      (aset new-array k v)
      (Vector. meta new-array)))

  IVector
  (-assoc-n [coll n val] (-assoc coll n val)))

(set! cljs.core.Vector.EMPTY (Vector. nil (array)))

(defn vec [obj]
  (loop [in obj, out cljs.core.Vector.EMPTY]
    (if (seq in)
      (recur (rest in) (conj out (first in)))
      out)))

(defn vector [& args] (vec args))

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
  fn (right-to-left) to the result, etc."
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

(defn juxt
  "Takes a set of functions and returns a fn that is the juxtaposition
  of those fns.  The returned fn takes a variable number of args, and
  returns a vector containing the result of applying each fn to the
  args (left-to-right).
  ((juxt a b c) x) => [(a x) (b x) (c x)]"
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


(comment
  (use 'cljs.compiler)

  (import '[javax.script ScriptEngineManager])

  (def jse (-> (ScriptEngineManager.) (.getEngineByName "JavaScript")))
  (.eval jse bootjs)
  (.eval jse (clojure.java.io/reader "closure/library/closure/goog/base.js"))

  (defmacro js [form & [ns]]
    `(emit (analyze {:ns (@namespaces '~(or ns 'cljs.user)) :context :statement :locals {}} '~form)))

  (defn jseval-prn [form & [ns]]
    (let [js (emits (analyze {:ns (@namespaces (or ns 'cljs.user)) :context :expr :locals {}}
                             form))]
      ;;(prn js)
      (.eval jse (str "print(" js ")"))))

  (defn jseval [form & [ns]]
    (let [js (emits (analyze {:ns (@namespaces (or ns 'cljs.user)) :context :expr :locals {}}
                             form))]
      ;;(prn js)
      (.eval jse (str js))))
  
  (with-open [r (java.io.PushbackReader. (clojure.java.io/reader "src/cljs/cljs/core.cljs"))]
    (doseq [f (take-while identity (repeatedly (fn [] (read r false nil))))]
      (jseval f 'cljs.user)))

  (jseval '(seq nil))
  (jseval '(next (cons 1 nil)))
  (jseval '(nnext (cons (cons 1 nil) (cons 2 (cons 1 nil)))))

  (jseval '(rest (conj (conj nil 1) 2)))
  ;; 3 arg case needs apply?
  (doseq [args [[1] [1 2] #_[1 2 3]]]
    (doseq [op ['+ '- '* '/ '> '>= '< '<=]]
      (println `(~op ~@args) " => " (jseval `(~op ~@args)))))

  (jseval '(+ 1 2))

  (js '(+ 1 2 3))

  )
