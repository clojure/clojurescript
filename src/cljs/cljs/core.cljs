;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core)

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

(defprotocol IReduce
  (ireduce [seq f start]))

(defprotocol IEquiv
  (iequiv [o other]))

(defprotocol ISeqable
  (iseq [o]))

(defn first
  "Returns the first item in the collection. Calls seq on its
  argument. If coll is nil, returns nil."
  [coll]
  (when coll
    (ifirst coll)))

(defn rest
  "Returns a possibly empty seq of the items after the first. Calls seq on its
  argument."
  [coll]
  (when coll
    (irest coll)))

(defn seq
  "Returns a seq on the collection. If the collection is
  empty, returns nil.  (seq nil) returns nil. seq also works on
  Strings, native Java arrays (of reference types) and any objects
  that implement Iterable."
  [coll]
  (when coll (iseq coll)))

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
       (ireduce s f val))))

(deftype Cons [meta first rest]
  IWithMeta
  (iwith-meta [coll meta] (new Cons meta first rest))

  IMeta
  (imeta [coll] meta)

  ISeq
  (ifirst [coll] first)
  (irest [coll] rest) ;; should this return empty list?

  ICollection
  (iconj [coll o] (new Cons nil o coll))

                                        ;  IEmptyableCollection
                                        ;  (iempty [coll] List.EMPTY)

  ISeqable
  (iseq [coll] coll))

(defn cons
  "Returns a new seq where x is the first element and seq is the rest."
  [first rest]
  (new Cons nil first rest))

(defn conj
  "conj[oin]. Returns a new collection with the xs
  'added'. (conj nil item) returns (item).  The 'addition' may
  happen at different 'places' depending on the concrete type."
  ([coll x]
     (if coll (iconj coll x) (cons x nil)))
  ([coll x & xs]
     (if xs
       (recur (iconj coll x) (first xs) (next xs))
       (iconj coll x))))

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

(comment
  (use 'cljs.compiler)

  (import '[javax.script ScriptEngineManager])

  (def jse (-> (ScriptEngineManager.) (.getEngineByName "JavaScript")))
  (.eval jse bootjs)

  (defmacro js [form]
    `(emit (analyze {:ns (@namespaces 'cljs.user) :context :statement :locals {}} '~form)))

  (defn jseval-prn [form]
    (let [js (emits (analyze {:ns (@namespaces 'cljs.user) :context :expr :locals {}}
                             form))]
      ;;(prn js)
      (.eval jse (str "print(" js ")"))))

  (defn jseval [form]
    (let [js (emits (analyze {:ns (@namespaces 'cljs.user) :context :expr :locals {}}
                             form))]
      ;;(prn js)
      (.eval jse (str js))))
  
  (with-open [r (java.io.PushbackReader. (clojure.java.io/reader "src/cljs/cljs/core.cljs"))]
    (doseq [f (take-while identity (repeatedly (fn []  (read r false nil))))]
      (jseval f)))

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
