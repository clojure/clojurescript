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
  [coll]
  (when coll
    (ifirst coll)))

(defn rest
  [coll]
  (when col
    (irest coll)))

#_(defn conj
    ([coll x]
       (if coll (iconj coll x) '(x))))

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
  (irest [coll] rest)

  #_ICounted
  #_(icount [coll] (inc (count rest)))

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

(comment
  (use 'cljs.compiler)

  (import '[javax.script ScriptEngineManager])

  (def jse (-> (ScriptEngineManager.) (.getEngineByName "JavaScript")))
  (.eval jse bootjs)

  (defmacro js [form]
    `(emit (analyze {:ns (@namespaces 'cljs.user) :context :statement :locals {}} '~form)))

  (defn jseval [form]
    (let [js (emits (analyze {:ns (@namespaces 'cljs.user) :context :expr :locals {}}
                             form))]
      ;;(prn js)
      (.eval jse (str "print(" js ")"))))

  (with-open [r (PushbackReader. (clojure.java.io/reader "src/cljs/cljs/core.cljs"))]
    (dorun (map-indexed #(do jseval (take-while identity (repeatedly #(read r false nil))))))
    (jseval '(ifirst (irest (cons 1 (cons 2 nil))))))
