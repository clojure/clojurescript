;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc
      "A library for reduction and parallel folding. Alpha and subject
      to change.  Note that fold and its derivatives require
      jsr166y.jar for fork/join support. See Clojure's pom.xml for the
      dependency info."
      :author "Rich Hickey"}
  clojure.core.reducers
  (:refer-clojure :exclude [reduce map mapcat filter remove take take-while drop flatten cat])
  (:require [cljs.core :as core]))

;;;;;;;;;;;;;; some fj stuff ;;;;;;;;;;
(defn- fjtask [f]
  f)

(defn- fjinvoke [f]
  (f))

(defn- fjfork [task]
  task)

(defn- fjjoin [task]
  (task))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reduce
  "Like core/reduce except:
     When init is not provided, (f) is used.
     Maps are reduced with reduce-kv"
  ([f coll] (reduce f (f) coll))
  ([f init coll]
     (if (map? coll)
       (-kv-reduce coll f init)
       (cond
         (nil? coll) init
         (array? coll) (array-reduce coll f init)
         :else (-reduce coll f init)))))

(defprotocol CollFold
  (coll-fold [coll n combinef reducef]))

(defn fold
  "Reduces a collection using a (potentially parallel) reduce-combine
  strategy. The collection is partitioned into groups of approximately
  n (default 512), each of which is reduced with reducef (with a seed
  value obtained by calling (combinef) with no arguments). The results
  of these reductions are then reduced with combinef (default
  reducef). combinef must be associative, and, when called with no
  arguments, (combinef) must produce its identity element. These
  operations may be performed in parallel, but the results will
  preserve order.

  Note: Performing operations in parallel is currently not implemented."
  ([reducef coll] (fold reducef reducef coll))
  ([combinef reducef coll] (fold 512 combinef reducef coll))
  ([n combinef reducef coll]
     (coll-fold coll n combinef reducef)))

(defn reducer
  "Given a reducible collection, and a transformation function xf,
  returns a reducible collection, where any supplied reducing
  fn will be transformed by xf. xf is a function of reducing fn to
  reducing fn."
  ([coll xf]
     (reify
       cljs.core/IReduce
       (-reduce [this f1]
         (-reduce this f1 (f1)))
       (-reduce [_ f1 init]
         (-reduce coll (xf f1) init)))))

(defn folder
  "Given a foldable collection, and a transformation function xf,
  returns a foldable collection, where any supplied reducing
  fn will be transformed by xf. xf is a function of reducing fn to
  reducing fn."
  ([coll xf]
     (reify
       cljs.core/IReduce
       (-reduce [_ f1]
         (-reduce coll (xf f1) (f1)))
       (-reduce [_ f1 init]
         (-reduce coll (xf f1) init))

       CollFold
       (coll-fold [_ n combinef reducef]
         (coll-fold coll n combinef (xf reducef))))))

(defcurried map
  "Applies f to every value in the reduction of coll. Foldable."
  {}
  [f coll]
  (folder coll
   (fn [f1]
     (rfn [f1 k]
          ([ret k v]
             (f1 ret (f k v)))))))

(defcurried mapcat
  "Applies f to every value in the reduction of coll, concatenating the result
  colls of (f val). Foldable."
  {}
  [f coll]
  (folder coll
   (fn [f1]
     (rfn [f1 k]
          ([ret k v]
             (reduce f1 ret (f k v)))))))

(defcurried filter
  "Retains values in the reduction of coll for which (pred val)
  returns logical true. Foldable."
  {}
  [pred coll]
  (folder coll
   (fn [f1]
     (rfn [f1 k]
          ([ret k v]
             (if (pred k v)
               (f1 ret k v)
               ret))))))

(defcurried flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat foldable
  collection."
  {}
  [coll]
  (folder coll
   (fn [f1]
     (fn
       ([] (f1))
       ([ret v]
          (if (sequential? v)
            (-reduce (flatten v) f1 ret)
            (f1 ret v)))))))

(defcurried remove
  "Removes values in the reduction of coll for which (pred val)
  returns logical true. Foldable."
  {}
  [pred coll]
  (filter (complement pred) coll))

(defcurried take-while
  "Ends the reduction of coll when (pred val) returns logical false."
  {}
  [pred coll]
  (reducer coll
   (fn [f1]
     (rfn [f1 k]
          ([ret k v]
             (if (pred k v)
               (f1 ret k v)
               (reduced ret)))))))

(defcurried take
  "Ends the reduction of coll after consuming n values."
  {}
  [n coll]
  (reducer coll
   (fn [f1]
     (let [cnt (atom n)]
       (rfn [f1 k]
         ([ret k v]
            (swap! cnt dec)
            (if (neg? @cnt)
              (reduced ret)
              (f1 ret k v))))))))

(defcurried drop
  "Elides the first n values from the reduction of coll."
  {}
  [n coll]
  (reducer coll
   (fn [f1]
     (let [cnt (atom n)]
       (rfn [f1 k]
         ([ret k v]
            (swap! cnt dec)
            (if (neg? @cnt)
              (f1 ret k v)
              ret)))))))

;;do not construct this directly, use cat
(deftype Cat [cnt left right]
  cljs.core/ICounted
  (-count [_] cnt)

  cljs.core/ISeqable
  (-seq [_] (concat (seq left) (seq right)))

  cljs.core/IReduce
  (-reduce [this f1] (-reduce this f1 (f1)))
  (-reduce
    [_  f1 init]
    (-reduce
     right f1
     (-reduce left f1 init)))

  CollFold
  (coll-fold
    [this n combinef reducef]
    (-reduce this reducef)))

(defn cat
  "A high-performance combining fn that yields the catenation of the
  reduced values. The result is reducible, foldable, seqable and
  counted, providing the identity collections are reducible, seqable
  and counted. The single argument version will build a combining fn
  with the supplied identity constructor. Tests for identity
  with (zero? (count x)). See also foldcat."
  ([] (array))
  ([ctor]
     (fn
       ([] (ctor))
       ([left right] (cat left right))))
  ([left right]
     (cond
       (zero? (count left)) right
       (zero? (count right)) left
       :else
       (Cat. (+ (count left) (count right)) left right))))

(defn append!
  ".adds x to acc and returns acc"
  [acc x]
  (doto acc (.push x)))

(defn foldcat
  "Equivalent to (fold cat append! coll)"
  [coll]
  (fold cat append! coll))

(defn monoid
  "Builds a combining fn out of the supplied operator and identity
  constructor. op must be associative and ctor called with no args
  must return an identity value for it."
  [op ctor]
  (fn m
    ([] (ctor))
    ([a b] (op a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
(require '[clojure.core.reduce :as r])
(def v (take 1000000 (range)))
(reduce + 0 (r/map inc [1 2 3 4]))
(into [] (r/take 12 (range 100)))
(into [] (r/drop 12 (range 100)))
(reduce + 0 (r/filter even? [1 2 3 4]))
(into [] (r/filter even? [1 2 3 4]))
(reduce + (filter even? [1 2 3 4]))
(dotimes [_ 10] (time (reduce + 0 (r/map inc v))))
(dotimes [_ 10] (time (reduce + 0 (map inc v))))
(dotimes [_ 100] (time (reduce + 0 v)))
(dotimes [_ 100] (time (reduce + 0 v)))
(dotimes [_ 20] (time (reduce + 0 (r/map inc (r/filter even? v)))))
(dotimes [_ 20] (time (reduce + 0 (map inc (filter even? v)))))
(reduce + 0 (r/take-while even? [2 4 3]))
(into [] (r/filter even? (r/flatten (r/remove #{4} [[1 2 3] 4 [5 [6 7 8]] [9] 10]))))
(into [] (r/flatten nil))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fold impls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- foldvec
  [v n combinef reducef]
  (cond
   (empty? v) (combinef)
   (<= (count v) n) (reduce reducef (combinef) v)
   :else
   (let [split (quot (count v) 2)
         v1 (subvec v 0 split)
         v2 (subvec v split (count v))
         fc (fn [child] #(foldvec child n combinef reducef))]
     (fjinvoke
      #(let [f1 (fc v1)
             t2 (fjtask (fc v2))]
         (fjfork t2)
         (combinef (f1) (fjjoin t2)))))))

(extend-protocol CollFold
 nil
 (coll-fold
  [coll n combinef reducef]
  (combinef))

 object
 (coll-fold
  [coll n combinef reducef]
  ;;can't fold, single reduce
  (reduce reducef (combinef) coll))

 cljs.core/PersistentVector
 (coll-fold
  [v n combinef reducef]
  (foldvec v n combinef reducef))

 #_
 cljs.core/PersistentHashMap
 #_
 (coll-fold
  [m n combinef reducef]
  (.fold m n combinef reducef fjinvoke fjtask fjfork fjjoin)))

