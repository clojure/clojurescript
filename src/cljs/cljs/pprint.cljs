;;; pprint.cljs -- Pretty printer and Common Lisp compatible format function (cl-format) for Clojure

;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Author: Tom Faulhaber
;; April 3, 2009

(ns 
    ^{:author "Tom Faulhaber",
      :doc "A Pretty Printer for ClojureScript

cljs.pprint implements a flexible system for printing structured data
in a pleasing, easy-to-understand format. Basic use of the pretty printer is 
simple, just call pprint instead of println. More advanced users can use 
the building blocks provided to create custom output formats. 

Out of the box, pprint supports a simple structured format for basic data 
and a specialized format for Clojure source code. More advanced formats,
including formats that don't look like Clojure data at all like XML and 
JSON, can be rendered by creating custom dispatch functions. 

In addition to the pprint function, this module contains cl-format, a text 
formatting function which is fully compatible with the format function in 
Common Lisp. Because pretty printing directives are directly integrated with
cl-format, it supports very concise custom dispatch. It also provides
a more powerful alternative to Clojure's standard format function.

See documentation for pprint and cl-format for more information or 
complete documentation on the the clojure web site on github."}
    cljs.pprint
    (:require [clojure.walk :refer [walk]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions for digesting formats in the various
;;; phases of their lives.
;;; These functions are actually pretty general.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- map-passing-context [func initial-context lis]
  (loop [context initial-context
         lis lis
         acc []]
    (if (empty? lis)
      [acc context]
    (let [this (first lis)
          remainder (next lis)
          [result new-context] (apply func [this context])]
      (recur new-context remainder (conj acc result))))))

(defn- consume [func initial-context]
  (loop [context initial-context
         acc []]
    (let [[result new-context] (apply func [context])]
      (if (not result)
        [acc new-context]
      (recur new-context (conj acc result))))))

(defn- consume-while [func initial-context]
  (loop [context initial-context
         acc []]
    (let [[result continue new-context] (apply func [context])]
      (if (not continue)
        [acc context]
      (recur new-context (conj acc result))))))

(defn- unzip-map [m]
  "Take a  map that has pairs in the value slots and produce a pair of maps, 
   the first having all the first elements of the pairs and the second all 
   the second elements of the pairs"
  [(into {} (for [[k [v1 v2]] m] [k v1]))
   (into {} (for [[k [v1 v2]] m] [k v2]))])

(defn- tuple-map [m v1]
  "For all the values, v, in the map, replace them with [v v1]"
  (into {} (for [[k v] m] [k [v v1]])))

(defn- rtrim [s c]
  "Trim all instances of c from the end of sequence s"
  (let [len (count s)]
    (if (and (pos? len) (= (nth s (dec (count s))) c))
      (loop [n (dec len)]
        (cond 
         (neg? n) ""
         (not (= (nth s n) c)) (subs s 0 (inc n))
         true (recur (dec n))))
      s)))

(defn- ltrim [s c]
  "Trim all instances of c from the beginning of sequence s"
  (let [len (count s)]
    (if (and (pos? len) (= (nth s 0) c))
      (loop [n 0]
        (if (or (= n len) (not (= (nth s n) c)))
          (subs s n)
          (recur (inc n))))
      s)))

(defn- prefix-count [aseq val]
  "Return the number of times that val occurs at the start of sequence aseq, 
if val is a seq itself, count the number of times any element of val occurs at the
beginning of aseq"
  (let [test (if (coll? val) (set val) #{val})]
    (loop [pos 0]
     (if (or (= pos (count aseq)) (not (test (nth aseq pos))))
       pos
       (recur (inc pos))))))

(defn- prerr [& args]
  "Println to *err*"
  ;; there is no *err* yet
  (apply println args))
       
;; Flush the pretty-print buffer without flushing the underlying stream
(defprotocol PrettyFlush (ppflush [this]))
