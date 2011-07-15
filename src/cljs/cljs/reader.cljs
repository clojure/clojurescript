;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.reader
  (:require [goog.string :as gstring
             goog.string.StringBuffer :as gstringbuf]))

(defn- whitespace?
  "Checks whether the character at the index is Clojurescript whitespace"
  [s idx]
  (let [ch (.charAt s idx)]
    (or (gstring/isBreakingWhitespace ch) (= \, ch))))

(defn- comment?
  "Checks whether the character at the index begins a comment"
  [s idx]
  (= \; (.charAt s idx)))

(defn- number-literal?
  "Checks whether a character at the index is the first digit of a number"
  [s idx]
  (let [ch (.charAt s idx)]
    (or (gstring/isNumeric ch)
        (and (or (= \+ ch) (= \- ch))
             (gstring/isNumeric (.charAt s (inc idx)))))))

(defn- string-literal?
  "Checks whether the character at the index is the first character of a string literal"
  [s idx])

(defn- vector-literal?
  "Checks whether the character at the index is the first character of a vector literal"
  [s idx])

(defn- map-literal?
  "Checks whether the character at the index is the first character of a map literal"
  [s idx])

(declare read)

(defn read-number
  [s idx])

(defn read-string
  [s idx])

(defn read-vector
  [s idx])

(defn read-map
  [s idx])

(defn read-symbol
  [s idx])

(defn read-first
  "Reads the first object from a string, starting at the specified index.
Returns a tuple of the object read and the last index read.
Returns nil of the string did not contain any forms."
  [s idx]
  (if (< idx (.length s))
    (cond
     (whitespace? s idx) (recur s (inc idx))
     (number-literal? s idx) (read-number s idx)
     (string-literal? s idx) (read-string s idx)
     (vector-literal? s idx) (read-vector s idx)
     (map-literal? s idx) (read-map s idx)
     :default (read-symbol s idx))))

(defn read
  "Reads a lazy sequence of objects from a string, starting at the specified index.
Reads to the end of the string."
  [s idx]
  (let [obj (read-first s idx)]
    (lazy-seq (cons (first obj) (read s (second obj))))))