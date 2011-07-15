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

(defprotocol PushbackReader
  (read-char [reader] "Returns the next char from the Reader,
nil if the end of stream has been reached")
  (unread [reader ch] "Push back a single character on to the stream"))

;; This implementation is quite inefficient. It can be improved by using an index into the original string
;; and using a seperate buffer to store pushed data.
(deftype StringPushbackReader [state]
  PushbackReader
  (read-char [reader] (let [original @state]
                      (reset! state (subs 1 original))
                      (first original)))
  (unread [reader ch] (reset! state (str ch @state))))

(defn push-back-reader [s]
  "Creates a StringPushbackReader from a given string"
  (StringPushbackReader. (atom s)))

;; Helper functions

(defn- whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (or (gstring/isBreakingWhitespace ch) (= \, ch)))

(defn- numeric?
  "Checks whether a given character is numeric"
  [ch]
  (gstring/isNumeric ch))

(defn- comment-prefix?
  "Checks whether the character begins a comment."
  [ch]
  (= \; ch))

(defn- number-literal?
  "Checks whether the reader is at the start of a number literal"
  [reader ch]
  (or (numeric? ch)
      (and (or (= \+ ch) (= \- ch))
           (numeric? (let [next-ch (read-char reader)]
                       (unread next-ch)
                       next-ch)))))

(defn- string-prefix?
  "Checks whether the character starts a string"
  [ch])

(defn- list-prefix?
  "Checks whether the char is the start of a list literal"
  [ch])

(defn- vector-prefix?
  "Checks whether the char is the start of a vector literal"
  [ch])

(defn- map-literal?
  "Checks whether the char is the start of a map literal"
  [ch])

(declare read)

(defn- skip-line
  "Advances the reader to the end of a line. Returns the reader"
  [reader]
  (let [ch (read-char reader)]
    (if (or (= ch \n) (= ch \r) (nil? ch))
      reader
      (recur reader))))


(def int-pattern (re-pattern "([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?"))
(def ratio-pattern (re-pattern "([-+]?[0-9]+)/([0-9]+)"))
(def float-pattern (re-pattern "([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?"))

(defn- match-int
  [s]
  (let [[substr groups] (re-find int-pattern s)]
    (if (nth groups 2)
      0
      (let [negate (if (= "-" (nth groups 1)) -1 1) 
            [[n radix]] (cond
                         (nth groups 3) [(nth groups 3) 10]
                         (nth groups 4) [(nth groups 4) 16]
                         (nth groups 5) [(nth groups 5) 8]
                         (nth groups 7) [(nth groups 7) (goog.global/parseInt (nth groups 7))] 
                         :default [nil nil])]
        (if (nil? n)
          nil
          (* negate (goog.global/parseInt n radix)))))))

(defn- match-ratio
  [s]
  (let [[substr groups] (re-find ratio-pattern s)
        numinator (nth groups 1)
        denominator (nth groups 2)]
    (/ (goog.global/parseInt numinator) (goog.global/parseInt denominator))))

(defn- match-float
  [s]
  (goog.global/parseFloat s))

(defn- match-number
  [s]
  (cond
   (re-matches int-pattern s) (match-int s)
   (re-matches ratio-pattern s) (match-ratio s)
   (re-matches float-pattern s) (match-float s)
   :default (throw (str "Invalid number format [" s "]"))))

(defn read-number
  [reader initch]
  (loop [buffer (gstring/StringBuffer. initch)
         ch (read-char reader)]
    (if (or (nil? ch) (whitespace? ch) (macro? ch))
      (do
        (unread reader ch)
        (match-number (.toString buffer)))
      (recur buffer (read-char reader)))))

(defn read-string
  [reader initch])

(defn read-list
  [reader initch is-recursive])

(defn read-vector
  [reader initch is-recursive])

(defn read-map
  [reader initch is-recursive])

(defn read-symbol
  [reader initch is-recursive])


(defn read
  "Reads the first object from a PushbackReader. Returns the object read.
Returns sentinel if the reader did not contain any forms."
  [reader eof-is-error sentinel is-recursive]
  (let [ch read-char] 
    (if (not (nil? ch))
      (cond
       (whitespace? ch) (recur reader)
       (comment-prefix? ch) (recur (skip-line reader))
       (number-literal? reader) (read-number reader ch)
       (string-prefix? ch) (read-string reader ch)
       (list-prefix? ch) (read-list reader ch is-recursive)
       (vector-prefix? ch) (read-vector reader ch is-recursive)
       (map-prefix? ch) (read-map reader ch is-recursive)
       :default (read-symbol reader ch)))
    sentinel))

(defn read-all
  "Reads a lazy sequence of objects from a reader."
  [reader]
  (lazy-seq (cons (read reader) (read-all reader))))