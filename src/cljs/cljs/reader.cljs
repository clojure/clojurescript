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
  (unread [reader ch] "Push back a single character on to the stream")
  (peek [reader] [reader n] "Returns the first (or nth) character of the reader without advancing the reader position"))

(deftype StringPushbackReader [state]
  PushbackReader
  (read-char [reader] (let [original @state]
                      (reset! state (subs 1 original))
                      (first original)))
  (unread [reader ch] (reset! state (str ch @state)))
  (peek [reader] (first @state))
  (peek [reader n] (nth @state n)))

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
  [reader]
  (let [ch (peek reader)]
    (or (numeric? ch)
        (and (or (= \+ ch) (= \- ch))
             (numeric? (peek reader 2))))))

(defn- string-prefix?
  "Checks whether the character starts a string"
  [ch])

(defn- vector-prefix ?
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
    (if (or (= ch \n) (= ch r) (nil? ch))
      reader
      (recur reader))))

(defn read-number
  [reader])

(defn read-string
  [reader])

(defn read-vector
  [reader])

(defn read-map
  [reader])

(defn read-symbol
  [reader])

(defn read
  "Reads the first object from a PushbackReader. Returns the object read.
Returns nil if the reader did not contain any forms."
  [reader]
  (let [ch (peek reader)] 
    (if (not (nil? ch))
      (cond
       (whitespace? ch) (do (read-char reader) (recur reader))
       (comment-prefix? ch) (recur (skip-line reader))
       (number-literal? reader) (read-number reader)
       (string-prefix? ch) (read-string reader)
       (vector-prefix? ch) (read-vector reader)
       (map-prefix? ch) (read-map reader)
       :default (read-symbol reader)))))

(defn read-all
  "Reads a lazy sequence of objects from a reader."
  [reader]
  (lazy-seq (cons (read reader) (read-all reader))))