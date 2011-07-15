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

(defn- whitespace?
  "Checks whether the reader is on a whitespace character."
  [reader]
  (let [ch (peek reader)]
    (or (gstring/isBreakingWhitespace ch) (= \, ch))))

(defn- comment?
  "Checks whether the reader is at the beginning of a comment"
  [reader]
  (= \; (peek reader)))

(defn- number-literal?
  "Checks whether the reader is at the first digit of a number"
  [reader]
  (let [ch (peek reader)]
    (or (gstring/isNumeric ch)
        (and (or (= \+ ch) (= \- ch))
             (gstring/isNumeric (peek reader 2))))))

(defn- string-literal?
  "Checks whether the reader is at the first character of a string literal"
  [reader])

(defn- vector-literal?
  "Checks whether the reader is at the first character of a vector literal"
  [reader])

(defn- map-literal?
  "Checks whether the reader is at the first character of a map literal"
  [reader])

(declare read)

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
  (if (not (nil? (peek reader)))
    (cond
     (whitespace? reader) (do (read-char reader) (recur reader))
     (comment? reader) nil ;; TODO: burn through until end of comment
     (number-literal? reader) (read-number reader)
     (string-literal? reader) (read-string reader)
     (vector-literal? reader) (read-vector reader)
     (map-literal? reader) (read-map reader)
     :default (read-symbol reader))))

(defn read-all
  "Reads a lazy sequence of objects from a reader."
  [reader]
  (lazy-seq (cons (read reader) (read-all reader))))