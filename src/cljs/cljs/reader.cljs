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
                      (reset! state (subs original 1))
                      (first original)))
  (unread [reader ch] (reset! state (str ch @state))))

(defn push-back-reader [s]
  "Creates a StringPushbackReader from a given string"
  (StringPushbackReader. (atom s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn- meta-prefix?
  "Checks whether the character begins metadata"
  [ch]
  (= \^ ch))

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
  [ch]
  (= ch "("))

(defn- vector-prefix?
  "Checks whether the char is the start of a vector literal"
  [ch]
  (= ch "["))

(defn- map-prefix?
  "Checks whether the char is the start of a map literal"
  [ch]
  (= ch "{"))

(declare read rmacros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- skip-line
  "Advances the reader to the end of a line. Returns the reader"
  [reader]
  (let [ch (read-char reader)]
    (if (or (= ch \n) (= ch \r) (nil? ch))
      reader
      (recur reader))))

(defn read-past
  "Read until first character that doesn't match pred, returning
   char."
  [pred rdr]
  (loop [ch (read-char rdr)]
    (if (pred ch)
      (recur (read-char rdr))
      ch)))

(defn read-delimited-list
  [delim rdr recursive?]
  (loop [a ()]
    (let [ch (read-past whitespace? rdr)]
      (when-not ch (throw "EOF"))
      (if (= delim ch)
        a
        (if-let [macrofn (get rmacros ch)]
          (let [mret (macrofn rdr ch)]
            (recur (if (= mret rdr) a (conj a mret))))
          (do
            (unread rdr ch)
            (let [o (read rdr true nil recursive?)]
              (recur (if (= o rdr) a (conj a o))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structure readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-list
  [rdr _]
  (read-delimited-list ")" rdr true))

(defn read-vector
  [rdr _]
  (vec (read-delimited-list "]" rdr true)))

(defn read-map
  [rdr _]
  (let [l (read-delimited-list "}" rdr true)]
    (when (odd? (count l))
      (throw "Map literal must contain an even number of forms"))
    (apply hash-map l)))

(defn read-set
  [rdr _]
  (set (read-delimited-list "}" rdr true)))

(def rmacros
  {"{" read-set})

(defn read-number
  [reader initch])

(defn read-string
  [reader initch])

(defn read-symbol
  [reader initch is-recursive])

(defn desugar-meta
  [f]
  (cond
   (symbol? f) {:tag f}
   (string? f) {:tag f}
   (keyword? f) {f true}
   :else f))

(defn read-meta
  [rdr _]
  (let [m (desugar-meta (read rdr true nil true))]
    (when-not (map? m)
      (throw "Metadata must be Symbol,Keyword,String or Map"))
    (let [o (read rdr true nil true)]
      (if (satisfies? IWithMeta o)
        (with-meta o (merge (meta o) m))
        (throw "Metadata can only be applied to IWithMetas")))))

(defn read
  "Reads the first object from a PushbackReader. Returns the object read.
Returns sentinel if the reader did not contain any forms."
  [reader eof-is-error sentinel is-recursive]
  (let [ch (read-char reader)] 
    (cond
     (nil? ch) sentinel
     (whitespace? ch) (recur reader eof-is-error sentinel is-recursive)
     (comment-prefix? ch) (recur (skip-line reader) eof-is-error sentinel is-recursive)
     (number-literal? reader) (read-number reader ch)
     (string-prefix? ch) (read-string reader ch)
     (list-prefix? ch) (read-list reader ch)
     (vector-prefix? ch) (read-vector reader ch)
     (map-prefix? ch) (read-map reader ch)
     (meta-prefix? ch) (read-meta reader ch)
     :default (read-symbol reader ch))))


(defn read-all
  "Reads a lazy sequence of objects from a reader."
  [reader]
  (lazy-seq (cons (read reader) (read-all reader))))
