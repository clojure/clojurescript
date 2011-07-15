;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.reader
  (:require [goog.string :as gstring]))

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

(defn- number-literal?
  "Checks whether the reader is at the start of a number literal"
  [reader ch]
  (or (numeric? ch)
      (and (or (= \+ ch) (= \- ch))
           (numeric? (let [next-ch (read-char reader)]
                       (unread next-ch)
                       next-ch)))))

(declare read macros dispatch-macros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn macro-terminating? [ch]
  (and (not= ch "#") (not= ch \') (contains? macros ch)))

(defn read-token
  [rdr initch]
  (loop [sb (gstring/StringBuffer. initch)
         ch (read-char rdr)]
    (if (or (nil? ch)
            (whitespace? ch)
            (macro-terminating? ch))
      (do (unread rdr ch) (. sb (toString)))
      (recur (do (.append sb ch) sb) (read-char rdr)))))

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

(def escape-char-map {\t "\t"
                      \r "\r"
                      \n "\n"
                      \\ \\
                      \" \"
                      \b "\b"
                      \f "\f"})

(defn read-unicode-char
  [reader initch]
  (throw "Unicode characters not supported by reader (yet)"))

(defn escape-char
  [buffer reader]
  (let [ch (read-char reader)
        mapresult (get escape-char-map ch)]
    (if mapresult
      mapresult
      (if (or (= \u ch) (numeric? ch))
        (read-unicode-char reader ch)
        (throw (str "Unsupported escape charater: \\" ch))))))

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
  (loop [a []]
    (let [ch (read-past whitespace? rdr)]
      (when-not ch (throw "EOF"))
      (if (= delim ch)
        a
        (if-let [macrofn (get dispatch-macros ch)]
          (let [mret (macrofn rdr ch)]
            (recur (if (= mret rdr) a (conj a mret))))
          (do
            (unread rdr ch)
            (let [o (read rdr true nil recursive?)]
              (recur (if (= o rdr) a (conj a o))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structure readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn not-implemented
  [rdr ch]
  (throw (str "Reader for " ch " not implemented yet")))

(defn read-dispatch
  [rdr _]
  (let [ch (read-char rdr)
        dm (get dispatch-macros ch)]
    (if dm
      (dm rdr _)
      (throw (str "No dispatch macro for " ch)))))

(defn read-unmatched-delimiter
  [rdr ch]
  (throw (str "Unmached delimiter " ch)))

(defn read-list
  [rdr _]
  (apply list (read-delimited-list ")" rdr true)))

(defn read-vector
  [rdr _]
  (read-delimited-list "]" rdr true))

(defn read-map
  [rdr _]
  (let [l (read-delimited-list "}" rdr true)]
    (when (odd? (count l))
      (throw "Map literal must contain an even number of forms"))
    (apply hash-map l)))

(defn read-set
  [rdr _]
  (set (read-delimited-list "}" rdr true)))

(defn read-number
  [reader initch]
  (loop [buffer (gstring/StringBuffer. initch)
         ch (read-char reader)]
    (if (or (nil? ch) (whitespace? ch) (contains? macros ch))
      (do
        (unread reader ch)
        (match-number (. buffer (toString))))
      (recur (do (.append buffer ch) buffer) (read-char reader)))))

(defn read-string
  [reader _]
  (loop [buffer (gstring/StringBuffer.)
         ch (read-char reader)]
    (cond
     (nil? ch) (throw "EOF while reading string")
     (= "\\" ch) (recur (escape-char buffer reader) (read-char reader))
     (= \" ch) (. buffer (toString))
     :default (recur (do (.append buffer ch) buffer) (read-char reader)))))

(defn read-symbol
  [reader initch]
  (let [token (read-token reader initch)]
    (if (gstring/contains token "/")
      (symbol (subs token 0 (.indexOf token "/"))
              (subs (inc (.indexOf token "/")) (.length token)))
      (symbol token))))

(defn read-keyword
  [reader initch]
  (let [token (read-token reader (read-char reader))]
    (if (gstring/contains token "/")
      (keyword (subs token 0 (.indexOf token "/"))
              (subs (inc (.indexOf token "/")) (.length token)))
      (keyword token))))

(defn desugar-meta
  [f]
  (cond
   (symbol? f) {:tag f}
   (string? f) {:tag f}
   (keyword? f) {f true}
   :else f))

(defn wrapping-reader
  [sym]
  (fn [rdr _]
    (list sym (read rdr true nil true))))

(defn read-meta
  [rdr _]
  (let [m (desugar-meta (read rdr true nil true))]
    (when-not (map? m)
      (throw "Metadata must be Symbol,Keyword,String or Map"))
    (let [o (read rdr true nil true)]
      (if (satisfies? IWithMeta o)
        (with-meta o (merge (meta o) m))
        (throw "Metadata can only be applied to IWithMetas")))))

(def macros
     { \" read-string
       \: read-keyword
       \; not-implemented ;; never hit this
       \' (wrapping-reader 'quote)
       \@ (wrapping-reader 'deref)
       \^ read-meta
       \` not-implemented
       \~ not-implemented
       \( read-list
       \) read-unmatched-delimiter
       \[ read-vector
       \] read-unmatched-delimiter
       \{ read-map
       \} read-unmatched-delimiter
       \\ read-char
       \% not-implemented
       \# read-dispatch
       })

(def dispatch-macros
  {"{" read-set})

(defn read
  "Reads the first object from a PushbackReader. Returns the object read.
Returns sentinel if the reader did not contain any forms."
  [reader eof-is-error sentinel is-recursive]
  (let [ch (read-char reader)] 
    (cond
     (nil? ch) sentinel
     (whitespace? ch) (recur reader eof-is-error sentinel is-recursive)
     (comment-prefix? ch) (recur (skip-line reader) eof-is-error sentinel is-recursive)
     (macros ch) ((macros ch) reader ch)
     :default (read-symbol reader ch))))

(defn read-all
  "Reads a lazy sequence of objects from a reader."
  [reader]
  (lazy-seq (cons (read reader) (read-all reader))))
