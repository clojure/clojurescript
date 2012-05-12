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

; Using two atoms is less idomatic, but saves the repeat overhead of map creation
(deftype StringPushbackReader [s index-atom buffer-atom]
  PushbackReader
  (read-char [reader]
             (if (empty? @buffer-atom)
               (let [idx @index-atom]
                 (swap! index-atom inc)
                 (aget s idx))
               (let [buf @buffer-atom]
                 (swap! buffer-atom rest)
                 (first buf))))
  (unread [reader ch] (swap! buffer-atom #(cons ch %))))

(defn push-back-reader [s]
  "Creates a StringPushbackReader from a given string"
  (StringPushbackReader. s (atom 0) (atom nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- ^boolean whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (or (gstring/isBreakingWhitespace ch) (identical? \, ch)))

(defn- ^boolean numeric?
  "Checks whether a given character is numeric"
  [ch]
  (gstring/isNumeric ch))

(defn- ^boolean comment-prefix?
  "Checks whether the character begins a comment."
  [ch]
  (identical? \; ch))

(defn- ^boolean number-literal?
  "Checks whether the reader is at the start of a number literal"
  [reader initch]
  (or (numeric? initch)
      (and (or (identical? \+ initch) (identical? \- initch))
           (numeric? (let [next-ch (read-char reader)]
                       (unread reader next-ch)
                       next-ch)))))

(declare read macros dispatch-macros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; later will do e.g. line numbers...
(defn reader-error
  [rdr & msg]
  (throw (apply str msg)))

(defn ^boolean macro-terminating? [ch]
  (and (coercive-not= ch "#")
       (coercive-not= ch \')
       (coercive-not= ch ":")
       (macros ch)))

(defn read-token
  [rdr initch]
  (loop [sb (gstring/StringBuffer. initch)
         ch (read-char rdr)]
    (if (or (nil? ch)
            (whitespace? ch)
            (macro-terminating? ch))
      (do (unread rdr ch) (. sb (toString)))
      (recur (do (.append sb ch) sb) (read-char rdr)))))

(defn skip-line
  "Advances the reader to the end of a line. Returns the reader"
  [reader _]
  (loop []
    (let [ch (read-char reader)]
      (if (or (identical? ch \n) (identical? ch \r) (nil? ch))
        reader
        (recur)))))

(def int-pattern (re-pattern "([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?"))
(def ratio-pattern (re-pattern "([-+]?[0-9]+)/([0-9]+)"))
(def float-pattern (re-pattern "([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?"))
(def symbol-pattern (re-pattern "[:]?([^0-9/].*/)?([^0-9/][^/]*)"))

(defn- re-find*
  [re s]
  (let [matches (.exec re s)]
    (when (coercive-not= matches nil)
      (if (== (alength matches) 1)
        (aget matches 0)
        matches))))

(defn- match-int
  [s]
  (let [groups (re-find* int-pattern s)
        group3 (aget groups 2)]
    (if (coercive-not
         (or (nil? group3)
             (< (alength group3) 1)))
      0
      (let [negate (if (identical? "-" (aget groups 1)) -1 1)
            a (cond
               (aget groups 3) (array (aget groups 3) 10)
               (aget groups 4) (array (aget groups 4) 16)
               (aget groups 5) (array (aget groups 5) 8)
               (aget groups 7) (array (aget groups 7) (js/parseInt (aget groups 7)))
               :default (array nil nil))
            n (aget a 0)
            radix (aget a 1)]
        (if (nil? n)
          nil
          (* negate (js/parseInt n radix)))))))


(defn- match-ratio
  [s]
  (let [groups (re-find* ratio-pattern s)
        numinator (aget groups 1)
        denominator (aget groups 2)]
    (/ (js/parseInt numinator) (js/parseInt denominator))))

(defn- match-float
  [s]
  (js/parseFloat s))

(defn- re-matches*
  [re s]
  (let [matches (.exec re s)]
    (when (and (coercive-not= matches nil)
               (identical? (aget matches 0) s))
      (if (== (alength matches) 1)
        (aget matches 0)
        matches))))

(defn- match-number
  [s]
  (cond
   (re-matches* int-pattern s) (match-int s)
   (re-matches* ratio-pattern s) (match-ratio s)
   (re-matches* float-pattern s) (match-float s)))

(defn escape-char-map [c]
  (case c
    \t "\t"
    \r "\r"
    \n "\n"
    \\ \\
    \" \"
    \b "\b"
    \f "\f"
    nil))

(defn read-unicode-char
  [reader initch]
  (reader-error reader "Unicode characters not supported by reader (yet)"))

(defn escape-char
  [buffer reader]
  (let [ch (read-char reader)
        mapresult (escape-char-map ch)]
    (if mapresult
      mapresult
      (if (or (identical? \u ch) (numeric? ch))
        (read-unicode-char reader ch)
        (reader-error reader "Unsupported escape character: \\" ch)))))

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
  (loop [a (transient [])]
    (let [ch (read-past whitespace? rdr)]
      (when-not ch (reader-error rdr "EOF"))
      (if (identical? delim ch)
        (persistent! a)
        (if-let [macrofn (macros ch)]
          (let [mret (macrofn rdr ch)]
            (recur (if (identical? mret rdr) a (conj! a mret))))
          (do
            (unread rdr ch)
            (let [o (read rdr true nil recursive?)]
              (recur (if (identical? o rdr) a (conj! a o))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structure readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn not-implemented
  [rdr ch]
  (reader-error rdr "Reader for " ch " not implemented yet"))

(declare maybe-read-tagged-type)

(defn read-dispatch
  [rdr _]
  (let [ch (read-char rdr)
        dm (dispatch-macros ch)]
    (if dm
      (dm rdr _)
      (if-let [obj (maybe-read-tagged-type rdr ch)]
        obj
        (reader-error rdr "No dispatch macro for " ch)))))

(defn read-unmatched-delimiter
  [rdr ch]
  (reader-error rdr "Unmached delimiter " ch))

(defn read-list
  [rdr _]
  (apply list (read-delimited-list ")" rdr true)))

(def read-comment skip-line)

(defn read-vector
  [rdr _]
  (read-delimited-list "]" rdr true))

(defn read-map
  [rdr _]
  (let [l (read-delimited-list "}" rdr true)]
    (when (odd? (count l))
      (reader-error rdr "Map literal must contain an even number of forms"))
    (apply hash-map l)))

(defn read-number
  [reader initch]
  (loop [buffer (gstring/StringBuffer. initch)
         ch (read-char reader)]
    (if (or (nil? ch) (whitespace? ch) (macros ch))
      (do
        (unread reader ch)
        (let [s (. buffer (toString))]
          (or (match-number s)
              (reader-error reader "Invalid number format [" s "]"))))
      (recur (do (.append buffer ch) buffer) (read-char reader)))))

(defn read-string*
  [reader _]
  (loop [buffer (gstring/StringBuffer.)
         ch (read-char reader)]
    (cond
     (nil? ch) (reader-error reader "EOF while reading string")
     (identical? "\\" ch) (recur (do (.append buffer (escape-char buffer reader)) buffer)
                        (read-char reader))
     (identical? \" ch) (. buffer (toString))
     :default (recur (do (.append buffer ch) buffer) (read-char reader)))))

(def special-symbols
  {"nil" nil
   "true" true
   "false" false})

(defn read-symbol
  [reader initch]
  (let [token (read-token reader initch)]
    (if (gstring/contains token "/")
      (symbol (subs token 0 (.indexOf token "/"))
              (subs token (inc (.indexOf token "/")) (.-length token)))
      (get special-symbols token (symbol token)))))

(defn read-keyword
  [reader initch]
  (let [token (read-token reader (read-char reader))
        a (re-matches* symbol-pattern token)
        token (aget a 0)
        ns (aget a 1)
        name (aget a 2)]
    (if (or (and (coercive-not (undefined? ns))
                 (identical? (. ns (substring (- (.-length ns) 2) (.-length ns))) ":/"))
            (identical? (aget name (dec (.-length name))) ":")
            (coercive-not (== (.indexOf token "::" 1) -1)))
      (reader-error reader "Invalid token: " token)
      (if ns
        (keyword (.substring ns 0 (.indexOf ns "/")) name)
        (keyword token)))))

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

(defn throwing-reader
  [msg]
  (fn [rdr _]
    (reader-error rdr msg)))

(defn read-meta
  [rdr _]
  (let [m (desugar-meta (read rdr true nil true))]
    (when-not (map? m)
      (reader-error rdr "Metadata must be Symbol,Keyword,String or Map"))
    (let [o (read rdr true nil true)]
      (if (satisfies? IWithMeta o)
        (with-meta o (merge (meta o) m))
        (reader-error rdr "Metadata can only be applied to IWithMetas")))))

(defn read-set
  [rdr _]
  (set (read-delimited-list "}" rdr true)))

(defn read-regex
  [rdr ch]
  (-> (read-string* rdr ch) re-pattern))

(defn read-discard
  [rdr _]
  (read rdr true nil true)
  rdr)

(defn macros [c]
  (case c
    \" read-string*
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
    nil))

;; omitted by design: var reader, eval reader
(defn dispatch-macros [s]
  (case s
    "{" read-set
    "<" (throwing-reader "Unreadable form")
    "\"" read-regex
    "!" read-comment
    "_" read-discard
    nil))

(defn read
  "Reads the first object from a PushbackReader. Returns the object read.
   If EOF, throws if eof-is-error is true. Otherwise returns sentinel."
  [reader eof-is-error sentinel is-recursive]
  (let [ch (read-char reader)]
    (cond
     (nil? ch) (if eof-is-error (reader-error reader "EOF") sentinel)
     (whitespace? ch) (recur reader eof-is-error sentinel is-recursive)
     (comment-prefix? ch) (recur (read-comment reader ch) eof-is-error sentinel is-recursive)
     :else (let [f (macros ch)
                 res
                 (cond
                  f (f reader ch)
                  (number-literal? reader ch) (read-number reader ch)
                  :else (read-symbol reader ch))]
     (if (identical? res reader)
       (recur reader eof-is-error sentinel is-recursive)
       res)))))

(defn read-string
  "Reads one object from the string s"
  [s]
  (let [r (push-back-reader s)]
    (read r true nil false)))

  
;; read table

(defn ^:private read-date
  [str]
  (js/Date. (Date/parse str)))


(defn ^:private read-queue
  [elems]
  (if (vector? elems)
    (into cljs.core.PersistentQueue/EMPTY elems)
    (reader-error nil "Queue literal expects a vector for its elements.")))

(def *tag-table* (atom {"inst"  identity
                        "uuid"  identity
                        "queue" read-queue}))

(defn maybe-read-tagged-type
  [rdr initch]
  (let [tag  (read-symbol rdr initch)
        form (read rdr true nil false)
        pfn  (get @*tag-table* (name tag))]
    (if pfn
      (pfn form)
      (reader-error rdr "Could not find tag parser for " (name tag) (pr-str @*tag-table*)))))

(defn register-tag-parser!
  [tag f]
  (let [tag (name tag)
        old-parser (get @*tag-table* tag)]
    (swap! *tag-table* assoc tag f)
    old-parser))