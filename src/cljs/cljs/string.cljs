(ns cljs.string
  ;; TODO get refer-clojure exclude working
  #_(:refer-clojure :exclude [replace reverse])
  (:require [goog.string :as gstring]
            [goog.string.StringBuffer :as gstringbuf]))

(defn- seq-reverse
  [coll]
  (reduce conj () coll))

;; TODO rename to reverse
(defn str-reverse
  "Returns s with its characters reversed."
  [s]
  (let [sb (gstring/StringBuffer.)]
    (loop [coll (seq-reverse (seq (.split s "")))]
      (when coll
        (.append sb (first coll))
        (recur (next coll))))
    (str sb)))

(defn- replace-by
  [s re f])

(defn replace
  "Replaces all instance of match with replacement in s.

   match/replacement can be:

   string / string
   char / char
   pattern / (string or function of match).

   See also replace-first."
  [s match replacement])

(defn- replace-first-by
  [s re f])

(defn- replace-first-char
  [s match replace])

(defn replace-first
  "Replaces the first instance of match with replacement in s.

   match/replacement can be:

   char / char
   string / string
   pattern / (string or function of match).

   See also replace-all."
  [s match replacement])

(comment

  (defn join
    "Returns a string of all elements in coll, as returned by (seq coll),
   separated by an optional separator."
    ([coll])
    ([separator coll]))

  (defn capitalize
    "Converts first character of the string to upper-case, all other
  characters to lower-case."
    [s])

  (defn upper-case
    "Converts string to all upper-case."
    [s])

  (defn lower-case
    "Converts string to all lower-case."
    [s])

  (defn split
    "Splits string on a regular expression.  Optional argument limit is
  the maximum number of splits. Not lazy. Returns vector of the splits."
    ([s re])
    ([s re limit]))

  (defn split-lines
    "Splits s on \\n or \\r\\n."
    [s])

  (defn trim
    "Removes whitespace from both ends of string."
    [s])

  (defn triml
    "Removes whitespace from the left side of string."
    [s])

  (defn trimr
    "Removes whitespace from the right side of string."
    [s])

  (defn trim-newline
    "Removes all trailing newline \\n or return \\r characters from
  string.  Similar to Perl's chomp."
    [s])

  (defn blank?
    "True if s is nil, empty, or contains only whitespace."
    [s])

  (defn escape
    "Return a new string, using cmap to escape each character ch
   from s as follows:

   If (cmap ch) is nil, append ch to the new string.
   If (cmap ch) is non-nil, append (str (cmap ch)) instead."
    [s cmap]))