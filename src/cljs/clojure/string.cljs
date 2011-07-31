;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.string
  (:refer-clojure :exclude [replace reverse])
  (:require [goog.string :as gstring]
            [goog.string.StringBuffer :as gstringbuf]))

(defn- seq-reverse
  [coll]
  (reduce conj () coll))

(defn reverse
  "Returns s with its characters reversed."
  [s]
  (let [sb (gstring/StringBuffer.)]
    (loop [coll (seq-reverse (seq (.split s "")))]
      (when coll
        (.append sb (first coll))
        (recur (next coll))))
    (. sb (toString))))

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
  [s match replacement]
  (.replace s match replacement))

(defn join
  "Returns a string of all elements in coll, as returned by (seq coll),
   separated by an optional separator."
  ([coll]
     (apply str coll))
  ([separator coll]
     (apply str (interpose separator coll))))

(defn upper-case
  "Converts string to all upper-case."
  [s]
  (. s (toUpperCase)))

(defn lower-case
  "Converts string to all lower-case."
  [s]
  (. s (toLowerCase)))

(defn capitalize
  "Converts first character of the string to upper-case, all other
  characters to lower-case."
  [s]
  (if (< (count s) 2)
    (upper-case s)
    (str (upper-case (subs s 0 1))
         (lower-case (subs s 1)))))

(defn split
  "Splits string on a regular expression. Optional argument limit is
  the maximum number of splits. Not lazy. Returns vector of the splits."
  ([s re]
     (vec (.split (str s) re)))
  ([s re limit]
     (vec (.split (str s) re limit))))

(defn split-lines
  "Splits s on \n or \r\n."
  [s]
  (split (str s) #"\n|\r\n"))

(defn trim
    "Removes whitespace from both ends of string."
    [s]
    (gstring/trim s))

(defn triml
    "Removes whitespace from the left side of string."
    [s]
    (gstring/trimLeft s))

(defn trimr
    "Removes whitespace from the right side of string."
    [s]
    (gstring/trimRight s))

(defn trim-newline
  "Removes all trailing newline \\n or return \\r characters from
  string.  Similar to Perl's chomp."
  [s])

;; TODO: see if you can follow the imp in string. It may be better.

(defn blank?
  "True is s is nil, empty, or contains only whitespace."
  [s]
  (let [s (str s)]
    (if (or
         (not s)
         (= "" s)
         (re-matches #"\s+" s))
      true
      false)))

#_(defn blank?
  "True if s is nil, empty, or contains only whitespace."
  {:added "1.2"}
  [^CharSequence s]
  (if s
    (loop [index (int 0)]
      (if (= (.length s) index)
        true
        (if (Character/isWhitespace (.charAt s index))
          (recur (inc index))
          false)))
    true))

(defn escape
  "Return a new string, using cmap to escape each character ch
   from s as follows:

   If (cmap ch) is nil, append ch to the new string.
   If (cmap ch) is non-nil, append (str (cmap ch)) instead."
  [s cmap]
  (let [buffer (gstring/StringBuffer.)
        length (.length s)]
    (loop [index 0]
      (if (= length index)
        (. buffer (toString))
        (let [ch (.charAt s index)]
          (if-let [replacement (get cmap ch)]
            (.append buffer (str replacement))
            (.append buffer ch))
          (recur (inc index)))))))

(comment

  ;; TODO pending regexp support
  (defn escape1
    "Is this a better implementation for JavaScript?"
    [s cmap]
    (loop [ret s
           cm cmap]
      (let [[ch replacement] (first cmap)
            ret (.replace ret
                          (re-pattern (str "/" ch "/g"))
                          (str replacement))]
        (if (next cm)
          (recur ret (rest cm))
          ret)))))

