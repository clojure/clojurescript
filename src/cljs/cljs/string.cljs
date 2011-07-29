;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.string
  (:require [goog.string :as gstring]
            [goog.string.StringBuffer :as gstringbuf]))

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

(defn capitalize
  "Converts first character of the string to upper-case, all other characters to lower-case."
  [s]
  (let [[x & xs] (str s)]
    (str (.toUpperCase x ()) (apply str xs))))

(defn escape
  "Return a new string, using cmap to escape each character ch
   from s as follows:

   If (cmap ch) is nil, append ch to the new string.
   If (cmap ch) is non-nil, append (str (cmap ch)) instead."
  [s cmap])

(defn join
  "Returns a string of all elements in coll, separated by
  an optional separator.  Like Perl's join."
  ([coll]
     (join "" coll))
  ([separator coll]
     (apply str (interpose separator coll))))

(defn lower-case
  "Converts string to all lower-case."
  [s]
  (.toLowerCase (str s) ()))

(defn replace
  "Replaces all instance of match with replacement in s.
   match/replacement can be:

   string / string
   char / char
   pattern / (string or function of match).

   See also replace-first."
  [s match replacement])

(defn replace-first
  "Replaces the first instance of match with replacement in s.
   match/replacement can be:

   char / char
   string / string
   pattern / (string or function of match).

   See also replace-all."
  [s match replacement])

;; Conflicts with core/reverse, need refer exclusions.
;;
;; (defn reverse
;;   "Returns s with its characters reversed."
;;   [s]
;;   (apply str (cljs.core/reverse s)))

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
  [s])

(defn trim-newline
  "Removes all trailing newline \n or return \r characters from
  string. Similar to Perl's chomp."
  [s])

(defn triml
  "Removes whitespace from the left side of the string."
  [s])

(defn trimr
  "Removes whitespace from the right side of the string."
  [s])

(defn upper-case
  "Converts s to all upper-case."
  [s]
  (.toUpperCase (str s) ()))
