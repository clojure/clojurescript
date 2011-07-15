(ns clojure.string
  ;; TODO pending refer-clojure exclude support
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
    (. sb (toString))))

;; TODO all replace* functions pending regexp support
#_(defn- replace-by
    [s re f]
    )

#_(defn replace
    "Replaces all instance of match with replacement in s.

   match/replacement can be:

   string / string
   char / char
   pattern / (string or function of match).

   See also replace-first."
    [s match replacement])

#_(defn- replace-first-by
    [s re f])

#_(defn- replace-first-char
    [s match replace])

#_(defn replace-first
    "Replaces the first instance of match with replacement in s.

   match/replacement can be:

   char / char
   string / string
   pattern / (string or function of match).

   See also replace-all."
    [s match replacement])

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

;; TODO pending regexp support
#_(defn split
    "Splits string on a regular expression.  Optional argument limit is
  the maximum number of splits. Not lazy. Returns vector of the splits."
    ([s re])
    ([s re limit]))

#_(defn split-lines
  "Splits s on \\n or \\r\\n."
  [s])

;; TODO all whitespace related functions pending regexp support
#_(def whitespace)

#_(defn triml
    "Removes whitespace from the left side of string."
    [s])

#_(defn trimr
    "Removes whitespace from the right side of string."
    [s])

#_(defn trim
    "Removes whitespace from both ends of string."
    [s])

#_(defn trim-newline
    "Removes all trailing newline \\n or return \\r characters from
  string.  Similar to Perl's chomp."
    [s])

#_(defn blank?
    "True if s is nil, empty, or contains only whitespace."
    [s]
    (if s
      (loop [index (int 0)]
        (if (= (.length s))
          true
          (if )))
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

