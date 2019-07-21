;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.source-map.base64)

(def chars64 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(def char->int (zipmap chars64 (range 0 64)))
(def int->char (zipmap (range 0 64) chars64))

(defn encode [^long n]
  (case n
    0 \A 1 \B 2 \C 3 \D 4 \E 5 \F 6 \G 7 \H 8 \I 9 \J 10 \K 11 \L 12 \M
    13 \N 14 \O 15 \P 16 \Q 17 \R 18 \S 19 \T 20 \U 21 \V 22 \W 23 \X 24 \Y 25 \Z
    26 \a 27 \b 28 \c 29 \d 30 \e 31 \f 32 \g 33 \h 34 \i 35 \j 36 \k 37 \l 38 \m
    39 \n 40 \o 41 \p 42 \q 43 \r 44 \s 45 \t 46 \u 47 \v 48 \w 49 \x 50 \y 51 \z
    52 \0 53 \1 54 \2 55 \3 56 \4 57 \5 58 \6 59 \7 60 \8 61 \9 62 \+ 63 \/
    (throw (Error. (str "Must be between 0 and 63: " n)))))

(defn ^Character decode [c]
  (let [e (find char->int c)]
   (if e
     (second e)
     (throw (Error. (str "Not a valid base 64 digit: " c))))))