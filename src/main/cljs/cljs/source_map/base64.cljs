;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.source-map.base64)

(def chars64 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(def char->int (zipmap chars64 (range 0 64)))
(def int->char (zipmap (range 0 64) chars64))

(defn encode [n]
  (let [e (find int->char n)]
   (if e
     (second e)
     (throw (js/Error. (str "Must be between 0 and 63: " n))))))

(defn decode [c]
  (let [e (find char->int c)]
   (if e
     (second e)
     (throw (js/Error. (str "Not a valid base 64 digit: " c))))))
