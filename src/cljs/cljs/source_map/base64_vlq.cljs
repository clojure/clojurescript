;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.source-map.base64-vlq
  (:require [clojure.string :as string]
            [cljs.source-map.base64 :as base64])
  (:import [goog.string StringBuffer]))

(def ^:const vlq-base-shift 5)
(def ^:const vlq-base (bit-shift-left 1 vlq-base-shift))
(def ^:const vlq-base-mask (dec vlq-base))
(def ^:const vlq-continuation-bit vlq-base)

(defn to-vlq-signed [v]
  (if (neg? v)
    (inc (bit-shift-left (- v) 1))
    (+ (bit-shift-left v 1) 0)))

(defn from-vlq-signed [v]
  (let [neg? (= (bit-and v 1) 1)
        shifted (bit-shift-right v 1)]
    (if neg?
      (- shifted)
      shifted)))

(defn encode-val [n]
  (let [sb (StringBuffer.)
        vlq (to-vlq-signed n)]
    (loop [digit (bit-and vlq vlq-base-mask)
           vlq   (bit-shift-right-zero-fill vlq vlq-base-shift)]
      (if (pos? vlq)
        (let [digit (bit-or digit vlq-continuation-bit)]
          (.append sb (base64/encode digit))
          (recur (bit-and vlq vlq-base-mask)
                 (bit-shift-right-zero-fill vlq vlq-base-shift)))
        (.append sb (base64/encode digit))))
    (str sb)))

(defn encode [v]
  (apply str (map encode-val v)))

(defn decode [s]
  (let [l (.-length s)]
    (loop [i 0 result 0 shift 0]
      (when (>= i l)
        (throw (js/Error. "Expected more digits in base 64 VLQ value.")))
      (let [digit (base64/decode (.charAt s i))]
        (let [i (inc i)
              continuation? (pos? (bit-and digit vlq-continuation-bit))
              digit (bit-and digit vlq-base-mask)
              result (+ result (bit-shift-left digit shift))
              shift (+ shift vlq-base-shift)]
          (if continuation?
            (recur i result shift)
            (lazy-seq
             (cons (from-vlq-signed result)
                   (let [s (.substring s i)]
                     (when-not (string/blank? s)
                       (decode s)))))))))))

(comment
  ;; tests

  (bit-shift-right-zero-fill 127 1) ;; 63
  (bit-shift-right-zero-fill -127 1) ;; 2147483584
  
  (to-vlq-signed 32) ;; 64
  (to-vlq-signed -32) ;; 65
  (from-vlq-signed 64) ;; 32
  (from-vlq-signed 65) ;; -32

  ;; Base64 VLQ can only represent 32bit values

  (encode-val 32) ; "gC"
  (decode "gC") ; {:value 32 :rest ""}

  (decode "AAgBC") ; (0 0 16 1)
  
  ;; lines kept count by semicolons, segments delimited by commas
  ;; the above is gline 0, gcol 0, file 0, line 16, col 1, no name if this was the first segment read

  (decode "AAggBC") ; very clever way to encode large values
  (decode "AAggBCA") ; 5 values instead of 4

  (encode [0 0 16 1]) ; "AAgBC"

  (decode "IAWdD") ; (4 0 11 -14 -1) this is correct
  ;; gline N, gcol +4, file +0, line +11, col -14, name -1

  ;; Notes about format
  ;; we always have 1, 4, or 5 values, all zero-based indexes
  ;; 1. generated col - relative - reset on every new line in generated source
  ;; 2. index into sources list - relative
  ;; 3. original line - relative
  ;; 4. origin column - relative
  ;; 5. name - relative
  )
