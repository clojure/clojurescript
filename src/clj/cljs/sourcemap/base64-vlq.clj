(ns cljs.sourcemap.base64-vlq
  (require [cljs.sourcemap.base64 :as base64]))

(def ^:const vlq-base-shift 5)
(def ^:const vlq-base (bit-shift-left 1 vlq-base-shift))
(def ^:const vlq-base-mask (dec vlq-base))
(def ^:const vlq-continuation-bit vlq-base)

(defn bit-shift-right-zero-fill [x n]
  (bit-shift-right (bit-and 0xFFFFFFFF x) n))

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

(defn encode [v]
  (let [sb (StringBuilder.)
        vlq (to-vlq-signed v)]
    (loop [vlq   (bit-shift-right-zero-fill vlq vlq-base-shift)
           digit (bit-and vlq vlq-base-mask)]
      (if (pos? vlq)
        (let [digit (bit-or digit vlq-continuation-bit)]
          (.append sb (base64/encode digit))
          (recur (bit-shift-right-zero-fill vlq vlq-base-shift)
                 (bit-and vlq vlq-base-mask)))))
    (str sb)))

(defn decode [^String s]
  (let [l (count str)]
   (loop [i 0 result 0 shift 0]
     (when (>= i l)
       (throw (Error. "Expected more digits in base 64 VLQ value.")))
     (let [i (inc i)
           digit (base64/decode (.charAt s i))]
       (if (pos? (bit-and digit vlq-continuation-bit))
         (recur i
                (+ result (bit-shift-left (bit-and digit vlq-base-mask) shift))
                (+ shift vlq-base-shift))
         [result i])))))