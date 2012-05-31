(ns cljs.sourcemap.base64-vlq
  (require [clojure.string :as string]
           [cljs.sourcemap.base64 :as base64]))

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
    (loop [digit (bit-and vlq vlq-base-mask)
           vlq   (bit-shift-right-zero-fill vlq vlq-base-shift)]
      (if (pos? vlq)
        (let [digit (bit-or digit vlq-continuation-bit)]
          (.append sb (base64/encode digit))
          (recur (bit-and vlq vlq-base-mask)
                 (bit-shift-right-zero-fill vlq vlq-base-shift)))
        (.append sb (base64/encode digit))))
    (str sb)))

(defn decode [^String s]
  (let [l (.length s)]
    (loop [i 0 result 0 shift 0]
      (when (>= i l)
        (throw (Error. "Expected more digits in base 64 VLQ value.")))
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

  (encode 32) ; "gC"
  (decode "gC") ; {:value 32 :rest ""}

  (decode "AAgBC") ; (0 0 16 1)

  ;; lines kept count by semicolons
  ;; the above is col 0, file 0, line 16, col 1
  )