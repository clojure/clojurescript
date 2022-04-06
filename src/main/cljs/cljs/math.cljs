(ns ^{:doc "ClojureScript wrapper functions for math operations"
      :author "Paula Gearon" }
    cljs.math)

(def
  ^{:doc "Constant for Euler's number e, the base for natural logarithms.
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/E"
    :added "1.11.10"
    :tag number
    :const true} E Math/E)

(def
  ^{:doc "Constant for pi, the ratio of the circumference of a circle to its diameter.
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/PI"
    :added "1.11.10"
    :tag number
    :const true} PI Math/PI)

(def
  ^{:doc "Constant used to convert an angular value in degrees to the equivalent in radians"
    :private true
    :added "1.11.10"
    :const true} DEGREES-TO-RADIANS 0.017453292519943295)

(def
  ^{:doc "Constant used to convert an angular value in radians to the equivalent in degrees"
    :private true
    :added "1.11.10"
    :const true} RADIANS-TO-DEGREES 57.29577951308232)

(def ^{:private true :const true} TWO-TO-THE-52 0x10000000000000)

(def ^{:private true :const true} SIGNIFICAND-WIDTH32 21)

(def ^{:private true :const true} EXP-BIAS 1023)

(def ^{:private true :const true} EXP-BITMASK32 0x7FF00000)

(def ^{:private true :const true} EXP-MAX EXP-BIAS)

(def ^{:private true :const true} EXP-MIN -1022)

;; js/Number.MIN_VALUE has a bit representation of 0x0000000000000001

;; js/Number.MAX_VALUE has a bit representation of 0x7FEFFFFFFFFFFFFF

(defn- get-little-endian
  "Tests the platform for endianness. Returns true when little-endian, false otherwise."
  []
  (let [a (js/ArrayBuffer. 4)
        i (js/Uint32Array. a)
        b (js/Uint8Array. a)]
    (aset i 0 0x33221100)
    (zero? (aget b 0))))

(defonce ^:private little-endian? (get-little-endian))

;; the HI and LO labels are terse to reflect the C macros they represent
(def ^{:private true :doc "offset of hi integers in 64-bit values"} HI (if little-endian? 1 0))

(def ^{:private true :doc "offset of hi integers in 64-bit values"} LO (- 1 HI))

(def ^{:private true :const true} INT32-MASK 0xFFFFFFFF)

(def ^{:private true :const true} INT32-NON-SIGN-BIT 0x80000000)

(def ^{:private true :const true} INT32-NON-SIGN-BITS 0x7FFFFFFF)

(defn u<
  {:doc "unsigned less-than comparator for 32-bit values"
   :private true}
  [a b]
  ;; compare the top nybble
  (let [ab (unsigned-bit-shift-right a 28)
        bb (unsigned-bit-shift-right b 28)]
    (or (< ab bb)  ;; if the top nybble of a is less then the whole value is less
        (and (== ab bb)  ;; if the top nybble is equal then compare the remaining bits of both
             (< (bit-and a 0x0fffffff) (bit-and b 0x0fffffff))))))

(defn ^number sin
  {:doc "Returns the sine of an angle.
  If a is ##NaN, ##-Inf, ##Inf => ##NaN
  If a is zero => zero with the same sign as a
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sin"
   :added "1.11.10"}
  [a] (Math/sin a))

(defn ^number cos
  {:doc "Returns the cosine of an angle.
  If a is ##NaN, ##-Inf, ##Inf => ##NaN
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cos"
   :added "1.11.10"}
  [a] (Math/cos a))

(defn ^number tan
  {:doc "Returns the tangent of an angle.
  If a is ##NaN, ##-Inf, ##Inf => ##NaN
  If a is zero => zero with the same sign as a
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/tan"
   :added "1.11.10"}
  [a] (Math/tan a))

(defn ^number asin
  {:doc "Returns the arc sine of an angle, in the range -pi/2 to pi/2.
  If a is ##NaN or |a|>1 => ##NaN
  If a is zero => zero with the same sign as a
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/asin"
   :added "1.11.10"}
  [a] (Math/asin a))

(defn ^number acos
  {:doc "Returns the arc cosine of a, in the range 0.0 to pi.
  If a is ##NaN or |a|>1 => ##NaN
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/acos"
   :added "1.11.10"}
  [a] (Math/acos a))

(defn ^number atan
  {:doc "Returns the arc tangent of a, in the range of -pi/2 to pi/2.
  If a is ##NaN => ##NaN
  If a is zero => zero with the same sign as a
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/atan"
   :added "1.11.10"}
  [a] (Math/atan a))

(defn ^number to-radians
  {:doc "Converts an angle in degrees to an approximate equivalent angle in radians.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#toRadians-double-"
   :added "1.11.10"}
  [deg]
  (* deg DEGREES-TO-RADIANS))

(defn ^number to-degrees
  {:doc "Converts an angle in radians to an approximate equivalent angle in degrees.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#toDegrees-double-"
   :added "1.11.10"}
  [r]
  (* r RADIANS-TO-DEGREES))

(defn ^number exp
  {:doc "Returns Euler's number e raised to the power of a.
  If a is ##NaN => ##NaN
  If a is ##Inf => ##Inf
  If a is ##-Inf => +0.0
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/exp"
   :added "1.11.10"}
  [a] (Math/exp a))

(defn ^number log
  {:doc "Returns the natural logarithm (base e) of a.
  If a is ##NaN or negative => ##NaN
  If a is ##Inf => ##Inf
  If a is zero => ##-Inf
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log"
   :added "1.11.10"}
  [a] (Math/log a))

(defn ^number log10
  {:doc "Returns the logarithm (base 10) of a.
  If a is ##NaN or negative => ##NaN
  If a is ##Inf => ##Inf
  If a is zero => ##-Inf
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log10"
   :added "1.11.10"}
  [a] (Math/log10 a))

(defn ^number sqrt
  {:doc "Returns the positive square root of a.
  If a is ##NaN or negative => ##NaN
  If a is ##Inf => ##Inf
  If a is zero => a
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sqrt"
   :added "1.11.10"}
  [a] (Math/sqrt a))

(defn ^number cbrt
  {:doc "Returns the cube root of a.
  If a is ##NaN => ##NaN
  If a is ##Inf or ##-Inf => a
  If a is zero => zero with sign matching a
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cbrt"
   :added "1.11.10"}
  [a] (Math/cbrt a))

(defn ^number fabs
  {:doc "Internal function to convert doubles to absolute values.
  This duplicates the C implementations in Java, in case there is are corner-case differences."
   :private true
   :added "1.11.10"}
  [x]
  ;; create a buffer large enough for a double
  (let [a (js/ArrayBuffer. 8)
        ;; represent the buffer as a double array
        d (js/Float64Array. a)
        ;; represent the buffer as 32 bit ints
        i (js/Uint32Array. a)
        hi (if little-endian? 1 0)]
    ;; insert the double value into the buffer
    (aset d 0 x)
    ;; update the sign bit
    (aset i hi (bit-and (aget i hi) INT32-NON-SIGN-BITS))
    ;; return the new double
    (aget d 0)))

(def ^{:private true} Zero
  ;; a buffer that can hold a pair of 64 bit doubles
  (let [a (js/ArrayBuffer. 16)
        ;; represent the buffer as a 2 double array
        d (js/Float64Array. a)
        ;; represent the buffer as an array of bytes
        b (js/Uint8Array. a)]
    ;; initialize both doubles to 0.0
    (aset d 0 0.0)
    (aset d 1 0.0)
    ;; update the sign bit on the second double
    (aset b (if little-endian? 15 8) -0x80)
    ;; save the array of 2 doubles [0.0, -0.0]
    d))

(def ^{:private true :const true} xpos 0)
(def ^{:private true :const true} ypos 1)
(def ^{:private true} HI-x (+ (* 2 xpos) HI))
(def ^{:private true} LO-x (+ (* 2 xpos) LO))
(def ^{:private true} HI-y (+ (* 2 ypos) HI))
(def ^{:private true} LO-y (+ (* 2 ypos) LO))

(defn ^number ilogb
  {:doc "internal function for ilogb(x)"
   :private true}
  [hx lx]
  (if (< hx 0x00100000) ;; subnormal
    (let [hx-zero? (zero? hx)
          start-ix (if hx-zero? -1043 -1022)
          start-i (if hx-zero? lx (bit-shift-left hx 11))]
      (loop [ix start-ix i start-i]
        (if-not (> i 0)
          ix
          (recur (dec ix) (bit-shift-left i 1)))))
    (- (bit-shift-right hx 20) 1023)))

(defn ^number setup-hl
  {:doc "internal function to setup and align integer words"
   :private true}
  [i h l]
  (if (>= i -1022)
    [(bit-or 0x00100000 (bit-and 0x000fffff h)) l]
    (let [n (- -1022 i)]
      (if (<= n 31)
        [(bit-or (bit-shift-left h n) (unsigned-bit-shift-right l (- 32 n))) (bit-shift-left l n)]
        [(bit-shift-left l (- n 32)) 0]))))

(defn ^number IEEE-fmod
  {:doc "Return x mod y in exact arithmetic. Method: shift and subtract.
  Reimplements __ieee754_fmod from the JDK.
  Ported from: https://github.com/openjdk/jdk/blob/master/src/java.base/share/native/libfdlibm/e_fmod.c
  bit-shift-left and bit-shift-right convert numbers to signed 32-bit
  Fortunately the values that are shifted are expected to be 32 bit signed."
  :private true}
  [x y]
  ;; return exception values
  (if (or (zero? y) ^boolean (js/isNaN y) (not ^boolean (js/isFinite x)))
    ##NaN

    ;; create a buffer large enough for 2 doubles
    (let [a (js/ArrayBuffer. 16)
          ;; represent the buffer as a double array
          d (js/Float64Array. a)
          ;; represent the buffer as 32 bit ints
          i (js/Uint32Array. a)
          ;; set the doubles to x and y
          _ (aset d xpos x)
          _ (aset d ypos y)
          hx (aget i HI-x)
          lx (aget i LO-x)
          hy (aget i HI-y)
          ly (aget i LO-y)
          sx (bit-and hx INT32-NON-SIGN-BIT) ;; capture the sign of x
          hx (bit-and hx INT32-NON-SIGN-BITS) ;; set x to |x|
          hy (bit-and hy INT32-NON-SIGN-BITS) ;; set y to |y|
          hx<=hy (<= hx hy)]
      (cond
        ;; additional exception values
        (and hx<=hy (or (< hx hy) (< lx ly))) x ;; |x|<|y| return x
        (and hx<=hy (== lx ly)) (aget Zero (unsigned-bit-shift-right sx 31)) ;; |x|=|y| return x*0

        :default
        ;; determine ix = ilogb(x), iy = ilogb(y)
        (try
          (let [ix (ilogb hx lx)
                iy (ilogb hy ly)
                ;; set up {hx,lx}, {hy,ly} and align y to x
                [hx lx] (setup-hl ix hx lx)
                [hy ly] (setup-hl iy hy ly)
                ;; fix point fmod
                [hx lx] (loop [n (- ix iy) hx hx lx lx]
                          (if (zero? n)
                            [hx lx]
                            (let [hz (if (u< lx ly) (- hx hy 1) (- hx hy))
                                  lz (- lx ly)
                                  [hx lx] (if (< hz 0)
                                            [(+ hx hx (unsigned-bit-shift-right lx 31)) (+ lx lx)]
                                            (if (zero? (bit-or hz lz))
                                              (throw (ex-info "Signed zero" {:zero true}))
                                              [(+ hz hz (unsigned-bit-shift-right lz 31)) (+ lz lz)]))]
                              (recur (dec n) (bit-and INT32-MASK hx) (bit-and INT32-MASK lx)))))
                hz (if (u< lx ly) (- hx hy 1) (- hx hy))
                lz (- lx ly)
                [hx lx] (if (>= hz 0) [hz lz] [hx lx])

                _ (when (zero? (bit-or hx lx))
                    (throw (ex-info "Signed zero" {:zero true})))
                ;; convert back to floating value and restore the sign
                [hx lx iy] (loop [hx hx lx lx iy iy]
                             (if-not (< hx 0x00100000)
                               [hx lx iy]
                               (recur (+ hx hx (unsigned-bit-shift-right lx 31)) (+ lx lx) (dec iy))))]
            ;; use these high and low ints to update the double and return it
            (if (>= iy -1022)
              (let [hx (bit-or (- hx 0x00100000) (bit-shift-left (+ iy 1023) 20))]
                (aset i HI-x (bit-or hx sx))
                (aset i LO-x lx)
                (aget d xpos))
              (let [n (- -1022 iy)
                    [hx lx] (cond
                              (<= n 20) [(bit-shift-right hx n)
                                         (bit-or (unsigned-bit-shift-right lx n) (bit-shift-left hx (- 32 n)))]
                              (<= n 31) [sx
                                         (bit-or (bit-shift-left hx (- 32 n)) (unsigned-bit-shift-right lx n))]
                              :default [sx (bit-shift-right hx (- n 32))])]
                (aset i HI-x (bit-or hx sx))
                (aset i LO-x lx)
                (* (aget d xpos) 1.0))))
          (catch :default _ (aget Zero (unsigned-bit-shift-right sx 31))))))))

(defn ^number IEEE-remainder
  {:doc "Returns the remainder per IEEE 754 such that
    remainder = dividend - divisor * n
   where n is the integer closest to the exact value of dividend / divisor.
   If two integers are equally close, then n is the even one.
   If the remainder is zero, sign will match dividend.
   If dividend or divisor is ##NaN, or dividend is ##Inf or ##-Inf, or divisor is zero => ##NaN
   If dividend is finite and divisor is infinite => dividend

   Method: based on fmod return x-[x/p]chopped*p exactlp.
   Ported from: https://github.com/openjdk/jdk/blob/master/src/java.base/share/native/libfdlibm/e_remainder.c
   See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#IEEEremainder-double-double-"
    :added "1.11.10"}
  [dividend divisor]
  ;; check for exception values
  (cond
    (zero? divisor) ##NaN
    ^boolean (js/isNaN divisor) ##NaN
    ;; check if dividend is ##Inf ##-Inf or ##NaN
    ^boolean (js/isNaN dividend) ##NaN
    (not ^boolean (js/isFinite dividend)) ##NaN
    ;; dividend is finish, check if divisor is infinite
    (not ^boolean (js/isFinite divisor)) dividend

    :default
    ;; create a buffer large enough for 2 doubles
    (let [a (js/ArrayBuffer. 16)
          ;; represent the buffer as a double array
          d (js/Float64Array. a)
          ;; represent the buffer as 32 bit ints
          i (js/Uint32Array. a)]
      (aset d 0 dividend)
      (aset d 1 divisor)
      ;; x gets the dividend high and low ints
      (let [hx (aget i HI)
            lx (aget i LO)
            ;; p gets the divisor high and low ints
            hp (aget i (+ HI 2))
            lp (aget i (+ LO 2))
            ;; sx is the sign bit
            sx (bit-and hx INT32-NON-SIGN-BIT)
            ;; strip the sign bit from hp and hx
            hp (bit-and hp INT32-NON-SIGN-BITS)
            hx (bit-and hx INT32-NON-SIGN-BITS)

            ;;make x < 2p
            dividend (if (<= hp 0x7FDFFFFF) (IEEE-fmod dividend (+ divisor divisor)) dividend)]
        (if (zero? (bit-or (- hx hp) (- lx lp)))
          (* 0.0 dividend)
          ;; convert dividend and divisor to absolute values. 
          (let [dividend (Math/abs dividend)
                divisor (Math/abs divisor)
                ;; reduce dividend within range of the divisor
                dividend (if (< hp 0x00200000)
                           ;; smaller divisor compare 2*dividend to the divisor
                           (if (> (+ dividend dividend) divisor)
                             (let [dividend (- dividend divisor)] ;; reduce the dividend
                               (if (>= (+ dividend dividend) divisor) ;; 2*dividend still larger
                                 (- dividend divisor) ;; reduce again
                                 dividend))
                             dividend)
                           ;; compare dividend to half the divisor
                           (let [divisor-half (* 0.5 divisor)]
                             (if (> dividend divisor-half)
                               (let [dividend (- dividend divisor)] ;; reduce the dividend
                                 (if (>= dividend divisor-half) ;; still larger than half divisor
                                   (- dividend divisor) ;; reduce again
                                   dividend))
                               dividend)))]
            ;; update the buffer with the new dividend value
            (aset d 0 dividend)
            ;; calculate a new hi int for the dividend using the saved sign bit
            (let [hx (bit-xor (aget i HI) sx)]
              ;; set the dividend with this new sign bit
              (aset i HI hx)
              ;; retrieve the updated dividend
              (aget d 0))))))))

(defn ^number ceil
  {:doc "Returns the smallest double greater than or equal to a, and equal to a
  mathematical integer.
  If a is ##NaN or ##Inf or ##-Inf or already equal to an integer => a
  Note that if a is `nil` then an exception will be thrown. This matches Clojure, rather than js/Math.ceil
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil"
   :added "1.11.10"}
  [a]
  (if (some? a)
    (Math/ceil a)
    (throw (ex-info "Unexpected Null passed to ceil" {:fn "ceil"}))))

(defn ^number floor
  {:doc "Returns the largest double less than or equal to a, and equal to a
  mathematical integer.
  If a is ##NaN or ##Inf or ##-Inf or already equal to an integer => a
  If a is less than zero but greater than -1.0 => -0.0
  Note that if a is `nil` then an exception will be thrown. This matches Clojure, rather than js/Math.floor
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor"
   :added "1.11.10"}
  [a]
  (if (some? a)
    (Math/floor a)
    (throw (ex-info "Unexpected Null passed to floor" {:fn "floor"}))))

(defn ^number copy-sign
  {:doc "Returns a double with the magnitude of the first argument and the sign of
  the second.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#copySign-double-double-"
   :added "1.11.10"}
  [magnitude sign]
  ;; create a buffer large enough for 2 doubles
  (let [a (js/ArrayBuffer. 16)
        ;; represent the buffer as a double array
        d (js/Float64Array. a)
        ;; represent the buffer as bytes
        b (js/Uint8Array. a)
        ;; find the offset of the byte that holds the sign bit
        sbyte (if little-endian? 7 0)]
    ;; the first double holds the magnitude, the second holds the sign value
    (aset d 0 magnitude)
    (aset d 1 sign)
    ;; read the sign bit from the sign value
    (let [sign-sbyte (bit-and 0x80 (aget b (+ 8 sbyte)))
          ;; read all the bits that aren't the sign bit in the same byte of the magnitude
          mag-sbyte (bit-and 0x7F (aget b sbyte))]
      ;; combine the sign bit from the sign value and the non-sign-bits from the magnitude value
      ;; write it back into the byte in the magnitude
      (aset b sbyte (bit-or sign-sbyte mag-sbyte))
      ;; retrieve the full magnitude value with the updated byte
      (aget d 0))))

(defn ^number rint
  {:doc "Returns the double closest to a and equal to a mathematical integer.
  If two values are equally close, return the even one.
  If a is ##NaN or ##Inf or ##-Inf or zero => a
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#rint-double-"
   :added "1.11.10"}
  [a]
  (let [sign (copy-sign 1.0, a)
        a (Math/abs a)
        a (if (< a TWO-TO-THE-52)
            (- (+ TWO-TO-THE-52 a) TWO-TO-THE-52) a)]
    (* sign a)))

(defn ^number atan2
  {:doc "Returns the angle theta from the conversion of rectangular coordinates (x, y) to polar coordinates (r, theta).
  Computes the phase theta by computing an arc tangent of y/x in the range of -pi to pi.
  For more details on special cases, see:
  https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/atan"
   :added "1.11.10"}
  [y x] (Math/atan2 y x))

(defn ^number pow
  {:doc "Returns the value of a raised to the power of b.
  For more details on special cases, see:
  https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/pow"
   :added "1.11.10"}
  [a b] (Math/pow a b))

(defn ^number round
  {:doc "Returns the closest long to a. If equally close to two values, return the one
  closer to ##Inf.
  If a is ##NaN => 0
  If a is ##-Inf => js/Number.MIN_SAFE_INTEGER
  If a is ##Inf => js/Number.MAX_SAFE_INTEGER
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round"
   :added "1.11.10"}
  [a]
  (cond
    ^boolean (js/isNaN a) 0
    ^boolean (js/isFinite a) (Math/round a)
    (== ##Inf a) js/Number.MAX_SAFE_INTEGER
    :default js/Number.MIN_SAFE_INTEGER))

(defn ^number random
  {:doc "Returns a positive double between 0.0 and 1.0, chosen pseudorandomly with
  approximately random distribution. Not cryptographically secure. The seed is chosen internally
  and cannot be selected.
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random"
   :added "1.11.10"}
  [] (Math/random))

(defn ^number add-exact
  {:doc "Returns the sum of x and y, throws an exception on overflow. "
   :added "1.11.10"}
  [x y]
  (let [r (clojure.core/+ x y)]
    (if (or (> r js/Number.MAX_SAFE_INTEGER) (< r js/Number.MIN_SAFE_INTEGER))
      (throw (ex-info "Integer overflow" {:fn "add-exact"}))
      r)))

(defn ^number subtract-exact
  {:doc "Returns the difference of x and y, throws ArithmeticException on overflow. "
   :added "1.11.10"}
  [x y]
  (let [r (- x y)]
    (if (or (> r js/Number.MAX_SAFE_INTEGER) (< r js/Number.MIN_SAFE_INTEGER))
      (throw (ex-info "Integer overflow" {:fn "subtract-exact"}))
      r)))

(defn ^number multiply-exact
  {:doc "Returns the product of x and y, throws ArithmeticException on overflow. "
   :added "1.11.10"}
  [x y]
  (let [r (* x y)]
    (if (or (> r js/Number.MAX_SAFE_INTEGER) (< r js/Number.MIN_SAFE_INTEGER))
      (throw (ex-info "Integer overflow" {:fn "multiply-exact"}))
      r)))

(defn ^number increment-exact
  {:doc "Returns a incremented by 1, throws ArithmeticException on overflow."
   :added "1.11.10"}
  [a]
  (if (or (>= a js/Number.MAX_SAFE_INTEGER) (< a js/Number.MIN_SAFE_INTEGER))
    (throw (ex-info "Integer overflow" {:fn "increment-exact"}))
    (inc a)))

(defn ^number decrement-exact
  {:doc "Returns a decremented by 1, throws ArithmeticException on overflow. "
   :added "1.11.10"}
  [a]
  (if (or (<= a js/Number.MIN_SAFE_INTEGER) (> a js/Number.MAX_SAFE_INTEGER))
    (throw (ex-info "Integer overflow" {:fn "decrement-exact"}))
    (dec a)))

(defn ^number negate-exact
  {:doc "Returns the negation of a, throws ArithmeticException on overflow. "
   :added "1.11.10"}
  [a]
  (if (or (> a js/Number.MAX_SAFE_INTEGER) (< a js/Number.MIN_SAFE_INTEGER))
    (throw (ex-info "Integer overflow" {:fn "negate-exact"}))
    (- a)))

(defn- xor
  [^boolean a ^boolean b]
  (or (and a (not b)) (and (not a) b)))

(defn ^number floor-div
  {:doc "Integer division that rounds to negative infinity (as opposed to zero).
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floorDiv-long-long-"
   :added "1.11.10"}
  [x y]
  (if-not (and ^boolean (js/Number.isSafeInteger x) ^boolean (js/Number.isSafeInteger y))
    (throw (ex-info "floor-div called with non-safe-integer arguments"
                    {:x-int? (js/Number.isSafeInteger x) :y-int? (js/Number.isSafeInteger y)}))
    (let [r (long (/ x y))]
      (if (and (xor (< x 0) (< y 0)) (not (== (* r y) x)))
        (dec r)
        r))))

(defn ^number floor-mod
  {:doc "Integer modulus x - (floorDiv(x, y) * y). Sign matches y and is in the
  range -|y| < r < |y|.
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floorMod-long-long-"
   :added "1.11.10"}
  [x y]
  (if-not (and ^boolean (js/Number.isSafeInteger x) ^boolean (js/Number.isSafeInteger y))
    (throw (ex-info "floor-mod called with non-safe-integer arguments"
                    {:x-int? (js/Number.isSafeInteger x) :y-int? (js/Number.isSafeInteger y)}))
    ;; this avoids using floor-div to keep within the safe integer range
    (let [r (long (/ x y))]
      (if (and (xor (< x 0) (< y 0)) (not (== (* r y) x)))
        (- x (* y r) (- y))
        (- x (* y r))))))

(defn ^number get-exponent
  {:doc "Returns the exponent of d.
  If d is ##NaN, ##Inf, ##-Inf => max_Float64_exponent + 1
  If d is zero or subnormal => min_Float64_exponent - 1
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#getExponent-double-"
   :added "1.11.10"}
  [d]
  (cond
    (or ^boolean (js/isNaN d) (not ^boolean (js/isFinite d))) (inc EXP-MAX)
    (zero? d) (dec EXP-MIN)
    :default (let [a (js/ArrayBuffer. 8)
                   f (js/Float64Array. a)
                   i (js/Uint32Array. a)
                   hi (if little-endian? 1 0)]
               (aset f 0 d)
               (- (bit-shift-right (bit-and (aget i hi) EXP-BITMASK32) (dec SIGNIFICAND-WIDTH32)) EXP-BIAS))))

(defn ^number hi-lo->double
  {:doc "Converts a pair of 32 bit integers into an IEEE-754 64 bit floating point number.
  h is the high 32 bits, l is the low 32 bits."
   :private true}
  [h l]
  (let [a (js/ArrayBuffer. 8)
        f (js/Float64Array. a)
        i (js/Uint32Array. a)]
    (aset i LO l)
    (aset i HI h)
    (aget f 0)))

(defn ^number power-of-two
  {:doc "returns a floating point power of two in the normal range"
   :private true}
  [n]
  (assert (and (>= n EXP-MIN) (<= n EXP-MAX)))
  (hi-lo->double
   (bit-and (bit-shift-left (+ n EXP-BIAS) (dec SIGNIFICAND-WIDTH32)) EXP-BITMASK32) 0))

(defn ^number ulp
  {:doc "Returns the size of an ulp (unit in last place) for d.
  If d is ##NaN => ##NaN
  If d is ##Inf or ##-Inf => ##Inf
  If d is zero => Number/MIN_VALUE
  If d is +/- Number/MAX_VALUE => 2^971
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#ulp-double-"
   :added "1.11.10"}
  [d]
  (cond
    ^boolean (js/isNaN d) d
    ^boolean (js/isFinite d)
    (let [e (get-exponent d)]
      (case e
        1024 (Math/abs d)  ;; EXP-MAX + 1
        -1023 js/Number.MIN_VALUE  ;; EXP-MIN - 1
        (let [e (- e (+ 31 SIGNIFICAND-WIDTH32))]  ;; SIGNIFICAND_WIDTH64 -1
          (if (>= e EXP-MIN)
            (power-of-two e)
            (let [shift (- e (- EXP-MIN 31 SIGNIFICAND-WIDTH32))]
              (if (< shift 32)
                (hi-lo->double 0 (bit-shift-left 1 shift))
                (hi-lo->double (bit-shift-left 1 (- shift 32)) 0)))))))
    :default ##Inf))

(defn ^number signum
  {:doc "Returns the signum function of d - zero for zero, 1.0 if >0, -1.0 if <0.
  If d is ##NaN => ##NaN
  If d is ##Inf or ##-Inf => sign of d
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#signum-double-"
   :added "1.11.10"}
  [d]
  (if (or (zero? d) ^boolean (js/isNaN d))
    d
    (copy-sign 1.0 d)))

(defn ^number sinh
  {:doc "Returns the hyperbolic sine of x, (e^x - e^-x)/2.
  If x is ##NaN => ##NaN
  If x is ##Inf or ##-Inf or zero => x
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sinh"
   :added "1.11.10"}
  [x] (Math/sinh x))

(defn ^number cosh
  {:doc "Returns the hyperbolic cosine of x, (e^x + e^-x)/2.
  If x is ##NaN => ##NaN
  If x is ##Inf or ##-Inf => ##Inf
  If x is zero => 1.0
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cosh"
   :added "1.11.10"}
  [x] (Math/cosh x))

(defn ^number tanh
  {:doc "Returns the hyperbolic tangent of x, sinh(x)/cosh(x).
  If x is ##NaN => ##NaN
  If x is zero => zero, with same sign
  If x is ##Inf => +1.0
  If x is ##-Inf => -1.0
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/tanh"
   :added "1.11.10"}
  [x] (Math/tanh x))

(defn ^number hypot
  {:doc "Returns sqrt(x^2 + y^2) without intermediate underflow or overflow.
  If x or y is ##Inf or ##-Inf => ##Inf
  If x or y is ##NaN and neither is ##Inf or ##-Inf => ##NaN
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/hypot"
   :added "1.11.10"}
  [x y] (Math/hypot x y))

(defn ^number expm1
  {:doc "Returns e^x - 1. Near 0, expm1(x)+1 is more accurate to e^x than exp(x).
  If x is ##NaN => ##NaN
  If x is ##Inf => #Inf
  If x is ##-Inf => -1.0
  If x is zero => x
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/expm1"
   :added "1.11.10"}
  [x] (Math/expm1 x))

(defn ^number log1p
  {:doc "Returns ln(1+x). For small values of x, log1p(x) is more accurate than
  log(1.0+x).
  If x is ##NaN or ##-Inf or < -1 => ##NaN
  If x is -1 => ##-Inf
  If x is ##Inf => ##Inf
  See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log1p"
   :added "1.11.10"}
  [x] (Math/log1p x))

(defn ^number add64
  {:doc "Takes the high and low words for 2 different 64 bit integers, and adds them.
  This handles overflow from the low-order words into the high order words."
   :private true}
  [hx lx hy ly]
  (let [sx (unsigned-bit-shift-right (bit-and lx INT32-NON-SIGN-BIT) 31)
        sy (unsigned-bit-shift-right (bit-and ly INT32-NON-SIGN-BIT) 31)
        lr (+ (bit-and INT32-NON-SIGN-BITS lx) (bit-and INT32-NON-SIGN-BITS ly))
        c31 (unsigned-bit-shift-right (bit-and lr INT32-NON-SIGN-BIT) 31)
        b31 (+ sx sy c31)
        lr (bit-or (bit-and lr INT32-NON-SIGN-BITS) (bit-shift-left b31 31))
        c32 (bit-shift-right b31 1)
        hr (bit-and INT32-MASK (+ hx hy c32))]
    [hr lr]))

(defn ^number next-after
  {:doc "Returns the adjacent floating point number to start in the direction of
  the second argument. If the arguments are equal, the second is returned.
  If either arg is #NaN => #NaN
  If both arguments are signed zeros => direction
  If start is +-Number/MIN_VALUE and direction would cause a smaller magnitude
    => zero with sign matching start
  If start is ##Inf or ##-Inf and direction would cause a smaller magnitude
    => Number/MAX_VALUE with same sign as start
  If start is equal to +=Number/MAX_VALUE and direction would cause a larger magnitude
    => ##Inf or ##-Inf with sign matching start
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextAfter-double-double-"
   :added "1.11.10"}
  [start direction]
  ; Branch to descending case first as it is more costly than ascending
  ; case due to start != 0.0f conditional.
  (let [a (js/ArrayBuffer. 8)
        f (js/Float64Array. a)
        i (js/Uint32Array. a)]
    (cond
      (> start direction) (if-not (zero? start)
                            (let [_ (aset f 0 start)
                                  ht (aget i HI)
                                  lt (aget i LO)
                                  ;; ht&lt != 0 since start != 0.0
                                  ;; So long as the top bit is not set, then whole number is > 0
                                  [hr lr] (if (zero? (bit-and ht INT32-NON-SIGN-BIT))
                                            (add64 ht lt 0xFFFFFFFF 0xFFFFFFFF)
                                            (add64 ht lt 0 1))]
                              (aset i HI hr)
                              (aset i LO lr)
                              (aget f 0))
                            ;; start == 0.0 && direction < 0.0
                            (- js/Number.MIN_VALUE))
      ;; Add +0.0 to get rid of a -0.0 (+0.0 + -0.0 => +0.0)
      ;; then bitwise convert start to integer
      (< start direction) (let [_ (aset f 0 (+ start 0.0))
                                ht (aget i HI)
                                lt (aget i LO)
                                [hr lr] (if (zero? (bit-and ht INT32-NON-SIGN-BIT))
                                          (add64 ht lt 0 1)
                                          (add64 ht lt 0xFFFFFFFF 0xFFFFFFFF))]
                            (aset i HI hr)
                            (aset i LO lr)
                            (aget f 0))
      (== start direction) direction
      :default (+ start direction))))  ;; isNaN(start) || isNaN(direction)

(defn ^number next-up
  {:doc "Returns the adjacent double of d in the direction of ##Inf.
  If d is ##NaN => ##NaN
  If d is ##Inf => ##Inf
  If d is zero => Number/MIN_VALUE
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextUp-double-"
   :added "1.11.10"}
  [d]
  ;; Use a single conditional and handle the likely cases first
  (if (< d js/Number.POSITIVE_INFINITY)
    (let [a (js/ArrayBuffer. 8)
          f (js/Float64Array. a)
          i (js/Uint32Array. a)
          ;; Add +0.0 to get rid of a -0.0 (+0.0 + -0.0 => +0.0)
          _ (aset f 0 (+ d 0.0))
          ht (aget i HI)
          lt (aget i LO)
          [hr lr] (if (zero? (bit-and ht INT32-NON-SIGN-BIT))
                    (add64 ht lt 0 1)
                    (add64 ht lt 0xFFFFFFFF 0xFFFFFFFF))]
      (aset i HI hr)
      (aset i LO lr)
      (aget f 0))
    ;; d is NaN or +Infinity
    d))

(defn ^number next-down
  {:doc "Returns the adjacent double of d in the direction of ##-Inf.
  If d is ##NaN => ##NaN
  If d is ##Inf => Number/MAX_VALUE
  If d is zero => -Number/MIN_VALUE
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextDown-double-"
   :added "1.11.10"}
  [d]
  (cond
    (or ^boolean (js/isNaN d) (== ##-Inf d)) d
    (zero? d) (- js/Number.MIN_VALUE)
    :default
    (let [a (js/ArrayBuffer. 8)
          f (js/Float64Array. a)
          i (js/Uint32Array. a)
          _ (aset f 0 d)
          ht (aget i HI)
          lt (aget i LO)
          [hr lr] (if (> d 0)
                    (add64 ht lt 0xFFFFFFFF 0xFFFFFFFF)
                    (add64 ht lt 0 1))]
      (aset i HI hr)
      (aset i LO lr)
      (aget f 0))))

(def ^:private MAX_SCALE (+ EXP-MAX (- EXP-MIN) SIGNIFICAND-WIDTH32 32 1))

(def ^:private two-to-the-double-scale-up (power-of-two 512))

(def ^:private two-to-the-double-scale-down (power-of-two -512))

(defn ^number scalb
  {:doc "Returns d * 2^scaleFactor, scaling by a factor of 2. If the exponent
  is between min_Float64_exponent and max_Float64_exponent.
  scaleFactor is an integer
  If d is ##NaN => ##NaN
  If d is ##Inf or ##-Inf => ##Inf or ##-Inf respectively
  If d is zero => zero of same sign as d
  See: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextDown-double-"
   :added "1.11.10"}
  [d scaleFactor]
  (let [[scale-factor
         scale-increment
         exp-delta] (if (< scaleFactor 0)
                      [(Math/max scaleFactor (- MAX_SCALE)) -512 two-to-the-double-scale-down]
                      [(Math/min scaleFactor MAX_SCALE) 512 two-to-the-double-scale-up])
        ;; Calculate (scaleFactor % +/-512), 512 = 2^9
        ;; technique from "Hacker's Delight" section 10-2
        t (unsigned-bit-shift-right (bit-shift-right scale-factor 8) 23)
        exp-adjust (- (bit-and (+ scale-factor t) 511) t)]
    (loop [d (* d (power-of-two exp-adjust)) scale-factor (- scale-factor exp-adjust)]
      (if (zero? scale-factor)
        d
        (recur (* d exp-delta) (- scale-factor scale-increment))))))
