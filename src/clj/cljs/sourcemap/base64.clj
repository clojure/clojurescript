(ns cljs.sourcemap.base64)

(def chars64 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(def char->int (zipmap chars64 (range 0 64)))
(def int->char (zipmap (range 0 64) chars64))

(defn encode [n]
  (let [e (find int->char n)]
   (if e
     (second e)
     (throw (Error. (str "Must be between 0 and 63: " n))))))

(defn ^Character decode [c]
  (let [e (find char->int c)]
   (if e
     (second e)
     (throw (Error. (str "Not a valid base 64 digit: " c))))))