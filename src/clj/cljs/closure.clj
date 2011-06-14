(ns cljs.closure
  (:use clojure.pprint)
  (:require [clojure.java.shell :as shell]))

(def chars (map char (concat (range 48 57) (range 65 90) (range 97 122))))

(defn random-char []
  (nth chars (.nextInt (java.util.Random.) (count chars))))

(defn random-string [length]
  (apply str (take length (repeatedly random-char))))

(defn optimize [code]
  (let [in (str "/tmp/" (random-string 4))
        out (str in "-compiled.js")]
    (spit in code)
    (shell/sh "java" "-jar" "closure/compiler/compiler.jar" "--js" in "--js_output_file" out)
    (pprint (slurp out))))
