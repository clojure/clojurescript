(ns hello.core
  (:require [hello.foo.bar :as bar]
            [cljs.reader :as reader]))

(defn ^{:export greet} greet [n]
  (str "Hello " n))

(defn ^:export sum [xs]
  (bar/sum xs))

(defn ^:export let-works? [day month year]
  (let [hour (first day)
        minutes (first hour)
        seconds (last hour)]
    (.log js/console "Date: " year month day)
    (str year month day hour minutes seconds)))

(defn bailey [proton neutron electron & comedies]
  (apply + proton neutron electron)
  (map identity comedies))

(defn videotape [& rainbows]
  (map :params rainbows))
