(ns cljs.tagged-literals
  (:require [clojure.instant :as inst]))

(defn read-queue
  [form]
  (assert (vector? form) "Queue literal expects a vector for its elements.")
  (list 'cljs.core/into 'cljs.core.PersistentQueue.EMPTY form))

(defn read-uuid
  [form]
  (assert (string? form) "UUID literal expects a string as its representation.")
  (try
    (let [uuid (java.util.UUID/fromString form)]
      (list (symbol "UUID.") form))
    (catch Throwable e
      (throw (RuntimeException. (.getMessage e))))))

(defn read-inst
  [form]
  (assert (string? form) "Instance literal expects a string for its timestamp.")
  (try
    (let [^java.util.Date d (inst/read-instant-date form)]
      (list (symbol "js/Date.") (.getTime d)))
    (catch Throwable e
      (throw (RuntimeException. (.getMessage e))))))

(defn valid-js-literal-key? [k]
  (or (string? k)
      (and (keyword? k)
           (nil? (namespace k)))))

(defn read-js
  [form]
  (assert (or (vector? form) (map? form))
    "JavaScript literal must use map or vector notation")
  (assert (or (not (map? form))
              (every? valid-js-literal-key? (keys form)))
    "JavaScript literal keys must be strings or unqualified keywords")
  (if (map? form)
    `(cljs.core/js-obj
       ~@(apply concat (map (fn [[k v]] [(name k) v]) form)))
    `(cljs.core/array ~@form)))

(def ^:dynamic *cljs-data-readers*
  {'queue read-queue
   'uuid  read-uuid
   'inst  read-inst
   'js    read-js})
