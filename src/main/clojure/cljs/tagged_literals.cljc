;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.tagged-literals
  #?(:clj  (:require [clojure.instant :as inst])
     :cljs (:require [cljs.reader :as reader])))

(defn read-queue
  [form]
  (when-not (vector? form)
    (throw
      #?(:clj  (RuntimeException.
                 "Queue literal expects a vector for its elements.")
         :cljs (js/Error.
                 "Queue literal expects a vector for its elements."))))
  (list 'cljs.core/into 'cljs.core.PersistentQueue.EMPTY form))

#?(:clj
   (defn read-uuid
     [form]
     (when-not (string? form)
       (throw (RuntimeException. "UUID literal expects a string as its representation.")))
     (try
       (java.util.UUID/fromString form)
       (catch Throwable e
         (throw (RuntimeException. (.getMessage e)))))))

#?(:cljs
   (defn read-uuid
     [form]
     (when-not (string? form)
       (throw (js/Error. "UUID literal expects a string as its representation.")))
     (try
       (uuid form)
       (catch :default e
         (throw (js/Error. (. e -message)))))))

#?(:clj
   (defn read-inst
     [form]
     (when-not (string? form)
       (throw (RuntimeException. "Instance literal expects a string for its timestamp.")))
     (try
       (inst/read-instant-date form)
       (catch Throwable e
         (throw (RuntimeException. (.getMessage e)))))))

#?(:cljs
   (defn read-inst
     [form]
     (when-not (string? form)
       (throw (js/Error. "Instance literal expects a string for its timestamp.")))
     (try
       (reader/read-date form)
       (catch :default e
         (throw (js/Error. (. e -message)))))))

(defn valid-js-literal-key? [k]
  (or (string? k)
      (and (keyword? k)
           (nil? (namespace k)))))

(deftype JSValue [val])

(defn read-js
  [form]
  (when-not (or (vector? form) (map? form))
    (throw
      #?(:clj  (RuntimeException.
                 "JavaScript literal must use map or vector notation")
         :cljs (js/Error.
                 "JavaScript literal must use map or vector notation"))))
  (when-not (or (not (map? form))
                (every? valid-js-literal-key? (keys form)))
    (throw
      #?(:clj  (RuntimeException.
                 "JavaScript literal keys must be strings or unqualified keywords")
         :cljs (js/Error.
                 "JavaScript literal keys must be strings or unqualified keywords"))))
  (JSValue. form))

(def ^:dynamic *cljs-data-readers*
  (merge ;; assumes we can read all data_readers
    #?(:clj *data-readers*)
    {'queue read-queue
     'uuid  read-uuid
     'inst  read-inst
     'js    read-js}))
