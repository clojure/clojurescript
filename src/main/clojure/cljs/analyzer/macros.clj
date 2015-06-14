(ns cljs.analyzer.macros
  (:refer-clojure :exclude [binding])
  (:require [cljs.core :refer [binding]]))

(defmacro with-warning-handlers [handlers & body]
  `(binding [cljs.analyzer/*cljs-warning-handlers* ~handlers]
     ~@body))

(defmacro no-warn [& body]
  `(binding [cljs.analyzer/*cljs-warnings*
             (zipmap (keys cljs.analyzer/*cljs-warnings*) (repeat false))]
     ~@body))

(defmacro with-core-macros
  [path & body]
  `(do
     (when (not= cljs.analyzer/*cljs-macros-path* ~path)
       (reset! cljs.analyzer/-cljs-macros-loaded false))
     (binding [cljs.analyzer/*cljs-macros-path* ~path]
       ~@body)))

(defmacro with-core-macros-file
  [path & body]
  `(do
     (when (not= cljs.analyzer/*cljs-macros-path* ~path)
       (reset! cljs.analyzer/-cljs-macros-loaded false))
     (binding [cljs.analyzer/*cljs-macros-path* ~path
               cljs.analyzer/*cljs-macros-is-classpath* false]
       ~@body)))

(defmacro wrapping-errors [env & body]
  `(try
     ~@body
     (catch :default err#
       (if (cljs.analyzer/analysis-error? err#)
         (throw err#)
         (throw (error ~env (.-message err#) err#))))))

(defmacro disallowing-recur [& body]
  `(binding [cljs.analyzer/*recur-frames*
             (cons nil *cljs.analyzer/*recur-frames*)]
     ~@body))

(defmacro allowing-redef [& body]
  `(binding [cljs.analyzer/*allow-redef* true]
     ~@body))