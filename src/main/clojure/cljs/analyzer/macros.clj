;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.macros
  (:refer-clojure :exclude [binding]))

(defmacro with-warning-handlers [handlers & body]
  `(cljs.core/binding [cljs.analyzer/*cljs-warning-handlers* ~handlers]
     ~@body))

(defmacro no-warn [& body]
  `(cljs.core/binding [cljs.analyzer/*cljs-warnings*
                       (zipmap (keys cljs.analyzer/*cljs-warnings*) (repeat false))]
     ~@body))

(defmacro with-core-macros
  [path & body]
  `(do
     (when (not= cljs.analyzer/*cljs-macros-path* ~path)
       (reset! cljs.analyzer/-cljs-macros-loaded false))
     (cljs.core/binding [cljs.analyzer/*cljs-macros-path* ~path]
       ~@body)))

(defmacro with-core-macros-file
  [path & body]
  `(do
     (when (not= cljs.analyzer/*cljs-macros-path* ~path)
       (reset! cljs.analyzer/-cljs-macros-loaded false))
     (cljs.core/binding [cljs.analyzer/*cljs-macros-path* ~path
                         cljs.analyzer/*cljs-macros-is-classpath* false]
       ~@body)))

(defmacro wrapping-errors [env & body]
  `(try
     ~@body
     (catch :default err#
       (cond
         (cljs.analyzer/has-error-data? err#) (throw err#)
         (cljs.analyzer/analysis-error? err#) (throw (ex-info nil (cljs.analyzer/error-data ~env :compilation) err#))
         :else (throw (ex-info nil (cljs.analyzer/error-data ~env :compilation) (cljs.analyzer/error ~env (.-message err#) err#)))))))

(defmacro disallowing-recur [& body]
  `(cljs.core/binding [cljs.analyzer/*recur-frames*
                       (cons nil cljs.analyzer/*recur-frames*)]
     ~@body))

(defmacro allowing-redef [& body]
  `(cljs.core/binding [cljs.analyzer/*allow-redef* true]
     ~@body))

(defmacro disallowing-ns* [& body]
  `(cljs.core/binding [cljs.analyzer/*allow-ns* false] ~@body))
