;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "A namespace that exists solely to provide a place for \"compiler\"
state that is accessed/maintained by many different components."}
  cljs.env
  #?(:clj (:require [cljs.js-deps :refer (js-dependency-index)]))
  (:refer-clojure :exclude [ensure]))

;; bit of a misnomer, but: an atom containing a map that serves as the bag of
;; state for the compiler, writ large (including analyzer, emitter, and
;; optimization stages). Each namespace has its own local var, to accommodate
;; multiple (lower-level) entry points.  Any state needed by the compiler across
;; multiple applications should be put into this map/atom.  Aside from
;; unfortunate current implementation details (e.g. depending on filesystem
;; state for certain things), the compiler should be idempotent with regard to
;; the environment passed to any entry point.
;;
;; Known slots in the compiler-env map:
;;
;; * :options - the [options] map argument, provided to this fn (defaults to {})
;; * :js-dependency-index - result from calling cljs.js-deps/js-dependency-index
;;   with [options]
;; * :cljs.analyzer/constant-table - map of (currently only keyword) constant
;;   values to fixed ids
;; * :cljs.analyzer/namespaces - map of symbols to "namespace" maps
;; * :cljs.compiler/compiled-cljs - cache of intermediate compilation results
;;   that speeds incremental builds in conjunction with source map generation
;; * :cljs.closure/compiled-cljs - cache from js file path to map of
;;   {:file .. :provides .. :requires ..}
;;
;; Note that this var is functionally private to the compiler, and contains
;; implementation-dependent data.
(def ^:dynamic *compiler* nil)

(defn default-compiler-env
  ([] (default-compiler-env {}))
  ([options]
     (atom (merge {:cljs.analyzer/namespaces {'cljs.user {:name 'cljs.user}}
                   :cljs.analyzer/constant-table {}
                   :options options}
             #?(:clj {:js-dependency-index (js-dependency-index options)})))))

#?(:clj
   (defmacro with-compiler-env
     "Evaluates [body] with [env] bound as the value of the `*compiler*` var in
   this namespace."
     [env & body]
     `(let [env# ~env
            env# (cond
                   (map? env#) (atom env#)
                   (and (instance? clojure.lang.Atom env#)
                        (map? @env#)) env#
                   :default (throw (IllegalArgumentException.
                                     (str "Compiler environment must be a map or atom containing a map, not "
                                       (class env#)))))]
        (binding [*compiler* env#] ~@body))))

#?(:clj
   (defmacro ensure
     [& body]
     `(let [val# *compiler*]
        (if (nil? val#)
          (push-thread-bindings
            (hash-map (var *compiler*) (default-compiler-env))))
        (try
          ~@body
          (finally
            (if (nil? val#)
              (pop-thread-bindings)))))))
