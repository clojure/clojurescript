;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.spec.gen.alpha
  (:refer-clojure :exclude [delay])
  (:require [cljs.core :as c]
            [clojure.string :as string]))

(defmacro dynaload [[quote s]]
  `(cljs.spec.gen.alpha/LazyVar.
     (fn []
       (if (c/exists? ~s)
         ~(vary-meta s assoc :cljs.analyzer/no-resolve true)
         (throw
           (js/Error.
             (str "Var " '~s " does not exist, "
                  (namespace '~s) " never required")))))
     nil))

(defmacro delay
  "given body that returns a generator, returns a
  generator that delegates to that, but delays
  creation until used."
  [& body]
  `(delay-impl (c/delay ~@body)))

(defmacro ^:skip-wiki lazy-combinator
  "Implementation macro, do not call directly."
  [s]
  (let [fqn (symbol "clojure.test.check.generators" (name s))
        doc (str "Lazy loaded version of " fqn)]
    `(let [g# (dynaload '~fqn)]
       (defn ~s
         ~doc
         [& ~'args]
         (apply @g# ~'args)))))

(defmacro ^:skip-wiki lazy-combinators
  "Implementation macro, do not call directly."
  [& syms]
  `(do
     ~@(map
         (fn [s] (list `lazy-combinator s))
         syms)))

(defmacro ^:skip-wiki lazy-prim
  "Implementation macro, do not call directly."
  [s]
  (let [fqn (symbol "clojure.test.check.generators" (name s))
        doc (str "Fn returning " fqn)]
    `(let [g# (dynaload '~fqn)]
       (defn ~s
         ~doc
         [& ~'args]
         @g#))))

(defmacro ^:skip-wiki lazy-prims
  "Implementation macro, do not call directly."
  [& syms]
  `(do
     ~@(map
         (fn [s] (list `lazy-prim s))
         syms)))