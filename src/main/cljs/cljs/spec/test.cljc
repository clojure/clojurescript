;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.spec.test
  (:require
    [cljs.util :refer [distinct-by]]
    [cljs.analyzer :as ana]
    [cljs.analyzer.api :as ana-api]
    [cljs.spec :as s]
    [cljs.spec.impl.gen :as gen]))

(defn- collectionize
  [x]
  (if (symbol? x)
    (list x)
    x))

(defmacro enumerate-namespace
  "Given a symbol naming an ns, or a collection of such symbols,
returns the set of all symbols naming vars in those nses."
  [[quote ns-sym-or-syms]]
  (let [xs (into #{}
             (mapcat (fn [ns-sym]
                       (->> (vals (ana-api/ns-interns ns-sym))
                         (filter #(not (:macro %)))
                         (map :name)
                         (map
                           (fn [name-sym]
                             (symbol (name ns-sym) (name name-sym)))))))
             (collectionize ns-sym-or-syms))]
    `(quote ~xs)))

(defmacro with-instrument-disabled
  "Disables instrument's checking of calls, within a scope."
  [& body]
  `(binding [*instrument-enabled* nil]
     ~@body))

(defmacro instrument-1
  [s opts]
  (let [v (ana-api/resolve &env s)]
    (when v
      `(let [checked# (instrument-1* ~s (var ~s) ~opts)]
         (when checked# (set! ~s checked#))
         '~(:name v)))))

(defmacro unstrument-1
  [s]
  (let [v (ana-api/resolve &env s)]
    (when v
      `(let [raw# (unstrument-1* ~s (var ~s))]
         (when raw# (set! ~s raw#))
         '~(:name v)))))

(defmacro instrument
  "Instruments the vars named by sym-or-syms, a symbol or collection
of symbols, or all instrumentable vars if sym-or-syms is not
specified.

If a var has an :args fn-spec, sets the var's root binding to a
fn that checks arg conformance (throwing an exception on failure)
before delegating to the original fn.

The opts map can be used to override registered specs, and/or to
replace fn implementations entirely. Opts for symbols not included
in sym-or-syms are ignored. This facilitates sharing a common
options map across many different calls to instrument.

The opts map may have the following keys:

  :spec     a map from var-name symbols to override specs
  :stub     a set of var-name symbols to be replaced by stubs
  :gen      a map from spec names to generator overrides
  :replace  a map from var-name symbols to replacement fns

:spec overrides registered fn-specs with specs your provide. Use
:spec overrides to provide specs for libraries that do not have
them, or to constrain your own use of a fn to a subset of its
spec'ed contract.

:stub replaces a fn with a stub that checks :args, then uses the
:ret spec to generate a return value.

:gen overrides are used only for :stub generation.

:replace replaces a fn with a fn that checks args conformance, then
invokes the fn you provide, enabling arbitrary stubbing and mocking.

:spec can be used in combination with :stub or :replace.

Returns a collection of syms naming the vars instrumented."
  ([]
   `(instrument (instrumentable-syms)))
  ([xs]
   `(instrument ~xs nil))
  ([[quote sym-or-syms] opts]
   `(let [opts# ~opts]
      (reduce
        (fn [ret [_ f]]
          (let [sym (f)]
            (cond-> ret sym (conj sym))))
        []
        (->> (zipmap
                (collectionize ~sym-or-syms)
                ~@(map
                    (fn [sym]
                      `(fn [] (instrument-1 '~sym opts#)))
                    (collectionize ~sym-or-syms)))
           (filter #((instrumentable-syms opts#) (first %)))
           (distinct-by first))))))

(defmacro unstrument
  "Undoes instrument on the vars named by sym-or-syms, specified
as in instrument. With no args, unstruments all instrumented vars.
Returns a collection of syms naming the vars unstrumented."
  ([]
   `(unstrument (map ->sym (keys @instrumented-vars))))
  ([sym-or-syms]
   `(into
      []
      (comp (filter symbol?)
        (map unstrument-1*)
        (remove nil?))
      (collectionize ~sym-or-syms))))

(defmacro run-tests
  "Like run-all-tests, but scoped to specific namespaces, or to
*ns* if no ns-sym are specified."
  ([]
   `(cljs.spec.test/run-tests '~ana/*cljs-ns*))
  ([& ns-syms]
   `(cljs.spec.test/run-var-tests
      (->> #?(:clj  ~(s/speced-vars* ns-syms)
              :cljs ~(cljs.spec$macros/speced-vars* ns-syms))
        (filter (fn [v#] (:args (cljs.spec/get-spec v#))))))))

(defmacro run-all-tests
  "Like clojure.test/run-all-tests, but runs test.check tests
for all speced vars. Prints per-test results to *out*, and
returns a map with :test,:pass,:fail, and :error counts."
  []
  `(cljs.spec.test/run-var-tests #?(:clj  ~(s/speced-vars*)
                                    :cljs ~(cljs.spec$macros/speced-vars*))))
