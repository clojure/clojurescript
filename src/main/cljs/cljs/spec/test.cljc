;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.spec.test
  (:require
    [cljs.analyzer :as ana]
    [cljs.analyzer.api :as ana-api]
    [cljs.spec :as s]
    [cljs.spec.impl.gen :as gen]))

(defonce ^:private instrumented-vars (atom #{}))

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
  [[quote s] opts]
  (let [v (ana-api/resolve &env s)]
    (when v
      (swap! instrumented-vars conj (:name v))
      `(let [checked# (instrument-1* ~s (var ~s) ~opts)]
         (when checked# (set! ~s checked#))
         '~(:name v)))))

(defmacro unstrument-1
  [[quote s]]
  (let [v (ana-api/resolve &env s)]
    (when v
      (swap! instrumented-vars disj (:name v))
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
   `(instrument '[~@(s/speced-vars)]))
  ([xs]
   `(instrument ~xs nil))
  ([sym-or-syms opts]
   (let [sym-or-syms (eval sym-or-syms)
         opts-sym    (gensym "opts")]
     `(let [~opts-sym ~opts]
        (reduce
          (fn [ret# [_# f#]]
            (let [sym# (f#)]
              (cond-> ret# sym# (conj sym#))))
          []
          (->> (zipmap
                 (collectionize '~sym-or-syms)
                 [~@(map
                      (fn [sym]
                        `(fn [] (instrument-1 '~sym ~opts-sym)))
                      (collectionize sym-or-syms))])
            (filter #((instrumentable-syms ~opts-sym) (first %)))
            (distinct-by first)))))))

(defmacro unstrument
  "Undoes instrument on the vars named by sym-or-syms, specified
as in instrument. With no args, unstruments all instrumented vars.
Returns a collection of syms naming the vars unstrumented."
  ([]
   `(unstrument '[~@(deref instrumented-vars)]))
  ([sym-or-syms]
   (let [sym-or-syms (eval sym-or-syms)]
     `(reduce
        (fn [ret# f#]
          (let [sym# (f#)]
            (cond-> ret# sym# (conj sym#))))
        []
        [~@(->> (collectionize sym-or-syms)
             (map
               (fn [sym]
                 (when (symbol? sym)
                   `(fn []
                      (unstrument-1 '~sym)))))
             (remove nil?))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; testing  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro check-1
  [[quote s :as qs] f spec opts]
  (let [{:keys [name] :as v} (when qs (ana-api/resolve &env s))]
    `(let [s#        '~name
           opts#     ~opts
           v#        ~(when v `(var ~name))
           spec#     (or ~spec ~(when v `(s/get-spec (var ~name))))
           re-inst?# (and v# (seq (unstrument '~name)) true)
           f#        (or ~f (when v# @v#))]
       (try
         (cond
           (nil? f#)
           {:failure (ex-info "No fn to spec" {::s/failure :no-fn})
            :sym     s# :spec spec#}

           (:args spec#)
           (let [tcret# (quick-check f# spec# opts#)]
             (make-check-result s# spec# tcret#))

           :default
           {:failure (ex-info "No :args spec" {::s/failure :no-args-spec})
            :sym     s# :spec spec#})
         (finally
           (when re-inst?# (instrument '~name)))))))

(defmacro check-fn
  "Runs generative tests for fn f using spec and opts. See
'check' for options and return."
  ([f spec]
   `(check-fn ~f ~spec nil))
  ([f spec opts]
   `(let [opts# ~opts]
      (validate-check-opts opts#)
      (check-1 nil ~f ~spec opts#))))