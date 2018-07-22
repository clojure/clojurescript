;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.spec.test.alpha
  (:require
    [cljs.analyzer :as ana]
    [cljs.analyzer.api :as ana-api]
    [clojure.string :as string]
    [cljs.spec.alpha :as s]
    [cljs.spec.gen.alpha :as gen]))

(defonce ^:private instrumented-vars (atom #{}))

(defn- collectionize
  [x]
  (if (symbol? x)
    (list x)
    x))

(defn- enumerate-namespace* [sym-or-syms]
  (into #{}
    (mapcat
      (fn [sym]
        (->> (vals (ana-api/ns-interns sym))
          (map :name)
          (map
            (fn [name-sym]
              (symbol (name sym) (name name-sym)))))))
    (collectionize sym-or-syms)))

(defmacro enumerate-namespace
  "Given a symbol naming an ns, or a collection of such symbols,
returns the set of all symbols naming vars in those nses."
  [ns-sym-or-syms]
  `'~(enumerate-namespace* (eval ns-sym-or-syms)))

(defn- fn-spec-name?
  [s]
  (symbol? s))

(defmacro with-instrument-disabled
  "Disables instrument's checking of calls, within a scope."
  [& body]
  `(binding [*instrument-enabled* nil]
     ~@body))

(defmacro instrument-1
  [[quote s] opts]
  (when-let [v (ana-api/resolve &env s)]
    (when (and (nil? (:const v))
               #?(:cljs (nil? (:macro v))))
      (swap! instrumented-vars conj (:name v))
      `(let [checked# (#'instrument-1* '~s (var ~s) ~opts)]
         (when checked# (set! ~s checked#))
         '~(:name v)))))

(defmacro unstrument-1
  [[quote s]]
  (when-let [v (ana-api/resolve &env s)]
    (when (@instrumented-vars (:name v))
      (swap! instrumented-vars disj (:name v))
      `(let [raw# (#'unstrument-1* '~s (var ~s))]
         (when raw# (set! ~s raw#))
         '~(:name v)))))

(defn- sym-or-syms->syms [sym-or-syms]
  (into []
    (mapcat
      (fn [sym]
        (if (and (string/includes? (str sym) ".")
                 (ana-api/find-ns sym))
          (->> (vals (ana-api/ns-interns sym))
            (filter #(not (:macro %)))
            (map :name)
            (map
              (fn [name-sym]
                (symbol (name sym) (name name-sym)))))
          [sym])))
    (collectionize sym-or-syms)))

(defmacro instrument
  "Instruments the vars named by sym-or-syms, a symbol or collection
of symbols, or all instrumentable vars if sym-or-syms is not
specified. If a symbol identifies a namespace then all symbols in that
namespace will be enumerated.

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
   `(instrument '[~@(#?(:clj  s/speced-vars
                        :cljs cljs.spec.alpha$macros/speced-vars))]))
  ([xs]
   `(instrument ~xs nil))
  ([sym-or-syms opts]
   (let [syms (sym-or-syms->syms (eval sym-or-syms))
         opts-sym (gensym "opts")]
     `(let [~opts-sym ~opts]
        (reduce
          (fn [ret# [_# f#]]
            (let [sym# (f#)]
              (cond-> ret# sym# (conj sym#))))
          []
          (->> (zipmap '~syms
                 [~@(map
                      (fn [sym]
                        `(fn [] (instrument-1 '~sym ~opts-sym)))
                      syms)])
            (filter #((instrumentable-syms ~opts-sym) (first %)))
            (distinct-by first)))))))

(defmacro unstrument
  "Undoes instrument on the vars named by sym-or-syms, specified
as in instrument. With no args, unstruments all instrumented vars.
Returns a collection of syms naming the vars unstrumented."
  ([]
   `(unstrument '[~@(deref instrumented-vars)]))
  ([sym-or-syms]
   (let [syms (sym-or-syms->syms (eval sym-or-syms))]
     `(reduce
        (fn [ret# f#]
          (let [sym# (f#)]
            (cond-> ret# sym# (conj sym#))))
        []
        [~@(->> syms
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
           (let [tcret# (#'quick-check f# spec# opts#)]
             (#'make-check-result s# spec# tcret#))

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

(defn checkable-syms*
  ([]
    (checkable-syms* nil))
  ([opts]
   (reduce into #{}
     [(filter fn-spec-name? (keys @@#'s/registry-ref))
      (keys (:spec opts))])))

(defmacro checkable-syms
  "Given an opts map as per check, returns the set of syms that
can be checked."
  ([]
   `(checkable-syms nil))
  ([opts]
   `(let [opts# ~opts]
      (validate-check-opts opts#)
      (reduce conj #{}
        '[~@(filter fn-spec-name? (keys @@#'s/registry-ref))
          ~@(keys (:spec opts))]))))

(defmacro check
  "Run generative tests for spec conformance on vars named by
sym-or-syms, a symbol or collection of symbols. If sym-or-syms
is not specified, check all checkable vars. If a symbol identifies a
namespace then all symbols in that namespace will be enumerated.

The opts map includes the following optional keys, where stc
aliases clojure.test.check:

::stc/opts  opts to flow through test.check/quick-check
:gen        map from spec names to generator overrides

The ::stc/opts include :num-tests in addition to the keys
documented by test.check. Generator overrides are passed to
spec/gen when generating function args.

Returns a lazy sequence of check result maps with the following
keys

:spec       the spec tested
:sym        optional symbol naming the var tested
:failure    optional test failure
::stc/ret   optional value returned by test.check/quick-check

The value for :failure can be any exception. Exceptions thrown by
spec itself will have an ::s/failure value in ex-data:

:check-failed   at least one checked return did not conform
:no-args-spec   no :args spec provided
:no-fn          no fn provided
:no-fspec       no fspec provided
:no-gen         unable to generate :args
:instrument     invalid args detected by instrument
"
  ([]
   `(check '~(checkable-syms*)))
  ([sym-or-syms]
   `(check ~sym-or-syms nil))
  ([sym-or-syms opts]
   (let [syms (sym-or-syms->syms (eval sym-or-syms))
         opts-sym (gensym "opts")]
     `(let [~opts-sym ~opts]
        [~@(->> syms
             (filter (checkable-syms* opts))
             (map
               (fn [sym]
                 (do `(check-1 '~sym nil nil ~opts-sym)))))]))))
