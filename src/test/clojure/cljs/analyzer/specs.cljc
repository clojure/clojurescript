;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.specs
  (:require [clojure.spec.alpha :as s]))

(s/def ::op keyword?)
(s/def ::form any?)
(s/def ::env map?)
(s/def ::context #{:expr :return :statement})

(defmulti node :op)
(s/def ::node (s/multi-spec node :op))

(s/def ::test ::node)
(s/def ::then ::node)
(s/def ::else ::node)

;; TODO: :tag
(s/def ::base
  (s/keys
    :req-un [::op ::env ::form]))

(s/def ::name symbol?)
(s/def :cljs.analyzer.specs.binding/local
  #{:arg :catch :fn :let :letfn :loop :field})
(s/def ::variadic? boolean?)
(s/def ::init ::node)
(s/def ::shadow
  (s/or :nil nil?
        :node ::node))

(defmethod node :binding [_]
  (s/merge
    ::base
    (s/keys
      :req-un [::name :cljs.analyzer.specs.binding/local]
      :opt-un [::variadic? ::init ::shadow])))

(s/def ::nodes (s/* ::node))
(s/def ::default ::node)

(defmethod node :case [_]
  (s/merge ::base
    (s/keys
      :req-un [::test ::nodes ::default])))

(defmethod node :case-node [_]
  (s/keys
    :req-un [::op ::env ::tests ::then]))

(defmethod node :case-test [_]
  (s/merge ::base
    (s/keys
      :req-un [::test])))

(defmethod node :case-then [_]
  (s/merge ::base
    (s/keys
      :req-un [::then])))

(s/def ::literal? boolean?)
(s/def ::val any?)

(defmethod node :const [_]
  (s/merge ::base
    (s/keys
      :req-un [::val]
      ;; ::literal? is required in the AST REF, but we don't actually use it
      ;; should check tools.analyzer
      :opt-un [::literal?])))

(defmethod node :def [_]
  (s/merge ::base
    (s/keys
      :req-un [::name]
      :opt-un [::init ::the-var])))

(s/def ::body ::node)
(s/def ::t symbol?)

(defmethod node :defrecord [_]
  (s/merge ::base
    (s/keys
      :req-un [::t ::body])))

(defmethod node :deftype [_]
  (s/merge ::base
    (s/keys
      :req-un [::t ::body])))

(s/def ::statements (s/* ::node))
(s/def ::ret ::node)
(s/def ::body? boolean?)

(defmethod node :do [_]
  (s/merge ::base
    (s/keys
      :req-un [::statements ::ret]
      :opt-un [::body?])))

(s/def ::local ::node)
(s/def ::max-fixed-arity int?)
(s/def ::methods (s/+ ::node))

(defmethod node :fn [_]
  (s/merge ::base
    (s/keys
      :req-un [::variadic? ::max-fixed-arity ::methods]
      :opt-un [::local])))

(s/def ::fixed-arity int?)
(s/def ::params (s/* ::node))

(defmethod node :fn-method [_]
  (s/merge ::base
    (s/keys
      :req-un [::fixed-arity ::params ::body])))

(s/def ::method symbol?)
(s/def ::target ::node)
(s/def ::args (s/* ::node))

(defmethod node :host-call [_]
  (s/merge ::base
    (s/keys
      :req-un [::method ::target ::args])))

(s/def ::field symbol?)

(defmethod node :host-field [_]
  (s/merge ::base
    (s/keys
      :req-un [::field ::target])))

(defmethod node :if [_]
  (s/merge ::base
    (s/keys
      :req-un [::test ::then]
      :opt-un [::else])))

(s/def ::fn ::node)

(defmethod node :invoke [_]
  (s/merge ::base
    (s/keys
      :req-un [::fn ::args])))

(s/def ::code string?)

(defmethod node :js [_]
  (s/merge ::base
    (s/keys
      :opt-un [::code])))

(defmethod node :js-array [_]
  (s/merge ::base
    (s/keys
      :req-un [::items])))

(defmethod node :js-object [_]
  (s/merge ::base
    (s/keys
      :req-un [::vals])))

(s/def ::ns symbol?)

(defmethod node :js-var [_]
  (s/merge ::base
    (s/keys
      :req-un [::ns ::name])))

(s/def ::bindings (s/* ::node))

(defmethod node :let [_]
  (s/merge ::base
    (s/keys
      :req-un [::bindings ::body])))

(defmethod node :letfn [_]
  (s/merge ::base
    (s/keys
      :req-un [::bindings ::body])))

(s/def ::items (s/* ::node))

;; TODO: not in ast-ref
(defmethod node :list [_]
  (s/merge ::base
    (s/keys
      :req-un [::items])))

(defmethod node :local [_]
  (s/merge ::base
    (s/keys
      :req-un [:cljs.analyzer.specs.binding/local ::name])))

(defmethod node :loop [_]
  (s/merge ::base
    (s/keys
      :req-un [::bindings ::body])))

(s/def ::vals (s/* ::node))

(defmethod node :map [_]
  (s/merge ::base
    (s/keys :req-un [::keys ::vals])))

(s/def ::class ::node)

(defmethod node :new [_]
  (s/merge ::base
    (s/keys
      :req-un [::class ::args])))

(defmethod node :no-op [_]
  (s/keys
    :req-un [::env ::op]))

(defmethod node :ns [_]
  ::base)

(defmethod node :ns* [_]
  ::base)

(s/def ::expr ::node)

(defmethod node :quote [_]
  (s/merge ::base
    (s/keys
      :req-un [::expr ::literal?])))

(s/def ::exprs (s/* ::node))

(defmethod node :recur [_]
  (s/merge ::base
    (s/keys
      :req-un [::exprs])))

(defmethod node :set [_]
  (s/merge ::base
    (s/keys
      :req-un [::items])))

(defmethod node :set! [_]
  (s/merge ::base
    (s/keys
      :req-un [::target ::val])))

(s/def ::var ::node)
(s/def ::sym ::node)
(s/def ::meta map?)

(defmethod node :the-var [_]
  (s/merge ::base
    (s/keys
      :opt-un [::var ::sym ::meta])))

(s/def ::the-var ::node)

(s/def ::exception ::node)

(defmethod node :throw [_]
  (s/merge ::base
    (s/keys
      :req-un [::exception])))

(s/def ::catch ::node)
(s/def ::finally ::node)

(defmethod node :try [_]
  (s/merge ::base
    (s/keys
      :req-un [::body ::catch ::name ::finally])))

(defmethod node :var [_]
  (s/merge ::base
    (s/keys
      :req-un [::ns ::name])))

(s/def ::meta ::node)

(defmethod node :vector [_]
  (s/merge ::base
    (s/keys
      :req-un [::items])))

(defmethod node :with-meta [_]
  (s/merge ::base
    (s/keys
      :req-un [::meta ::expr])))

(comment

  (s/valid? ::node 1)
  (s/valid? ::node
    {:op :const
     :env {}
     :form 1
     :literal? true
     :val 1})

  (s/explain-data ::node
    {:op :if
     :env {}
     :form '(if true true false)
     :test {:op :const
            :env {}
            :form true
            :literal? true
            :val true}
     :then {:op :const
            :env {}
            :form true
            :literal? true
            :val true}
     :else {:op :const
            :env 1
            :form false
            :literal? true
            :val false}})

  )
