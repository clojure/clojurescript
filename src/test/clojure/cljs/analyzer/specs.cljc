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

(defmethod node :if [_]
  (s/merge ::base
    (s/keys
      :req-un [::test ::then]
      :opt-un [::else])))

(s/def ::literal? boolean?)
(s/def ::val any?)

(defmethod node :const [_]
  (s/merge ::base
    (s/keys
      :req-un [::literal? ::val]
      :opt-un [])))

(s/def ::keys (s/* ::node))
(s/def ::vals (s/* ::node))

(defmethod node :map [_]
  (s/merge ::base
    (s/keys :req-un [::keys ::vals])))

(s/def ::items (s/* ::node))

(defmethod node :list [_]
  (s/merge ::base
    (s/keys
     :req-un [::items])))

(defmethod node :vector [_]
  (s/merge ::base
    (s/keys
      :req-un [::items])))

(defmethod node :set [_]
  (s/merge ::base
    (s/keys
      :req-un [::items])))

(defmethod node :js-object [_]
  (s/merge ::base
    (s/keys
      :req-un [::keys ::vals])))

(defmethod node :js-array [_]
  (s/merge ::base
    (s/keys
      :req-un [::items])))

(s/def ::var ::node)
(s/def ::sym ::node)
(s/def ::meta map?)

(defmethod node :the-var [_]
  (s/merge ::base
    (s/keys
      :opt-un [::var ::sym ::meta])))

(s/def ::nodes (s/* ::node))
(s/def ::default ::node)

(defmethod node ::case [_]
  (s/merge ::base
    (s/keys
      :req-un [::test ::nodes ::default])))

(defmethod node ::case-node [_]
  (s/merge ::base
    (s/keys
      :req-un [::tests ::then])))

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
