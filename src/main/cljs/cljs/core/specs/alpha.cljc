;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core.specs.alpha
  (:require [clojure.spec.alpha :as s]
            #?(:clj  [cljs.core :as core]
               :cljs [cljs.core$macros :as core])))

;;;; destructure

(s/def ::local-name (s/and simple-symbol? #(not= '& %)))

(s/def ::binding-form
  (s/or :sym ::local-name
        :seq ::seq-binding-form
        :map ::map-binding-form))

;; sequential destructuring

(s/def ::seq-binding-form
  (s/and vector?
         (s/cat :elems (s/* ::binding-form)
                :rest (s/? (s/cat :amp #{'&} :form ::binding-form))
                :as (s/? (s/cat :as #{:as} :sym ::local-name)))))

;; map destructuring

(s/def ::keys (s/coll-of ident? :kind vector?))
(s/def ::syms (s/coll-of symbol? :kind vector?))
(s/def ::strs (s/coll-of simple-symbol? :kind vector?))
(s/def ::or (s/map-of simple-symbol? any?))
(s/def ::as ::local-name)

(s/def ::map-special-binding
  (s/keys :opt-un [::as ::or ::keys ::syms ::strs]))

(s/def ::map-binding (s/tuple ::binding-form any?))

(s/def ::ns-keys
  (s/tuple
    (s/and qualified-keyword? #(-> % name #{"keys" "syms"}))
    (s/coll-of simple-symbol? :kind vector?)))

(s/def ::map-bindings
  (s/every (s/or :mb ::map-binding
                 :nsk ::ns-keys
                 :msb (s/tuple #{:as :or :keys :syms :strs} any?)) :into {}))

(s/def ::map-binding-form (s/merge ::map-bindings ::map-special-binding))

;; bindings

(s/def ::binding (s/cat :binding ::binding-form :init-expr any?))
(s/def ::bindings (s/and vector? (s/* ::binding)))

;; let, if-let, when-let

(s/fdef core/let
  :args (s/cat :bindings ::bindings
               :body (s/* any?)))

(s/fdef core/if-let
  :args (s/cat :bindings (s/and vector? ::binding)
               :then any?
               :else (s/? any?)))

(s/fdef core/when-let
  :args (s/cat :bindings (s/and vector? ::binding)
               :body (s/* any?)))

;; defn, defn-, fn

(s/def ::arg-list
  (s/and
    vector?
    (s/cat :args (s/* ::binding-form)
           :varargs (s/? (s/cat :amp #{'&} :form ::binding-form)))))

(s/def ::args+body
  (s/cat :args ::arg-list
         :body (s/alt :prepost+body (s/cat :prepost map?
                                           :body (s/+ any?))
                      :body (s/* any?))))

(s/def ::defn-args
  (s/cat :name simple-symbol?
         :docstring (s/? string?)
         :meta (s/? map?)
         :bs (s/alt :arity-1 ::args+body
                    :arity-n (s/cat :bodies (s/+ (s/spec ::args+body))
                                    :attr (s/? map?)))))

(s/fdef core/defn
  :args ::defn-args
  :ret any?)

(s/fdef core/defn-
  :args ::defn-args
  :ret any?)

(s/fdef core/fn
  :args (s/cat :name (s/? simple-symbol?)
               :bs (s/alt :arity-1 ::args+body
                          :arity-n (s/+ (s/spec ::args+body))))
  :ret any?)

;;;; ns

(s/def ::exclude (s/coll-of simple-symbol?))
(s/def ::only (s/coll-of simple-symbol?))
(s/def ::rename (s/map-of simple-symbol? simple-symbol?))
(s/def ::filters (s/keys* :opt-un [::exclude ::only ::rename]))

(s/def ::ns-refer-clojure
  (s/spec (s/cat :clause #{:refer-clojure}
                 :filters ::filters)))

(s/def ::refer (s/coll-of simple-symbol?))
(s/def ::refer-macros (s/coll-of simple-symbol?))
(s/def ::include-macros #{true})

(s/def ::lib (s/or :sym simple-symbol?
                   :str string?))

(s/def ::libspec
  (s/alt :lib ::lib
         :lib+opts (s/spec (s/cat :lib ::lib
                                  :options (s/keys* :opt-un [::as ::refer ::refer-macros ::include-macros])))))

(s/def ::macros-libspec
  (s/alt :lib simple-symbol?
         :lib+opts (s/spec (s/cat :lib simple-symbol?
                                  :options (s/keys* :opt-un [::as ::refer])))))

(s/def ::ns-require
  (s/spec (s/cat :clause #{:require}
                 :body (s/+ (s/alt :libspec ::libspec
                                   :flag #{:reload :reload-all :verbose})))))

(s/def ::ns-require-macros
  (s/spec (s/cat :clause #{:require-macros}
                :body (s/+ (s/alt :libspec ::macros-libspec
                                  :flag #{:reload :reload-all :verbose})))))

(s/def ::package-list
  (s/spec
    (s/cat :package simple-symbol?
           :classes (s/* simple-symbol?))))

(s/def ::import-list
  (s/* (s/alt :class simple-symbol?
              :package-list ::package-list)))

(s/def ::ns-import
  (s/spec
    (s/cat :clause #{:import}
           :classes ::import-list)))

;; same as ::libspec, but also supports the ::filters options in the libspec
(s/def ::use-libspec
  (s/alt :lib ::lib
         :lib+opts (s/spec (s/cat :lib ::lib
                                  :options (s/keys* :req-un [::only] :opt-un [::rename])))))

(s/def ::ns-use
  (s/spec (s/cat :clause #{:use}
                 :libs (s/+ (s/alt :libspec ::use-libspec
                                   :flag #{:reload :reload-all :verbose})))))

;; same as ::libspec-macros, but also supports the ::filters options in the libspec
(s/def ::use-macros-libspec
  (s/alt :lib simple-symbol?
         :lib+opts (s/spec (s/cat :lib simple-symbol?
                                  :options (s/keys* :req-un [::only] :opt-un [::rename])))))

(s/def ::ns-use-macros
  (s/spec (s/cat :clause #{:use-macros}
                 :libs (s/+ (s/alt :libspec ::use-macros-libspec
                                   :flag #{:reload :reload-all :verbose})))))


(s/def ::ns-clauses
  (s/* (s/alt :refer-clojure ::ns-refer-clojure
              :require ::ns-require
              :require-macros ::ns-require-macros
              :import ::ns-import
              :use ::ns-use
              :use-macros ::ns-use-macros)))

(s/def ::ns-form
  (s/cat :name simple-symbol?
         :docstring (s/? string?)
         :attr-map (s/? map?)
         :clauses ::ns-clauses))

(s/fdef core/ns-special-form
  :args ::ns-form)

(defmacro ^:private quotable
  "Returns a spec that accepts both the spec and a (quote ...) form of the spec"
  [spec]
  `(s/or :spec ~spec :quoted-spec (s/cat :quote #{'quote} :spec ~spec)))

(s/def ::quotable-import-list
  (s/* (s/alt :class (quotable simple-symbol?)
              :package-list (quotable ::package-list))))

(s/fdef core/import
  :args ::quotable-import-list)

(defn- quoted
  "Returns a spec that accepts a (quote ...) form of the spec"
  [spec]
  (s/spec (s/cat :quote #{'quote} :spec spec)))

(s/fdef core/require
  :args (s/+ (s/alt :libspec (quoted ::libspec)
                    :flag #{:reload :reload-all :verbose})))

(s/fdef core/require-macros
  :args (s/+ (s/alt :libspec (quoted ::macros-libspec)
                    :flag #{:reload :reload-all :verbose})))

(s/fdef core/use
  :args (s/+ (s/alt :libspec (quoted ::use-libspec)
                    :flag #{:reload :reload-all :verbose})))

(s/fdef core/use-macros
  :args (s/+ (s/alt :libspec (quoted ::use-macros-libspec)
                    :flag #{:reload :reload-all :verbose})))
