;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.api
  (:refer-clojure :exclude [all-ns ns-interns ns-resolve resolve find-ns
                            ns-publics])
  (:require [cljs.env :as env]
            [cljs.analyzer :as ana]))

(defn resolve
  "Given an analysis environment resolve a var. Analogous to
   clojure.core/resolve"
  [env sym]
  {:pre [(map? env) (symbol? sym)]}
  (try
    (ana/resolve-var env sym
      (ana/confirm-var-exists-throw))
    (catch Exception e)))

(defn all-ns
  "Return all namespaces. Analagous to clojure.core/all-ns but
  returns symbols identifying namespaces not Namespace instances."
  []
  (keys (get @env/*compiler* ::ana/namespaces)))

(defn find-ns
  "Given a namespace return the corresponding namespace analysis map. Analagous
  to clojure.core/find-ns."
  [sym]
  {:pre [(symbol? sym)]}
  (get-in @env/*compiler* [::ana/namespaces sym]))

(defn ns-interns
  "Given a namespace return all the var analysis maps. Analagous to
  clojure.core/ns-interns but returns var analysis maps not vars."
  [ns]
  {:pre [(symbol? ns)]}
  (get-in @env/*compiler* [::ana/namespaces ns :defs]))

(defn ns-publics
  "Given a namespace return all the public var analysis maps. Analagous to
  clojure.core/ns-publics but returns var analysis maps not vars."
  [ns]
  {:pre [(symbol? ns)]}
  (->> (get-in @env/*compiler* [::ana/namespaces ns :defs])
       (remove (fn [[k v]] (:private v)))
       (into {})))

(defn ns-resolve
  "Given a namespace and a symbol return the corresponding var analysis map.
  Analagous to clojure.core/ns-resolve but returns var analysis map not Var."
  [ns sym]
  {:pre [(symbol? ns) (symbol? sym)]}
  (get-in @env/*compiler* [::ana/namespaces ns :defs sym]))

(defmacro in-cljs-user
  "Binds cljs.analyzer/*cljs-ns* to 'cljs.user and uses the given compilation
  environment atom and runs body."
  [env & body]
  `(binding [cljs.analyzer/*cljs-ns* 'cljs.user]
     (cljs.env/with-compiler-env ~env
       ~@body)))