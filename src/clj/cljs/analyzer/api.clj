;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.api
  (:refer-clojure :exclude [all-ns ns-interns ns-resolve resolve find-ns
                            ns-publics remove-ns])
  (:require [cljs.env :as env]
            [cljs.analyzer :as ana]))

;; =============================================================================

(defn empty-env
  "Creates an empty analysis environment."
  []
  (ana/empty-env))

(defn analyze
  "Given an environment, a map containing {:locals (mapping of names to bindings), :context
  (one of :statement, :expr, :return), :ns (a symbol naming the
  compilation ns)}, and form, returns an expression object (a map
  containing at least :form, :op and :env keys). If expr has any (immediately)
  nested exprs, must have :children [exprs...] entry. This will
  facilitate code walking without knowing the details of the op set."
  ([env form] (ana/analyze env form nil))
  ([env form name] (ana/analyze env form name nil))
  ([env form name opts] (ana/analyze env form name opts)))

(defn forms-seq
  "Seq of Clojure/ClojureScript forms from rdr, a java.io.Reader. Optionally
  accepts a filename argument which will be used in any emitted errors."
  ([rdr] (ana/forms-seq* rdr nil))
  ([rdr filename] (ana/forms-seq* rdr filename)))

(defn parse-ns
  "Helper for parsing only the essential namespace information from a
   ClojureScript source file and returning a cljs.closure/IJavaScript compatible
   map _not_ a namespace AST node.

   By default does not load macros or perform any analysis of dependencies. If
   opts parameter provided :analyze-deps and :load-macros keys their values will
   be used for *analyze-deps* and *load-macros* bindings respectively. This
   function does _not_ side-effect the ambient compilation environment unless
   requested via opts where :restore is false."
  ([src] (ana/parse-ns src nil nil))
  ([src opts] (ana/parse-ns src nil opts))
  ([src dest opts] (ana/parse-ns src dest opts)))

(defn analyze-file
  "Given a java.io.File, java.net.URL or a string identifying a resource on the
   classpath attempt to analyze it.

   This function side-effects the ambient compilation environment
   `cljs.env/*compiler*` to aggregate analysis information. opts argument is
   compiler options, if :cache-analysis true will cache analysis to
   \":output-dir/some/ns/foo.cljs.cache.edn\". This function does not return a
   meaningful value."
  ([f] (ana/analyze-file f nil))
  ([f opts] (ana/analyze-file f opts)))

;; =============================================================================
;; Main API

(defn resolve
  "Given an analysis environment resolve a var. Analogous to
   clojure.core/resolve"
  [env sym]
  {:pre [(map? env) (symbol? sym)]}
  (try
    (ana/resolve-var env sym
      (ana/confirm-var-exists-throw))
    (catch Exception e
      (ana/resolve-macro-var env sym))))

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
  (merge
    (get-in @env/*compiler* [::ana/namespaces ns :macros])
    (get-in @env/*compiler* [::ana/namespaces ns :defs])))

(defn ns-publics
  "Given a namespace return all the public var analysis maps. Analagous to
  clojure.core/ns-publics but returns var analysis maps not vars."
  [ns]
  {:pre [(symbol? ns)]}
  (->> (merge
         (get-in @env/*compiler* [::ana/namespaces ns :macros])
         (get-in @env/*compiler* [::ana/namespaces ns :defs]))
       (remove (fn [[k v]] (:private v)))
       (into {})))

(defn ns-resolve
  "Given a namespace and a symbol return the corresponding var analysis map.
  Analagous to clojure.core/ns-resolve but returns var analysis map not Var."
  [ns sym]
  {:pre [(symbol? ns) (symbol? sym)]}
  (get-in @env/*compiler* [::ana/namespaces ns :defs sym]))

(defn remove-ns
  "Removes the namespace named by the symbol."
  [ns]
  {:pre [(symbol? ns)]}
  (swap! env/*compiler* update-in [::ana/namespaces] dissoc ns))

(defmacro in-cljs-user
  "Binds cljs.analyzer/*cljs-ns* to 'cljs.user and uses the given compilation
  environment atom and runs body."
  [env & body]
  `(binding [cljs.analyzer/*cljs-ns* 'cljs.user]
     (cljs.env/with-compiler-env ~env
       ~@body)))