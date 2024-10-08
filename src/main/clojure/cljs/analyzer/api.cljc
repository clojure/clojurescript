;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.api
  "This is intended to be a stable api for those who need programmatic access
  to the analyzer."
  (:refer-clojure :exclude [all-ns ns-interns ns-resolve resolve find-ns
                            ns-publics remove-ns the-ns])
  #?(:clj  (:require [cljs.analyzer :as ana]
                     [cljs.env :as env]
                     [cljs.util :as util]
                     [clojure.edn :as edn]
                     [clojure.java.io :as io])
     :cljs (:require [cljs.analyzer :as ana]
                     [cljs.env :as env])))

;; =============================================================================
;; Useful Utilities

(defn empty-state
  "Creates an empty compilation state Atom<Map>. The optional opts arg is a map
   representing the compiler configuration. See the documentation
   for details: https://clojurescript.org/reference/compiler-options"
  ([]
   (if-not (nil? env/*compiler*)
     env/*compiler*
     (env/default-compiler-env)))
  ([opts]
   (env/default-compiler-env opts)))

(defn current-state
  "Return the current compiler state atom."
  []
  env/*compiler*)

(defn current-file
  "Return the current file under analysis or compilation."
  []
  ana/*cljs-file*)

(defn current-ns
  "Return the current ns under analysis or compilation."
  []
  ana/*cljs-ns*)

(defmacro with-state
  "Run the body with the given compilation state Atom<Map>."
  [state & body]
  `(env/with-compiler-env ~state
     ~@body))

(defn empty-env
  "Creates an empty analysis environment."
  []
  (ana/empty-env))

(defmacro no-warn
  "Disable analyzer warnings for any analysis executed in body."
  [& body]
  (let [no-warnings (zipmap (keys ana/*cljs-warnings*) (repeat false))]
    `(binding [ana/*cljs-warnings* ~no-warnings]
       ~@body)))

(defn warning-enabled?
  "Test if the given warning-type is enabled."
  [warning-type]
  (ana/*cljs-warnings* warning-type))

(defn default-warning-handler
  "The default warning handler.

   Outputs the warning messages to *err*."
  [warning-type env extra]
  (ana/default-warning-handler warning-type env extra))

(defmacro with-warning-handlers
  "Helper macro for custom handling of emitted warnings. Handlers should be
   a vector of functions. The signature of these functions is
   [warn-type env warn-info]. warn-type is a keyword describing the warning,
   env is the analysis environment, and warn-info is a map of extra useful
   information for a particular warning type."
  [handlers & body]
  `(ana/with-warning-handlers ~handlers
     ~@body))

(defn warning-message
  "Helper for generating the standard analyzer messages for warnings. Should be
  passed warn-type and warn-info. See with-warning-handlers."
  [warn-type warn-info]
  (ana/error-message warn-type warn-info))

(defn enabled-warnings
  "Get the enabled warning types."
  []
  ana/*cljs-warnings*)

(defn get-options
  "Return the compiler options from compiler state."
  ([] (get-options (current-state)))
  ([state]
   (get @state :options)))

(defn get-js-index
  "Return the currently computed Google Closure js dependency index from the
  compiler state."
  ([] (get-js-index (current-state)))
  ([state]
   (get @state :js-dependency-index)))

(def
  ^{:doc "ClojureScript's default analysis passes."}
  default-passes ana/default-passes)

(defmacro with-passes
  "Evaluate the body with the provided sequence of compiler passes."
  [passes & body]
  `(binding [ana/*passes* ~passes]
     ~@body))

#?(:clj
   (defn analyze
     "Given an environment, a map containing {:locals (mapping of names to bindings), :context
     (one of :statement, :expr, :return), :ns (a symbol naming the
     compilation ns)}, and form, returns an expression object (a map
     containing at least :form, :op and :env keys). If expr has any (immediately)
     nested exprs, must have :children entry. This must be a vector of keywords naming
     the immediately nested fields mapped to an expr or vector of exprs. This will
     facilitate code walking without knowing the details of the op set."
     ([env form] (analyze env form nil))
     ([env form name] (analyze env form name nil))
     ([env form name opts]
      (analyze (or (current-state) (empty-state opts)) env form name opts))
     ([state env form name opts]
      (env/with-compiler-env state
        (binding [ana/*cljs-warning-handlers* (:warning-handlers opts ana/*cljs-warning-handlers*)]
          (ana/analyze env form name opts))))))

#?(:clj
   (defn forms-seq
     "Seq of Clojure/ClojureScript forms from rdr, a java.io.Reader. Optionally
     accepts a filename argument which will be used in any emitted errors."
     ([rdr] (ana/forms-seq* rdr nil))
     ([rdr filename] (ana/forms-seq* rdr filename))))

#?(:clj
   (defn parse-ns
     "Helper for parsing only the essential namespace information from a
      ClojureScript source file and returning a cljs.closure/IJavaScript compatible
      map _not_ a namespace AST node.

      By default does not load macros or perform any analysis of dependencies. If
      opts parameter provided :analyze-deps and :load-macros keys their values will
      be used for *analyze-deps* and *load-macros* bindings respectively. This
      function does _not_ side-effect the ambient compilation environment unless
      requested via opts where :restore is false."
     ([src] (parse-ns src nil nil))
     ([src opts] (parse-ns src nil opts))
     ([src dest opts]
      (parse-ns (or (current-state) (empty-state opts)) src dest opts))
     ([state src dest opts]
      (env/with-compiler-env state
        (binding [ana/*cljs-warning-handlers* (:warning-handlers opts ana/*cljs-warning-handlers*)]
          (ana/parse-ns src dest opts))))))

#?(:clj
   (defn analyze-file
     "Given a java.io.File, java.net.URL or a string identifying a resource on the
      classpath attempt to analyze it.

      This function side-effects the ambient compilation environment
      `cljs.env/*compiler*` to aggregate analysis information. opts argument is
      compiler options, if :cache-analysis true will cache analysis to
      \":output-dir/some/ns/foo.cljs.cache.edn\". This function does not return a
      meaningful value."
     ([f] (analyze-file f nil))
     ([f opts]
      (analyze-file (or (current-state) (empty-state opts)) f opts))
     ([state f opts]
      (env/with-compiler-env state
        (binding [ana/*cljs-warning-handlers* (:warning-handlers opts ana/*cljs-warning-handlers*)]
          (ana/analyze-file f opts))))))

#?(:clj
   (defn read-analysis-cache
     "Read an analysis cache."
     [cache-file]
     (case (util/ext cache-file)
       "edn" (edn/read-string (slurp cache-file))
       "json" (let [{:keys [reader read]} @ana/transit]
                (with-open [is (io/input-stream cache-file)]
                  (read (reader is :json ana/transit-read-opts)))))))

;; =============================================================================
;; Main API

(defn resolve
  "Given an analysis environment resolve a var. Analogous to
   clojure.core/resolve"
  [env sym]
  {:pre [(map? env) (symbol? sym)]}
  (try
    (binding [ana/*private-var-access-nowarn* true]
      (ana/resolve-var env sym
        (ana/confirm-var-exists-throw)))
    (catch #?(:clj Exception :cljs :default) e
      (ana/resolve-macro-var env sym))))

(defn all-ns
  "Return all namespaces. Analagous to clojure.core/all-ns but
  returns symbols identifying namespaces not Namespace instances."
  ([]
   (all-ns env/*compiler*))
  ([state]
   (keys (get @state ::ana/namespaces))))

(defn find-ns
  "Given a namespace return the corresponding namespace analysis map. Analagous
  to clojure.core/find-ns."
  ([sym]
   (find-ns env/*compiler* sym))
  ([state sym]
   {:pre [(symbol? sym)]}
   (get-in @state [::ana/namespaces sym])))

(defn the-ns
  "Given a namespace return the corresponding namespace analysis map, throwing an
  exception if not found. Analagous to clojure.core/the-ns."
  ([ns]
   (the-ns env/*compiler* ns))
  ([state sym]
   {:pre [(symbol? sym)]}
   (or (find-ns state sym)
       (throw (ex-info (str "No namespace found: " sym) {:ns sym})))))

(defn ns-interns
  "Given a namespace return all the var analysis maps. Analagous to
  clojure.core/ns-interns but returns var analysis maps not vars."
  ([ns]
   (ns-interns env/*compiler* ns))
  ([state ns]
   {:pre [(symbol? ns)]}
   (let [ns (the-ns state ns)]
     (merge
      (:macros ns)
      (:defs ns)))))

(defn ns-publics
  "Given a namespace return all the public var analysis maps. Analagous to
  clojure.core/ns-publics but returns var analysis maps not vars."
  ([ns]
   (ns-publics env/*compiler* ns))
  ([state ns]
   {:pre [(symbol? ns)]}
   (->> (ns-interns state ns)
        (remove (fn [[k v]] (:private v)))
        (into {}))))

(defn ns-resolve
  "Given a namespace and a symbol return the corresponding var analysis map.
  Analagous to clojure.core/ns-resolve but returns var analysis map not Var."
  ([ns sym]
   (ns-resolve env/*compiler* ns sym))
  ([state ns sym]
   {:pre [(symbol? ns) (symbol? sym)]}
   (get-in @state [::ana/namespaces ns :defs sym])))

(defn remove-ns
  "Removes the namespace named by the symbol."
  ([ns]
   (remove-ns env/*compiler* ns))
  ([state ns]
   {:pre [(symbol? ns)]}
   (swap! state update-in [::ana/namespaces] dissoc ns)))

(defmacro in-cljs-user
  "Binds cljs.analyzer/*cljs-ns* to 'cljs.user and uses the given compilation
  environment atom and runs body."
  [env & body]
  `(binding [cljs.analyzer/*cljs-ns* 'cljs.user]
     (cljs.env/with-compiler-env ~env
       ~@body)))
