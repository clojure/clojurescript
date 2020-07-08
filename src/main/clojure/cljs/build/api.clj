;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software

(ns cljs.build.api
  "This is intended to be a stable api for those who need programmatic access
  to ClojureScript's project building facilities.

  For example: a build script may need to how to invalidate compiled
  files so that they will be recompiled."
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :as io]
            [cljs.util :as util]
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.closure :as closure]
            [cljs.js-deps :as deps])
  (:import [java.io File]))

;; =============================================================================
;; Useful Utilities

(defn ^File target-file-for-cljs-ns
  "Given an output directory and a clojurescript namespace return the
  compilation target file for that namespace.

  For example:
  (target-file-from-cljs-ns \"resources/out\" 'example.core) ->
  <File: \"resources/out/example/core.js\">"
  ([ns-sym] (closure/target-file-for-cljs-ns ns-sym nil))
  ([ns-sym output-dir] (closure/target-file-for-cljs-ns ns-sym output-dir)))

(defn mark-cljs-ns-for-recompile!
  "Backdates a cljs target file so that it the cljs compiler will recompile it."
  ([ns-sym] (closure/mark-cljs-ns-for-recompile! ns-sym nil))
  ([ns-sym output-dir] (closure/mark-cljs-ns-for-recompile! ns-sym output-dir)))

(defn cljs-dependents-for-macro-namespaces
  "Takes a list of Clojure (.clj) namespaces that define macros and
  returns a list ClojureScript (.cljs) namespaces that depend on those macro
  namespaces.

  For example where example.macros is defined in the clojure file
  \"example/macros.clj\" and both 'example.core and 'example.util are
  ClojureScript namespaces that require and use the macros from
  'example.macros :
  (cljs-dependents-for-macro-namespaces 'example.macros) ->
  ('example.core 'example.util)"
  ([namespaces]
   (closure/cljs-dependents-for-macro-namespaces
     (or (ana-api/current-state) (ana-api/empty-state)) namespaces))
  ([state namespaces]
   (closure/cljs-dependents-for-macro-namespaces state namespaces)))

(defn parse-js-ns
  "Given a Google Closure style JavaScript file or resource return the namespace
  information for the given file. Only returns the value extracted from the
  first provide statement."
  [f]
  (closure/parse-js-ns f))

(defn ^File src-file->target-file
  "Given a ClojureScript source file return the target file. May optionally
  provide build options with :output-dir specified."
  ([src] (src-file->target-file src nil))
  ([src opts]
   (src-file->target-file
     (or (ana-api/current-state) (ana-api/empty-state opts)) src opts))
  ([state src opts]
   (ana-api/with-state state
     (binding [ana/*cljs-warning-handlers* (:warning-handlers opts ana/*cljs-warning-handlers*)]
       (closure/src-file->target-file src opts)))))

(defn ^String src-file->goog-require
  "Given a ClojureScript or Google Closure style JavaScript source file return
  the goog.require statement for it."
  ([src] (src-file->goog-require src nil))
  ([src opts]
   (src-file->goog-require
     (or (ana-api/current-state) (ana-api/empty-state opts)) src opts))
  ([state src opts]
   (ana-api/with-state state
     (binding [ana/*cljs-warning-handlers* (:warning-handlers opts ana/*cljs-warning-handlers*)]
       (closure/src-file->goog-require src opts)))))

(defn index-ijs
  "Given a sequence of cljs.closure/IJavaScript values, create an index using
  :provides. The original values will appear under each :provide."
  [xs]
  (reduce
    (fn [index x]
      (merge index
        (zipmap (:provides x) (repeat x))))
    {} xs))

;; =============================================================================
;; Main API

(defn goog-dep-string
  "Given compiler options and a IJavaScript instance return the corresponding
  goog.addDependency string"
  [opts ijs]
  (closure/add-dep-string opts ijs))

(defn source-on-disk
  "Ensure that the given IJavaScript exists on disk in the output directory.
  Return updated IJavaScript with the new location if necessary."
  [opts ijs]
  (closure/source-on-disk opts ijs))

(defn ns->source
  "Given a namespace as a symbol return the corresponding resource if it exists."
  [ns]
  (util/ns->source ns))

(defn ns->location
  "Given a namespace and compilation environment return the relative path and
  uri of the corresponding source regardless of the source language extension:
  .cljs, .cljc, .js. Returns a map containing :relative-path a string, and
  :uri a URL."
  ([ns]
   (ns->location ns (or (ana-api/current-state) (ana-api/empty-state))))
  ([ns compiler-env]
   (closure/source-for-namespace ns compiler-env)))

(defn compilable->ijs
  "Given a cljs.closure/Compilable value, return the corresponding
  cljs.closure/IJavaScript value."
  ([x]
   (compilable->ijs x {}))
  ([x opts]
   (closure/-find-sources x opts)))

(defn add-dependency-sources
  "Given a sequence of cljs.closure/IJavaScript values, return a set that includes
  all dependencies."
  ([xs]
   (add-dependency-sources xs {}))
  ([xs opts]
   (add-dependency-sources (or (ana-api/current-state) (ana-api/empty-state opts)) xs opts))
  ([state xs opts]
   (ana-api/with-state state
     (closure/add-dependency-sources xs opts))))

(defn add-dependencies
  "DEPRECATED: Given one or more IJavaScript objects in dependency order, produce
  a new sequence of IJavaScript objects which includes the input list
  plus all dependencies in dependency order."
  [opts & ijss]
  (closure/add-dependencies opts ijss))

(defn handle-js-modules
  "Given a collection of IJavaScript values representing a build, index all
  node modules, convert all JS modules (ES6 etc), and store the updated
  js-dependency-index (likely changed due to modules) in compiler state."
  [state xs opts]
  (closure/handle-js-modules opts xs state))

(defn dependency-order
  "Topologically sort a collection of IJavaScript values."
  [xs]
  (deps/dependency-order xs))

(defn add-implicit-options
  "Given a valid map of build options add any standard implicit options. For
  example :optimizations :none implies :cache-analysis true and :source-map
  true."
  [opts]
  (closure/add-implicit-options opts))

(defn inputs
  "Given a list of directories and files, return a compilable object that may
  be passed to build or watch."
  [& xs]
  (reify
    closure/Inputs
    (-paths [_]
      (map io/file xs))
    closure/Compilable
    (-compile [_ opts]
      (letfn [(compile-input [x]
                (let [compiled (closure/-compile x opts)]
                  (if (sequential? compiled)
                    compiled
                    [compiled])))]
        (mapcat compile-input xs)))
    (-find-sources [_ opts]
      (mapcat #(closure/-find-sources % opts) xs))))

(defn compile
  "Given a Compilable, compile it and return an IJavaScript."
  ([opts compilable]
   (compile (or (ana-api/current-state) (ana-api/empty-state opts)) opts compilable))
  ([state opts compilable]
   (ana-api/with-state state
     (closure/compile compilable opts))))

(defn output-unoptimized
  "Ensure that all JavaScript source files are on disk (not in jars),
   write the goog deps file including only the libraries that are being
   used and write the deps file for the current project.

   The deps file for the current project will include third-party
   libraries."
  [opts & sources]
  (apply closure/output-unoptimized opts sources))

(defn build
  "Given compiler options, produce runnable JavaScript. An optional source
   parameter may be provided."
  ([opts]
   (build nil opts))
  ([source opts]
   (build source opts
     (or
       (ana-api/current-state)
       (ana-api/empty-state
         ;; need to dissoc :foreign-libs since we won't know what overriding
         ;; foreign libspecs are referring to until after add-implicit-options
         ;; - David
         (closure/add-externs-sources (dissoc opts :foreign-libs))))))
  ([source opts compiler-env]
   (doseq [[unknown-opt suggested-opt] (util/unknown-opts (set (keys opts)) closure/known-opts)]
     (when suggested-opt
       (println (str "WARNING: Unknown compiler option '" unknown-opt "'. Did you mean '" suggested-opt "'?"))))
   (binding [ana/*cljs-warning-handlers* (:warning-handlers opts ana/*cljs-warning-handlers*)]
     (closure/build source opts compiler-env))))

(defn watch
  "Given a source which can be compiled, watch it for changes to produce."
  ([source opts]
   (watch source opts
     (or (ana-api/current-state)
         (ana-api/empty-state
           (closure/add-externs-sources opts)))))
  ([source opts compiler-env]
   (watch source opts compiler-env nil))
  ([source opts compiler-env stop]
   (binding [ana/*cljs-warning-handlers* (:warning-handlers opts ana/*cljs-warning-handlers*)]
     (closure/watch source opts compiler-env stop))))

;; =============================================================================
;; Node.js / NPM dependencies

(defn compiler-opts? [m]
  (and (map? m)
       (or (contains? m :output-to)
           (contains? m :modules)
           (contains? m :npm-deps)
           (contains? m :main)
           (contains? m :optimizations)
           (contains? m :foreign-libs))))

(defn install-node-deps!
  "EXPERIMENTAL: Install the supplied dependencies via NPM. dependencies must be
   a map of name to version or a valid compiler options map."
  ([dependencies]
   (if (compiler-opts? dependencies)
     (install-node-deps! (:npm-deps dependencies) dependencies)
     (install-node-deps! dependencies
       (when-let [state (ana-api/current-state)]
         (:options @state)))))
  ([dependencies opts]
   {:pre [(map? dependencies)]}
   (closure/check-npm-deps opts)
   (closure/maybe-install-node-deps!
     (update-in opts [:npm-deps] merge dependencies))))

(defn get-node-deps
  "EXPERIMENTAL: Get the Node.js dependency graph of the supplied dependencies.
   Dependencies must be a sequence of strings or symbols naming packages or paths
   within packages (e.g. [react \"react-dom/server\"] or a valid compiler options
   map. Assumes dependencies have been been previously installed, either by
   `cljs.build.api/install-node-deps!` or by an NPM client, and reside in the
   `node_modules` directory."
  ([dependencies]
   (if (compiler-opts? dependencies)
     (get-node-deps (keys (:npm-deps dependencies)) dependencies)
     (get-node-deps dependencies
       (when-let [state (ana-api/current-state)]
         (:options @state)))))
  ([dependencies opts]
   {:pre [(sequential? dependencies)]}
   (closure/index-node-modules
     (distinct (concat (keys (:npm-deps opts)) (map str dependencies)))
     opts)))

(defn node-inputs
  "EXPERIMENTAL: return the foreign libs entries as computed by running
   the module-deps package on the supplied JavaScript entry points. Assumes
   that the `@cljs-oss/module-deps` NPM package is either locally or globally
   installed."
  ([entries]
   (node-inputs entries
     (:options (or (ana-api/current-state) (ana-api/empty-state)))))
  ([entries opts]
   (closure/node-inputs entries opts)))

(defn node-modules
  "Return a sequence of requirable libraries found under node_modules."
  ([]
   (node-modules {}))
  ([opts]
   (ana-api/with-state (or (ana-api/current-state) (ana-api/empty-state opts))
     (filter :provides (closure/index-node-modules-dir)))))
