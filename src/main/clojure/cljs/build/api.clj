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
  (:require [cljs.util :as util]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.closure :as closure]
            [clojure.set :refer [intersection]]
            [cljs.js-deps :as js-deps]
            [clojure.java.io :as io])
  (:import java.io.File))

;; =============================================================================
;; Useful Utilities

(defn ^File target-file-for-cljs-ns
  "Given an output directory and a clojurescript namespace return the
  compilation target file for that namespace.

  For example:
  (target-file-from-cljs-ns \"resources/out\" 'example.core) ->
  <File: \"resources/out/example/core.js\">"
  ([ns-sym] (target-file-for-cljs-ns ns-sym nil))
  ([ns-sym output-dir]
    (util/to-target-file
      (util/output-directory {:output-dir output-dir})
      {:ns ns-sym})))

(defn mark-cljs-ns-for-recompile!
  "Backdates a cljs target file so that it the cljs compiler will recompile it."
  ([ns-sym] (mark-cljs-ns-for-recompile! ns-sym nil))
  ([ns-sym output-dir]
    (let [s (target-file-for-cljs-ns output-dir ns-sym)]
      (when (.exists s)
        (.setLastModified s 5000)))))

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
  ([namespaces] (cljs-dependents-for-macro-namespaces env/*compiler* namespaces))
  ([state namespaces]
   (map :name
        (let [namespaces-set (set namespaces)]
          (filter (fn [x] (not-empty
                            (intersection namespaces-set (-> x :require-macros vals set))))
                  (vals (:cljs.analyzer/namespaces @state)))))))

(defn cljs-ns-dependents
  "Given a namespace symbol return a seq of all dependent
  namespaces sorted in dependency order. Will include
  transient dependents."
  ([ns] (cljs-ns-dependents env/*compiler* ns))
  ([state ns]
   (env/with-compiler-env state
     (ana/ns-dependents ns))))

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
  ([src opts] (src-file->target-file env/*compiler* src opts))
  ([state src opts]
   (env/with-compiler-env state
     (binding [ana/*cljs-warning-handlers* (:warning-handlers opts ana/*cljs-warning-handlers*)]
       (closure/src-file->target-file src opts)))))

(defn ^String src-file->goog-require
  "Given a ClojureScript or Google Closure style JavaScript source file return
  the goog.require statement for it."
  ([src] (src-file->goog-require src nil))
  ([src options] (src-file->goog-require env/*compiler* src options))
  ([state src options]
   (env/with-compiler-env state
     (binding [ana/*cljs-warning-handlers* (:warning-handlers options ana/*cljs-warning-handlers*)]
       (closure/src-file->goog-require src options)))))

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
  ([ns] (ns->location ns env/*compiler*))
  ([ns compiler-env]
   (closure/source-for-namespace ns compiler-env)))

(defn add-dependencies
  "Given one or more IJavaScript objects in dependency order, produce
  a new sequence of IJavaScript objects which includes the input list
  plus all dependencies in dependency order."
  [opts & ijss]
  (closure/add-dependencies opts ijss))

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
        (mapcat compile-input xs)))))

(defn compile
  "Given a Compilable, compile it and return an IJavaScript."
  ([opts compilable] (compile env/*compiler* opts compilable))
  ([state opts compilable]
   (env/with-compiler-env state
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
  "Given a source which can be compiled, produce runnable JavaScript."
  ([source opts]
   (build source opts nil))
  ([source opts compiler-env]
   (binding [ana/*cljs-warning-handlers* (:warning-handlers opts ana/*cljs-warning-handlers*)]
     (closure/build source opts compiler-env))))

(defn watch
  "Given a source which can be compiled, watch it for changes to produce."
  ([source opts]
   (watch source opts (if-not (nil? env/*compiler*)
                        env/*compiler*
                        (env/default-compiler-env opts))))
  ([source opts compiler-env]
   (watch source opts compiler-env nil))
  ([source opts compiler-env stop]
   (binding [ana/*cljs-warning-handlers* (:warning-handlers opts ana/*cljs-warning-handlers*)]
     (closure/watch source opts compiler-env stop))))

(comment

  (def test-cenv (atom {}))
  (def test-env (assoc-in (ana/empty-env) [:ns :name] 'cljs.user))

  (binding [ana/*cljs-ns* 'cljs.user]
    (env/with-compiler-env test-cenv
      (ana/no-warn
        (ana/analyze test-env
         '(ns cljs.user
            (:use [clojure.string :only [join]]))))))

  (env/with-compiler-env test-cenv
    (ns-dependents 'clojure.string))

  (map
    #(target-file-for-cljs-ns % "out-dev")
    (env/with-compiler-env test-cenv
     (ns-dependents 'clojure.string)))
  )
