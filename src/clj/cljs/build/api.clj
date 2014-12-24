;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software
(ns cljs.build.api
  "This is intended to be a stable api for those who intend to create
  tools that use compiler data.

  For example: a build script may need to how to invalidate compiled
  files so that they will be recompiled."
  (:require [cljs.util :as util]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.closure]
            [clojure.set :refer [intersection]])
  (:import java.io.File))

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
  ('example.core 'example.util)

  This must be called when cljs.env/*compiler* is bound to the
  compile env that you are inspecting. See cljs.env/with-compile-env."
  [namespaces]
  (map :name
       (let [namespaces-set (set namespaces)]
         (filter (fn [x] (not-empty
                         (intersection namespaces-set (-> x :require-macros vals set))))
                 (vals (:cljs.analyzer/namespaces @env/*compiler*))))))

(defn cljs-ns-dependents
  "Given a namespace symbol return a seq of all dependent
  namespaces sorted in dependency order. Will include
  transient dependents."
  [ns]
  (ana/ns-dependents ns))

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