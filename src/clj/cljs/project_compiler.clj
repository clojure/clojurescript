;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(set! *warn-on-reflection* true)

(ns cljs.project-compiler
  (:refer-clojure :exclude [munge load-file loaded-libs macroexpand-1])
  (:require [cljs.compiler :as comp])
  (:import java.io.File
           com.google.javascript.jscomp.CommandLineRunner))

(def default-output-dir "out")

(def required-goog (map comp/to-path [["base"]
                                      ["string" "string"]
                                      ["useragent" "jscript"]
                                      ["string" "stringbuffer"]
                                      ["object" "object"]]))

(defn cljs-home-relative
  [^String cljs-home ^String path]
  (if (.endsWith cljs-home File/separator)
    (str cljs-home path)
    (str cljs-home File/separator path)))

(defn default-args
  [cljs-home]
  (concat
   (mapcat #(vector "--js" (cljs-home-relative cljs-home (comp/to-path ["closure" "library" "closure" "goog" (str % ".js")])))
           required-goog)
   ["--js" (cljs-home-relative cljs-home "core.js")]))

(defn- dfs-visit
  [state ns-name]
  (let [file (get state ns-name)]
    (if (:visited file)
      state
      (let [state (assoc-in state [ns-name :visited] true)
            deps (:requires file)
            state (reduce dfs-visit state deps)]
        (assoc state :order (conj (:order state) file))))))

(defn sort-by-dep
  "Perform a depth-first search of the dependency graph returning a
  list of file names in dependency order."
  [files]
  (let [state (reduce #(assoc %1 (:ns %2) %2) {} files)]
    (map :file-name (:order (reduce dfs-visit (assoc state :order []) (map :ns files))))))

(defn- to-args
  "Create a list of js args for gclosure compiler."
  [file-info]
  (map #(str "--js=" %) (sort-by-dep file-info)))

(defn- non-gclosure-arg?
  [^String arg]
  (not (.startsWith arg "--")))

(defn output-directory
  "Get the output directory form a list of arguments. If it exists, it
  will be the first argument and will not be preceded by --."
  [args]
  (if-let [first-arg (first args)]
    (if (non-gclosure-arg? first-arg)
      first-arg
      default-output-dir)
    default-output-dir))

(defn compile-project
  "Wraps gclosure compiler adding the initial step of compiling all
  cljs files to JavaScript. The first and second arguments are the
  ClojureScript home directory and the root directory which contains
  all cljs source files to compile.

  An optional third argument is the name of the directory where output
  js files will be written. This argument defaults to 'out'.

  Any additional arguments must be valid gclosure compiler arguments
  and will be passed through.

  CommandLineRunner/main finishes by calling System/exit which makes
  this unusable from the repl."
  [cs-home src-dir & args]
  (let [out-dir (output-directory args)
        compiled-info (comp/compile-root src-dir out-dir)]
    (CommandLineRunner/main (into-array (concat (drop-while non-gclosure-arg? args)
                                                (default-args cs-home)
                                                (to-args compiled-info))))))
