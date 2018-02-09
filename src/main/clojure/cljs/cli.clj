;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.cli
  (:require [clojure.java.io :as io]
            [cljs.util :as util]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.compiler :as comp]
            [cljs.repl :as repl]
            [cljs.build.api :as build]
            [clojure.edn :as edn])
  (:import [java.io StringReader]))

(declare main)

(def ^:dynamic *cli-opts* nil)

(defn output-dir-opt
  [repl-env output-dir]
  (set! *cli-opts* (merge *cli-opts* {:output-dir output-dir})))

(defn verbose-opt
  [repl-env value]
  (set! *cli-opts* (merge *cli-opts* {:verbose (or (= value "true") false)})))

(defn- init-opt
  [repl-env file]
  (set! *cli-opts* (merge *cli-opts* {:init-script file})))

(defn- eval-opt
  [repl-env form-str]
  (set! *cli-opts*
    (merge *cli-opts* {:eval-forms (ana-api/forms-seq (StringReader. form-str))})))

;; TODO: need support for feature that init options like -e and -i can appear
;; multiple times interleaved - David

(defn init-dispatch
  "Returns the handler associated with an init opt"
  [repl-env opt]
  ({"-i"           (partial init-opt repl-env)
    "--init"       (partial init-opt repl-env)
    "-e"           (partial eval-opt repl-env)
    "--eval"       (partial eval-opt repl-env)
    "-v"           (partial verbose-opt repl-env)
    "--verbose"    (partial verbose-opt repl-env)
    "-o"           (partial output-dir-opt repl-env)
    "--output-dir" (partial output-dir-opt repl-env)} opt))

(defn- initialize
  "Common initialize routine for repl, script, and null opts"
  [repl-env inits]
  (doseq [[opt arg] inits]
    ((init-dispatch repl-env opt) arg)))

(defn repl-opt
  "Start a repl with args and inits. Print greeting if no eval options were
present"
  [repl-env [_ & args] inits]
  ;; TODO: handle eval forms
  (repl/repl* (repl-env) (build/add-implicit-options *cli-opts*)))

(defn main-opt
  "Call the -main function from a namespace with string arguments from
  the command line."
  [repl-env [_ main-ns & args] inits]
  (env/ensure
    (initialize repl-env inits)
    (let [renv   (repl-env)
          opts   *cli-opts*
          coptsf (when-let [od (:output-dir opts)]
                   (io/file od ".cljsc_opts"))]
      (binding [repl/*repl-opts*
                (as->
                  (build/add-implicit-options
                    (merge (repl/-repl-options renv) opts)) opts
                  (let [copts (when (and coptsf (.exists coptsf))
                                (-> (edn/read-string (slurp coptsf))
                                  ;; need to remove the entry point bits,
                                  ;; user is trying load some arbitrary ns
                                  (dissoc :main)
                                  (dissoc :output-to)))]
                    (merge copts opts)))
                ana/*verbose* (:verbose repl/*repl-opts*)]
        (when ana/*verbose*
          (util/debug-prn "Compiler options:" repl/*repl-opts*))
        (comp/with-core-cljs repl/*repl-opts*
          (fn []
            (repl/-setup renv (merge (repl/-repl-options renv) repl/*repl-opts*))
            ;; REPLs don't normally load cljs_deps.js
            (when (and coptsf (.exists coptsf))
              (let [depsf (io/file (:output-dir opts) "cljs_deps.js")]
                (when (.exists depsf)
                  (repl/-evaluate renv "cljs_deps.js" 1 (slurp depsf)))))
            (doseq [form (:eval-forms repl/*repl-opts*)]
              (println (repl/evaluate-form renv (ana/empty-env) "<cljs repl>" form)))
            (when-let [init-script (:init-script opts)]
              (repl/load-file renv init-script))
            (when main-ns
              (ana-api/analyze-file (build/ns->source main-ns) opts)
              (repl/evaluate-form renv (ana/empty-env) "<cljs repl>"
                `(do
                   (set! *command-line-args* (list ~@args))
                   (.require js/goog ~(-> main-ns munge str))
                   (~(symbol (name main-ns) "-main") ~@args))))
            (repl/-tear-down renv)))))))

(defn- null-opt
  "No repl or script opt present, just bind args and run inits"
  [repl-env args inits]
  (initialize repl-env inits))

(defn- help-opt
  [_ _ _]
  (println (:doc (meta (var main)))))

(defn main-dispatch
  "Returns the handler associated with a main option"
  [repl-env opt]
  ({"-r"     (partial repl-opt repl-env)
    "--repl" (partial repl-opt repl-env)
    "-m"     (partial main-opt repl-env)
    "--main" (partial main-opt repl-env)
    nil      (partial null-opt repl-env)
    "-h"     (partial help-opt repl-env)
    "--help" (partial help-opt repl-env)
    "-?"     (partial help-opt repl-env)} opt
    ;script-opt
    ))

(defn adapt-args [args]
  (cond-> args
    (and (some #{"-e" "--eval"} args)
         (not (some #{"-m" "--main"} args)))
    (concat ["-m"])))

;; TODO: validate arg order to produce better error message - David
(defn main
  "Usage: java -cp cljs.jar clojure.main -m REPL-NS [init-opt*] [main-opt] [arg*]

  REPL-NS is any Clojure namespace that supplies a -main that builds a
  ClojureScript REPL. Note that cljs.repl.node, cljs.repl.browser, cljs.repl.rhino
  and cljs.repl.nashorn ship with ClojureScript.

  With no options or args, runs an interactive Read-Eval-Print Loop

  init options:
    -i, --init path        Load a file or resource
    -e, --eval string      Evaluate expressions in string; print non-nil values
    -v, --verbose bool     if true, will enable ClojureScriptt verbose logging
    -o, --output-dir path  Set the output directory to use. If supplied, .cljsc_opts
                           in that direction will be used to set ClojureScript
                           compiler options.

  main options:
    -m, --main ns-name     Call the -main function from a namespace with args
    -r, --repl             Run a repl
    path                   Run a script from a file or resource
    -                      Run a script from standard input
    -h, -?, --help         Print this help message and exit

  operation:

    - Enters the user namespace
    - Binds *command-line-args* to a seq of strings containing command line
      args that appear after any main option
    - Runs all init options in order
    - Calls a -main function or runs a repl or script if requested

  The init options may be repeated and mixed freely, but must appear before
  any main option. The appearance of any eval option before running a repl
  suppresses the usual repl greeting message: \"Clojure ~(clojure-version)\".

  Paths may be absolute or relative in the filesystem or relative to
  classpath. Classpath-relative paths have prefix of @ or @/"
  [repl-env & args]
  (binding [*cli-opts* {}]
    (try
      (if args
        (let [args' (adapt-args args)]
          (loop [[opt arg & more :as args] args' inits []]
            (if (init-dispatch repl-env opt)
              (recur more (conj inits [opt arg]))
              ((main-dispatch repl-env opt) args inits))))
        (repl-opt repl-env nil nil))
      (finally
        (flush)))))
