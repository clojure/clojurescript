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

(defn output-dir-opt
  [inits-map output-dir]
  (assoc-in inits-map [:options :output-dir] output-dir))

(defn verbose-opt
  [inits-map value]
  (assoc-in inits-map [:options :verbose] (= value "true")))

(defn init-opt
  [inits-map file]
  (update-in inits-map [:inits]
    (fnil conj [])
    {:type :init-script
     :script file}))

(defn eval-opt
  [inits-map form-str]
  (update-in inits-map [:inits]
    (fnil conj [])
    {:type :eval-forms
     :forms (ana-api/forms-seq (StringReader. form-str))}))

(defn init-dispatch
  "Returns the handler associated with an init opt"
  [opt]
  ({"-i"           init-opt
    "--init"       init-opt
    "-e"           eval-opt
    "--eval"       eval-opt
    "-v"           verbose-opt
    "--verbose"    verbose-opt
    "-o"           output-dir-opt
    "--output-dir" output-dir-opt} opt))

(defn- initialize
  "Common initialize routine for repl, script, and null opts"
  [inits]
  (reduce
    (fn [ret [opt arg]]
      ((init-dispatch opt) ret arg))
    {} inits))

(defn repl-opt
  "Start a repl with args and inits. Print greeting if no eval options were
present"
  [repl-env [_ & args] inits]
  (let [{:keys [options inits]} (initialize inits)
        renv (repl-env)
        opts (build/add-implicit-options
               (merge (repl/-repl-options renv) options))]
    (repl/repl* renv
      (assoc opts
        :inits
        (into
          [{:type :init-forms
            :forms (when-not (empty? args)
                     [`(set! *command-line-args* (list ~@args))])}]
          inits)))))

(defn main-opt*
  [repl-env {:keys [main script args inits]}]
  (env/ensure
    (let [{:keys [options inits]} (initialize inits)
          renv   (repl-env)
          coptsf (when-let [od (:output-dir options)]
                   (io/file od ".cljsc_opts"))
          opts   (as->
                   (build/add-implicit-options
                     (merge (repl/-repl-options renv) options)) opts
                   (let [copts (when (and coptsf (.exists coptsf))
                                 (-> (edn/read-string (slurp coptsf))
                                   ;; need to remove the entry point bits,
                                   ;; user is trying load some arbitrary ns
                                   (dissoc :main)
                                   (dissoc :output-to)))]
                     (merge copts opts)))]
      (binding [ana/*cljs-ns*    'cljs.user
                repl/*repl-opts* opts
                ana/*verbose*    (:verbose opts)]
        (when ana/*verbose*
          (util/debug-prn "Compiler options:" repl/*repl-opts*))
        (comp/with-core-cljs repl/*repl-opts*
          (fn []
            (repl/-setup renv repl/*repl-opts*)
            ;; REPLs don't normally load cljs_deps.js
            (when (and coptsf (.exists coptsf))
              (let [depsf (io/file (:output-dir options) "cljs_deps.js")]
                (when (.exists depsf)
                  (repl/-evaluate renv "cljs_deps.js" 1 (slurp depsf)))))
            (repl/evaluate-form renv (ana/empty-env) "<cljs repl>"
              (when-not (empty? args)
                `(set! *command-line-args* (list ~@args))))
            (repl/evaluate-form renv (ana/empty-env) "<cljs repl>"
              `(~'ns ~'cljs.user))
            (repl/run-inits renv inits)
            (when script
              (if (= "-" script)
                (repl/load-stream renv "<cljs repl>" *in*)
                (repl/load-file renv script)))
            (when main
              (repl/load-file renv (build/ns->source main))
              (repl/evaluate-form renv (ana/empty-env) "<cljs repl>"
                `(~(symbol (name main) "-main") ~@args)))
            (repl/-tear-down renv)))))))

(defn main-opt
  "Call the -main function from a namespace with string arguments from
  the command line."
  [repl-env [_ main-ns & args] inits]
  (main-opt* repl-env
    {:main  main-ns
     :args  args
     :inits inits}))

(defn- null-opt
  "No repl or script opt present, just bind args and run inits"
  [repl-env args inits]
  (main-opt* repl-env
    {:args args
     :inits inits}))

(defn- help-opt
  [_ _ _]
  (println (:doc (meta (var main)))))

(defn script-opt
  [repl-env [path & args] inits]
  (main-opt* repl-env
    {:script path
     :args   args
     :inits  inits}))

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
    (partial script-opt repl-env)))

(def main-opts
  #{"-r" "--repl"
    "-m" "--main"
    nil})

(def valid-opts
  (into main-opts
    #{"-i" "--init"
      "-e" "--eval"
      "-v" "--verbose"
      "-o" "--output-dir"
      "-h" "--help" "-?"}))

(defn normalize [args]
  (when (seq args)
    (let [pred (complement #{"-v" "--verbose"})
          [pre post] ((juxt #(take-while pred %)
                            #(drop-while pred %))
                       args)]
      (concat pre
        (if (contains? valid-opts (fnext post))
          (concat pre [(first post) "true"]
            (normalize (next post)))
          (concat pre [(first post) (fnext post)]
            (normalize (nnext post))))))))

;; TODO: validate arg order to produce better error message - David
(defn main
  "Usage: java -cp cljs.jar cljs.main [init-opt*] [main-opt] [arg*]

  With no options or args, runs an interactive Read-Eval-Print Loop

  init options:
    -js, --js-engine engine  The JavaScript engine to use. Built-in supported
                             engines: nashorn, node, browser, rhino. Defaults to
                             nashorn
    -i,  --init path         Load a file or resource
    -e,  --eval string       Evaluate expressions in string; print non-nil values
    -v,  --verbose bool      if true, will enable ClojureScript verbose logging
    -o,  --output-dir path   Set the output directory to use. If supplied, .cljsc_opts
                             in that directory will be used to set ClojureScript
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
  any main option.

  Paths may be absolute or relative in the filesystem or relative to
  classpath. Classpath-relative paths have prefix of @ or @/"
  [repl-env & args]
  (try
    (if args
      (loop [[opt arg & more :as args] args inits []]
        (if (init-dispatch opt)
          (recur more (conj inits [opt arg]))
          ((main-dispatch repl-env opt) args inits)))
      (repl-opt repl-env nil nil))
    (finally
      (flush))))
