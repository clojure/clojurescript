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
  [cfg output-dir]
  (assoc-in cfg [:options :output-dir] output-dir))

(defn verbose-opt
  [cfg value]
  (assoc-in cfg [:options :verbose] (= value "true")))

(defn watch-opt
  [cfg path]
  (assoc-in cfg [:options :watch] path))

(defn optimize-opt
  [cfg level]
  (assoc-in cfg [:options :optimizations] (keyword level)))

(defn output-to-opt
  [cfg path]
  (assoc-in cfg [:options :output-to] path))

(defn target-opt
  [cfg target]
  (assoc-in cfg [:options :target] (keyword target)))

(defn init-opt
  [cfg file]
  (update-in cfg [:inits]
    (fnil conj [])
    {:type :init-script
     :script file}))

(defn eval-opt
  [cfg form-str]
  (update-in cfg [:inits]
    (fnil conj [])
    {:type :eval-forms
     :forms (ana-api/forms-seq (StringReader. form-str))}))

(def init-dispatch
  (atom
    {"-i"              {:fn init-opt}
     "--init"          {:fn init-opt}
     "-e"              {:fn eval-opt}
     "--eval"          {:fn eval-opt}
     "-v"              {:fn verbose-opt}
     "--verbose"       {:fn verbose-opt}
     "-d"              {:fn output-dir-opt}
     "--output-dir"    {:fn output-dir-opt}
     "-o"              {:fn output-to-opt}
     "--output-to"     {:fn output-to-opt}
     "-t"              {:fn target-opt}
     "--target"        {:fn target-opt}
     "-O"              {:fn optimize-opt}
     "--optimizations" {:fn optimize-opt}
     "-w"              {:fn watch-opt}
     "--watch"         {:fn watch-opt}}))

(defn- initialize
  "Common initialize routine for repl, script, and null opts"
  [inits]
  (reduce
    (fn [ret [opt arg]]
      ((:fn (@init-dispatch opt)) ret arg))
    {} inits))

(defn repl-opt
  "Start a repl with args and inits. Print greeting if no eval options were
present"
  [repl-env [_ & args] {:keys [options inits] :as cfg}]
  (let [renv (repl-env)
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
  [repl-env {:keys [main script args options inits] :as cfg}]
  (env/ensure
    (let [renv   (repl-env)
          coptsf (when-let [od (:output-dir options)]
                   (io/file od "cljsc_opts.edn"))
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
  [repl-env [_ ns & args] cfg]
  ((::main (repl/-repl-options (repl-env)) main-opt*)
    repl-env (merge cfg {:main ns :args args})))

(defn- null-opt
  "No repl or script opt present, just bind args and run inits"
  [repl-env args cfg]
  ((::main (repl/-repl-options (repl-env)) main-opt*)
    repl-env (merge cfg {:args args})))

(defn- help-opt
  [_ _ _]
  (println (:doc (meta (var main)))))

(defn script-opt
  [repl-env [path & args] cfg]
  ((::main (repl/-repl-options (repl-env)) main-opt*)
    repl-env (merge cfg {:script path :args args})))

(defn compile-opt*
  [repl-env {:keys [ns args options] :as cfg}]
  (let [env-opts (repl/-repl-options (repl-env))
        main-ns  (symbol ns)
        opts     (merge
                   (select-keys env-opts [:target :browser-repl])
                   options
                   {:main main-ns})
        source   (when (= :none (:optimizations opts :none))
                   (:uri (build/ns->location main-ns)))]
    (if-let [path (:watch opts)]
      (build/watch path opts)
      (build/build source opts))))

(defn compile-opt
  [repl-env [_ ns & args] cfg]
  ((::compile (repl/-repl-options (repl-env)) compile-opt*)
    repl-env (merge cfg {:args args :ns ns})))

(def main-dispatch
  (atom
    {"-r"        {:fn repl-opt}
     "--repl"    {:fn repl-opt}
     "-m"        {:fn main-opt}
     "--main"    {:fn main-opt}
     "-c"        {:fn compile-opt}
     "--compile" {:fn compile-opt}
     nil         {:fn null-opt}
     "-h"        {:fn help-opt}
     "--help"    {:fn help-opt}
     "-?"        {:fn help-opt}}))

(defn normalize [args]
  (if (not (contains? (set (keys @main-dispatch)) (first args)))
    (let [pred (complement #{"-v" "--verbose"})
          [pre post] ((juxt #(take-while pred %)
                            #(drop-while pred %))
                       args)]
      (cond
        (= pre args) pre

        (contains?
          (into (set (keys @main-dispatch))
                     (keys @init-dispatch)) (fnext post))
        (concat pre [(first post) "true"]
          (normalize (next post)))

        :else
        (concat pre [(first post) (fnext post)]
          (normalize (nnext post)))))
    args))

;; TODO: validate arg order to produce better error message - David
(defn main
  "Usage: java -cp cljs.jar cljs.main [init-opt*] [main-opt] [arg*]

  With no options or args, runs an interactive Read-Eval-Print Loop

  init options:
    -t, --target name        The JavaScript target. Supported values: nodejs,
                             nashorn, webworker

  init options only for --main and --repl:
    -re, --repl-env          The REPL environment to use. Built-in supported
                             values: nashorn, node, browser, rhino. Defaults to
                             nashorn
    -i,  --init path         Load a file or resource
    -e,  --eval string       Evaluate expressions in string; print non-nil values
    -v,  --verbose bool      if true, will enable ClojureScript verbose logging
    -d,  --output-dir path   Set the output directory to use. If supplied,
                             cljsc_opts.edn in that directory will be used to
                             set ClojureScript compiler options

  init options only for --compile:
    -o,  --output-to         Set the output compiled file
    -O,  --optimizations     Set optimization level, only effective with -c main
                             option. Valid values are: none, whitespace, simple,
                             advanced
    -w,  --watch path        Continuously build, only effect with -c main option

  main options:
    -m, --main ns-name       Call the -main function from a namespace with args
    -r, --repl               Run a repl
    -c, --compile ns-name    Compile a namespace
    path                     Run a script from a file or resource
    -                        Run a script from standard input
    -h, -?, --help           Print this help message and exit

  For --main and --repl:

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
  ;; On OS X suppress the Dock icon
  (System/setProperty "apple.awt.UIElement" "true")
  (java.awt.Toolkit/getDefaultToolkit)
  (try
    (if args
      (loop [[opt arg & more :as args] (normalize args) inits []]
        (if (contains? @init-dispatch opt)
          (recur more (conj inits [opt arg]))
          ((:fn (@main-dispatch opt script-opt)) repl-env args
            (initialize inits))))
      (repl-opt repl-env nil nil))
    (finally
      (flush))))
