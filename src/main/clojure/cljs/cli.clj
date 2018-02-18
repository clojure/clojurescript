;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.cli
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [cljs.util :as util]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.compiler.api :as comp]
            [cljs.build.api :as build]
            [cljs.repl :as repl])
  (:import [java.io StringReader]
           [java.text BreakIterator]
           [java.util Locale]))

(declare main)

;; -----------------------------------------------------------------------------
;; Help String formatting

(def ^{:private true} help-template
  "Usage: java -cp cljs.jar cljs.main [init-opt*] [main-opt] [arg*]

With no options or args, runs an interactive Read-Eval-Print Loop

%s
For --main and --repl:

  - Enters the user namespace
  - Binds *command-line-args* to a seq of strings containing command line
    args that appear after any main option
  - Runs all init options in order
  - Calls a -main function or runs a repl or script if requested

The init options may be repeated and mixed freely, but must appear before
any main option.

Paths may be absolute or relative in the filesystem or relative to
classpath. Classpath-relative paths have prefix of @ or @/")

(defn- auto-fill
  ([ws]
   (auto-fill ws 50))
  ([^String ws max-len]
   (let [b (BreakIterator/getLineInstance Locale/ENGLISH)]
     (.setText b ws)
     (loop [s (.first b) e (.next b) line-len 0 line "" ret []]
       (if (not= e BreakIterator/DONE)
         (let [w (.substring ws s e)
               word-len (.length w)
               line-len (+ line-len word-len)]
           (if (> line-len max-len)
             (recur e (.next b) word-len w (conj ret line))
             (recur e (.next b) line-len (str line w) ret)))
         (conj ret (str line (.substring ws s (.length ws)))))))))

(defn- opt->str [cs {:keys [arg doc]}]
  (letfn [(desc-string [filled]
            (string/join "\n"
              (map #(apply str (concat (repeat 6 "     ") [%]))
                filled)))]
    (let [[f & r] cs

          fstr (cond-> (if (= 1 (count cs))
                         (str "   " f)
                         (format "%1$5s" f))
                 (not (empty? r)) (str ", " (string/join ", " r))
                 arg (str " " arg))
          filled (auto-fill doc)]
      (if (< (.length fstr) 30)
        (cond-> (str (format "%1$-30s" fstr) (first filled) "\n")
          (seq (rest filled)) (str (desc-string (rest filled)) "\n"))
        (str
          fstr "\n"
          (desc-string fstr) "\n")))))

(defn- group->str [options group]
  (let [{:keys [desc pseudos]} (get-in options [:groups group])]
    (apply str
      desc ":\n"
      (->> (:init options)
        (filter (fn [[k v]] (= (:group v) group)))
        (concat pseudos)
        (sort-by ffirst)
        (map (fn [[k v]] (opt->str k v)))))))

(defn- primary-groups-str [options]
  (str
    (group->str options ::main&compile) "\n"
    (group->str options ::main) "\n"
    (group->str options ::compile) "\n"))

(defn- all-groups-str [{:keys [groups] :as options}]
  (let [custom-groups
        (disj (set (keys groups))
          ::main&compile ::main ::compile)]
    (apply str
      (primary-groups-str options)
      (map
        (fn [group]
          (str (group->str options group) "\n"))
        custom-groups))))

(defn- main-str [options]
  (let [pseudos {["path"] {:doc "Run a script from a file or resource"}
                 ["-"] {:doc "Run a script from standard input"}}]
    (apply str
      "main options:\n"
      (->> (:main options)
        (concat pseudos)
        (sort-by ffirst)
        (remove (fn [[k v]] (nil? (ffirst k))))
        (map (fn [[k v]] (opt->str k v)))))))

(defn- options-str [options]
  (str
    (all-groups-str options)
    (main-str options)))

(declare merged-commands)

(defn help-str [repl-env]
  (format help-template
    (options-str (merged-commands repl-env))))

;; -----------------------------------------------------------------------------
;; Main

(defn- output-dir-opt
  [cfg output-dir]
  (assoc-in cfg [:options :output-dir] output-dir))

(defn- verbose-opt
  [cfg value]
  (assoc-in cfg [:options :verbose] (= value "true")))

(defn- watch-opt
  [cfg path]
  (assoc-in cfg [:options :watch] path))

(defn- optimize-opt
  [cfg level]
  (assoc-in cfg [:options :optimizations] (keyword level)))

(defn- output-to-opt
  [cfg path]
  (assoc-in cfg [:options :output-to] path))

(defn- target-opt
  [cfg target]
  (let [target (if (= "node" target) "nodejs" target)]
    (assoc-in cfg [:options :target] (keyword target))))

(defn- init-opt
  [cfg file]
  (update-in cfg [:inits]
    (fnil conj [])
    {:type :init-script
     :script file}))

(defn- eval-opt
  [cfg form-str]
  (update-in cfg [:inits]
    (fnil conj [])
    {:type :eval-forms
     :forms (ana-api/forms-seq (StringReader. form-str))}))

(defn get-dispatch
  ([commands k opt]
    (get-dispatch commands k opt nil))
  ([commands k opt default]
   (let [k' (keyword (str (name k) "-dispatch"))]
     (or (get-in commands [k' opt]) default))))

(defn initialize
  "Common initialize routine for repl, script, and null opts"
  [inits commands]
  (reduce
    (fn [ret [opt arg]]
      ((get-dispatch commands :init opt) ret arg))
    {} inits))

(defn- repl-opt
  "Start a repl with args and inits. Print greeting if no eval options were
present"
  [repl-env [_ & args] {:keys [options inits] :as cfg}]
  (let [renv (repl-env)
        opts (build/add-implicit-options
               (merge (repl/repl-options renv) options))]
    (repl/repl* renv
      (assoc opts
        :inits
        (into
          [{:type :init-forms
            :forms (when-not (empty? args)
                     [`(set! *command-line-args* (list ~@args))])}]
          inits)))))

(defn default-main
  [repl-env {:keys [main script args options inits] :as cfg}]
  (env/ensure
    (let [renv   (repl-env)
          coptsf (when-let [od (:output-dir options)]
                   (io/file od "cljsc_opts.edn"))
          opts   (as->
                   (build/add-implicit-options
                     (merge (repl/repl-options renv) options)) opts
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
            (repl/setup renv repl/*repl-opts*)
            ;; REPLs don't normally load cljs_deps.js
            (when (and coptsf (.exists coptsf))
              (let [depsf (io/file (:output-dir options) "cljs_deps.js")]
                (when (.exists depsf)
                  (repl/evaluate renv "cljs_deps.js" 1 (slurp depsf)))))
            (repl/evaluate-form renv (ana-api/empty-env) "<cljs repl>"
              (when-not (empty? args)
                `(set! *command-line-args* (list ~@args))))
            (repl/evaluate-form renv (ana-api/empty-env) "<cljs repl>"
              `(~'ns ~'cljs.user))
            (repl/run-inits renv inits)
            (when script
              (if (= "-" script)
                (repl/load-stream renv "<cljs repl>" *in*)
                (repl/load-file renv script)))
            (when main
              (repl/load-file renv (build/ns->source main))
              (repl/evaluate-form renv (ana-api/empty-env) "<cljs repl>"
                `(~(symbol (name main) "-main") ~@args)))
            (repl/tear-down renv)))))))

(defn- main-opt
  "Call the -main function from a namespace with string arguments from
  the command line."
  [repl-env [_ ns & args] cfg]
  ((::main (repl/repl-options (repl-env)) default-main)
    repl-env (merge cfg {:main ns :args args})))

(defn- null-opt
  "No repl or script opt present, just bind args and run inits"
  [repl-env args cfg]
  ((::main (repl/repl-options (repl-env)) default-main)
    repl-env (merge cfg {:args args})))

(defn- help-opt
  [repl-env _ _]
  (println (help-str repl-env)))

(defn- script-opt
  [repl-env [path & args] cfg]
  ((::main (repl/repl-options (repl-env)) default-main)
    repl-env (merge cfg {:script path :args args})))

(defn default-compile
  [repl-env {:keys [ns args options] :as cfg}]
  (let [env-opts (repl/repl-options (repl-env))
        main-ns  (symbol ns)
        opts     (as->
                   (merge
                     (select-keys env-opts [:target :browser-repl])
                     options
                     {:main main-ns}) opts
                   (cond-> opts
                     (not (:output-to opts))
                     (assoc :output-to
                       (.getPath (io/file (:output-dir opts "out") "main.js")))
                     (= :advanced (:optimizations opts))
                     (dissoc :browser-repl)))
        source   (when (= :none (:optimizations opts :none))
                   (:uri (build/ns->location main-ns)))]
    (if-let [path (:watch opts)]
      (build/watch path opts)
      (do
        (build/build source opts)
        (when (#{"-r" "--repl"} (first args))
          (repl-opt repl-env args cfg))))))

(defn- compile-opt
  [repl-env [_ ns & args] cfg]
  ((::compile (repl/-repl-options (repl-env)) default-compile)
    repl-env (merge cfg {:args args :ns ns})))

(defn get-options [commands k]
  (if (= :all k)
    (into (get-options commands :main) (get-options commands :init))
    (-> (get commands (keyword (str (name k) "-dispatch")))
      keys set)))

(defn dispatch? [commands k opt]
  (contains? (get-options commands k) opt))

(defn add-commands
  ([commands]
    (add-commands {:main-dispatch nil :init-dispatch nil} commands))
  ([commands {:keys [groups main init]}]
   (letfn [(merge-dispatch [st k options]
             (update-in st [k]
               (fn [m]
                 (reduce
                   (fn [ret [cs csm]]
                     (merge ret
                       (zipmap cs (repeat (:fn csm)))))
                   m options))))]
     (-> commands
       (update-in [:groups] merge groups)
       (update-in [:main] merge main)
       (update-in [:init] merge init)
       (merge-dispatch :init-dispatch init)
       (merge-dispatch :main-dispatch main)))))

(def default-commands
  (add-commands
    {:groups {::main&compile {:desc "init option"
                              :pseudos
                              {["-re" "--repl-env"]
                               {:doc (str "The REPL environment to use. Built-in "
                                       "supported values: nashorn, node, browser, "
                                       "rhino. Defaults to nashorn")}}}
              ::main {:desc "init options only for --main and --repl"}
              ::compile {:desc "init options only for --compile"}}
     :init
     {["-i" "--init"]          {:group ::main :fn init-opt
                                :arg "path"
                                :doc "Load a file or resource"}
      ["-e" "--eval"]          {:group ::main :fn eval-opt
                                :arg "string"
                                :doc "Evaluate expressions in string; print non-nil values"}
      ["-v" "--verbose"]       {:group ::main :fn verbose-opt
                                :arg "bool"
                                :doc "If true, will enable ClojureScript verbose logging"}
      ["-d" "--output-dir"]    {:group ::main&compile :fn output-dir-opt
                                :arg "path"
                                :doc (str "Set the output directory to use. If "
                                       "supplied, cljsc_opts.edn in that directory "
                                       "will be used to set ClojureScript compiler "
                                       "options") }
      ["-w" "--watch"]         {:group ::compile :fn watch-opt
                                :arg "path"
                                :doc "Continuously build, only effective with -c main option"}
      ["-o" "--output-to"]     {:group ::compile :fn output-to-opt
                                :arg "file"
                                :doc "Set the output compiled file"}
      ["-O" "--optimizations"] {:group ::compile :fn optimize-opt
                                :arg "level"
                                :doc
                                (str "Set optimization level, only effective with "
                                  "-c main option. Valid values are: none, "
                                  "whitespace, simple, advanced")}

      ["-t" "--target"]        {:group ::main&compile :fn target-opt
                                :arg "name"
                                :doc
                                (str "The JavaScript target. Supported values: "
                                     "nodejs, nashorn, webworker") }}
     :main
     {["-r" "--repl"]          {:fn repl-opt
                                :doc "Run a repl"}
      ["-m" "--main"]          {:fn main-opt
                                :arg "ns"
                                :doc "Call the -main function from a namespace with args"}
      ["-c" "--compile"]       {:fn compile-opt
                                :arg "ns"
                                :doc (str "Compile a namespace. If -r / --repl present after "
                                       "namespace will launch a REPL after the compile completes")}
      [nil]                    {:fn null-opt}
      ["-h" "--help" "-?"]     {:fn help-opt
                                :doc "Print this help message and exit"}}}))

(defn normalize [commands args]
  (if (not (contains? (get-options commands :main) (first args)))
    (let [pred (complement #{"-v" "--verbose"})
          [pre post] ((juxt #(take-while pred %)
                            #(drop-while pred %))
                       args)]
      (cond
        (= pre args) pre

        (contains? (get-options commands :all) (fnext post))
        (concat pre [(first post) "true"]
          (normalize commands (next post)))

        :else
        (concat pre [(first post) (fnext post)]
          (normalize commands (nnext post)))))
    args))

(defn merged-commands [repl-env]
  (add-commands default-commands
    (::commands (repl/repl-options (repl-env)))))

(defn main
  "A generic runner for ClojureScript. repl-env must satisfy
  cljs.repl/IReplEnvOptions and cljs.repl/IJavaScriptEnv protocols. args is a
  sequence of command line flags."
  [repl-env & args]
  (try
    (let [commands (merged-commands repl-env)]
      (if args
        (loop [[opt arg & more :as args] (normalize commands args) inits []]
          (if (dispatch? commands :init opt)
            (recur more (conj inits [opt arg]))
            ((get-dispatch commands :main opt script-opt)
              repl-env args (initialize inits commands))))
        (repl-opt repl-env nil nil)))
    (finally
      (flush))))
