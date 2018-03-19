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
  (:import [java.io File StringReader FileWriter]
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

  - Enters the cljs.user namespace
  - Binds *command-line-args* to a seq of strings containing command line
    args that appear after any main option
  - Runs all init options in order
  - Calls a -main function or runs a repl or script if requested

The init options may be repeated and mixed freely, but must appear before
any main option.

In the case of --compile you may supply --repl or --serve options afterwards.

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
  (when-not (.exists (io/file path))
    (if (or (string/starts-with? path "-")
            (string/blank? path))
      (throw
        (ex-info
          (str "Missing watch path")
          {:cljs.main/error :invalid-arg}))
      (throw
        (ex-info
          (str "Watch path " path " does not exist")
          {:cljs.main/error :invalid-arg}))))
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

(defn missing-file [x]
  (throw
    (ex-info
      (str "File " x " does not exist")
      {:cljs.main/error :invalid-arg})))

(defn missing-resource [x]
  (throw
    (ex-info
      (str "Resource "
        (if (string/starts-with? x "@/")
          (subs x 2)
          (subs x 1))
        " does not exist")
      {:cljs.main/error :invalid-arg})))

(defn read-edn-opts [str]
  (letfn [(read-rsrc [rsrc-str orig-str]
            (if-let [rsrc (io/resource rsrc-str)]
              (edn/read-string (slurp rsrc))
              (missing-resource orig-str)))]
    (cond
     (string/starts-with? str "@/") (read-rsrc (subs str 2) str)
     (string/starts-with? str "@") (read-rsrc (subs str 1) str)
     :else
     (let [f (io/file str)]
       (if (.exists f)
         (edn/read-string (slurp f))
         (missing-file str))))))

(defn load-edn-opts [str]
  (reduce merge {} (map read-edn-opts (string/split str #":"))))

(defn- repl-env-opts-opt
  [cfg ropts]
  (let [ropts (string/trim ropts)
        edn   (if (string/starts-with? ropts "{")
                (edn/read-string ropts)
                (load-edn-opts ropts))]
    (update cfg :repl-env-options merge edn)))

(defn- compile-opts-opt
  [cfg copts]
  (let [copts (string/trim copts)
        edn   (if (string/starts-with? copts "{")
                (edn/read-string copts)
                (load-edn-opts copts))]
    (update cfg :options merge edn)))

(defn- init-opt
  [cfg file]
  (let [file' (cond
                (string/starts-with? file "@/")
                (io/resource (subs file 2))
                (string/starts-with? file "@")
                (io/resource (subs file 1))
                :else
                (let [f (io/file file)]
                  (if (.exists f)
                    f
                    (missing-file file))))]
    (when-not file'
      (missing-resource file))
    (update-in cfg [:inits]
      (fnil conj [])
      {:type :init-script
       :script file'})))

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

(defn dissoc-entry-point-opts
  "Dissoc the entry point options from the input. Necessary when the user
is trying load some arbitrary ns."
  [opts]
  (dissoc opts :main :output-to))

(defn temp-out-dir []
  (let [f (File/createTempFile "out" (Long/toString (System/nanoTime)))]
    (.delete f)
    (util/mkdirs f)
    (util/path f)))

(defn- repl-opt
  "Start a repl with args and inits. Print greeting if no eval options were
present"
  [repl-env [_ & args] {:keys [repl-env-options options inits] :as cfg}]
  (let [opts   (cond-> options
                 (not (:output-dir options))
                 (assoc :output-dir (temp-out-dir) :temp-output-dir? true)
                 (not (contains? options :aot-cache))
                 (assoc :aot-cache true))
        reopts (merge repl-env-options (select-keys opts [:output-to :output-dir]))
        _      (when (or ana/*verbose* (:verbose opts))
                 (util/debug-prn "REPL env options:" (pr-str reopts)))
        renv   (apply repl-env (mapcat identity reopts))]
    (repl/repl* renv
      (assoc (dissoc-entry-point-opts opts)
        :inits
        (into
          [{:type :init-forms
            :forms (when-not (empty? args)
                     [`(set! *command-line-args* (list ~@args))])}]
          inits)))))

(defn default-main
  [repl-env {:keys [main script args repl-env-options options inits] :as cfg}]
  (env/ensure
    (let [opts   (cond-> options
                   (not (:output-dir options))
                   (assoc :output-dir (temp-out-dir) :temp-output-dir? true)
                   (not (contains? options :aot-cache))
                   (assoc :aot-cache true))
          reopts (merge repl-env-options
                   (select-keys opts [:output-to :output-dir]))
          _      (when (or ana/*verbose* (:verbose opts))
                   (util/debug-prn "REPL env options:" (pr-str reopts)))
          renv   (apply repl-env (mapcat identity reopts))
          coptsf (when-let [od (:output-dir opts)]
                   (io/file od "cljsc_opts.edn"))
          copts  (when (and coptsf (.exists coptsf))
                   (-> (edn/read-string (slurp coptsf))
                       (dissoc-entry-point-opts)))
          opts   (merge copts
                   (build/add-implicit-options
                     (merge (repl/repl-options renv) opts)))]
      (binding [ana/*cljs-ns*    'cljs.user
                repl/*repl-opts* opts
                ana/*verbose*    (:verbose opts)
                repl/*repl-env*  renv]
        (when ana/*verbose*
          (util/debug-prn "Compiler options:" (pr-str repl/*repl-opts*)))
        (comp/with-core-cljs repl/*repl-opts*
          (fn []
            (try
              (repl/setup renv repl/*repl-opts*)
              ;; REPLs don't normally load cljs_deps.js
              (when (and coptsf (.exists coptsf))
                (let [depsf (io/file (:output-dir opts) "cljs_deps.js")]
                  (when (.exists depsf)
                    (repl/evaluate renv "cljs_deps.js" 1 (slurp depsf)))))
              (repl/evaluate-form renv (ana-api/empty-env) "<cljs repl>"
                (when-not (empty? args)
                  `(set! *command-line-args* (list ~@args))))
              (repl/evaluate-form renv (ana-api/empty-env) "<cljs repl>"
                `(~'ns ~'cljs.user))
              (repl/run-inits renv inits)
              (when script
                (cond
                  (= "-" script)
                  (repl/load-stream renv "<cljs repl>" *in*)

                  (.exists (io/file script))
                  (with-open [stream (io/reader script)]
                    (repl/load-stream renv script stream))

                  (string/starts-with? script "@/")
                  (if-let [rsrc (io/resource (subs script 2))]
                    (repl/load-stream renv (util/get-name rsrc) rsrc)
                    (missing-resource script))

                  (string/starts-with? script "@")
                  (if-let [rsrc (io/resource (subs script 1))]
                    (repl/load-stream renv (util/get-name rsrc) rsrc)
                    (missing-resource script))

                  (string/starts-with? script "-")
                  (throw
                    (ex-info
                      (str "Expected script or -, got flag " script " instead")
                      {:cljs.main/error :invalid-arg}))

                  :else
                  (throw
                    (ex-info
                      (str "Script " script " does not exist")
                      {:cljs.main/error :invalid-arg}))))
              (when main
                (let [src (build/ns->source main)]
                  (when-not src
                    (throw
                      (ex-info
                        (str "Namespace " main " does not exist")
                        {:cljs.main/error :invalid-arg})))
                  (repl/load-stream renv (util/get-name src) src)
                  (repl/evaluate-form renv (ana-api/empty-env) "<cljs repl>"
                    `(~(symbol (name main) "-main") ~@args))))
              (finally
                (repl/tear-down renv)))))))))

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

(defn watch-proc [cenv path opts]
  (let [log-file (io/file (util/output-directory opts) "watch.log")]
    (util/mkdirs log-file)
    (repl/err-out (println "Watch compilation log available at:" (str log-file)))
    (let [log-out (FileWriter. log-file)]
      (binding [*err* log-out
                *out* log-out]
        (build/watch path (dissoc opts :watch) cenv)))))

(defn- serve-opt
  [_ [_ address-port & args] {:keys [options] :as cfg}]
  (let [[host port] (if address-port
                      (string/split address-port #":")
                      ["localhost" 9000])]
    (require 'cljs.repl.browser)
    ((ns-resolve 'cljs.repl.browser 'serve)
      {:host host
       :port (if port
               (cond-> port (string? port) Integer/parseInt)
               9000)
       :output-dir (:output-dir options "out")})))

(defn default-compile
  [repl-env {:keys [ns args options] :as cfg}]
  (let [rfs      #{"-r" "--repl"}
        sfs      #{"-s" "--serve"}
        env-opts (repl/repl-options (repl-env))
        repl?    (boolean (or (rfs ns) (rfs (first args))))
        serve?   (boolean (or (sfs ns) (sfs (first args))))
        main-ns  (if (and ns (not ((into rfs sfs) ns)))
                   (symbol ns)
                   (:main options))
        coptsf   (when-let [od (:output-dir options)]
                   (io/file od "cljsc_opts.edn"))
        opts     (as->
                   (merge
                     (when (and coptsf (.exists coptsf))
                       (edn/read-string (slurp coptsf)))
                     (select-keys env-opts
                       (cond-> [:target] repl? (conj :browser-repl)))
                     options
                     (when main-ns
                       {:main main-ns})) opts
                   (cond-> opts
                     (not (:output-to opts))
                     (assoc :output-to
                       (.getPath (io/file (:output-dir opts "out") "main.js")))
                     (= :advanced (:optimizations opts))
                     (dissoc :browser-repl)
                     (not (:output-dir opts))
                     (assoc :output-dir "out")
                     (not (contains? opts :aot-cache))
                     (assoc :aot-cache true)))
        convey   (into [:output-dir] repl/known-repl-opts)
        cfg      (update cfg :options merge (select-keys opts convey))
        source   (when (and (= :none (:optimizations opts :none)) main-ns)
                   (:uri (build/ns->location main-ns)))
        cenv     (env/default-compiler-env)]
    (env/with-compiler-env cenv
      (if-let [path (:watch opts)]
        (when-not repl?
          (build/watch path opts cenv))
        (build/build source opts cenv))
      (when repl?
        (repl-opt repl-env args cfg))
      (when serve?
        (serve-opt repl-env args cfg)))))

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
    {:groups {::main&compile {:desc "init options"
                              :pseudos
                              {["-re" "--repl-env"]
                               {:arg "env"
                                :doc (str "The REPL environment to use. Built-in "
                                          "supported values: nashorn, node, browser, "
                                          "rhino. Defaults to browser")}}}
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
                                :doc "Continuously build, only effective with the --compile main option"}
      ["-o" "--output-to"]     {:group ::compile :fn output-to-opt
                                :arg "file"
                                :doc "Set the output compiled file"}
      ["-O" "--optimizations"] {:group ::compile :fn optimize-opt
                                :arg "level"
                                :doc
                                (str "Set optimization level, only effective with "
                                  "--compile main option. Valid values are: none, "
                                  "whitespace, simple, advanced")}
      ["-t" "--target"]        {:group ::main&compile :fn target-opt
                                :arg "name"
                                :doc
                                (str "The JavaScript target. Configures environment bootstrap and "
                                     "defaults to browser. Supported values: node or nodejs, nashorn, "
                                     "webworker, none") }
      ["-ro" "--repl-opts"]    {:group ::main&compile :fn repl-env-opts-opt
                                :arg "edn"
                                :doc (str "Options to configure the repl-env")}
      ["-co" "--compile-opts"] {:group ::main&compile :fn compile-opts-opt
                                :arg "edn"
                                :doc (str "Options to configure the build")}}
     :main
     {["-r" "--repl"]          {:fn repl-opt
                                :doc "Run a repl"}
      ["-m" "--main"]          {:fn main-opt
                                :arg "ns"
                                :doc "Call the -main function from a namespace with args"}
      ["-c" "--compile"]       {:fn compile-opt
                                :arg "[ns]"
                                :doc (str "Run a compile. If optional namespace specified, use as "
                                          "the main entry point. If --repl follows, "
                                          "will launch a REPL after the compile completes. "
                                          "If --server follows, will start a web server that serves "
                                          "the current directory after the compile completes.")}
      ["-s" "--serve"]         {:fn serve-opt
                                :arg "host:port"
                                :doc (str "Start a simple web server to serve the current directory")}
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

        (not (#{"true" "false"} (fnext post)))
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
