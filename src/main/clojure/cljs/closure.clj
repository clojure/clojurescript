;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.closure
  (:refer-clojure :exclude [compile])
  (:require [cljs.util :as util :refer [distinct-by]]
            [cljs.core :as cljsm]
            [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.source-map :as sm]
            [cljs.env :as env]
            [cljs.js-deps :as deps]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.reflect]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [cljs.module-graph :as module-graph])
  (:import [java.lang ProcessBuilder]
           [java.io
            File BufferedReader BufferedInputStream
            Writer InputStreamReader IOException StringWriter ByteArrayInputStream]
           [java.net URI URL]
           [java.util.logging Level]
           [java.util List Random HashMap]
           [java.util.concurrent
            TimeUnit LinkedBlockingDeque Executors CountDownLatch]
           [com.google.javascript.jscomp CompilerOptions CompilationLevel
                                         CompilerInput CompilerInput$ModuleType DependencyOptions
                                         CompilerOptions$LanguageMode SourceMap$Format
                                         SourceMap$DetailLevel ClosureCodingConvention SourceFile
                                         Result JSError CheckLevel DiagnosticGroup DiagnosticGroups
                                         CommandLineRunner
                                         JSChunk SourceMap VariableMap PrintStreamErrorManager DiagnosticType
                                         VariableRenamingPolicy PropertyRenamingPolicy]
           [com.google.javascript.jscomp.deps ClosureBundler ModuleLoader$ResolutionMode ModuleNames
                                              SimpleDependencyInfo]
           [com.google.javascript.rhino Node]
           [java.nio.file Path Paths Files StandardWatchEventKinds WatchKey
                          WatchEvent FileVisitor FileVisitResult FileSystems]
           [java.nio.charset Charset StandardCharsets]
           [com.sun.nio.file SensitivityWatchEventModifier]))

;; Copied from clojure.tools.gitlibs

(def ^:private GITLIBS-CACHE-DIR
  (delay
    (.getCanonicalPath
      (let [env (System/getenv "GITLIBS")]
        (if (string/blank? env)
          (io/file (System/getProperty "user.home") ".gitlibs")
          (io/file env))))))

(defn- gitlibs-cache-dir
  "Returns the gitlibs cache dir, a string."
  []
  @GITLIBS-CACHE-DIR)

(defn- gitlibs-src?
  "Returns true if the file comes from the gitlibs cache."
  [file]
  #_(string/starts-with? (util/path file) (gitlibs-cache-dir))
  ;; NOTE: does not work on first build see CLJS-2765
  false)

(def name-chars (map char (concat (range 48 57) (range 65 90) (range 97 122))))

(defn random-char []
  (nth name-chars (.nextInt (Random.) (count name-chars))))

(defn random-string [length]
  (apply str (take length (repeatedly random-char))))

(defn- sym->var
  "Converts a namespaced symbol to a var, loading the requisite namespace if
  needed. For use with a function defined under a keyword in opts. The kw and
  ex-data arguments are used to form exceptions."
  ([sym kw]
   (sym->var sym kw nil))
  ([sym kw ex-data]
   (let [ns     (namespace sym)
         _      (when (nil? ns)
                  (throw
                    (ex-info (str kw " symbol " sym " is not fully qualified")
                      (merge ex-data {kw sym
                                      :clojure.error/phase :compilation}))))
         var-ns (symbol ns)]
     (when (not (find-ns var-ns))
       (try
         (locking ana/load-mutex
           (require var-ns))
         (catch Throwable t
           (throw (ex-info (str "Cannot require namespace referred by " kw " value " sym)
                    (merge ex-data {kw sym
                                    :clojure.error/phase :compilation})
                    t)))))

     (find-var sym))))

(defn- opts-fn
  "Extracts a function from opts, by default expecting a function value, but
  converting from a namespaced symbol if needed."
  [kw opts]
  (when-let [fn-or-sym (kw opts)]
    (cond-> fn-or-sym (symbol? fn-or-sym) (sym->var kw {}))))

;; Closure API
;; ===========

(defmulti js-source-file (fn [_ source] (class source)))

(defmethod js-source-file String [^String name ^String source]
  (-> (SourceFile/builder)
    (.withPath name)
    (.withContent source)
    (.build)))

(defmethod js-source-file File [_ ^File source]
  (-> (SourceFile/builder)
    (.withPath (.toPath source))
    (.withCharset StandardCharsets/UTF_8)
    (.build)))

(defmethod js-source-file URL [_ ^URL source]
  (js-source-file _ (io/file (.getPath source))))

(defmethod js-source-file BufferedInputStream [^String name ^BufferedInputStream source]
  (-> (SourceFile/builder)
    (.withPath name)
    (.withContent source)
    (.build)))

(def check-level
  {:error CheckLevel/ERROR
   :warning CheckLevel/WARNING
   :off CheckLevel/OFF})

(def warning-types
  {:access-controls DiagnosticGroups/ACCESS_CONTROLS
   :analyzer-checks DiagnosticGroups/ANALYZER_CHECKS
   :check-regexp DiagnosticGroups/CHECK_REGEXP
   :check-types DiagnosticGroups/CHECK_TYPES
   :check-useless-code DiagnosticGroups/CHECK_USELESS_CODE
   :check-variables DiagnosticGroups/CHECK_VARIABLES
   :closure-dep-method-usage-checks DiagnosticGroups/CLOSURE_DEP_METHOD_USAGE_CHECKS
   :conformance-violations DiagnosticGroups/CONFORMANCE_VIOLATIONS
   :const DiagnosticGroups/CONST
   :constant-property DiagnosticGroups/CONSTANT_PROPERTY
   :debugger-statement-present DiagnosticGroups/DEBUGGER_STATEMENT_PRESENT
   :deprecated DiagnosticGroups/DEPRECATED
   :deprecated-annotations DiagnosticGroups/DEPRECATED_ANNOTATIONS
   :duplicate-message DiagnosticGroups/DUPLICATE_MESSAGE
   :duplicate-vars DiagnosticGroups/DUPLICATE_VARS
   :es5-strict DiagnosticGroups/ES5_STRICT
   :externs-validation DiagnosticGroups/EXTERNS_VALIDATION
   :extra-require DiagnosticGroups/EXTRA_REQUIRE
   :function-params DiagnosticGroups/FUNCTION_PARAMS
   :global-this DiagnosticGroups/GLOBAL_THIS
   :invalid-casts DiagnosticGroups/INVALID_CASTS
   :j2cl-checks DiagnosticGroups/J2CL_CHECKS
   :jsdoc-missing-type DiagnosticGroups/JSDOC_MISSING_TYPE
   :late-provide DiagnosticGroups/LATE_PROVIDE
   :lint-checks DiagnosticGroups/LINT_CHECKS
   :message-descriptions DiagnosticGroups/MESSAGE_DESCRIPTIONS
   :misplaced-msg-annotation DiagnosticGroups/MISPLACED_MSG_ANNOTATION
   :misplaced-type-annotation DiagnosticGroups/MISPLACED_TYPE_ANNOTATION
   :missing-override DiagnosticGroups/MISSING_OVERRIDE
   :missing-polyfill DiagnosticGroups/MISSING_POLYFILL
   :missing-properties DiagnosticGroups/MISSING_PROPERTIES
   :missing-provide DiagnosticGroups/MISSING_PROVIDE
   :missing-require DiagnosticGroups/MISSING_REQUIRE
   :missing-return DiagnosticGroups/MISSING_RETURN
   :missing-sources-warnings DiagnosticGroups/MISSING_SOURCES_WARNINGS
   :module-load DiagnosticGroups/MODULE_LOAD
   :msg-conventions DiagnosticGroups/MSG_CONVENTIONS
   :non-standard-jsdoc DiagnosticGroups/NON_STANDARD_JSDOC
   :report-unknown-types DiagnosticGroups/REPORT_UNKNOWN_TYPES
   :strict-missing-properties DiagnosticGroups/STRICT_MISSING_PROPERTIES
   :strict-module-dep-check DiagnosticGroups/STRICT_MODULE_DEP_CHECK
   :strict-requires DiagnosticGroups/STRICT_REQUIRES
   :suspicious-code DiagnosticGroups/SUSPICIOUS_CODE
   :too-many-type-params DiagnosticGroups/TOO_MANY_TYPE_PARAMS
   :tweaks DiagnosticGroups/TWEAKS
   :type-invalidation DiagnosticGroups/TYPE_INVALIDATION
   :undefined-variables DiagnosticGroups/UNDEFINED_VARIABLES
   :underscore DiagnosticGroups/UNDERSCORE
   :unknown-defines DiagnosticGroups/UNKNOWN_DEFINES
   :unnecessary-escape DiagnosticGroups/UNNECESSARY_ESCAPE
   :unused-local-variable DiagnosticGroups/UNUSED_LOCAL_VARIABLE
   :unused-private-property DiagnosticGroups/UNUSED_PRIVATE_PROPERTY
   :violated-module-dep DiagnosticGroups/VIOLATED_MODULE_DEP
   :visibility DiagnosticGroups/VISIBILITY})

(def known-opts
  "Set of all known compiler options."
  #{:anon-fn-naming-policy :asset-path :cache-analysis :closure-defines :closure-extra-annotations
    :closure-warnings :compiler-stats :dump-core :elide-asserts :externs :foreign-libs
    :hashbang :language-in :language-out :libs :main :modules :source-map-path :source-map-asset-path
    :optimizations :optimize-constants :output-dir :output-to :output-wrapper :parallel-build :preamble
    :pretty-print :print-input-delimiter :pseudo-names :recompile-dependents :source-map
    :source-map-inline :source-map-timestamp :static-fns :target :verbose :warnings
    :emit-constants :ups-externs :ups-foreign-libs :ups-libs :warning-handlers :preloads
    :browser-repl :cache-analysis-format :infer-externs :closure-generate-exports :npm-deps
    :fn-invoke-direct :checked-arrays :closure-module-roots :rewrite-polyfills :use-only-custom-externs
    :watch :watch-error-fn :watch-fn :install-deps :process-shim :rename-prefix :rename-prefix-namespace
    :closure-variable-map-in :closure-property-map-in :closure-variable-map-out :closure-property-map-out
    :stable-names :ignore-js-module-exts :opts-cache :aot-cache :elide-strict :fingerprint :spec-skip-macros
    :nodejs-rt :target-fn :deps-cmd :bundle-cmd})

(def string->charset
  {"iso-8859-1" StandardCharsets/ISO_8859_1
   "us-ascii"   StandardCharsets/US_ASCII
   "utf-16"     StandardCharsets/UTF_16
   "utf-16be"   StandardCharsets/UTF_16BE
   "utf-16le"   StandardCharsets/UTF_16LE
   "utf-8"      StandardCharsets/UTF_8})

(defn to-charset [charset]
  (cond
    (instance? Charset charset) charset
    (and (string? charset)
         (contains? string->charset (string/lower-case charset)))
    (get string->charset (string/lower-case charset))
    :else
    (throw
      (ex-info
        (str "Invalid :closure-output-charset " charset " given, only "
             (string/join ", " (keys string->charset)) " supported ")
        {:clojure.error/phase :compilation}))))

(def lang-level
  [:ecmascript3 :ecmascript5 :ecmascript5-strict :ecmascript6 :ecmascript6-strict
   :ecmascript-2015 :ecmascript-2016 :ecmascript-2017 :ecmascript-next
   :no-transpile])

(defn expand-lang-key [key]
  (keyword (string/replace (name key) #"^es" "ecmascript")))

(defn ^CompilerOptions$LanguageMode lang-key->lang-mode [key]
  (case (expand-lang-key key)
    :no-transpile          CompilerOptions$LanguageMode/NO_TRANSPILE ;; same mode as input (for language-out only)
    :ecmascript3           CompilerOptions$LanguageMode/ECMASCRIPT3
    :ecmascript5           CompilerOptions$LanguageMode/ECMASCRIPT5
    :ecmascript5-strict    CompilerOptions$LanguageMode/ECMASCRIPT5_STRICT
    :ecmascript6           CompilerOptions$LanguageMode/ECMASCRIPT_2015 ;; (deprecated and remapped)
    :ecmascript6-strict    CompilerOptions$LanguageMode/ECMASCRIPT_2015 ;; (deprecated and remapped)
    :ecmascript-2015       CompilerOptions$LanguageMode/ECMASCRIPT_2015
    :ecmascript-2016       CompilerOptions$LanguageMode/ECMASCRIPT_2016
    :ecmascript-2017       CompilerOptions$LanguageMode/ECMASCRIPT_2017
    :ecmascript-next       CompilerOptions$LanguageMode/ECMASCRIPT_NEXT))

(defn set-options
  "TODO: Add any other options that we would like to support."
  [opts ^CompilerOptions compiler-options]
  (.setModuleResolutionMode compiler-options ModuleLoader$ResolutionMode/NODE)

  (when (contains? opts :pretty-print)
    (.setPrettyPrint compiler-options (:pretty-print opts)))

  (when (contains? opts :pseudo-names)
    (set! (.generatePseudoNames compiler-options) (:pseudo-names opts)))

  (when-let [lang-key (:language-in opts :ecmascript5)]
    (.setLanguageIn compiler-options (lang-key->lang-mode lang-key)))

  (when-let [lang-key (:language-out opts)]
    (.setLanguageOut compiler-options (lang-key->lang-mode lang-key)))

  (when (contains? opts :print-input-delimiter)
    (set! (.printInputDelimiter compiler-options)
      (:print-input-delimiter opts)))

  (when (contains? opts :closure-warnings)
    (doseq [[type level] (:closure-warnings opts)]
      (. compiler-options
        (setWarningLevel (type warning-types) (level check-level)))))

  (when (contains? opts :closure-extra-annotations)
    (. compiler-options
      (setExtraAnnotationNames (map name (:closure-extra-annotations opts)))))

  (when (contains? opts :closure-module-roots)
    (. compiler-options
      (setModuleRoots (:closure-module-roots opts))))

  (when (contains? opts :closure-generate-exports)
    (. compiler-options
      (setGenerateExports (:closure-generate-exports opts))))

  (when (contains? opts :rewrite-polyfills)
    (. compiler-options
      (setRewritePolyfills (:rewrite-polyfills opts))))

  (when (contains? opts :rename-prefix)
    (. compiler-options
       (setRenamePrefix (:rename-prefix opts))))

  (when (contains? opts :rename-prefix-namespace)
    (. compiler-options
       (setRenamePrefixNamespace (:rename-prefix-namespace opts))))

  (when (contains? opts :closure-variable-map-in)
    (let [var-in (io/file (:closure-variable-map-in opts))]
      (when (.exists var-in)
        (.setInputVariableMap compiler-options
          (VariableMap/load (.getAbsolutePath var-in))))))

  (when (contains? opts :closure-property-map-in)
    (let [prop-in (io/file (:closure-property-map-in opts))]
      (when (.exists prop-in)
        (.setInputPropertyMap compiler-options
          (VariableMap/load (.getAbsolutePath prop-in))))))

  (. compiler-options
    (setOutputCharset (to-charset (:closure-output-charset opts "UTF-8"))) ;; only works > 20160125 Closure Compiler
    )

  compiler-options)

(defn ^CompilerOptions make-options
  "Create a CompilerOptions object and set options from opts map."
  [opts]
  (let [level (case (:optimizations opts)
                :advanced CompilationLevel/ADVANCED_OPTIMIZATIONS
                :whitespace CompilationLevel/WHITESPACE_ONLY
                :simple CompilationLevel/SIMPLE_OPTIMIZATIONS)
        compiler-options (doto (CompilerOptions.)
                           (.setCodingConvention (ClosureCodingConvention.)))]
    (doseq [[key val] (:closure-defines opts)]
      (let [key (name key)]
        (cond
          (string? val) (.setDefineToStringLiteral compiler-options key val)
          (number? val) (.setDefineToDoubleLiteral compiler-options key val)
          (or (true? val)
              (false? val)) (.setDefineToBooleanLiteral compiler-options key val)
          :else (println "value for" key "must be string, int, float, or bool"))))
    (if-let [extra-annotations (:closure-extra-annotations opts)]
      (. compiler-options (setExtraAnnotationNames (map name extra-annotations))))
    (when (:source-map opts)
      (if (:modules opts)
        ;; name is not actually used by Closure in :modules case,
        ;; but we need to provide _something_ for Closure to not
        ;; complain
        (set! (.sourceMapOutputPath compiler-options)
              (str (io/file (util/output-directory opts)
                            "cljs_modules.map")))
        (set! (.sourceMapOutputPath compiler-options)
              (:source-map opts)))
      (set! (.sourceMapDetailLevel compiler-options)
            SourceMap$DetailLevel/ALL)
      (set! (.sourceMapFormat compiler-options)
            SourceMap$Format/V3))
    (do
      (.setOptionsForCompilationLevel level compiler-options)
      (set-options opts compiler-options)
      compiler-options)))

(defn load-externs
  "Externs are JavaScript files which contain empty definitions of
  functions which will be provided by the environment. Any function in
  an extern file will not be renamed during optimization.

  Options may contain an :externs key with a list of file paths to
  load. The :use-only-custom-externs flag may be used to indicate that
  the default externs should be excluded."
  [{:keys [externs use-only-custom-externs target ups-externs infer-externs] :as opts}]
  (let [validate (fn validate [p us]
                   (if (empty? us)
                     (throw (util/compilation-error (IllegalArgumentException.
                                                      (str "Extern " p " does not exist"))))
                     us))
        filter-cp-js (fn [paths]
                       (for [p paths
                             u (deps/find-js-classpath p)]
                         u))
        filter-js (fn [paths]
                    (for [p paths
                          u (deps/find-js-resources p)]
                      u))
        add-target (fn [ext]
                     (cons (io/resource "cljs/externs.js")
                       (if (= :nodejs target)
                         (cons (io/resource "cljs/nodejs_externs.js")
                           (or ext []))
                         ext)))
        load-js (fn [ext]
                  (map #(js-source-file (.getFile %) (slurp %)) ext))]
    (let [js-sources  (-> externs filter-js add-target load-js)
          ups-sources (-> ups-externs filter-cp-js load-js)
          all-sources (vec (concat js-sources ups-sources))]
      (cond->
        (if use-only-custom-externs
          all-sources
          (into all-sources (CommandLineRunner/getDefaultExterns)))
        infer-externs
        (conj (js-source-file nil
                (io/file (util/output-directory opts) "inferred_externs.js")))))))

(defn ^com.google.javascript.jscomp.Compiler make-closure-compiler []
  (let [compiler (com.google.javascript.jscomp.Compiler.)]
    (com.google.javascript.jscomp.Compiler/setLoggingLevel Level/WARNING)
    compiler))

(defn report-failure [^Result result]
  (let [errors (.errors result)
        warnings (.warnings result)]
    (binding [*out* *err*]
      (doseq [next (seq errors)]
        (println "ERROR:" (.toString ^JSError next)))
      (doseq [next (seq warnings)]
        (println "WARNING:" (.toString ^JSError next)))
      (when (seq errors)
        (throw (util/compilation-error (Exception. "Closure compilation failed")))))))

;; Protocols for IJavaScript and Compilable
;; ========================================



(defprotocol ISourceMap
  (-source-url [this] "Return the CLJS source url")
  (-source-map [this] "Return the CLJS compiler generated JS source mapping"))

(extend-protocol deps/IJavaScript

  String
  (-foreign? [this] false)
  (-closure-lib? [this] false)
  (-url
    ([this] nil)
    ([this _] nil))
  (-relative-path
    ([this] nil)
    ([this _] nil))
  (-provides [this]
    (let [{:keys [provides]} (deps/parse-js-ns (string/split-lines this))]
      (cond-> provides
        (empty? provides)
        (conj (util/content-sha this 7)))))
  (-requires [this] (:requires (deps/parse-js-ns (string/split-lines this))))
  (-source
    ([this] this)
    ([this _] this))

  clojure.lang.IPersistentMap
  (-foreign? [this] (:foreign this))
  (-closure-lib? [this] (:closure-lib this))
  (-url
    ([this] (deps/-url this nil))
    ([this opts]
     (let [[url file] (if-let [url-min (and (#{:advanced :simple} (:optimizations opts))
                                            (:url-min this))]
                        [url-min (:file-min this)]
                        [(:url this) (:file this)])]
       (or url (deps/to-url file)))))
  (-relative-path
    ([this] (deps/-relative-path this nil))
    ([this opts]
     (let [file (if-let [file-min (and (#{:advanced :simple} (:optimizations opts))
                                       (:file-min this))]
                  file-min
                  (:file this))
           as-file (io/as-file file)]
       (when (and as-file (not (.isAbsolute as-file)))
         file))))
  (-provides [this] (map name (:provides this)))
  (-requires [this] (map name (:requires this)))
  (-source
    ([this] (deps/-source this nil))
    ([this opts]
      (if-let [s (:source this)]
        s
        (with-open [reader (io/reader (deps/-url this opts))]
          (slurp reader))))))

(defrecord JavaScriptFile [foreign ^URL url ^URL source-url provides requires lines source-map]
  deps/IJavaScript
  (-foreign? [this] foreign)
  (-closure-lib? [this] (:closure-lib this))
  (-url [this] url)
  (-url [this opts] url)
  (-relative-path [this] nil)
  (-relative-path [this opts] nil)
  (-provides [this] provides)
  (-requires [this] requires)
  (-source [this] (deps/-source this nil))
  (-source [this opts]
    (with-open [reader (io/reader url)]
      (slurp reader)))
  ISourceMap
  (-source-url [this] source-url)
  (-source-map [this] source-map))

(defn javascript-file
  ([foreign ^URL url provides requires]
     (javascript-file foreign url nil provides requires nil nil))
  ([foreign ^URL url source-url provides requires lines source-map]
    (assert (first provides) (str source-url " does not provide a namespace"))
    (JavaScriptFile. foreign url source-url (map name provides) (map name requires) lines source-map)))

(defn map->javascript-file [m]
  (merge
    (javascript-file
      (:foreign m)
      (when-let [f (or (:file m) (:url m))]
        (deps/to-url f))
      (when-let [sf (:source-file m)]
        (deps/to-url sf))
      (:provides m)
      (:requires m)
      (:lines m)
      (:source-map m))
    (when-let [source-file (:source-file m)]
      {:source-file source-file})
    (when-let [out-file (:out-file m)]
      {:out-file out-file})
    (when (:closure-lib m)
      {:closure-lib true})
    (when-let [module (:module m)]
      {:module module})
    (when-let [lang (:lang m)]
      {:lang lang})
    (when-let [ns (:ns m)]
      {:ns ns})
    (when (:macros-ns m)
      {:macros-ns true})))

(defn read-js
  "Read a JavaScript file returning a map of file information."
  [f]
  (let [source (slurp f)
        m (deps/parse-js-ns (string/split-lines source))]
    (map->javascript-file (assoc m :file f))))


;; Compile
;; =======

(defprotocol Inputs
  (-paths [this] "Returns the file paths to the source inputs"))

(extend-protocol Inputs
  String
  (-paths [this] [(io/file this)])
  File
  (-paths [this] [this]))

(defprotocol Compilable
  (-compile [this opts] "Returns one or more IJavaScripts.")
  (-find-sources [this opts] "Returns one or more IJavascripts, without compiling them."))

(defn compilable-input-paths
  "Takes a coll of inputs as strings or files and returns a
  single Inputs and Compilable object."
  [paths]
  (reify
    cljs.closure/Inputs
    (-paths [_]
      (mapcat cljs.closure/-paths paths))
    cljs.closure/Compilable
    (-compile [_ opts]
      (mapcat #(cljs.closure/-compile % opts)
              paths))
    (-find-sources [_ opts]
      (mapcat #(cljs.closure/-find-sources % opts)
              paths))))

(defn compile-form-seq
  "Compile a sequence of forms to a JavaScript source string."
  ([forms]
    (compile-form-seq forms
      (when env/*compiler*
        (:options @env/*compiler*))))
  ([forms opts]
   (comp/with-core-cljs opts
     (fn []
       (with-out-str
         (binding [ana/*cljs-ns* 'cljs.user]
           (doseq [form forms]
             (comp/emit (ana/analyze (ana/empty-env) form)))))))))

(defn compiled-file
  "Given a map with at least a :file key, return a map with
   {:file .. :provides .. :requires ..}.

   Compiled files are cached so they will only be read once."
  [m]
  (let [path (.getPath (.toURL ^File (:file m)))
        js (if (:provides m)
             (map->javascript-file m)
             (if-let [js (get-in @env/*compiler* [::compiled-cljs path])]
               js
               (read-js (:file m))))]
    (swap! env/*compiler* update-in [::compiled-cljs] assoc path js)
    js))

(defn compile
  "Given a Compilable, compile it and return an IJavaScript."
  [compilable opts]
  (-compile compilable opts))

(def ^:private USER-HOME-WRITABLE
  (delay (.canWrite (io/file (System/getProperty "user.home")))))

(defn- aot-cache? [opts]
  "Returns true if compilation artifacts shuold be placed in the
  shared AOT cache."
  (and (:aot-cache opts)
       @USER-HOME-WRITABLE))

(defn- copy-from-cache
  [cache-path cacheable source-file opts]
  (doseq [[k ^File f] cacheable]
    (when (.exists f)
      (let [target (io/file (util/output-directory opts)
                     (-> (.getAbsolutePath f)
                       (string/replace (.getAbsolutePath cache-path) "")
                       (subs 1)))]
        (when (and (or ana/*verbose* (:verbose opts)) (= :output-file k))
          (util/debug-prn (str "Copying cached " f " to " target)))
        (util/mkdirs target)
        (spit target (slurp f))
        (.setLastModified target (util/last-modified source-file))))))

(defn find-sources
  "Given a Compilable, find sources and return a sequence of IJavaScript."
  [compilable opts]
  (-find-sources compilable opts))

(defn compile-file
  "Compile a single cljs file. If no output-file is specified, returns
  a string of compiled JavaScript. With an output-file option, the
  compiled JavaScript will written to this location and the function
  returns a JavaScriptFile. In either case the return value satisfies
  IJavaScript."
  [^File file {:keys [output-file] :as opts}]
    (if output-file
      (let [out-file (io/file (util/output-directory opts) output-file)]
        (if (and (aot-cache? opts)
                 (gitlibs-src? file))
          (let [cacheable  (ana/cacheable-files file (util/ext file) opts)
                cache-path (ana/cache-base-path (util/path file) opts)]
            (if (not (.exists (:output-file cacheable)))
              (let [ret (compiled-file (comp/compile-file file (:output-file cacheable)
                                         (assoc opts :output-dir (util/path cache-path))))]
                (copy-from-cache cache-path cacheable file opts)
                ret)
              (do
                (when-not (.exists out-file)
                  (copy-from-cache cache-path cacheable file opts))
                (compiled-file (comp/compile-file file (.toString out-file) opts)))))
          (compiled-file (comp/compile-file file (.toString out-file) opts))))
      (let [path (.getPath ^File file)]
        (binding [ana/*cljs-file* path]
          (with-open [rdr (io/reader file)]
            (compile-form-seq (ana/forms-seq* rdr path)))))))

(defn compile-dir
  "Recursively compile all cljs files under the given source
  directory. Return a list of JavaScriptFiles."
  [^File src-dir opts]
  (let [out-dir (util/output-directory opts)]
    (map compiled-file
         (comp/compile-root src-dir out-dir opts))))

(defn ^String path-from-jarfile
  "Given the URL of a file within a jar, return the path of the file
  from the root of the jar."
  [^URL url]
  (last (string/split (.getFile url) #"\.jar!/")))

(defn jar-file-to-disk
  "Copy a file contained within a jar to disk. Return the created file."
  ([url out-dir]
    (jar-file-to-disk url out-dir
      (when env/*compiler*
        (:options @env/*compiler*))))
  ([url out-dir opts]
   (let [out-file (io/file out-dir (path-from-jarfile url))
         content  (with-open [reader (io/reader url)]
                    (slurp reader))]
     (when (and url (or ana/*verbose* (:verbose opts)))
       (util/debug-prn "Copying" (str url) "to" (str out-file)))
     (util/mkdirs out-file)
     (spit out-file content)
     (.setLastModified ^File out-file (util/last-modified url))
     out-file)))

(defn compile-from-jar
  "Compile a file from a jar if necessary. Returns IJavaScript."
  [jar-file {:keys [output-file] :as opts}]
  (let [out-file (when output-file
                   (io/file (util/output-directory opts) output-file))
        cacheable (ana/cacheable-files jar-file (util/ext jar-file) opts)]
    (when (or (nil? out-file)
              (comp/requires-compilation? jar-file out-file opts))
      ;; actually compile from JAR
      (if (not (aot-cache? opts))
        (-compile (jar-file-to-disk jar-file (util/output-directory opts) opts) opts)
        (let [cache-path (ana/cache-base-path (util/path jar-file) opts)]
          (when (comp/requires-compilation? jar-file (:output-file cacheable) opts)
            (-compile (jar-file-to-disk jar-file cache-path opts)
              (assoc opts :output-dir (util/path cache-path))))
          (copy-from-cache cache-path cacheable jar-file opts))))
    ;; Files that don't require compilation (cljs.loader for example)
    ;; need to be copied from JAR to disk.
    (when (or (nil? out-file)
              (not (.exists out-file)))
      (jar-file-to-disk jar-file (util/output-directory opts) opts))
    ;; have to call compile-file as it includes more IJavaScript
    ;; information than ana/parse-ns for now
    (compile-file
      (io/file (util/output-directory opts)
        (last (string/split (.getPath ^URL jar-file) #"\.jar!/")))
      opts)))

(defn find-jar-sources [this opts]
  [(comp/find-source this)])

(extend-protocol Compilable

  File
  (-compile [this opts]
    (if (.isDirectory this)
      (compile-dir this opts)
      (compile-file this opts)))
  (-find-sources [this _]
    (if (.isDirectory this)
      (comp/find-root-sources this)
      [(comp/find-source this)]))

  URL
  (-compile [this opts]
    (case (.getProtocol this)
      "file" (-compile (io/file this) opts)
      "jar" (compile-from-jar this opts)))
  (-find-sources [this opts]
    (case (.getProtocol this)
      "file" (-find-sources (io/file this) opts)
      "jar" (find-jar-sources this opts)))

  clojure.lang.PersistentList
  (-compile [this opts]
    (compile-form-seq [this]))
  (-find-sources [this opts]
    [(ana/parse-ns [this] opts)])

  String
  (-compile [this opts] (-compile (io/file this) opts))
  (-find-sources [this opts] (-find-sources (io/file this) opts))

  clojure.lang.Symbol
  (-compile [this opts]
    (-compile (util/ns->source this) opts))
  (-find-sources [this opts]
    (-find-sources (util/ns->source this) opts))

  clojure.lang.PersistentVector
  (-compile [this opts] (compile-form-seq this))
  (-find-sources [this opts]
    [(ana/parse-ns this opts)])

  clojure.lang.IPersistentSet
  (-compile [this opts]
    (doall (map (comp #(-compile % opts) util/ns->source) this)))
  (-find-sources [this opts]
    (into [] (mapcat #(-find-sources % opts)) this))
  )

(comment
  ;; compile a file in memory
  (-compile "samples/hello/src/hello/core.cljs" {})
  (-find-sources "samples/hello/src/hello/core.cljs" {})
  ;; compile a file to disk - see file @ 'out/clojure/set.js'
  (-compile (io/resource "clojure/set.cljs") {:output-file "clojure/set.js"})
  (-find-sources (io/resource "clojure/set.cljs") {:output-file "clojure/set.js"})
  ;; compile a project
  (-compile (io/file "samples/hello/src") {})
  (-find-sources (io/file "samples/hello/src") {})
  ;; compile a project with a custom output directory
  (-compile (io/file "samples/hello/src") {:output-dir "my-output"})
  (-find-sources (io/file "samples/hello/src") {:output-dir "my-output"})
  ;; compile a form
  (-compile '(defn plus-one [x] (inc x)) {})
  ;; compile a vector of forms
  (-compile '[(ns test.app (:require [goog.array :as array]))
              (defn plus-one [x] (inc x))]
            {})

  (-find-sources 'cljs.core {})
  )

(defn js-dependencies
  "Given a sequence of Closure namespace strings, return the list of
  all dependencies. The returned list includes all Google and
  third-party library dependencies.

  Third-party libraries are configured using the :libs option where
  the value is a list of directories containing third-party
  libraries."
  [opts requires]
  (loop [requires requires
         visited (set requires)
         deps #{}]
    (if (seq requires)
      (let [node (or (get (@env/*compiler* :js-dependency-index) (first requires))
                     (deps/find-classpath-lib (first requires)))
            new-req (remove #(contains? visited %)
                      (into (:requires node) (:require-types node)))]
        (recur (into (rest requires) new-req)
               (into visited new-req)
               (conj deps node)))
      (remove nil? deps))))

(comment
  ;; find dependencies
  (binding [env/*compiler* (env/default-compiler-env)]
    (js-dependencies {} ["goog.array"]))

  ;; find dependencies in an external library
  (binding [env/*compiler* (env/default-compiler-env)]
    (js-dependencies {:libs ["closure/library/third_party/closure"]} ["goog.dom.query"]))

  (binding [env/*compiler* (env/default-compiler-env)]
    (js-dependencies {} ["goog.math.Long"]))

  (binding [env/*compiler* (env/default-compiler-env)]
    (js-dependencies {} ["goog.string.StringBuffer"]))
  )

(defn add-core-macros-if-cljs-js
  "If a compiled entity is the cljs.js namespace, explicitly
  add the cljs.core macros namespace dependency to it."
  [compiled]
  (cond-> compiled
    ;; TODO: IJavaScript :provides :requires should really
    ;; always be Vector<MungedString> - David
    (= ["cljs.js"] (into [] (map str) (deps/-provides compiled)))
    (update-in [:requires] concat ["cljs.core$macros"])))

(defn get-compiled-cljs
  "Return an IJavaScript for this file. Compiled output will be
   written to the working directory."
  [opts {:keys [relative-path uri]}]
  (let [js-file  (comp/rename-to-js relative-path)
        compiled (-compile uri (merge opts {:output-file js-file}))]
    (add-core-macros-if-cljs-js compiled)))

(defn cljs-source-for-namespace
  "Given a namespace return the corresponding source with either a .cljs or
  .cljc extension."
  [ns]
  (if (= "cljs.core$macros" (str ns))
    (let [relpath "cljs/core.cljc"]
      {:relative-path relpath :uri (io/resource relpath) :ext :cljc})
    (let [path    (-> (munge ns) (string/replace \. \/))
          relpath (str path ".cljs")]
      (if-let [res (io/resource relpath)]
        {:relative-path relpath :uri res :ext :cljs}
        (let [relpath (str path ".cljc")]
          (if-let [res (io/resource relpath)]
            {:relative-path relpath :uri res :ext :cljc}))))))

(defn source-for-namespace
  "Given a namespace and compilation environment return the relative path and
  uri of the corresponding source regardless of the source language extension:
  .cljs, .cljc, .js"
  [ns compiler-env]
  (let [ns-str  (str (comp/munge ns {}))
        path    (string/replace ns-str \. \/)
        relpath (str path ".cljs")]
    (if-let [cljs-res (io/resource relpath)]
      {:relative-path relpath :uri cljs-res :ext :cljs}
      (let [relpath (str path ".cljc")]
        (if-let [cljc-res (io/resource relpath)]
          {:relative-path relpath :uri cljc-res :ext :cljc}
          (let [relpath (str path ".js")]
            (if-let [js-res (io/resource relpath)]
              {:relative-path relpath :uri js-res :ext :js}
              (let [ijs (get-in @compiler-env [:js-dependency-index (str ns)])
                   relpath (or (:file ijs) (:url ijs))]
               (if-let [js-res (and relpath
                                 ;; try to parse URL, otherwise just return local
                                 ;; resource
                                 (or (and (util/url? relpath) relpath)
                                   (try (URL. relpath) (catch Throwable t))
                                   (io/resource relpath)))]
                 {:relative-path relpath :uri js-res :ext :js}
                 (throw
                   (util/compilation-error
                     (IllegalArgumentException.
                       (str "Namespace " ns " does not exist."
                         (when (string/includes? ns "-")
                           " Please check that namespaces with dashes use underscores in the ClojureScript file name."))))))))))))))

(defn cljs-dependencies
  "Given a list of all required namespaces, return a list of
  IJavaScripts which are the cljs dependencies. The returned list will
  not only include the explicitly required files but any transitive
  dependencies as well. JavaScript files will be compiled to the
  working directory if they do not already exist.

  Only load dependencies from the classpath."
  [opts requires]
  (letfn [(cljs-deps [lib-names]
            (->> lib-names
              (remove #(or ((@env/*compiler* :js-dependency-index) %)
                           (deps/find-classpath-lib %)))
              (map cljs-source-for-namespace)
              (remove (comp nil? :uri))))]
    (loop [required-files (cljs-deps requires)
           visited        (set required-files)
           js-deps        #{}]
      (if (seq required-files)
        (let [next-file (first required-files)
              js        (get-compiled-cljs opts next-file)
              new-req   (remove #(contains? visited %) (cljs-deps (deps/-requires js)))]
          (recur (into (rest required-files) new-req)
                 (into visited new-req)
                 (conj js-deps js)))
        (disj js-deps nil)))))

(comment
  ;; only get cljs deps
  (cljs-dependencies {} ["goog.string" "cljs.core"])
  ;; get transitive deps
  (cljs-dependencies {} ["clojure.string"])
  ;; don't get cljs.core twice
  (cljs-dependencies {} ["cljs.core" "clojure.string"])
  )

(defn find-cljs-dependencies
  "Given set of cljs namespace symbols, find IJavaScript objects for the namespaces."
  [requires]
  (letfn [(cljs-deps [namespaces]
            (->> namespaces
                 (remove #(or ((@env/*compiler* :js-dependency-index) %)
                              (deps/find-classpath-lib %)))
                 (map cljs-source-for-namespace)
                 (remove (comp nil? :uri))))]
    (loop [required-files (cljs-deps requires)
           visited (set required-files)
           cljs-namespaces #{}]
      (if (seq required-files)
        (let [next-file (first required-files)
              ns-info (ana/parse-ns (:uri next-file))
              new-req (remove #(contains? visited %) (cljs-deps (cond-> (deps/-requires ns-info)
                                                                  (= 'cljs.js (:ns ns-info)) (conj "cljs.core$macros"))))]
          (recur (into (rest required-files) new-req)
                 (into visited new-req)
                 (conj cljs-namespaces ns-info)))
        (disj cljs-namespaces nil)))))

(defn- constants-filename
  "Returns the filename of the constants table."
  [opts]
  (str (util/output-directory opts) File/separator
       (string/replace (str ana/constants-ns-sym) "." File/separator) ".js"))

(defn- constants-javascript-file
  "Returns the constants table as a JavaScriptFile."
  [opts]
  (let [url (deps/to-url (constants-filename opts))]
    (javascript-file nil url [(str ana/constants-ns-sym)] ["cljs.core"])))

(defn add-dependencies
  "DEPRECATED: Given one or more IJavaScript objects in dependency order, produce
  a new sequence of IJavaScript objects which includes the input list
  plus all dependencies in dependency order."
  [opts & inputs]
  (let [inputs        (set inputs)
        requires      (set (mapcat deps/-requires inputs))
        required-cljs (clojure.set/difference (cljs-dependencies opts requires) inputs)
        required-js   (js-dependencies opts
                        (into (set (mapcat deps/-requires required-cljs)) requires))]
    (cons
      (javascript-file nil (io/resource "goog/base.js") ["goog"] nil)
      (deps/dependency-order
        (concat
          (map
            (fn [{:keys [type foreign url file provides requires] :as js-map}]
              ;; ignore :seed inputs, only for REPL - David
              (if (not= :seed type)
                (let [url (or url (io/resource file))]
                 (merge
                   (javascript-file foreign url provides requires)
                   js-map))
                js-map))
            required-js)
          (when (-> @env/*compiler* :options :emit-constants)
            [(constants-javascript-file opts)])
          required-cljs
          inputs)))))

(comment
  (alter-var-root #'env/*compiler* (constantly (env/default-compiler-env)))
  ;; only get cljs deps
  (find-cljs-dependencies ["goog.string" "cljs.core"])
  ;; get transitive deps
  (find-cljs-dependencies ["clojure.string"])
  ;; don't get cljs.core twice
  (find-cljs-dependencies ["cljs.core" "clojure.string"])
  )

(defn- module-entries
  "Return the module entries of `compile-opts` as a set."
  [compile-opts]
  (->> compile-opts :modules vals
       (map :entries)
       (remove nil?)
       (apply concat)
       (set)))

(defn add-dependency-sources
  "Given list of IJavaScript objects, produce a new sequence of IJavaScript objects
  of all dependencies of inputs."
  ([inputs]
   (add-dependency-sources inputs
     (when env/*compiler*
       (:options @env/*compiler*))))
  ([inputs compile-opts]
   (let [inputs         (set inputs)
         requires       (set (mapcat deps/-requires inputs))
         module-entries (module-entries compile-opts)]
     (into inputs (find-cljs-dependencies (set/union requires module-entries))))))

(defn check-unprovided
  [inputs]
  (let [requires   (set (mapcat deps/-requires inputs))
        provided   (set (mapcat deps/-provides inputs))
        unprovided (clojure.set/difference requires provided)]
    (when (seq unprovided)
      (ana/warning :unprovided @env/*compiler* {:unprovided (sort unprovided)}))
    inputs))

(defn compile-task [^LinkedBlockingDeque deque input-set compiled opts failed]
  (loop [ns-info (.pollFirst deque)]
    (when (and ns-info (not @failed))
      (let [{:keys [requires]} ns-info
            input-set' @input-set
            {:keys [compiler-stats verbose]} opts]
        (if (every? #(not (contains? input-set' %)) requires)
          (do
            (try
              (swap! compiled conj
                (-compile (or (:source-file ns-info)
                              (:source-forms ns-info))
                  ; - ns-info -> ns -> cljs file relpath -> js relpath
                  (merge opts
                    {:output-file (comp/rename-to-js
                                    (util/ns->relpath (:ns ns-info)))})))
              (catch Throwable e
                (reset! failed e)))
            (when-not @failed
              (when-let [ns (:ns ns-info)]
                (swap! input-set disj ns))
              (recur (.pollFirst deque))))
          (do
            (Thread/sleep 10)
            (recur ns-info)))))))

(defn parallel-compile-sources [inputs compiler-stats opts]
  (module-graph/validate-inputs inputs)
  (let [deque     (LinkedBlockingDeque. inputs)
        input-set (atom (into #{} (comp (remove nil?) (map :ns)) inputs))
        cnt       (+ 2 (int (* 0.6 (.. Runtime getRuntime availableProcessors))))
        latch     (CountDownLatch. cnt)
        es        (Executors/newFixedThreadPool cnt)
        compiled  (atom [])
        failed    (atom false)]
    (dotimes [_ cnt]
      (.execute es
        (bound-fn []
          (compile-task deque input-set compiled opts failed)
          (.countDown latch))))
    (util/measure compiler-stats "Compile sources" (.await latch))
    (.shutdown es)
    (when @failed
      (throw @failed))
    @compiled))

(defn compile-sources
  "Takes dependency ordered list of IJavaScript compatible maps from parse-ns
  and compiles them."
  ([inputs opts]
   (compile-sources inputs (:compiler-stats opts) opts))
  ([inputs compiler-stats opts]
   (if (:parallel-build opts)
     (parallel-compile-sources inputs compiler-stats opts)
     (util/measure compiler-stats
       "Compile sources"
       (binding [comp/*inputs* (zipmap (map :ns inputs) inputs)]
         (doall
           (for [ns-info inputs]
             ; TODO: compile-file calls parse-ns unnecessarily to get ns-info
             ; TODO: we could mark dependent namespaces for recompile here
             (-compile (or (:source-file ns-info)
                           (:source-forms ns-info))
               ; - ns-info -> ns -> cljs file relpath -> js relpath
               (merge opts {:output-file (comp/rename-to-js (util/ns->relpath (:ns ns-info)))})))))))))

(defn remove-goog-base
  [inputs]
  (remove #(= (:provides %) ["goog"]) inputs))

(defn add-goog-base
  [inputs]
  (cons (javascript-file nil (io/resource "goog/base.js") ["goog"] nil)
        inputs))

(defn add-js-sources
  "Given list of IJavaScript objects, add foreign-deps, constants-table
   IJavaScript objects to the list."
  [inputs opts]
  (let [requires    (set (mapcat deps/-requires inputs))
        required-js (js-dependencies opts requires)]
    (concat
      (map
        (fn [{:keys [foreign url file provides requires] :as js-map}]
          (let [url (or url (io/resource file))]
            (merge
              (javascript-file foreign url provides requires)
              js-map)))
        required-js)
      (when (-> @env/*compiler* :options :emit-constants)
        [(constants-javascript-file opts)])
      inputs)))

(defn add-preloads
  "Add :preloads to a given set of inputs (IJavaScript). Returns a new
  list of inputs where the preloaded namespaces and their deps come immediately after
  cljs.core or the constants table depending on the optimization setting. Any
  files needing copying or compilation will be compiled and/or copied to the
  appropiate location."
  [inputs opts]
  (if-not (:preloads opts)
    inputs
    (let [pred     (fn [x]
                     (if (:emit-constants opts)
                       (not= [(str ana/constants-ns-sym)] (:provides x))
                       (not= ["cljs.core"] (:provides x))))
          pre      (take-while pred inputs)
          post     (drop-while pred inputs)
          preloads (remove nil?
                     (map
                       (fn [preload]
                         (try
                           (comp/find-source preload)
                           (catch Throwable t
                             (util/debug-prn "WARNING: preload namespace" preload "does not exist"))))
                       (:preloads opts)))]
      (distinct-by :provides
        (concat pre [(first post)]
          (-> (add-dependency-sources preloads opts)
            deps/dependency-order
            (compile-sources opts)
            (add-js-sources opts)
            deps/dependency-order)
          (next post))))))

(comment
  (comp/find-sources-root "samples/hello/src")
  (find-dependency-sources (find-sources-root "samples/hello/src"))
  (find-sources "samples/hello/src"))

(defn preamble-from-paths [paths]
  (when-let [missing (seq (remove io/resource paths))]
    (ana/warning :preamble-missing @env/*compiler* {:missing (sort missing)}))
  (let [resources (remove nil? (map io/resource paths))]
    (str (string/join "\n" (map slurp resources)) "\n")))

(defn make-preamble [{:keys [target preamble hashbang]}]
  (str (when (and (= :nodejs target) (not (false? hashbang)))
         (str "#!" (or hashbang "/usr/bin/env node") "\n"))
       (when preamble (preamble-from-paths preamble))))

;; Optimize
;; ========

(defmulti javascript-name class)

(defmethod javascript-name URL [^URL url]
  (if url (.getPath url) "cljs/user.js"))

(defmethod javascript-name String [s]
  (if-let [name (first (deps/-provides s))] name "cljs/user.js"))

(defmethod javascript-name JavaScriptFile [js]
  (when-let [url (deps/-url js)]
    (javascript-name url)))

(defn build-provides
  "Given a vector of provides, builds required goog.provide statements"
  [provides]
  (apply str (map #(str "goog.provide('" % "');\n") provides)))

(defmethod js-source-file JavaScriptFile [_ js]
  (if-let [url (deps/-url js)]
    (js-source-file (javascript-name url) (io/input-stream url))
    (when-let [source (:source js)]
      (js-source-file (javascript-name source) source))))

(defn ensure-cljs-base-module
  "Ensure that compiler :modules map has :cljs-base module with defined
  :output-to. If :output-to not provided will default to :output-dir location
  and the name of the file will be \"cljs_base.js.\""
  ([modules]
   (ensure-cljs-base-module modules
     (when env/*compiler*
       (:options @env/*compiler*))))
  ([modules opts]
   (update-in modules [:cljs-base :output-to]
     (fnil io/file
       (io/file
         (util/output-directory opts)
         "cljs_base.js")))))

(comment
  (ensure-cljs-base-module
    {:cljs-base
     {:output-to "out/modules/base.js"}
     :core
     {:output-to "out/modules/core.js"
      :entries '#{cljs.core}}
     :landing
     {:output-to "out/modules/reader.js"
      :entries '#{cljs.reader}
      :depends-on #{:core}}})
  )

(defn- const-expr-form
  "Returns the :const-expr form for `sym` from `compiler-state`."
  [compiler-state sym]
  (let [const-expr (get-in compiler-state [::ana/namespaces (symbol (namespace sym)) :defs (symbol (name sym)) :const-expr])]
    (some-> const-expr ana/const-expr->constant-value)))

(defn compile-loader
  "Special compilation pass for cljs.loader namespace. cljs.loader must be
  compiled last after all inputs. This is because all inputs must be known and
  they must already be sorted in dependency order."
  [inputs {:keys [modules] :as opts}]
  (when-let [loader (when (seq modules)
                      (->> inputs
                           (filter
                            (fn [input]
                              (some '#{"cljs.loader" cljs.loader}
                                    (:provides input))))
                           first))]
    (let [module-uris  (module-graph/modules->module-uris modules inputs opts)
          module-infos (module-graph/modules->module-infos modules)]
      (swap! env/*compiler* ana/add-consts
             {'cljs.core/MODULE_INFOS
              (merge (const-expr-form @env/*compiler* 'cljs.core/MODULE_INFOS) module-infos)
              'cljs.core/MODULE_URIS
              (merge (const-expr-form @env/*compiler* 'cljs.core/MODULE_URIS) module-uris)})
      (-compile (:source-file loader)
        (merge opts
          {:cache-key   (util/content-sha (pr-str module-uris))
           :output-file (comp/rename-to-js (util/ns->relpath (:ns loader)))}))))
  inputs)

(defn build-modules
  "Given a list of IJavaScript sources in dependency order and compiler options
   return a dependency sorted list of module name / description tuples. The
   module descriptions will be augmented with a :closure-module entry holding
   the Closure JSChunk. Each module description will also be augmented with
   a :foreign-deps vector containing foreign IJavaScript sources in dependency
   order."
  [sources opts]
  (let [sources (map
                  (fn [js]
                    (cond
                      (instance? JavaScriptFile js)
                      js
                      (map? js)
                      (map->JavaScriptFile js)
                      (string? js)
                      (merge
                        (map->javascript-file {:provides (deps/-provides js)})
                        {:source js})
                      :else js))
                  sources)
        used (atom #{}) ;; track used inputs to avoid dupes
        modules
        (reduce
          (fn [ret [name {:keys [entries depends-on] :as module-desc}]]
            (assert (or (= name :cljs-base) (not (empty? entries)))
              (str "Module " name " does not define any :entries"))
            (when (:verbose opts)
              (util/debug-prn "Building module" name))
            (let [js-module (JSChunk. (clojure.core/name name))
                  module-sources
                  (reduce
                    (fn [ret entry-sym]
                      (if-let [entries (module-graph/find-sources-for-module-entry entry-sym sources)]
                        (let [unused (set/difference entries @used)]
                          (swap! used into entries)
                          (into ret unused))
                        (throw
                          (util/compilation-error (IllegalArgumentException.
                                                    (str "Could not find matching namespace for " entry-sym))))))
                    [] entries)
                  foreign-deps (atom [])]
              ;; add inputs to module
              (doseq [ijs module-sources]
                (when (:verbose opts)
                  (util/debug-prn "  adding entry" (:provides ijs)))
                (if-not (deps/-foreign? ijs)
                  (.add js-module
                    ^SourceFile (js-source-file (javascript-name ijs) ijs))
                  (swap! foreign-deps conj ijs)))
              ;; add module dependencies, will always work
              ;; since modules are already in dependency order
              (doseq [dep depends-on]
                (if-let [parent-module (get-in (into {} ret) [dep :closure-module])]
                  (do
                    (when (:verbose opts)
                      (util/debug-prn "  module" name "depends on" dep))
                    (.addDependency js-module ^JSChunk parent-module))
                  (throw (util/compilation-error (IllegalArgumentException.
                                                   (str "Parent module " dep " does not exist"))))))
              (conj ret
                [name (assoc module-desc
                        :closure-module js-module
                        :foreign-deps @foreign-deps)])))
          [] (module-graph/sort-modules
               (ensure-cljs-base-module
                 (module-graph/expand-modules (:modules opts) sources) opts)))]
    modules))

(comment
  (build "samples/hello/src"
    {:optimizations :none
     :output-dir "out"
     :output-to "out/hello.js"
     :source-map true})

  (let [modules
        (build-modules
          [(map->javascript-file
             (ana/parse-ns 'cljs.core (io/file "out/cljs/core.js") nil))
           (map->javascript-file
             (ana/parse-ns 'cljs.reader (io/file "out/cljs/reader.js") nil))]
          {:optimizations  :advanced
           :output-dir     "out"
           :cache-analysis true
           :modules        {:core
                            {:output-to "out/modules/core.js"
                             :entries   '#{cljs.core}}
                            :landing
                            {:output-to  "out/modules/reader.js"
                             :entries    '#{cljs.reader}
                             :depends-on #{:core}}}})]
    modules)
  )

(defn emit-optimized-source-map
  "Given a JSON parsed Google Closure JavaScript to JavaScript source map,
   the entire list of original IJavaScript sources output a merged JavaScript
   to ClojureScript source map file with the given file name. opts should
   supply :preamble-line-count and :foreign-deps-line-count if they are
   relevant."
  [sm-json sources name opts]
  (let [closure-source-map (sm/decode-reverse sm-json)]
    (loop [sources (seq sources)
           relpaths {}
           merged (sorted-map-by
                    (sm/source-compare
                      (remove nil?
                        (map (fn [source]
                               (if-let [^URL source-url (:source-url source)]
                                 (.getPath source-url)
                                 (if-let [^URL url (:url source)]
                                   (.getPath url))))
                          sources))))]
      (if sources
        (let [source (first sources)]
          (recur
            (next sources)
            (let [{:keys [provides source-url]} source]
              (if (and provides source-url)
                (assoc relpaths
                  (.getPath ^URL source-url)
                  (util/ns->relpath (first provides) (util/ext source-url)))
                relpaths))
            (if-let [url (:url source)]
              (let [path (.getPath ^URL url)]
                (if-let [compiled (get-in @env/*compiler* [::comp/compiled-cljs path])]
                  (if-let [source-url (:source-url source)]
                    (assoc merged
                      (.getPath ^URL source-url)
                      (sm/merge-source-maps
                        (:source-map compiled)
                        (get closure-source-map path)))
                    merged)
                  (assoc merged path (get closure-source-map path))))
              merged)))
        (spit
          (io/file name)
          (sm/encode merged
            {:preamble-line-count (+ (:preamble-line-count opts 0)
                                     (:foreign-deps-line-count opts 0))
             :lines (+ (:lineCount sm-json)
                       (:preamble-line-count opts 0)
                       (:foreign-deps-line-count opts 0)
                       2)
             :file name
             :output-dir (util/output-directory opts)
             :source-map (:source-map opts)
             :source-map-path (:source-map-path opts)
             :source-map-timestamp (:source-map-timestamp opts)
             :source-map-pretty-print (:source-map-pretty-print opts)
             :relpaths relpaths}))))))

(defn write-variable-maps [^Result result opts]
  (let [var-out (:closure-variable-map-out opts)]
    (when-let [var-map (and var-out (.-variableMap result))]
      (util/mkdirs var-out)
      (io/copy (ByteArrayInputStream. (.toBytes var-map))
        (io/file var-out))))
  (let [prop-out (:closure-property-map-out opts)]
    (when-let [prop-map (and prop-out (.-propertyMap result))]
      (util/mkdirs prop-out)
      (io/copy (ByteArrayInputStream. (.toBytes prop-map))
        (io/file prop-out)))))

(defn optimize-modules
  "Use the Closure Compiler to optimize one or more Closure JSChunks. Returns
   a dependency sorted list of module name and description tuples."
  [opts & sources]
  ;; the following pre-condition can't be enabled
  ;; lein-cljsbuild adds :output-to?
  #_{:pre [(and (contains? opts :modules)
                (not (contains? opts :output-to)))]}
  (assert (= (count (:modules opts))
             (count (into #{}
                      (map (comp :output-to second)
                        (:modules opts)))))
    "Each :output-to of :modules must be unique")
  (let [closure-compiler (make-closure-compiler)
        ^List externs (load-externs opts)
        compiler-options (make-options opts)
        _ (.initOptions closure-compiler compiler-options)
        sources (if (= :whitespace (:optimizations opts))
                  (cons "var CLOSURE_NO_DEPS = true;" sources)
                  sources)
        modules (build-modules sources opts)
        ^List inputs (map (comp :closure-module second) modules)
        _ (doseq [^JSChunk input inputs]
            (.sortInputsByDeps input closure-compiler))
        _ (when (or ana/*verbose* (:verbose opts))
            (util/debug-prn "Applying optimizations" (:optimizations opts) "to" (count sources) "sources"))
        ^Result result (.compileModules closure-compiler externs inputs compiler-options)
        ^SourceMap source-map (when (:source-map opts)
                                (.getSourceMap closure-compiler))]
    (assert (or (nil? (:source-map opts)) source-map)
      "Could not create source maps for modules")
    (if (.success result)
      (do
        (write-variable-maps result opts)
        (vec
          (for [[name {:keys [output-to closure-module] :as module}] modules]
            [name
             (merge
               (assoc module
                 :source
                 (do
                   (when source-map (.reset source-map))
                   (.toSource closure-compiler ^JSChunk closure-module)))
               (when source-map
                 (let [sw (StringWriter.)
                       source-map-name (str output-to ".map.closure")]
                   (.appendTo source-map sw source-map-name)
                   {:source-map-json (.toString sw)
                    :source-map-name source-map-name})))])))
      (report-failure result))))

(defn ->js-source-files [sources]
  (doall
    (map (fn [src]
           (let [src' (cond-> src
                        (and (not (record? src)) (map? src))
                        map->javascript-file)]
             (js-source-file (javascript-name src') src')))
      sources)))

(defn optimize
  "Use the Closure Compiler to optimize one or more JavaScript files."
  [opts & sources]
  (when (or ana/*verbose* (:verbose opts))
    (util/debug-prn "Applying optimizations" (:optimizations opts) "to" (count sources) "sources"))
  (let [closure-compiler (make-closure-compiler)
        ^List externs (load-externs opts)
        compiler-options (make-options opts)
        sources (if (= :whitespace (:optimizations opts))
                  (cons "var CLOSURE_NO_DEPS = true;" sources)
                  sources)
        ^List inputs (->js-source-files sources)
        ^Result result (util/measure (:compiler-stats opts)
                         "Optimizing with Google Closure Compiler"
                         (.compile closure-compiler externs inputs compiler-options))]
    (if (.success result)
      ;; compiler.getSourceMap().reset()
      (do
        (write-variable-maps result opts)
        (let [source (.toSource closure-compiler)]
         (when-let [name (:source-map opts)]
           (let [name' (str name ".closure")
                 sw (StringWriter.)
                 sm-json-str (do
                               (.appendTo (.getSourceMap closure-compiler) sw name')
                               (.toString sw))]
             (when (true? (:closure-source-map opts))
               (spit (io/file name') sm-json-str))
             (emit-optimized-source-map
               (json/read-str sm-json-str :key-fn keyword)
               sources name
               (assoc opts
                 :preamble-line-count
                 (+ (- (count (.split #"\r?\n" (make-preamble opts) -1)) 1)
                    (if (:output-wrapper opts) 1 0))))))
         source))
      (report-failure result))))

(comment
  ;; optimize JavaScript strings
  (optimize {:optimizations :whitespace} "var x = 3 + 2; alert(x);")
  ;; => "var x=3+2;alert(x);"
  (optimize {:optimizations :simple} "var x = 3 + 2; alert(x);")
  ;; => "var x=5;alert(x);"
  (optimize {:optimizations :advanced} "var x = 3 + 2; alert(x);")
  ;; => "alert(5);"

  ;; optimize a ClojureScript form
  (optimize {:optimizations :simple} (-compile '(def x 3) {}))
  )

;; Output
;; ======
;;
;; The result of a build is always a single string of JavaScript. The
;; build process may produce files on disk but a single string is
;; always output. What this string contains depends on whether the
;; input has been optimized or not. If the :output-to option is set
;; then this string will be written to the specified file. If not, it
;; will be returned.
;;
;; The :output-dir option can be used to set the working directory
;; where any files will be written to disk. By default this directory
;; is 'out'.
;;
;; If inputs are optimized then the output string will be the complete
;; application with all dependencies included.
;;
;; For unoptimized output, the string will be a Closure deps file
;; describing where the JavaScript files are on disk and their
;; dependencies. All JavaScript files will be located in the working
;; directory, including any dependencies from the Closure library.
;;
;; Unoptimized mode is faster because the Closure Compiler is not
;; run. It also makes debugging much simpler because each file is
;; loaded in its own script tag.
;;
;; When working with uncompiled files, you will need to add additional
;; script tags to the hosting HTML file: one which pulls in Closure
;; library's base.js and one which calls goog.require to load your
;; code. See samples/hello/hello-dev.html for an example.

(defn ^String path-relative-to
  "Generate a string which is the path to the input IJavaScript relative
  to the specified base file."
  [^File base input]
  (let [base-path  (util/path-seq (.getCanonicalPath base))
        input-path (util/path-seq (.getCanonicalPath (io/file (deps/-url input))))
        count-base (count base-path)
        common     (count (take-while true? (map #(= %1 %2) base-path input-path)))
        prefix     (repeat (- count-base common 1) "..")]
    (if (= count-base common)
      (last input-path) ;; same file
      (util/to-path (concat prefix (drop common input-path)) "/"))))

(defn add-dep-string
  "Return a goog.addDependency string for an input."
  [opts input]
  (letfn [(ns-list [coll] (when (seq coll) (apply str (interpose ", " (map #(str "'" (comp/munge %) "'") coll)))))]
    (str "goog.addDependency(\""
         (path-relative-to
           (io/file (util/output-directory opts) "goog" "base.js") input)
         "\", ["
         (ns-list (deps/-provides input))
         "], ["
         ;; even under Node.js where runtime require is possible
         ;; this is necessary - see CLJS-2151
         (ns-list (cond->> (deps/-requires input)
                    ;; under Node.js we emit native `require`s for these
                    (= :nodejs (:target opts))
                    (filter (complement ana/node-module-dep?))))
         "]"
         (if (deps/-foreign? input) ", {'foreign-lib': true}")
         ");\n")))

(defn deps-file
  "Return a deps file string for a sequence of inputs."
  [opts sources]
  (apply str (map #(add-dep-string opts %) sources)))

(comment
  (path-relative-to (io/file "out/goog/base.js") {:url (deps/to-url "out/cljs/core.js")})
  (add-dep-string {} {:url (deps/to-url "out/cljs/core.js") :requires ["goog.string"] :provides ["cljs.core"]})
  (deps-file {:output-dir "pubic/js"} [{:url (deps/to-url "out/cljs/core.js") :requires ["goog.string"] :provides ["cljs.core"]}])
  )

(defn elide-strict [js {:keys [elide-strict] :as opts}]
  (cond-> js
    (not (false? elide-strict)) (string/replace #"(?m)^['\"]use strict['\"]" "            ")))

(defn ^File fingerprint-out-file
  [content ^File out-file]
  (let [dir  (.getParent out-file)
        fn   (.getName out-file)
        idx  (.lastIndexOf fn ".")
        ext  (subs fn (inc idx))
        name (subs fn 0 idx)]
    (io/file dir
      (str name "-"
        (string/lower-case
          (util/content-sha content 7)) "." ext))))

(defn output-one-file [{:keys [output-to fingerprint] :as opts} js]
  (let [js (elide-strict js opts)]
    (cond
      (nil? output-to) js

      (or (string? output-to)
          (util/file? output-to))
      (let [f (io/file output-to)]
        (util/mkdirs f)
        (spit f js)
        (when fingerprint
          (let [dir  (.getParent f)
                mf   (io/file dir "manifest.edn")
                g    (fingerprint-out-file js f)]
            (.renameTo f g)
            (spit mf (pr-str {(.toString f) (.toString g)})))))

      :else (println js))))

(defn output-deps-file [opts sources]
  (output-one-file opts (deps-file opts sources)))

(declare foreign-deps-str add-header add-source-map-link)

(defn preloads
  ([syms]
    (preloads syms nil))
  ([syms mode]
   (letfn [(preload-str [sym]
             (str (when (= :browser mode) "document.write('<script>")
                  "goog.require(\"" (comp/munge sym) "\");"
                  (if (= :browser mode) "</script>');\n" "\n")))]
     (map preload-str syms))))

(defn bundle? [opts]
  (false? (:nodejs-rt opts)))

(defn export-dep [dep]
  (str "\""dep "\": require('" dep "')" ))

(defn npm-deps-js
  "Returns the JavaScript code to support runtime require of bundled modules."
  [node-requires]
  (str
    "module.exports = {\n"
    "  npmDeps: {\n"
    (string/join ",\n" (map (comp #(str "    " %) export-dep) node-requires))
    "  }\n"
    "};\n"))

(defn output-main-file
  "Output an entry point. In the non-modules case, opts is simply compiler
  options. When emitting a module entry point, opts must contain :module-name."
  [opts]
  (assert (or (not (contains? opts :module-name))
              (get (:modules opts) (:module-name opts)))
    (str "Module " (:module-name opts) " does not exist"))
  (let [module (get (:modules opts) (:module-name opts))]
    (output-one-file
      (merge opts
        (when module
          {:output-to (:output-to module)}))
      (if-let [target-fn (opts-fn :target-fn opts)]
        (target-fn opts)
        (let [asset-path (or (:asset-path opts)
                             (util/output-directory opts))
              closure-defines (json/write-str (:closure-defines opts))]
          (case (:target (cond-> opts (bundle? opts) (dissoc :target)))
            :nodejs
            (add-header opts
              (str (when (or (not module) (= :cljs-base (:module-name opts)))
                     (str "var path = require(\"path\");\n"
                          "try {\n"
                          "    require(\"source-map-support\").install();\n"
                          "} catch(err) {\n"
                          "}\n"
                          "require(path.join(path.resolve(\".\"),\"" asset-path "\",\"goog\",\"bootstrap\",\"nodejs.js\"));\n"
                          "require(path.join(path.resolve(\".\"),\"" asset-path "\",\"cljs_deps.js\"));\n"
                          "goog.global.CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
                       (apply str (preloads (:preloads opts)))))
                (apply str
                  (map (fn [entry]
                         (str "goog.require(\"" (comp/munge entry) "\");\n"))
                    (if-let [entries (when module (:entries module))]
                      entries
                      [(:main opts)])))
                (when (:nodejs-rt opts)
                  "goog.require(\"cljs.nodejscli\");\n")))

            :webworker
            (str (when (or (not module) (= :cljs-base (:module-name opts)))
                   (str "var CLOSURE_BASE_PATH = \"" asset-path "/goog/\";\n"
                        "var CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
                        "var CLOSURE_IMPORT_SCRIPT = (function(global) { return function(src) {global['importScripts'](src); return true;};})(this);\n"
                        "if(typeof goog == 'undefined') importScripts(\"" asset-path "/goog/base.js\");\n"
                        "importScripts(\"" asset-path "/cljs_deps.js\");\n"
                     (apply str (preloads (:preloads opts)))))
              (apply str
                (map (fn [entry]
                       (when-not (= "goog" entry)
                         (str "goog.require(\"" (comp/munge entry) "\");\n")))
                  (if-let [entries (when module (:entries module))]
                    entries
                    (when-let [main (:main opts)]
                      [main])))))

            (str
              (when (bundle? opts)
                "import {npmDeps} from \"./npm_deps.js\";\n")
              (when (or (not module) (= :cljs-base (:module-name opts)))
                (str
                  "window.CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
                  "window.CLOSURE_NO_DEPS = true;\n"
                  "if(typeof goog == \"undefined\") document.write('<script src=\"" asset-path "/goog/base.js\"></script>');\n"
                  "document.write('<script src=\"" asset-path "/goog/deps.js\"></script>');\n"
                  "document.write('<script src=\"" asset-path "/cljs_deps.js\"></script>');\n"
                  "document.write('<script>if (typeof goog == \"undefined\") console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\");</script>');\n"
                  (apply str (preloads (:preloads opts) :browser))))
              (apply str
                (map (fn [entry]
                       (when-not (= "goog" entry)
                         (str "document.write('<script>goog.require(\"" (comp/munge entry) "\");</script>');\n")))
                  (if-let [entries (when module (:entries module))]
                    entries
                    (when-let [main (:main opts)]
                      [main]))))
              (when (bundle? opts)
                (str
                  "window.require = function(lib) {\n"
                  "   return npmDeps[lib];\n"
                  "}\n")))))))))

(defn fingerprinted-modules [modules fingerprint-info]
  (into {}
    (map
      (fn [[module-name module-info]]
        (let [module-info'
              (assoc module-info :output-to
                (get-in fingerprint-info
                  [module-name :output-to-fingerprint]))]
          [module-name module-info'])))
    modules))

(defn output-modules
  "Given compiler options, original IJavaScript sources and a sequence of
   module name and module description tuples output module sources to disk.
   Modules description must define :output-to and supply :source entry with
   the JavaScript source to write to disk."
  [opts js-sources modules]
  (let [fingerprint-info (atom {})]
    (doseq [[name {:keys [output-to source foreign-deps] :as module-desc}] modules]
      (assert (not (nil? output-to))
        (str "Module " name " does not define :output-to"))
      (assert (not (nil? source))
        (str "Module " name " did not supply :source"))
      (let [fdeps-str (when-not (empty? foreign-deps)
                        (foreign-deps-str opts foreign-deps))
            sm-name   (when (:source-map opts)
                        (str output-to ".map"))
            out-file  (io/file output-to)
            _         (util/mkdirs out-file)
            js        (as-> source source
                        (if (= name :cljs-base)
                          (add-header opts source)
                          source)
                        (if fdeps-str
                          (str fdeps-str "\n" source)
                          source)
                        (elide-strict source opts)
                        (if sm-name
                          (add-source-map-link
                            (assoc opts
                              :output-to output-to
                              :source-map sm-name)
                            source)
                          source))
            fingerprint-base? (and (:fingerprint opts) (= :cljs-base name))]
        (when-not fingerprint-base?
          (spit out-file js))
        (when (:fingerprint opts)
          (let [out-file' (fingerprint-out-file js out-file)]
            (when-not fingerprint-base?
              (.renameTo out-file out-file'))
            (swap! fingerprint-info update name merge
              (when fingerprint-base? {:source js})
              {:output-to             (.toString output-to)
               :output-to-fingerprint (.toString out-file')})))
        (when (:source-map opts)
          (let [sm-json-str (:source-map-json module-desc)
                sm-json (json/read-str sm-json-str :key-fn keyword)]
            (when (true? (:closure-source-map opts))
              (spit (io/file (:source-map-name module-desc)) sm-json-str))
            (emit-optimized-source-map sm-json js-sources sm-name
              (merge opts
                {:source-map sm-name
                 :preamble-line-count
                 (if (= name :cljs-base)
                   (+ (- (count (.split #"\r?\n" (make-preamble opts) -1)) 1)
                      (if (:output-wrapper opts) 1 0)
                      (if (:fingerprint opts) 1 0))
                   0)
                 :foreign-deps-line-count
                 (if fdeps-str
                   (- (count (.split #"\r?\n" fdeps-str -1)) 1)
                   0)}))))))
    (when (:fingerprint opts)
      (let [fi   @fingerprint-info
            g    (get-in fi [:cljs-base :output-to-fingerprint])
            out  (io/file g)
            dir  (.getParent out)
            mnf  (io/file dir "manifest.edn")
            uris (module-graph/modules->module-uris
                   (fingerprinted-modules modules fi) js-sources opts)]
        (spit mnf
          (pr-str
            (into {}
              (map (juxt :output-to :output-to-fingerprint))
              (vals fi))))
        (spit out
          (str "var COMPILED_MODULE_URIS = "
            (json/write-str
              (into {}
                (map (fn [[k v]] [(-> k name munge) v])) uris))
            ";\n"
            (get-in fi [:cljs-base :source])))))))

(defn lib-rel-path [{:keys [lib-path url provides] :as ijs}]
  (if (nil? lib-path)
    (util/ns->relpath (first provides) "js")
    (if (.endsWith lib-path ".js")
      (util/get-name url)
      (let [path (util/path url)
            lib-path (util/normalize-path lib-path)]
        (subs path (+ (inc (.lastIndexOf path lib-path)) (.length lib-path)))))))

(defn ^String rel-output-path
  "Given a IJavaScript which points to a .js file either in memory, in a jar file,
  or is a foreign lib, return the path relative to the output directory."
  ([js]
   (rel-output-path js
     (when env/*compiler*
       (:options @env/*compiler*))))
  ([js opts]
   (let [url (deps/-url js opts)]
     (cond
       url
       (cond
         (deps/-closure-lib? js) (lib-rel-path js)
         (deps/-foreign? js) (or (deps/-relative-path js opts)
                                 (util/relative-name url))
         :else (path-from-jarfile url))

       (string? js)
       (str (util/content-sha js 7) ".js")

       :else (str (random-string 5) ".js")))))

(defn get-source-files [js-modules opts]
  (map (fn [lib]
         (let [file (if-let [file-min (and (#{:advanced :simple} (:optimizations opts))
                                           (:file-min lib))]
                      file-min
                      (:file lib))]
           (js-source-file file (deps/-source lib))))
    js-modules))

(defn make-convert-js-module-options [opts]
  (-> opts
    (select-keys
      [:closure-warnings :closure-extra-annotations :pretty-print
       :language-in :language-out :closure-module-roots :rewrite-polyfills])
    (assoc-in [:closure-warnings :non-standard-jsdoc] :off)
    (set-options (CompilerOptions.))))

(defn module-type->keyword [^CompilerInput$ModuleType module-type]
  (case (.name module-type)
    "NONE" :none
    "GOOG" :goog
    "ES6" :es6
    "COMMONJS" :commonjs
    "JSON" :json
    "IMPORTED_SCRIPT" :imported-script))

(defn add-converted-source
  [closure-compiler inputs-by-name opts {:keys [file-min file provides requires] :as ijs}]
  (let [processed-file (if-let [min (and (#{:advanced :simple} (:optimizations opts))
                                         file-min)]
                         min
                         file)
        processed-file (string/replace processed-file "\\" "/")
        ^CompilerInput input (get inputs-by-name processed-file)
        ^Node ast-root (.getAstRoot input closure-compiler)
        provides (distinct (map #(ModuleNames/fileToModuleName %)
                                (cons processed-file provides)))
        ;; getJsModuleType returns NONE for ES6 files, but getLoadsFlags module returns es6 for those
        module-type (or (some-> (.get (.getLoadFlags input) "module") keyword)
                        (module-type->keyword (.getJsModuleType input)))]
    (assoc ijs
      :module-type module-type
      :source
      ;; Add goog.provide/require calls ourselves, not emited by Closure since
      ;; https://github.com/google/closure-compiler/pull/2641
      (str
        (apply str (map (fn [n]
                          (str "goog.provide(\"" n "\");\n"))
                        provides))
        (->> (.getRequires input)
             ;; v20180204 returns string
             ;; next Closure returns DependencyInfo.Require object
             (map (fn [i]
                    (if (string? i)
                      i
                      (.getSymbol i))))
             ;; If CJS/ES6 module uses goog.require, goog is added to requires
             ;; but this would cause problems with Cljs.
             (remove #{"goog"})
             (map (fn [n]
                    (str "goog.require(\"" n "\");\n")))
             (apply str))
        (.toSource closure-compiler ast-root)))))

(defn- package-json-entries
  "Takes options and returns a sequence with the desired order of package.json
   entries for the given :package-json-resolution mode. If no mode is provided,
   defaults to :webpack (if no target is set) and :nodejs (if the target is
   :nodejs)."
  [opts]
  {:pre [(or (= (:package-json-resolution opts) :webpack)
             (= (:package-json-resolution opts) :nodejs)
             (and (sequential? (:package-json-resolution opts))
                  (every? string? (:package-json-resolution opts)))
             (not (contains? opts :package-json-resolution)))]}
  (let [modes {:nodejs ["main"]
               :webpack ["browser" "module" "main"]}]
    (if-let [mode (:package-json-resolution opts)]
      (if (sequential? mode) mode (get modes mode))
      (case (:target opts)
        :nodejs (:nodejs modes)
        (:webpack modes)))))

(comment
  (= (package-json-entries {}) ["browser" "module" "main"])
  (= (package-json-entries {:package-json-resolution :nodejs}) ["main"])
  (= (package-json-entries {:package-json-resolution :webpack}) ["browser" "module" "main"])
  (= (package-json-entries {:package-json-resolution ["foo" "bar" "baz"]}) ["foo" "bar" "baz"])
  (= (package-json-entries {:target :nodejs}) ["main"])
  (= (package-json-entries {:target :nodejs :package-json-resolution :nodejs}) ["main"])
  (= (package-json-entries {:target :nodejs :package-json-resolution :webpack}) ["browser" "module" "main"])
  (= (package-json-entries {:target :nodejs :package-json-resolution ["foo" "bar"]}) ["foo" "bar"]))

(defn- sorting-dependency-options []
  (try
    (if (contains? (:flags (clojure.reflect/reflect DependencyOptions)) :abstract)
      (eval '(do
               (import '(com.google.javascript.jscomp DependencyOptions))
               (DependencyOptions/sortOnly)))
      (doto (DependencyOptions.)
        (.setDependencySorting true)))))

(defn convert-js-modules
  "Takes a list JavaScript modules as an IJavaScript and rewrites them into a Google
  Closure-compatible form. Returns list IJavaScript with the converted module
  code set as source."
  [js-modules opts]
  (let [^List externs '()
        ^List source-files (get-source-files js-modules opts)
        ^CompilerOptions options (doto (make-convert-js-module-options opts)
                                   (.setProcessCommonJSModules true)
                                   (.setLanguageIn (lang-key->lang-mode :ecmascript6))
                                   (.setLanguageOut (lang-key->lang-mode (:language-out opts :ecmascript3)))
                                   (.setDependencyOptions (sorting-dependency-options))
                                   (.setPackageJsonEntryNames ^List (package-json-entries opts)))
        closure-compiler (doto (make-closure-compiler)
                           (.init externs source-files options))
        _ (.parse closure-compiler)
        _ (report-failure (.getResult closure-compiler))
        inputs-by-name (into {} (map (juxt #(.getName %) identity) (vals (.getInputsById closure-compiler))))]

    ;; This will take care of converting ES6 to CJS
    ;; Based on language-in setting, this could also handle ES7/8/TypeScript transpilation.
    (.transpileAndDontCheck closure-compiler)
    ;; This will rewrite CommonJS modules
    (.whitespaceOnlyPasses closure-compiler)

    (map (partial add-converted-source
           closure-compiler inputs-by-name opts)
      js-modules)))

(defmulti js-transforms
  "Takes an IJavaScript with the source code set as source, transforms the
  source code and returns an IJavascript with the new code set as source."
  (fn [ijs opts]
    (:preprocess ijs)))

(defmethod js-transforms :default [ijs opts]
  (ana/warning :unsupported-preprocess-value @env/*compiler* ijs)
  ijs)

(defn url->nio-path [url]
  (let [raw-uri (.toURI url)
        arr     (-> raw-uri .toString (.split "!"))
        uri     (-> arr (aget 0) URI/create)
        fs      (try
                  (FileSystems/getFileSystem uri)
                  (catch Throwable t
                    (FileSystems/newFileSystem uri (HashMap.))))]
    (.getPath fs ^String (.toString raw-uri) (make-array String 0))))

(defn add-goog-load [source]
  (let [sb (StringBuilder.)
        module (-> (SimpleDependencyInfo/builder "" "")
                 (.setGoogModule true) .build)
        bundler (ClosureBundler.)]
    (.appendTo bundler sb module source)
    (.toString sb)))

(defn ^DiagnosticGroup es5-warnings []
  (DiagnosticGroup.
    (into-array DiagnosticType
      [(DiagnosticType/error "JSC_CANNOT_CONVERT" "")])))

(defn ^CompilerOptions transpile-options []
  (doto (CompilerOptions.)
    (.setQuoteKeywordProperties true)
    (.setSkipNonTranspilationPasses true)
    (.setVariableRenaming VariableRenamingPolicy/OFF)
    (.setPropertyRenaming PropertyRenamingPolicy/OFF)
    (.setWrapGoogModulesForWhitespaceOnly false)
    (.setPrettyPrint true)
    (.setSourceMapOutputPath "/dev/null")
    (.setSourceMapIncludeSourcesContent true)
    (.setWarningLevel (es5-warnings) CheckLevel/OFF)))

(defn closure-transpile
  "Transpile a single JavaScript file to JavaScript. Used to lower Closure
  Library files written in more recent versions of the JavaScript standard."
  ([rsc opts]
   (closure-transpile (util/path rsc) (slurp rsc) opts))
  ([path source opts]
   (let [cc      (make-closure-compiler)
         cc-opts (set-options opts (transpile-options))
         externs (SourceFile/fromCode "externs.js" "function Symbol() {}")
         source  (SourceFile/fromCode path source)
         result  (.compile cc externs source cc-opts)]
     ;; TODO: error handling
     (.toSource cc))))

;; TODO: better error handling
(defn transpile
  [{:keys [language-out] :or {language-out :es3} :as opts} rsc {:keys [module lang] :as js}]
  (let [source  (slurp rsc)
        source' (if (and lang
                         (< (.indexOf lang-level (expand-lang-key language-out))
                            (.indexOf lang-level (expand-lang-key lang))))
                  (closure-transpile (util/path rsc) source opts)
                  source)]
    (str "/*TRANSPILED*/"
      (cond-> source'
        (= :goog module) add-goog-load))))

(defn requires-transpile? [out-file]
  (let [line (first (line-seq (io/reader out-file)))]
    (not (string/starts-with? line "/*TRANSPILED*/"))))

(comment
  (println (slurp (io/resource "goog/math/long.js")))

  (deps/parse-js-ns (-> (io/resource "goog/math/long.js") io/reader line-seq))
  (deps/parse-js-ns (-> (io/resource "goog/string/stringbuffer.js") io/reader line-seq))

  (url->nio-path (io/resource "goog/math/long.js"))

  (println
    (maybe-transpile {} (io/resource "goog/math/long.js") {:module :goog :lang :es6}))
  )

(defn transpile? [opts {:keys [module lang]}]
  (or module lang))

(defn write-javascript
  "Write or copy a JavaScript file to output directory. Only write if the file
   does not already exist. Return IJavaScript for the file on disk at the new
   location."
  [{:keys [optimizations] :as opts} js]
  (let [out-dir    (io/file (util/output-directory opts))
        out-name   (rel-output-path js opts)
        out-file   (io/file out-dir out-name)
        res        (or (:url js) (:source-file js))
        js-module? (and res out-dir
                     (.startsWith (util/path res) (util/path out-dir))) ;; We already Closure processed it and wrote it out
        transpile? (transpile? opts js)
        ijs        (merge
                     {:requires (deps/-requires js)
                      :provides (deps/-provides js)
                      :group (:group js)}
                     (when-not js-module?
                       {:url (deps/to-url out-file)
                        :out-file (.toString out-file)}))]
    (when (and (not js-module?)
               (or (not (.exists out-file))
                   (and res (util/changed? out-file res))
                   ;; always re-emit GCL libs under optimizations higher than :none
                   ;; :none will just use the cached transpiled result
                   (and transpile?
                        (or (not= :none optimizations)
                            (requires-transpile? out-file)))))
      (when (and res (or ana/*verbose* (:verbose opts)))
        (util/debug-prn "Copying" (str res) "to" (str out-file)))
      (util/mkdirs out-file)
      (if (and transpile? (= :none optimizations))
        (spit out-file (transpile opts res js))
        (spit out-file (deps/-source js)))
      (when res
        (.setLastModified ^File out-file (util/last-modified res))))
    (if (map? js)
      (merge js ijs)
      ijs)))

(defn write-js?
  "Returns true if IJavaScript instance needs to be written/copied to output
  directory. True when in memory, in a JAR, or if foreign library."
  [js]
  (try
    (let [url ^URL (deps/-url js)]
      (or (not url)
          (= (.getProtocol url) "jar")
          (deps/-closure-lib? js)
          (deps/-foreign? js)))
    (catch Throwable t
      (throw (util/compilation-error (Exception. (str "Could not write JavaScript " (pr-str js))))))))

(defn source-on-disk
  "Ensure that the given IJavaScript exists on disk in the output directory.
   Return updated IJavaScript with the new location if necessary."
  [opts js]
  (if (write-js? js)
    (write-javascript opts js)
    ;; always copy original ClojureScript sources to the output directory
    ;; when source maps enabled
    (let [source-url  (:source-url js)
          out-file (when-let [ns (and (:source-map opts)
                                   source-url
                                   (first (:provides js)))]
                     (io/file (io/file (util/output-directory opts))
                       (util/ns->relpath ns (util/ext source-url))))]
      (when (and out-file source-url
              (or (not (.exists ^File out-file))
                (util/changed? (io/file source-url) out-file)))
        (do
          (when (or ana/*verbose* (:verbose opts))
            (util/debug-prn "Copying" (str source-url) "to" (str out-file)))
          (util/mkdirs out-file)
          (spit out-file (slurp source-url))
          (.setLastModified ^File out-file (util/last-modified source-url))))
      js)))

(comment
  (write-javascript {} "goog.provide('demo');\nalert('hello');\n")
  ;; write something from a jar file to disk
  (source-on-disk {}
                  {:url (io/resource "goog/base.js")
                   :source (with-open [reader (io/reader (io/resource "goog/base.js"))]
                             (slurp reader))})
  ;; doesn't write a file that is already on disk
  (source-on-disk {} {:url (io/resource "cljs/core.cljs")})
  )

(defn output-unoptimized
  "Ensure that all JavaScript source files are on disk (not in jars),
   write the goog deps file including only the libraries that are being
   used and write the deps file for the current project.

   The deps file for the current project will include third-party
   libraries."
  [{:keys [modules] :as opts} & sources]
  ;; this source-on-disk call is currently necessary for REPLs - David
  (let [disk-sources (doall (map #(source-on-disk opts %) sources))
        goog-deps    (io/file (util/output-directory opts) "goog" "deps.js")
        main         (:main opts)
        output-deps  #(output-deps-file
                        (assoc opts :output-to
                          (str (util/output-directory opts)
                            File/separator "cljs_deps.js"))
                        disk-sources)]
    (util/mkdirs goog-deps)
    (spit goog-deps (slurp (io/resource "goog/deps.js")))
    (when (:debug-inputs opts)
      (util/debug-prn "DEBUG: all compiler inputs")
      (util/debug-prn (pr-str sources)))
    (cond
      modules
      (let [modules' (module-graph/expand-modules modules sources)]
        (output-deps)
        (doall
          (map
            (fn [[module-name _]]
              (output-main-file
                (merge opts
                  {:module-name module-name
                   :modules modules'})))
            modules)))

      (and main (not= :none (:target opts)))
      (do
        (output-deps)
        (output-main-file opts))

      :else (output-deps-file opts disk-sources))))

(defn get-upstream-deps*
  "returns a merged map containing all upstream dependencies defined
  by libraries on the classpath."
  ([]
   (get-upstream-deps* (. (Thread/currentThread) (getContextClassLoader))))
  ([classloader]
   (let [upstream-deps (map #(read-string (slurp %))
                         (enumeration-seq (. classloader (getResources "deps.cljs"))))]
     (apply merge-with
       (fn [a b]
         (if (map? a)
           (merge-with #(into #{%1} #{%2}) a b)
           (concat a b)))
       upstream-deps))))

(def get-upstream-deps (memoize get-upstream-deps*))

(defn add-header [opts js]
  (str (make-preamble opts) js))

(defn foreign-deps-str [opts sources]
  (letfn [(to-js-str [ijs]
            (if-let [url (or (and (#{:advanced :simple} (:optimizations opts))
                                  (:url-min ijs))
                             (:url ijs))]
              (slurp url)
              (throw (util/compilation-error (IllegalArgumentException.
                                               (str "Foreign lib " ijs " does not exist"))))))]
    (str (string/join "\n" (map to-js-str sources)) "\n")))

(defn add-wrapper [{:keys [output-wrapper] :as opts} js]
  (if output-wrapper
    (cond
      (fn? output-wrapper)     (output-wrapper js)
      (string? output-wrapper) (format output-wrapper js)
      :else                    (str ";(function(){\n" js "\n})();\n"))
    js))

(defn add-source-map-link [{:keys [source-map output-to] :as opts} js]
  (if source-map
    (if (= output-to :print)
      (str js "\n//# sourceMappingURL=" source-map "\n\n")
      (str js "\n//# sourceMappingURL=" (path-relative-to (io/file output-to) {:url source-map}) "\n\n"))
    js))

(defn absolute-path? [path]
  (.isAbsolute (io/file path)))

(defn absolute-parent [path]
  (.getParent (.getAbsoluteFile (io/file path))))

(defn in-same-dir?
  "Checks that path-1 and path-2 are siblings in the same logical directory."
  [path-1 path-2]
  (= (absolute-parent path-1)
     (absolute-parent path-2)))

(defn same-or-subdirectory-of?
  "Checks that path names a file or directory that is the dir or a subdirectory there of."
  [dir path]
  (let [dir-path  (.getAbsolutePath (io/file dir))
        path-path (.getAbsolutePath (io/file path))]
    (.startsWith path-path dir-path)))

(defn check-output-to [{:keys [output-to] :as opts}]
  (when (contains? opts :output-to)
    (assert (or (string? output-to)
                (= :print output-to))
            (format ":output-to %s must specify a file or be :print"
                    (pr-str output-to))))
  true)

(defn check-output-dir [{:keys [output-dir] :as opts}]
  (when (contains? opts :output-dir)
    (assert (string? output-dir)
            (format ":output-dir %s must specify a directory"
                    (pr-str output-dir))))
  true)

(defn check-source-map
  "When :source-map is specified in opts, "
  [{:keys [output-to source-map output-dir optimizations] :as opts}]
  (when (and (contains? opts :source-map)
             (:source-map opts)
             (not (= optimizations :none)))
    (assert (and (or (contains? opts :output-to)
                     (contains? opts :modules))
                 (contains? opts :output-dir))
      (str ":source-map cannot be specified without also specifying :output-dir "
           "and either :output-to or :modules if optimization setting applied"))
    (assert (or (nil? (:output-to opts)) (:modules opts) (string? source-map))
      (format (str ":source-map %s must specify a file in the same directory "
                   "as :output-to %s if optimization setting applied")
        (pr-str source-map)
        (pr-str output-to)))
    (assert (or (nil? (:output-to opts)) (:modules opts) (in-same-dir? source-map output-to))
      (format (str ":source-map %s must specify a file in the same directory as "
                   ":output-to %s if optimization setting applied")
        (pr-str source-map)
        (pr-str output-to)))
    (assert (or (nil? (:output-to opts)) (:modules opts) (same-or-subdirectory-of? (absolute-parent output-to) output-dir))
      (format (str ":output-dir %s must specify a directory in :output-to's "
                   "parent %s if optimization setting applied")
        (pr-str output-dir)
        (pr-str (absolute-parent output-to)))))
  (when (and (contains? opts :source-map)
             (= optimizations :none))
    (assert (util/boolean? source-map)
            (format ":source-map must be true or false when compiling with :optimizations :none but it is: %s"
                    (pr-str source-map))))
  true)

(defn check-source-map-path [{:keys [source-map-path] :as opts}]
  (when (contains? opts :source-map-path)
    (assert (string? source-map-path)
            (format ":source-map-path %s must be a directory"
                    source-map-path))
    (when-not (= (:optimizations opts) :none)
      (assert (and (contains? opts :output-to)
                   (contains? opts :source-map))
        (str ":source-map-path cannot be specified without also specifying "
             ":output-to and :source-map if optimization setting applied"))))
  true)

(defn check-output-wrapper [{:keys [output-wrapper optimizations]}]
  (assert (not (and output-wrapper (= :whitespace optimizations)))
          ":output-wrapper cannot be combined with :optimizations :whitespace"))

(defn check-node-target [{:keys [nodejs-rt optimizations] :as opts}]
  (assert (not (and nodejs-rt (= optimizations :whitespace)))
    (format ":nodejs target not compatible with :whitespace optimizations"))
  (assert (not (and nodejs-rt (= optimizations :none) (not (contains? opts :main))))
    (format ":nodejs target with :none optimizations requires a :main entry")))

(defn check-main [{:keys [main] :as opts}]
  (when main
    (assert (or (symbol? main) (string? main))
      (format ":main must be a symbol or string, got %s instead" main))
    (when (symbol? main)
      (assert (not (string/starts-with? (str main) "'"))
        (format ":main must be an unquoted symbol, got %s instead" main)))))

(defn check-preloads [{:keys [preloads optimizations] :as opts}]
  (when (and (some? preloads)
             (not= preloads '[process.env])
             (not= optimizations :none))
    (binding [*out* *err*]
      (println "WARNING: :preloads should only be specified with :none optimizations"))))

(defn check-cache-analysis-format [{:keys [cache-analysis cache-analysis-format] :as opts}]
  (assert (not (and cache-analysis
                    ((complement #{:edn :transit}) cache-analysis-format)
                    (not (nil? cache-analysis-format))))
    (format ":cache-analysis format must be :edn or :transit but it is: %s"
      (pr-str cache-analysis-format))))

(defn check-npm-deps [{:keys [npm-deps]}]
  (let [npm-deps (if (true? npm-deps) {} npm-deps)
        {ups-npm-deps :npm-deps} (get-upstream-deps)
        conflicts (filter (fn [[dep v]]
                            (and (coll? v) (not (contains? npm-deps dep))))
                    ups-npm-deps)]
    (binding [*out* *err*]
      (doseq [[dep versions] conflicts]
        (println (str "WARNING: NPM dependency " (name dep)
                   " conflicts between versions "
                   (util/conjunction-str versions)
                   ". Specify a version in :npm-deps or the latest will be installed."))))))

(defn foreign-source? [js]
  (and (satisfies? deps/IJavaScript js)
       (deps/-foreign? js)))

(defn expand-libs
  "EXPERIMENTAL. Given a set of libs expand any entries which only name
   directories into a sequence of lib entries for all JS files recursively
   found in that directory. All other options will be shared with the original
   entry. The computed :provides assumes the specified directory is on the
   classpath."
  [libs]
  (letfn [(prep-path [p root]
            (subs (string/replace (subs p 0 (- (count p) 3)) root "") 1))
          (path->provides [p]
            (let [p' (string/replace p File/separator ".")]
              (cond-> [p']
                (string/includes? p' "_")
                (conj (string/replace p' "_" "-")))))
          (expand-lib* [{:keys [file] :as lib}]
            (if-not file
              [lib] ;; foreign-lib override case - David
              (let [root (.getAbsolutePath (io/file file))
                    dir (io/file file)]
                (if (.isDirectory dir)
                  (into []
                    (comp
                      (filter #(.endsWith (.getName ^File %) ".js"))
                      (filter #(not (.isHidden ^File %)))
                      (map
                        (fn [^File f]
                          (let [p (.getPath f)
                                ap (.getAbsolutePath f)]
                            (merge lib
                              {:file p :provides (path->provides (prep-path ap root))})))))
                    (file-seq dir))
                  [lib]))))]
    (into [] (mapcat expand-lib* libs))))

(declare index-node-modules)

(defn compute-upstream-npm-deps
  ([]
   (compute-upstream-npm-deps
     (when env/*compiler*
       (:options @env/*compiler*))))
  ([{:keys [npm-deps]}]
   (let [{ups-npm-deps :npm-deps} (get-upstream-deps)]
     (reduce
       (fn [m [dep v]]
         (cond-> m
           (and (or (nil? npm-deps) (map? npm-deps))
                (not (contains? npm-deps dep)))
           (assoc dep (if (coll? v)
                        (last (sort v))
                        v))))
       {} ups-npm-deps))))

(defn ensure-module-opts [opts]
  (update opts :modules
    #(ensure-cljs-base-module % opts)))

(defn shim-process?
  [{:keys [target process-shim] :as opts}]
  (if (= :nodejs target)
    (true? process-shim)
    (not (false? process-shim))))

(defn normalize-closure-defines [defines]
  (into {}
    (map (fn [[k v]]
           [(if (symbol? k) (str (comp/munge k)) k) v])
      defines)))

(defn resolve-warning-handlers [fns]
  (reduce
    (fn [ret afn]
      (cond
        (fn? afn) (conj ret afn)

        (symbol? afn)
        (let [afn' (sym->var afn :warning-handlers)]
          (when-not afn'
            (throw
              (ex-info (str "Could not resolve warning handler: " afn)
                {:warning-handlers fns
                 :clojure.error/phase :compilation})))
          (conj ret afn'))

        :else
        (throw
          (ex-info (str "Invalid warning handler " afn " of type " (type afn))
            {:warning-handlers fns
             :clojure.error/phase :compilation}))))
    [] fns))

(defn add-implicit-options
  [{:keys [optimizations output-dir]
    :or {optimizations :none
         output-dir "out"}
    :as opts}]
  (let [opts (cond-> opts
               (shim-process? opts)
               (-> (update-in [:preloads] (fnil conj []) 'process.env)
                 (cond->
                   (not= :none optimizations)
                   (update-in [:closure-defines 'process.env/NODE_ENV] (fnil str "production"))))

               (or (:closure-defines opts) (shim-process? opts))
               (update :closure-defines normalize-closure-defines)

               (:browser-repl opts)
               (update-in [:preloads] (fnil conj []) 'clojure.browser.repl.preload)

               (and (contains? opts :modules)
                    (not (contains? opts :stable-names)))
               (assoc :stable-names true))
        {:keys [libs foreign-libs externs]} (get-upstream-deps)
        emit-constants (or (and (= optimizations :advanced)
                                (not (false? (:optimize-constants opts))))
                           (:optimize-constants opts))]
    (cond->
      (-> opts
        (assoc
          :optimizations optimizations
          :output-dir output-dir
          :ups-libs libs
          :ups-foreign-libs (expand-libs foreign-libs)
          :ups-externs externs
          :emit-constants emit-constants
          :cache-analysis-format (:cache-analysis-format opts :transit))
        (update-in [:preamble] #(into (or % []) ["cljs/imul.js"])))

      (:target opts)
      (assoc-in [:closure-defines (str (comp/munge 'cljs.core/*target*))]
        (name (:target opts)))

      (= :nodejs (:target opts))
      (merge
        (when (nil? (:nodejs-rt opts))
          {:nodejs-rt true}))

      ;; :bundle is just sugar
      (= :bundle (:target opts))
      (merge
        {:hashbang      false
         :infer-externs true
         :nodejs-rt     false
         :target        :nodejs}
        (when-not (:npm-deps opts)
          {:npm-deps true}))

      (= optimizations :none)
      (assoc
        :cache-analysis (:cache-analysis opts true)
        :source-map (:source-map opts true))

      (:aot-cache opts)
      (assoc :cache-analysis true)

      (= optimizations :advanced)
      (cond->
        (not (false? (:static-fns opts))) (assoc :static-fns true)
        (not (false? (:optimize-constants opts))) (assoc :optimize-constants true))

      (nil? (find (:closure-warnings opts) :check-types))
      (assoc-in [:closure-warnings :check-types] :off)

      (nil? (find (:closure-warnings opts) :check-variables))
      (assoc-in [:closure-warnings :check-variables] :off)

      (nil? (:closure-module-roots opts))
      (assoc :closure-module-roots [])

      (nil? (:opts-cache opts))
      (assoc :opts-cache "cljsc_opts.edn")

      (not (contains? opts :aot-cache))
      (assoc :aot-cache false)

      (contains? opts :modules)
      (ensure-module-opts)

      (nil? (:language-in opts))
      (assoc :language-in :es6)

      (:stable-names opts)
      (as-> opts
        (let [out-dir (if (true? (:stable-names opts))
                        output-dir
                        (:stable-names opts))]
          (merge
            {:closure-variable-map-in  (io/file out-dir "closure_var.map")
             :closure-variable-map-out (io/file out-dir "closure_var.map")
             :closure-property-map-in  (io/file out-dir "closure_prop.map")
             :closure-property-map-out (io/file out-dir "closure_prop.map")}
            opts)))

      (nil? (:ignore-js-module-exts opts))
      (assoc :ignore-js-module-exts [".css"])

      (:warning-handlers opts)
      (update :warning-handlers resolve-warning-handlers))))

(defn- alive? [proc]
  (try (.exitValue proc) false (catch IllegalThreadStateException _ true)))

(defn- pipe [^Process proc in ^Writer out]
  ;; we really do want system-default encoding here
  (with-open [^java.io.Reader in (-> in InputStreamReader. BufferedReader.)]
    (loop [buf (char-array 1024)]
      (when (alive? proc)
        (try
          (let [len (.read in buf)]
            (when-not (neg? len)
              (.write out buf 0 len)
              (.flush out)))
          (catch IOException e
            (when (and (alive? proc) (not (.contains (.getMessage e) "Stream closed")))
              (.printStackTrace e *err*))))
        (recur buf)))))

(defn maybe-install-node-deps!
  [{:keys [deps-cmd npm-deps verbose] :or {deps-cmd "npm"} :as opts}]
  (let [npm-deps (merge (if (map? npm-deps)
                          npm-deps
                          {})
                        (compute-upstream-npm-deps opts))]
    (when-not (empty? npm-deps)
      (let [pkg-json (io/file "package.json")]
        (when (or ana/*verbose* verbose)
          (util/debug-prn "Installing Node.js dependencies"))
        (when-not (.exists pkg-json)
          (spit pkg-json "{}"))
        (let [proc (-> (ProcessBuilder.
                         (into (cond->>
                                 [deps-cmd
                                  ({"npm" "install" "yarn" "add"} deps-cmd)
                                  "@cljs-oss/module-deps"]
                                 util/windows? (into ["cmd" "/c"]))
                           (map (fn [[dep version]] (str (name dep) "@" version)))
                           npm-deps))
                     .start)
              is   (.getInputStream proc)
              iw   (StringWriter. (* 16 1024 1024))
              es   (.getErrorStream proc)
              ew   (StringWriter. (* 1024 1024))
              _    (do (.start
                         (Thread.
                           (bound-fn [] (pipe proc is iw))))
                       (.start
                         (Thread.
                           (bound-fn [] (pipe proc es ew)))))
              err  (.waitFor proc)]
          (when (and (not (zero? err)) (not (.isAlive proc)))
            (println (str ew)))))
      true)))

(defn node-module-deps
  "EXPERIMENTAL: return the foreign libs entries as computed by running
   the module-deps package on the supplied JavaScript entry point. Assumes
   that the `@cljs-oss/module-deps` NPM package is either locally or globally
   installed."
  ([entry]
   (node-module-deps entry
     (when env/*compiler*
       (:options @env/*compiler*))))
  ([{:keys [file]} {:keys [target] :as opts}]
   ;; NOTE: The code value should only employ single quotes for strings.
   ;; If double quotes are used, then when the contents of this file
   ;; are passed to node via --eval on Windows, the double quotes
   ;; will be elided, leading to syntactically incorrect JavaScript.
   (let [main-entries (str "[" (->> (package-json-entries opts)
                                    (map #(str "'" % "'"))
                                    (string/join ",")) "]")
         escape-backslashes #(string/replace % "\\" "\\\\")
         code (-> (slurp (io/resource "cljs/module_deps.js"))
                (string/replace "JS_FILE" (escape-backslashes file))
                (string/replace "CLJS_TARGET" (str "" (when target (name target))))
                (string/replace "MAIN_ENTRIES" main-entries)
                (string/replace "FILE_SEPARATOR" (escape-backslashes File/separator)))
         proc (-> (ProcessBuilder. ["node" "--eval" code])
                .start)
         is   (.getInputStream proc)
         iw   (StringWriter. (* 16 1024 1024))
         es   (.getErrorStream proc)
         ew   (StringWriter. (* 1024 1024))
         _    (do (.start
                    (Thread.
                      (bound-fn [] (pipe proc is iw))))
                  (.start
                    (Thread.
                      (bound-fn [] (pipe proc es ew)))))
         err  (.waitFor proc)]
     (if (zero? err)
       (into []
         (map (fn [{:strs [file provides]}] file
                (merge
                  {:file file
                   ;; Just tag everything es6 here, add-converted-source will
                   ;; ask the real type, CJS/ES6, from Closure.
                   :module-type :es6}
                  (when provides
                    {:provides provides}))))
         (next (json/read-str (str iw))))
       (do
         (when-not (.isAlive proc)
           (binding [*out* *err*]
             (println (str ew))))
         [])))))

(defn node-inputs
  "EXPERIMENTAL: return the foreign libs entries as computed by running
   the module-deps package on the supplied JavaScript entry points. Assumes
   that the `@cljs-oss/module-deps` NPM package is either locally or globally
   installed."
  ([entries]
   (node-inputs entries
     (when env/*compiler*
       (:options @env/*compiler*))))
  ([entries opts]
   (into [] (distinct (mapcat #(node-module-deps % opts) entries)))))

(defn index-node-modules
  ([modules]
   (index-node-modules
     modules
     (when env/*compiler*
       (:options @env/*compiler*))))
  ([modules opts]
   (let [node-modules (io/file "node_modules")]
     (if (and (not (empty? modules)) (.exists node-modules) (.isDirectory node-modules))
       (let [modules (into #{} (map name) modules)
             deps-file (io/file (util/output-directory opts) "cljs$node_modules.js")
             old-contents (when (.exists deps-file)
                            (slurp deps-file))
             new-contents (let [sb (StringBuffer.)]
                            (run! #(.append sb (str "require('" % "');\n")) modules)
                            (str sb))]
         (util/mkdirs deps-file)
         (if (or (not= old-contents new-contents)
                 (nil? env/*compiler*)
                 (nil? (::transitive-dep-set @env/*compiler*)))
           (do
             (spit deps-file new-contents)
             (let [transitive-js (node-inputs [{:file (.getAbsolutePath deps-file)}] opts)]
               (when-not (nil? env/*compiler*)
                 (swap! env/*compiler* update-in [::transitive-dep-set]
                   assoc modules transitive-js))
               transitive-js))
           (when-not (nil? env/*compiler*)
             (get-in @env/*compiler* [::transitive-dep-set modules]))))
       []))))

(defn- node-file-seq->libs-spec*
  [module-fseq opts]
  (letfn [(package-json? [path]
            (boolean (re-find #"node_modules[/\\](@[^/\\]+?[/\\])?[^/\\]+?[/\\]package\.json$" path)))]
    (let [pkg-jsons (into {}
                      (comp
                        (map #(.getAbsolutePath %))
                        (filter package-json?)
                        (map (fn [path]
                               [path (json/read-str (slurp path))])))
                      module-fseq)
          trim-package-json (fn [s]
                              (if (string/ends-with? s "package.json")
                                (subs s 0 (- (count s) 12))
                                s))]
      (into []
        (comp
          (map #(.getAbsolutePath %))
          (map (fn [path]
                 (merge
                   {:file path
                    :module-type :es6}
                   (when-not (package-json? path)
                     (let [pkg-json-main (some
                                           (fn [[pkg-json-path {:as pkg-json :strs [name]}]]
                                             (let [entries (package-json-entries opts)
                                                   entry (first (keep (partial get pkg-json) entries))]
                                               (when-not (nil? entry)
                                                 ;; should be the only edge case in
                                                 ;; the package.json main field - Antonio
                                                 (let [entry (cond-> entry
                                                              (string/starts-with? entry "./")
                                                              (subs 2))
                                                       entry-path (-> pkg-json-path
                                                                     (string/replace \\ \/)
                                                                     trim-package-json
                                                                     (str entry))]
                                                   (some (fn [candidate]
                                                           (when (= candidate (string/replace path \\ \/))
                                                             name))
                                                         (cond-> [entry-path]
                                                           (not (or (string/ends-with? entry-path ".js")
                                                                    (string/ends-with? entry-path ".json")))
                                                           (into [(str entry-path ".js") (str entry-path "/index.js") (str entry-path ".json")])))))))
                                           pkg-jsons)]
                       {:provides (let [module-rel-name (-> (subs path (.lastIndexOf path "node_modules"))
                                                            (string/replace \\ \/)
                                                            (string/replace #"node_modules[\\\/]" ""))
                                        provides (cond-> [module-rel-name (string/replace module-rel-name #"\.js(on)?$" "")]
                                                   (some? pkg-json-main)
                                                   (conj pkg-json-main))
                                        index-replaced (string/replace module-rel-name #"[\\\/]index\.js(on)?$" "")]
                                    (cond-> provides
                                      (and (boolean (re-find #"[\\\/]index\.js(on)?$" module-rel-name))
                                           (not (some #{index-replaced} provides)))
                                      (conj index-replaced)))}))))))
        module-fseq))))

(def node-file-seq->libs-spec (memoize node-file-seq->libs-spec*))

(defn index-node-modules-dir
  ([]
   (index-node-modules-dir
     (when env/*compiler*
       (:options @env/*compiler*))))
  ([opts]
   (let [module-fseq (util/module-file-seq)]
     (node-file-seq->libs-spec module-fseq opts))))

(defn preprocess-js
  "Given js-module map, apply preprocessing defined by :preprocess value in the map."
  [{:keys [preprocess] :as js-module} opts]
  (cond
    (keyword? preprocess)
    (js-transforms js-module opts)

    (symbol? preprocess)
    (let [preprocess-var (sym->var preprocess :preprocess {:file (:file js-module)})]
      (try
        (preprocess-var js-module opts)
        (catch Throwable t
          (throw (ex-info (str "Error running preprocessing function " preprocess)
                   {:file       (:file js-module)
                    :preprocess preprocess
                    :clojure.error/phase :compilation}
                   t)))))

    :else
    (do
      (ana/warning :unsupported-preprocess-value @env/*compiler* js-module)
      js-module)))

(defn- to-absolute-path [^String file-str]
  (.getAbsolutePath (io/file file-str)))

(defn process-js-modules
  "Given the current compiler options, converts JavaScript modules to Google
  Closure modules and writes them to disk. Adds mapping from original module
  namespace to new module namespace to compiler env. Returns modified compiler
  options where new modules are passed with :libs option."
  [opts]
  (let [;; Modules from both :foreign-libs (compiler options) and :ups-foreign-libs (deps.cljs)
        ;; are processed together, so that files from both sources can depend on each other.
        ;; e.g. commonjs module in :foreign-libs can depend on commonjs module from :ups-foreign-libs.
        js-modules (filter :module-type (concat (:foreign-libs opts) (:ups-foreign-libs opts)))]
    (if (seq js-modules)
      (util/measure (:compiler-stats opts)
        "Process JS modules"
        (let [_ (when-let [unsupported (first (filter (complement #{:es6 :commonjs})
                                                      (map :module-type js-modules)))]
                  (ana/warning :unsupported-js-module-type @env/*compiler* unsupported))
              ;; Load all modules - add :source so preprocessing and conversion can access it
              js-modules (into []
                           (comp
                             (map (fn [lib]
                                    (let [js (deps/load-foreign-library lib)
                                          url (str (deps/-url js opts))]
                                      (if (and url (some (fn [ext]
                                                           (.endsWith url ext))
                                                         (:ignore-js-module-exts opts)))
                                        (do
                                          (when (or ana/*verbose* (:verbose opts))
                                            (util/debug-prn "Ignoring JS module" url "based on the file extension"))
                                          (assoc js :source ""))
                                        (assoc js :source (deps/-source js opts))))))
                             (map (fn [js]
                                    (if (:preprocess js)
                                      (preprocess-js js opts)
                                      js)))
                             (map (fn [js]
                                    (cond-> (update-in js [:file] to-absolute-path)
                                      (some? (:file-min js))
                                      (update-in [:file-min] to-absolute-path)))))
                           js-modules)
              js-modules (convert-js-modules js-modules opts)]
          ;; Write modules to disk, update compiler state and build new options
          (reduce (fn [new-opts {:keys [file module-type] :as ijs}]
                    (let [ijs (write-javascript opts ijs)
                          module-name (-> (deps/load-library (:out-file ijs)) first :provides first)]
                      (swap! env/*compiler*
                             #(assoc-in % [:js-namespaces module-name] {:module-type module-type}))
                      (doseq [provide (:provides ijs)]
                        (swap! env/*compiler*
                          #(update-in % [:js-module-index] assoc provide {:name module-name
                                                                          :module-type module-type})))
                      (-> new-opts
                        (update-in [:libs] (comp vec conj) (:out-file ijs))
                        ;; js-module might be defined in either, so update both
                        (update-in [:foreign-libs]
                          (fn [libs]
                            (into []
                              (remove #(= (to-absolute-path (:file %)) file))
                              libs)))
                        (update-in [:ups-foreign-libs]
                          (fn [libs]
                            (into []
                              (remove #(= (to-absolute-path (:file %)) (to-absolute-path file)))
                              libs))))))
            opts js-modules)))
      opts)))

(defn- load-data-reader-file [mappings ^java.net.URL url]
  (with-open [rdr (readers/input-stream-push-back-reader (.openStream url))]
    (binding [*file* (.getFile url)]
      (let [new-mappings (reader/read {:eof nil :read-cond :allow} rdr)]
        (when (not (map? new-mappings))
          (throw (ex-info (str "Not a valid data-reader map")
                   {:url url
                    :clojure.error/phase :compilation})))
        (reduce
          (fn [m [k v]]
            (when (not (symbol? k))
              (throw (ex-info (str "Invalid form in data-reader file")
                       {:url url
                        :form k
                        :clojure.error/phase :compilation})))
            (when (and (contains? mappings k)
                    (not= (mappings k) v))
              (throw (ex-info "Conflicting data-reader mapping"
                       {:url url
                        :conflict k
                        :mappings m
                        :clojure.error/phase :compilation})))
            (assoc m k v))
          mappings
          new-mappings)))))

(defn get-data-readers*
  "returns a merged map containing all data readers defined by libraries
   on the classpath."
  ([]
   (get-data-readers* (. (Thread/currentThread) (getContextClassLoader))))
  ([classloader]
   (let [data-reader-urls (enumeration-seq (. classloader (getResources "data_readers.cljc")))]
     (reduce load-data-reader-file {} data-reader-urls))))

(def get-data-readers (memoize get-data-readers*))

(defn load-data-readers! [compiler]
  (let [data-readers (get-data-readers)
        nses (map (comp symbol namespace) (vals data-readers))]
    (swap! compiler update-in [:cljs.analyzer/data-readers] merge (get-data-readers))
    (doseq [ns nses]
      (try
        (locking ana/load-mutex
          (require ns))
        (catch Throwable _)))))

(defn add-externs-sources [opts]
  (cond-> opts
    (:infer-externs opts)
    (assoc :externs-sources (load-externs (dissoc opts :infer-externs)))))

(defn handle-js-modules
  "Given all Cljs sources (build inputs and dependencies in classpath)

  - index all the node modules
  - process the JS modules (preprocess + convert to Closure JS)
  - save js-dependency-index for compilation"
  [{:keys [npm-deps target] :as opts} js-sources compiler-env]
  ;; Find all the top-level Node packages and their files
  (let [top-level (reduce
                    (fn [acc m]
                      (reduce (fn [acc p] (assoc acc p m)) acc (:provides m)))
                    {}
                    ;; if :npm-deps option is false, node_modules/ dir shouldn't be indexed
                    (if (not (false? npm-deps))
                      (index-node-modules-dir)))
        requires (->> (mapcat deps/-requires js-sources)
                   ;; fixup foo$default cases, foo is the lib, default is a property
                   (map #(-> % ana/lib&sublib first))
                   set)
        ;; Select Node files that are required by Cljs code,
        ;; and create list of all their dependencies
        node-required (set/intersection (set (keys top-level)) requires)
        expanded-libs (expand-libs (:foreign-libs opts))
        output-dir (util/output-directory opts)
        opts (update opts :foreign-libs
               (fn [libs]
                 (into (if (= target :nodejs)
                         []
                         (index-node-modules node-required))
                   (into expanded-libs
                     (node-inputs (filter (fn [{:keys [module-type]}]
                                            (some? module-type))
                                    expanded-libs))))))
        ;; If compiler-env doesn't contain JS module info we need to process
        ;; modules even if files haven't changed since last compile.
        opts (if (or (nil? (:js-namespaces @compiler-env))
                     (nil? (:js-module-index @compiler-env))
                     (some
                       (fn [ijs]
                         (let [dest (io/file output-dir (rel-output-path (assoc ijs :foreign true) opts))]
                           (util/changed? (deps/-url ijs opts) dest)))
                       (:foreign-libs opts)))
               (process-js-modules opts)
               (:options @compiler-env))]
    (swap! compiler-env
      (fn [cenv]
        (-> cenv
          ;; we need to also track the whole top level - this is to support
          ;; cljs.analyze/analyze-deps, particularly in REPL contexts - David
          (merge {:js-dependency-index (deps/js-dependency-index opts)})
          (update-in [:options] merge opts)
          (update-in [:node-module-index] (fnil into #{})
            (if (= target :nodejs)
              (map str node-required)
              (map str (keys top-level)))))))
    opts))

(defn output-bootstrap [{:keys [target target-fn] :as opts}]
  (when (or (and (#{:nodejs} target)
                 (not= (:optimizations opts) :whitespace))
            target-fn)
    (let [target-str (name target)
          outfile    (io/file (util/output-directory opts)
                       "goog" "bootstrap" (str target-str ".js"))]
      ;; not all targets using :target-fn might provide a bootstrap file to include
      (when-let [bootstrap-file (io/resource (str "cljs/bootstrap_" target-str ".js"))]
        (util/mkdirs outfile)
        (spit outfile (slurp bootstrap-file))))))

(defn compile-inputs
  "Compile inputs and all of their transitive dependencies including JS modules,
   libs, and foreign libs. Duplicates the pipeline of build."
  [inputs opts]
  (env/ensure
    (let [sources (-> inputs
                    (#(map add-core-macros-if-cljs-js %))
                    (add-dependency-sources opts))
          opts    (handle-js-modules opts sources env/*compiler*)
          sources (-> sources
                    deps/dependency-order
                    (compile-sources false opts)
                    (add-js-sources opts) deps/dependency-order
                    (->> (map #(source-on-disk opts %)) doall))]
      sources)))

(defn compile-ns
  "Compiles a namespace and all of its transitive dependencies.
   See compile-inputs."
  [ns opts]
  (compile-inputs (find-sources ns opts) opts))

(defn validate-opts [opts]
  (check-output-to opts)
  (check-output-dir opts)
  (check-source-map opts)
  (check-source-map-path opts)
  (check-output-wrapper opts)
  (check-node-target opts)
  (check-preloads opts)
  (check-cache-analysis-format opts)
  (check-main opts)
  opts)

(defn run-bundle-cmd [opts]
  (let [cmd-type (or (#{:none} (:optimizations opts)) :default)]
    (when-let [cmd (get-in opts [:bundle-cmd cmd-type])]
      (let [{:keys [exit out err]}
            (try
              (apply sh/sh cmd)
              (catch Throwable t
                (throw
                  (ex-info (str ":build-cmd " cmd-type " failed")
                    {:cmd cmd} t))))]
        (when-not (== 0 exit)
          (throw
            (ex-info (str ":bundle-cmd " cmd-type " failed")
              {:cmd cmd :exit-code exit :stdout out :stderr err})))))))

(defn build
  "Given compiler options, produce runnable JavaScript. An optional source
   parameter may be provided."
  ([opts]
   (build nil opts))
  ([source opts]
    (build source opts
      (if-not (nil? env/*compiler*)
        env/*compiler*
        (env/default-compiler-env
          ;; need to dissoc :foreign-libs since we won't know what overriding
          ;; foreign libspecs are referring to until after add-implicit-options
          ;; - David
          (add-externs-sources (dissoc opts :foreign-libs))))))
  ([source opts compiler-env]
     (env/with-compiler-env compiler-env
       (let [orig-opts opts
             opts (add-implicit-options opts)
             ;; we want to warn about NPM dep conflicts before installing the modules
             _ (when (:install-deps opts)
                 (check-npm-deps opts)
                 (swap! compiler-env update-in [:npm-deps-installed?]
                        (fn [installed?]
                          (if-not installed?
                            (maybe-install-node-deps! opts)
                            installed?))))

             compiler-stats (:compiler-stats opts)
             checked-arrays (or (:checked-arrays opts)
                                ana/*checked-arrays*)
             static-fns? (or (and (= (:optimizations opts) :advanced)
                               (not (false? (:static-fns opts))))
                           (:static-fns opts)
                           ana/*cljs-static-fns*)
             sources (when source
                       (-find-sources source opts))]
         (validate-opts opts)
         (swap! compiler-env
           #(-> %
             (update-in [:options] merge opts)
             (assoc :target (:target opts))
             ;; Save the current js-dependency index once we have computed opts
             ;; or the analyzer won't be able to find upstream dependencies - Antonio
             (assoc :js-dependency-index (deps/js-dependency-index opts))
             ;; Save list of sources for cljs.analyzer/locate-src - Juho Teperi
             (assoc :sources sources)))
         (binding [comp/*recompiled* (when-not (false? (:recompile-dependents opts))
                                       (atom #{}))
                   ana/*checked-arrays* checked-arrays
                   ana/parse-ns (memoize ana/parse-ns)
                   ana/*cljs-static-fns* static-fns?
                   ana/*fn-invoke-direct* (or (and static-fns?
                                                   (:fn-invoke-direct opts))
                                              ana/*fn-invoke-direct*)
                   *assert* (not= (:elide-asserts opts) true)
                   ana/*load-tests* (not= (:load-tests opts) false)
                   ana/*cljs-warnings*
                   (let [warnings (opts :warnings true)]
                     (merge
                       ana/*cljs-warnings*
                       (if (or (true? warnings)
                               (false? warnings))
                         (zipmap
                           [:unprovided :undeclared-var
                            :undeclared-ns :undeclared-ns-form]
                           (repeat warnings))
                         warnings)))
                   ana/*verbose* (:verbose opts)]
           (when (and ana/*verbose* (not (::watch-triggered-build? opts)))
             (util/debug-prn "Options passed to ClojureScript compiler:" (pr-str opts)))
           (let [one-file? (and (:main opts)
                                (#{:advanced :simple :whitespace} (:optimizations opts)))
                 source (if (or one-file?
                                ;; if source is nil, :main is supplied, :optimizations :none,
                                ;; fix up source for the user, see CLJS-3255
                                (and (nil? source) (:main opts) (= :none (:optimizations opts))))
                          (let [main (:main opts)
                                uri  (:uri (cljs-source-for-namespace main))]
                            (assert uri (str "No file for namespace " main " exists"))
                            uri)
                          ;; old compile directory behavior, or code-splitting
                          source)
                 compile-opts (if one-file?
                                (assoc opts :output-file (:output-to opts))
                                opts)
                 _ (load-data-readers! compiler-env)
                 ;; reset :js-module-index so that ana/parse-ns called by -find-sources
                 ;; can find the missing JS modules
                 js-sources (env/with-compiler-env (dissoc @compiler-env :js-module-index)
                              (-> (if source
                                    (-find-sources source opts)
                                    (-find-sources (reduce into #{} (map (comp :entries val) (:modules opts))) opts))
                                  (add-dependency-sources compile-opts)))
                 opts       (handle-js-modules opts js-sources compiler-env)
                 js-sources (-> js-sources
                                deps/dependency-order
                                (compile-sources compiler-stats compile-opts)
                                (#(map add-core-macros-if-cljs-js %))
                                (add-js-sources opts)
                                (cond->
                                  (and (= :nodejs (:target opts))
                                       (:nodejs-rt opts))
                                  (concat
                                    [(-compile (io/resource "cljs/nodejs.cljs")
                                       (assoc opts :output-file "nodejs.js"))]))
                                deps/dependency-order
                                (add-preloads opts)
                                remove-goog-base
                                add-goog-base
                                (cond->
                                  (and (= :nodejs (:target opts))
                                       (:nodejs-rt opts))
                                  (concat
                                    [(-compile (io/resource "cljs/nodejscli.cljs")
                                       (assoc opts :output-file "nodejscli.js"))]))
                                (->> (map #(source-on-disk opts %)) doall)
                                (compile-loader opts))
                 _ (when (:emit-constants opts)
                     (comp/emit-constants-table-to-file
                      (::ana/constant-table @env/*compiler*)
                      (constants-filename opts)))
                 _ (when (:infer-externs opts)
                     (comp/emit-inferred-externs-to-file
                       (reduce util/map-merge {}
                         (map (comp :externs second)
                           (get @compiler-env ::ana/namespaces)))
                       (str (util/output-directory opts) "/inferred_externs.js")))
                 _ (spit (io/file (util/output-directory opts) (:opts-cache opts)) (pr-str orig-opts))
                 optim (:optimizations opts)
                 ret (if (and optim (not= optim :none))
                       (do
                         (when-let [fname (:source-map opts)]
                           (assert (or (nil? (:output-to opts)) (:modules opts) (string? fname))
                             (str ":source-map must name a file when using :whitespace, "
                                  ":simple, or :advanced optimizations with :output-to")))
                         (if (:modules opts)
                           (->>
                             (util/measure compiler-stats
                               (str "Optimizing " (count js-sources) " sources")
                               (apply optimize-modules opts js-sources))
                             (output-modules opts js-sources))
                           (let [fdeps-str (foreign-deps-str opts
                                             (filter foreign-source? js-sources))
                                 opts      (assoc opts
                                             :foreign-deps-line-count
                                             (- (count (.split #"\r?\n" fdeps-str -1)) 1))]
                             (->>
                               (util/measure compiler-stats
                                 (str "Optimizing " (count js-sources) " sources")
                                 (apply optimize opts
                                   (remove foreign-source? js-sources)))
                               (add-wrapper opts)
                               (add-source-map-link opts)
                               (str fdeps-str)
                               (add-header opts)
                               (output-one-file opts)))))
                       (do
                         (when (bundle? opts)
                           (spit (io/file (util/output-directory opts) "npm_deps.js")
                             (npm-deps-js (:node-module-index @env/*compiler*))))
                         (apply output-unoptimized opts js-sources)))]
             (output-bootstrap opts)
             (when (bundle? opts) (run-bundle-cmd opts))
             ret))))))

(comment
  ;; testing modules
  (build "samples/hello/src"
    {:optimizations :advanced
     :output-dir "samples/hello/out"
     :source-map true
     :modules
     {:hello
      {:output-to "samples/hello/out/hello.js"
       :entries '#{cljs.reader hello.core}}}})

  (require '[cljs.externs :as externs])

  (externs/parse-externs
    (js-source-file "cljs/externs.js" (io/file "src/main/cljs/cljs/externs.js")))
  )

(defn ^File target-file-for-cljs-ns
  [ns-sym output-dir]
  (util/to-target-file
    (util/output-directory {:output-dir output-dir})
    {:ns ns-sym}))

(defn mark-cljs-ns-for-recompile!
  [ns-sym output-dir]
  (let [s (target-file-for-cljs-ns ns-sym output-dir)]
    (when (.exists s)
      (.setLastModified s 5000))))

(defn cljs-dependents-for-macro-namespaces
  [state namespaces]
  (map :name
    (let [namespaces-set (set namespaces)]
      (filter (fn [x] (not-empty
                        (set/intersection namespaces-set (-> x :require-macros vals set))))
        (vals (:cljs.analyzer/namespaces @state))))))

(defn watch
  "Given a source directory, produce runnable JavaScript. Watch the source
   directory for changes rebuilding when necessary. Takes the same arguments as
   cljs.closure/build in addition to some watch-specific options:
    - :watch-fn, a function of no arguments to run after a successful build. May
                 be a function value or a namespaced symbol identifying a function,
                 in which case the associated namespace willl be loaded and the
                 symbol resolved.
    - :watch-error-fn, a function receiving the exception of a failed build. May
                       be a function value or a namespaced symbol, loaded as
                       with :watch-fn."
  ([source opts]
    (watch source opts
      (if-not (nil? env/*compiler*)
        env/*compiler*
        (env/default-compiler-env opts))))
  ([source opts compiler-env]
    (watch source opts compiler-env nil))
  ([source opts compiler-env quit]
    (let [opts  (cond-> opts
                  (= (:verbose opts :not-found) :not-found)
                  (assoc :verbose true))
          paths (map #(Paths/get (.toURI %)) (-paths source))
          path  (first paths)
          fs    (.getFileSystem path)
          srvc  (.newWatchService fs)]
      (letfn [(buildf []
                (try
                  (let [start (System/nanoTime)
                        watch-opts (assoc opts ::watch-triggered-build? true)]
                    (build source watch-opts compiler-env)
                    (println "... done. Elapsed"
                      (/ (unchecked-subtract (System/nanoTime) start) 1e9) "seconds")
                    (flush))
                  (when-let [f (opts-fn :watch-fn opts)]
                    (f))
                  (catch Throwable e
                    (if-let [f (opts-fn :watch-error-fn opts)]
                      (f e)
                      (binding [*out* *err*]
                        (println e))))))
              (watch-all [^Path root]
                (Files/walkFileTree root
                  (reify
                    FileVisitor
                    (preVisitDirectory [_ dir _]
                      (let [^Path dir dir]
                        (. dir
                          (register srvc
                            (into-array [StandardWatchEventKinds/ENTRY_CREATE
                                         StandardWatchEventKinds/ENTRY_DELETE
                                         StandardWatchEventKinds/ENTRY_MODIFY])
                            (into-array [SensitivityWatchEventModifier/HIGH]))))
                      FileVisitResult/CONTINUE)
                    (postVisitDirectory [_ dir exc]
                      FileVisitResult/CONTINUE)
                    (visitFile [_ file attrs]
                      FileVisitResult/CONTINUE)
                    (visitFileFailed [_ file exc]
                      FileVisitResult/CONTINUE))))]
        (println "Building ...")
        (flush)
        (buildf)
        (println "Watching paths:" (apply str (interpose ", " paths)))
        (doseq [path paths]
          (watch-all path))
        (loop [key nil]
          (when (and (or (nil? quit) (not @quit))
                     (or (nil? key) (. ^WatchKey key reset)))
            (let [key (. srvc (poll 300 TimeUnit/MILLISECONDS))
                  poll-events-seq (when key (seq (.pollEvents key)))]
              (when (and key
                         (some
                           (fn [^WatchEvent e]
                             (let [fstr (.. e context toString)]
                               (and (or (. fstr (endsWith "cljc"))
                                        (. fstr (endsWith "cljs"))
                                        (. fstr (endsWith "clj"))
                                        (. fstr (endsWith "js")))
                                    (not (. fstr (startsWith ".#"))))))
                           poll-events-seq))
                (when-let [clj-files (seq (keep (fn [^WatchEvent e]
                                                  (let [ctx (.context e)
                                                        fstr (.toString ctx)]
                                                    (when (and (or (. fstr (endsWith "cljc"))
                                                                   (. fstr (endsWith "clj")))
                                                               (not (. fstr (startsWith ".#"))))
                                                      ctx)))
                                            poll-events-seq))]
                  (let [^Path dir (.watchable key)
                        file-seq (map #(.toFile (.resolve dir %)) clj-files)
                        nses (map (comp :ns ana/parse-ns) file-seq)]
                    (doseq [ns nses]
                      (require ns :reload))
                    (doseq [ns (cljs-dependents-for-macro-namespaces compiler-env nses)]
                      (mark-cljs-ns-for-recompile! ns (:output-dir opts)))))
                (println "Change detected, recompiling ...")
                (flush)
                (buildf))
              (recur key))))))))

(comment
  (watch "samples/hello/src"
    {:optimizations :none
     :output-to "samples/hello/out/hello.js"
     :output-dir "samples/hello/out"
     :cache-analysis true
     :source-map true
     :verbose true
     :watch-fn
     (fn []
       (println "Success!"))})
  )

;; =============================================================================
;; Utilities

;; for backwards compatibility
(defn output-directory [opts]
  (util/output-directory opts))

(defn parse-js-ns [f]
  (deps/parse-js-ns (line-seq (io/reader f))))

(defn ^File src-file->target-file
  ([src]
   (src-file->target-file src
     (when env/*compiler*
       (:options @env/*compiler*))))
  ([src opts]
    (util/to-target-file
      (when (:output-dir opts)
        (util/output-directory opts))
      (ana/parse-ns src))))

(defn ^String src-file->goog-require
  ([src] (src-file->goog-require src {:wrap true}))
  ([src {:keys [wrap all-provides macros-ns] :as options}]
    (let [goog-ns
          (case (util/ext src)
            ("cljs" "cljc") (let [ns-str (str (comp/munge (:ns (ana/parse-ns src))))]
                              (cond-> ns-str
                                (and macros-ns (not (.endsWith ns-str "$macros")))
                                (str "$macros")))
            "js" (cond-> (:provides (parse-js-ns src))
                   (not all-provides) first)
            (throw
              (util/compilation-error (IllegalArgumentException.
                                        (str "Can't create goog.require expression for " src)))))]
      (if (and (not all-provides) wrap)
        (if (:reload options)
          (str "goog.require(\"" goog-ns "\", true);")
          (str "goog.require(\"" goog-ns "\");"))
        (if (vector? goog-ns)
          goog-ns
          (str goog-ns))))))

;; Browser REPL client stuff

(defn compile-client-js [opts]
  (let [copts (select-keys opts [:optimizations :output-dir :language-in])]
    ;; we're inside the REPL process where cljs.env/*compiler* is already
    ;; established, need to construct a new one to avoid mutating the one
    ;; the REPL uses
    (build
      '[(ns clojure.browser.repl.client
          (:require [goog.events :as event]
                    [clojure.browser.repl :as repl]))
        (defn start [url]
          (event/listen js/window
            "load"
            (fn []
              (repl/start-evaluator url))))]
      copts (env/default-compiler-env copts))))

(defn create-client-js-file [opts file-path]
  (if-let [cached (io/resource "brepl_client.js")]
    cached
    (let [file (io/file file-path)]
      (when (not (.exists file))
        (spit file (compile-client-js opts)))
      file)))

;; AOTed resources

(defn aot-cache-core []
  (let [base-path (io/file "src" "main" "cljs" "cljs")
        src       (io/file base-path "core.cljs")
        dest      (io/file base-path "core.aot.js")
        cache     (io/file base-path "core.cljs.cache.aot.edn")
        tcache    (io/file base-path "core.cljs.cache.aot.json")]
    (util/mkdirs dest)
    (env/with-compiler-env (env/default-compiler-env {:infer-externs true})
      (comp/compile-file src dest
        {:static-fns true
         :source-map true
         :source-map-url "core.js.map"
         :output-dir (str "src" File/separator "main" File/separator "cljs")})
      (ana/write-analysis-cache 'cljs.core cache src)
      (ana/write-analysis-cache 'cljs.core tcache src))
    (create-client-js-file
      {:language-in :es6
       :optimizations :simple
       :output-dir "aot_out"}
      (io/file "resources" "brepl_client.js"))
    (doseq [f (file-seq (io/file "aot_out"))
            :when (.isFile f)]
      (.delete f))))

(comment
  (time
    (do (aot-cache-core) nil))

  (time
    (do (ana/analyze-file "cljs/core.cljs") nil))

  (println (build '[(ns hello.core)
                    (defn ^{:export greet} greet [n] (str "Hola " n))
                    (defn ^:export sum [xs] 42)]
                  {:optimizations :simple :pretty-print true}))

  ;; build a project with optimizations
  (build "samples/hello/src" {:optimizations :advanced})
  (build "samples/hello/src" {:optimizations :advanced :output-to "samples/hello/hello.js"})
  ;; open 'samples/hello/hello.html' to see the result in action

  ;; build a project without optimizations
  (build "samples/hello/src" {:output-dir "samples/hello/out" :output-to "samples/hello/hello.js"})
  ;; open 'samples/hello/hello-dev.html' to see the result in action
  ;; notice how each script was loaded individually

  ;; build unoptimized from raw ClojureScript
  (build '[(ns hello.core)
           (defn ^{:export greet} greet [n] (str "Hola " n))
           (defn ^:export sum [xs] 42)]
         {:output-dir "samples/hello/out" :output-to "samples/hello/hello.js"})
  ;; open 'samples/hello/hello-dev.html' to see the result in action
  )
