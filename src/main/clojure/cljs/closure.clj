;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.closure
  "Compile ClojureScript to JavaScript with optimizations from Google
   Closure Compiler producing runnable JavaScript.

   The Closure Compiler (compiler.jar) must be on the classpath.

   Use the 'build' function for end-to-end compilation.

   build = find-sources -> add-dependencies -> compile -> optimize -> output

   Two protocols are defined: IJavaScript and Compilable. The
   Compilable protocol is satisfied by something which can return one
   or more IJavaScripts.

   With IJavaScript objects in hand, calling add-dependencies will
   produce a sequence of IJavaScript objects which includes all
   required dependencies from the Closure library and ClojureScript,
   in dependency order. This function replaces the closurebuilder
   tool.

   The optimize function converts one or more IJavaScripts into a
   single string of JavaScript source code using the Closure Compiler
   API.

   The produced output is either a single string of optimized
   JavaScript or a deps file for use during development.
  "
  (:refer-clojure :exclude [compile])
  (:require [cljs.util :as util]
            [cljs.core :as cljsm]
            [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.source-map :as sm]
            [cljs.env :as env]
            [cljs.js-deps :as deps]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.data.json :as json])
  (:import [java.io File BufferedInputStream StringWriter]
           [java.net URL]
           [java.util.logging Level]
           [java.util List Random]
           [java.util.concurrent
            TimeUnit LinkedBlockingDeque Executors CountDownLatch]
           [com.google.javascript.jscomp CompilerOptions CompilationLevel
              CompilerOptions$LanguageMode SourceMap$Format
              SourceMap$DetailLevel ClosureCodingConvention SourceFile
              Result JSError CheckLevel DiagnosticGroups
              CommandLineRunner AnonymousFunctionNamingPolicy
              JSModule JSModuleGraph SourceMap ProcessCommonJSModules
              ES6ModuleLoader AbstractCompiler TransformAMDToCJSModule
              ProcessEs6Modules CompilerInput]
           [com.google.javascript.rhino Node]
           [java.security MessageDigest]
           [javax.xml.bind DatatypeConverter]
           [java.nio.file Path Paths Files StandardWatchEventKinds WatchKey
                          WatchEvent FileVisitor FileVisitResult]
           [java.nio.charset Charset StandardCharsets]
           [com.sun.nio.file SensitivityWatchEventModifier]
           [com.google.common.base Throwables]))

(def name-chars (map char (concat (range 48 57) (range 65 90) (range 97 122))))

(defn random-char []
  (nth name-chars (.nextInt (Random.) (count name-chars))))

(defn random-string [length]
  (apply str (take length (repeatedly random-char))))

(util/compile-if
 (.getConstructor ES6ModuleLoader
   (into-array java.lang.Class
               [java.util.List java.lang.Iterable]))
 (do (def is-new-es6-loader? true)
     (def default-module-root ES6ModuleLoader/DEFAULT_FILENAME_PREFIX))
 (def is-new-es6-loader? false))

(util/compile-if
 (.getConstructor ES6ModuleLoader
   (into-array java.lang.Class
               [AbstractCompiler java.lang.String]))
 (def is-old-es6-loader? true)
 (def is-old-es6-loader? false))

(util/compile-if
 (and (.getConstructor ProcessCommonJSModules
        (into-array java.lang.Class
                    [com.google.javascript.jscomp.Compiler ES6ModuleLoader]))
      (or is-new-es6-loader? is-old-es6-loader?))
 (def can-convert-commonjs? true)
 (def can-convert-commonjs? false))

(util/compile-if
 (and can-convert-commonjs?
      (.getConstructor TransformAMDToCJSModule
        (into-array java.lang.Class
                    [AbstractCompiler])))
 (def can-convert-amd? true)
 (def can-convert-amd? false))

(util/compile-if
 (and (.getConstructor ProcessEs6Modules
        (into-array java.lang.Class
                    [com.google.javascript.jscomp.Compiler ES6ModuleLoader Boolean/TYPE]))
      (or is-new-es6-loader? is-old-es6-loader?))
 (def can-convert-es6? true)
 (def can-convert-es6? false))

;; Closure API
;; ===========

(defmulti js-source-file (fn [_ source] (class source)))

(defmethod js-source-file String [^String name ^String source]
  (SourceFile/fromCode name source))

(defmethod js-source-file File [_ ^File source]
  (SourceFile/fromFile source))

(defmethod js-source-file BufferedInputStream [^String name ^BufferedInputStream source]
  (SourceFile/fromInputStream name source))

(def check-level
  {:error CheckLevel/ERROR
   :warning CheckLevel/WARNING
   :off CheckLevel/OFF})

(def warning-types
  {:access-controls DiagnosticGroups/ACCESS_CONTROLS
   :ambiguous-function-decl DiagnosticGroups/AMBIGUOUS_FUNCTION_DECL
   :debugger-statement-present DiagnosticGroups/DEBUGGER_STATEMENT_PRESENT
   :check-regexp DiagnosticGroups/CHECK_REGEXP
   :check-types DiagnosticGroups/CHECK_TYPES
   :check-useless-code DiagnosticGroups/CHECK_USELESS_CODE
   :check-variables DiagnosticGroups/CHECK_VARIABLES
   :const DiagnosticGroups/CONST
   :constant-property DiagnosticGroups/CONSTANT_PROPERTY
   :deprecated DiagnosticGroups/DEPRECATED
   :duplicate-message DiagnosticGroups/DUPLICATE_MESSAGE
   :es5-strict DiagnosticGroups/ES5_STRICT
   :externs-validation DiagnosticGroups/EXTERNS_VALIDATION
   :fileoverview-jsdoc DiagnosticGroups/FILEOVERVIEW_JSDOC
   :global-this DiagnosticGroups/GLOBAL_THIS
   :internet-explorer-checks DiagnosticGroups/INTERNET_EXPLORER_CHECKS
   :invalid-casts DiagnosticGroups/INVALID_CASTS
   :missing-properties DiagnosticGroups/MISSING_PROPERTIES
   :non-standard-jsdoc DiagnosticGroups/NON_STANDARD_JSDOC
   :strict-module-dep-check DiagnosticGroups/STRICT_MODULE_DEP_CHECK
   :tweaks DiagnosticGroups/TWEAKS
   :undefined-names DiagnosticGroups/UNDEFINED_NAMES
   :undefined-variables DiagnosticGroups/UNDEFINED_VARIABLES
   :unknown-defines DiagnosticGroups/UNKNOWN_DEFINES
   :visiblity DiagnosticGroups/VISIBILITY})

(def known-opts
  "Set of all known compiler options."
  #{:anon-fn-naming-policy :asset-path :cache-analysis :closure-defines :closure-extra-annotations
    :closure-warnings :compiler-stats :dump-core :elide-asserts :externs :foreign-libs
    :hashbang :language-in :language-out :libs :main :modules :source-map-path :optimizations
    :optimize-constants :output-dir :output-to :output-wrapper :parallel-build :preamble
    :pretty-print :print-input-delimiter :pseudo-names :recompile-dependents :source-map
    :source-map-inline :source-map-timestamp :static-fns :target :verbose :warnings
    :emit-constants :ups-externs :ups-foreign-libs :ups-libs :warning-handlers})

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
        {}))))

(defn set-options
  "TODO: Add any other options that we would like to support."
  [opts ^CompilerOptions compiler-options]
  (when (contains? opts :pretty-print)
    (set! (.prettyPrint compiler-options) (:pretty-print opts)))

  (when (contains? opts :pseudo-names)
    (set! (.generatePseudoNames compiler-options) (:pseudo-names opts)))

  (when (contains? opts :anon-fn-naming-policy)
    (let [policy (:anon-fn-naming-policy opts)]
      (set! (.anonymousFunctionNaming compiler-options)
        (case policy
          :off AnonymousFunctionNamingPolicy/OFF
          :unmapped AnonymousFunctionNamingPolicy/UNMAPPED
          :mapped AnonymousFunctionNamingPolicy/MAPPED
          (throw (IllegalArgumentException. (str "Invalid :anon-fn-naming-policy value " policy " - only :off, :unmapped, :mapped permitted")))))))

  (when (contains? opts :language-in)
    (case (:language-in opts)
      :no-transpile       (.setLanguageIn compiler-options CompilerOptions$LanguageMode/NO_TRANSPILE)
      :ecmascript6        (.setLanguageIn compiler-options CompilerOptions$LanguageMode/ECMASCRIPT6)
      :ecmascript6-strict (.setLanguageIn compiler-options CompilerOptions$LanguageMode/ECMASCRIPT6_STRICT)
      :ecmascript6-typed  (.setLanguageIn compiler-options CompilerOptions$LanguageMode/ECMASCRIPT6_TYPED)
      :ecmascript5        (.setLanguageIn compiler-options CompilerOptions$LanguageMode/ECMASCRIPT5)
      :ecmascript5-strict (.setLanguageIn compiler-options CompilerOptions$LanguageMode/ECMASCRIPT5_STRICT)
      :ecmascript3        (.setLanguageIn compiler-options CompilerOptions$LanguageMode/ECMASCRIPT3)))

  (when (contains? opts :language-out)
    (case (:language-out opts)
      :no-transpile       (.setLanguageOut compiler-options CompilerOptions$LanguageMode/NO_TRANSPILE)
      :ecmascript6        (.setLanguageOut compiler-options CompilerOptions$LanguageMode/ECMASCRIPT6)
      :ecmascript6-strict (.setLanguageOut compiler-options CompilerOptions$LanguageMode/ECMASCRIPT6_STRICT)
      :ecmascript6-typed  (.setLanguageOut compiler-options CompilerOptions$LanguageMode/ECMASCRIPT6_TYPED)
      :ecmascript5        (.setLanguageOut compiler-options CompilerOptions$LanguageMode/ECMASCRIPT5)
      :ecmascript5-strict (.setLanguageOut compiler-options CompilerOptions$LanguageMode/ECMASCRIPT5_STRICT)
      :ecmascript3        (.setLanguageOut compiler-options CompilerOptions$LanguageMode/ECMASCRIPT3)))

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
          (integer? val) (.setDefineToIntegerLiteral compiler-options key val)
          (float? val) (.setDefineToDoubleLiteral compiler-options key val)
          (or (true? val)
              (false? val)) (.setDefineToBooleanLiteral compiler-options key val)
          :else (println "value for" key "must be string, int, float, or bool"))))
    (if-let [extra-annotations (:closure-extra-annotations opts)]
      (. compiler-options (setExtraAnnotationNames (map name extra-annotations))))
    (when (contains? opts :source-map)
      (if (:modules opts)
        ;; name is not actually used by Closur in :modules case,
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
  [{:keys [externs use-only-custom-externs target ups-externs]}]
  (let [validate (fn validate [p us]
                   (if (empty? us)
                     (throw (IllegalArgumentException.
                              (str "Extern " p " does not exist")))
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
    (let [js-sources (-> externs filter-js add-target load-js)
          ups-sources (-> ups-externs filter-cp-js load-js)
          all-sources (concat js-sources ups-sources)] 
      (if use-only-custom-externs
        all-sources
        (into all-sources (CommandLineRunner/getDefaultExterns))))))

(defn ^com.google.javascript.jscomp.Compiler make-closure-compiler []
  (let [compiler (com.google.javascript.jscomp.Compiler.)]
    (com.google.javascript.jscomp.Compiler/setLoggingLevel Level/WARNING)
    compiler))

(defn report-failure [^Result result]
  (let [errors (.errors result)
        warnings (.warnings result)]
    (doseq [next (seq errors)]
      (println "ERROR:" (.toString ^JSError next)))
    (doseq [next (seq warnings)]
      (println "WARNING:" (.toString ^JSError next)))))


;; Protocols for IJavaScript and Compilable
;; ========================================



(defprotocol ISourceMap
  (-source-url [this] "Return the CLJS source url")
  (-source-map [this] "Return the CLJS compiler generated JS source mapping"))

(extend-protocol deps/IJavaScript
  
  String
  (-foreign? [this] false)
  (-closure-lib? [this] false)
  (-url [this] nil)
  (-provides [this] (:provides (deps/parse-js-ns (string/split-lines this))))
  (-requires [this] (:requires (deps/parse-js-ns (string/split-lines this))))
  (-source [this] this)
  
  clojure.lang.IPersistentMap
  (-foreign? [this] (:foreign this))
  (-closure-lib? [this] (:closure-lib this))
  (-url [this] (or (:url this)
                   (deps/to-url (:file this))))
  (-provides [this] (map name (:provides this)))
  (-requires [this] (map name (:requires this)))
  (-source [this] (if-let [s (:source this)]
                    s (with-open [reader (io/reader (deps/-url this))]
                        (slurp reader)))))

(defrecord JavaScriptFile [foreign ^URL url ^URL source-url provides requires lines source-map]
  deps/IJavaScript
  (-foreign? [this] foreign)
  (-closure-lib? [this] (:closure-lib this))
  (-url [this] url)
  (-provides [this] provides)
  (-requires [this] requires)
  (-source [this]
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
      (when-let [f (:file m)]
        (deps/to-url f))
      (when-let [sf (:source-file m)]
        (deps/to-url sf))
      (:provides m)
      (:requires m)
      (:lines m)
      (:source-map m))
    (when (:closure-lib m)
      {:closure-lib true})
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

(defn compile-form-seq
  "Compile a sequence of forms to a JavaScript source string."
  [forms]
  (comp/with-core-cljs nil
    (fn []
      (with-out-str
        (binding [ana/*cljs-ns* 'cljs.user]
          (doseq [form forms]
            (comp/emit (ana/analyze (ana/empty-env) form))))))))

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

(defn compile-file
  "Compile a single cljs file. If no output-file is specified, returns
  a string of compiled JavaScript. With an output-file option, the
  compiled JavaScript will written to this location and the function
  returns a JavaScriptFile. In either case the return value satisfies
  IJavaScript."
  [^File file {:keys [output-file] :as opts}]
    (if output-file 
      (let [out-file (io/file (util/output-directory opts) output-file)]
        (compiled-file (comp/compile-file file out-file opts)))
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
    (jar-file-to-disk url out-dir nil))
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

;; TODO: it would be nice if we could consolidate requires-compilation?
;; logic - David
(defn compile-from-jar
  "Compile a file from a jar if necessary. Returns IJavaScript."
  [jar-file {:keys [output-file] :as opts}]
  (let [out-file (when output-file
                   (io/file (util/output-directory opts) output-file))]
    (if (or (nil? out-file)
            (not (.exists ^File out-file))
            (not= (util/compiled-by-version out-file)
                  (util/clojurescript-version))
            (util/changed? jar-file out-file))
      ;; actually compile from JAR
      (let [file-on-disk (jar-file-to-disk jar-file (util/output-directory opts) opts)]
        (-compile file-on-disk opts))
      ;; have to call compile-file as it includes more IJavaScript
      ;; information than ana/parse-ns
      (compile-file
        (io/file (util/output-directory opts)
          (last (string/split (.getPath ^URL jar-file) #"\.jar!/")))
        opts))))

(defn find-jar-sources
  [this opts]
  [(comp/find-source (jar-file-to-disk this (util/output-directory opts)))])

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
  
  clojure.lang.PersistentVector
  (-compile [this opts] (compile-form-seq this))
  (-find-sources [this opts]
    [(ana/parse-ns this opts)])
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
            new-req (remove #(contains? visited %) (:requires node))]
        (recur (into (rest requires) new-req)
               (into visited new-req)
               (conj deps node)))
      (remove nil? deps))))

(comment
  ;; find dependencies
  (js-dependencies {} ["goog.array"])
  ;; find dependencies in an external library
  (js-dependencies {:libs ["closure/library/third_party/closure"]} ["goog.dom.query"])
  )

(defn- add-core-macros-if-cljs-js
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
      {:relative-path relpath :uri (io/resource relpath)})
    (let [path    (-> (munge ns) (string/replace \. \/))
          relpath (str path ".cljs")]
      (if-let [res (io/resource relpath)]
        {:relative-path relpath :uri res}
        (let [relpath (str path ".cljc")]
          (if-let [res (io/resource relpath)]
            {:relative-path relpath :uri res}))))))

(defn source-for-namespace
  "Given a namespace and compilation environment return the relative path and
  uri of the corresponding source regardless of the source language extension:
  .cljs, .cljc, .js"
  [ns compiler-env]
  (let [ns-str  (str (comp/munge ns {}))
        path    (string/replace ns-str \. \/)
        relpath (str path ".cljs")]
    (if-let [cljs-res (io/resource relpath)]
      {:relative-path relpath :uri cljs-res}
      (let [relpath (str path ".cljc")]
        (if-let [cljc-res (io/resource relpath)]
          {:relative-path relpath :uri cljc-res}
          (let [relpath (str path ".js")]
            (if-let [js-res (io/resource relpath)]
              {:relative-path relpath :uri js-res}
              (let [ijs (get-in @compiler-env [:js-dependency-index (str ns)])
                   relpath (or (:file ijs) (:url ijs))]
               (if-let [js-res (and relpath
                                 ;; try to parse URL, otherwise just return local
                                 ;; resource
                                 (or (and (util/url? relpath) relpath)
                                   (try (URL. relpath) (catch Throwable t))
                                   (io/resource relpath)))]
                 {:relative-path relpath :uri js-res}
                 (throw
                   (IllegalArgumentException.
                     (str "Namespace " ns " does not exist"))))))))))))

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

(defn add-dependencies
  "Given one or more IJavaScript objects in dependency order, produce
  a new sequence of IJavaScript objects which includes the input list
  plus all dependencies in dependency order."
  [opts & inputs]
  (let [inputs        (set inputs)
        requires      (set (mapcat deps/-requires inputs))
        required-cljs (clojure.set/difference (cljs-dependencies opts requires) inputs)
        required-js   (js-dependencies opts
                        (into (set (mapcat deps/-requires required-cljs)) requires))
        provided      (set (mapcat deps/-provides (clojure.set/union inputs required-cljs required-js)))
        unprovided    (clojure.set/difference requires provided #{"constants-table"})]
    (when (seq unprovided)
      (ana/warning :unprovided @env/*compiler* {:unprovided (sort unprovided)}))
    (cons
      (javascript-file nil (io/resource "goog/base.js") ["goog"] nil)
      (deps/dependency-order
        (concat
          (map
            (fn [{:keys [foreign url file provides requires] :as js-map}]
              (let [url (or url (io/resource file))]
                (merge
                  (javascript-file foreign url provides requires)
                  js-map)))
            required-js)
          [(when (-> @env/*compiler* :options :emit-constants)
             (let [url (deps/to-url (str (util/output-directory opts) "/constants_table.js"))]
               (javascript-file nil url url ["constants-table"] ["cljs.core"] nil nil)))]
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
   (add-dependency-sources inputs nil))
  ([inputs compile-opts]
   (let [inputs         (set inputs)
         requires       (set (mapcat deps/-requires inputs))
         module-entries (module-entries compile-opts)]
     (into inputs (find-cljs-dependencies (set/union requires module-entries))))))

(defn check-unprovided
  [inputs]
  (let [requires   (set (mapcat deps/-requires inputs))
        provided   (set (mapcat deps/-provides inputs))
        unprovided (clojure.set/difference requires provided #{"constants-table"})]
    (when (seq unprovided)
      (ana/warning :unprovided @env/*compiler* {:unprovided (sort unprovided)}))
    inputs))

(defn compile-task [^LinkedBlockingDeque deque input-set compiled opts failed]
  (loop [ns-info (.pollFirst deque)]
    (when (and ns-info (not @failed))
      (let [{:keys [requires]} ns-info
            input-set' @input-set]
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
  (let [deque     (LinkedBlockingDeque. inputs)
        input-set (atom (into #{} (comp (remove nil?) (map :ns)) inputs))
        cnt       (+ 2 (.. Runtime getRuntime availableProcessors))
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

(defn add-goog-base
  [inputs]
  (cons (javascript-file nil (io/resource "goog/base.js") ["goog"] nil)
        inputs))

(defn add-js-sources
  "Given list of IJavaScript objects, add foreign-deps and constants-table
  IJavaScript objects to the list."
  [inputs opts]
  (let [requires      (set (mapcat deps/-requires inputs))
        required-js   (js-dependencies opts requires)]
    (concat
      (map
        (fn [{:keys [foreign url file provides requires] :as js-map}]
          (let [url (or url (io/resource file))]
            (merge
              (javascript-file foreign url provides requires)
              js-map)))
        required-js)
      [(when (-> @env/*compiler* :options :emit-constants)
         (let [url (deps/to-url (str (util/output-directory opts) "/constants_table.js"))]
           (javascript-file nil url url ["constants-table"] ["cljs.core"] nil nil)))]
      inputs)))

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

(comment
  ;; add dependencies to literal js
  (add-dependencies {} "goog.provide('test.app');\ngoog.require('cljs.core');")
  (add-dependencies {} "goog.provide('test.app');\ngoog.require('goog.array');")
  (add-dependencies {} (str "goog.provide('test.app');\n"
                            "goog.require('goog.array');\n"
                            "goog.require('clojure.set');"))
  ;; add dependencies with external lib
  (add-dependencies {:libs ["closure/library/third_party/closure"]}
                    (str "goog.provide('test.app');\n"
                         "goog.require('goog.array');\n"
                         "goog.require('goog.dom.query');"))
  ;; add dependencies with foreign lib
  (add-dependencies {:foreign-libs [{:file "samples/hello/src/hello/core.cljs"
                                     :provides ["example.lib"]}]}
                    (str "goog.provide('test.app');\n"
                         "goog.require('example.lib');\n"))
  ;; add dependencies to a JavaScriptFile record
  (add-dependencies {} (javascript-file false
                                        (deps/to-url "samples/hello/src/hello/core.cljs")
                                        ["hello.core"]
                                        ["goog.array"]))
  )

;; Optimize
;; ========

(defmulti javascript-name class)

(defmethod javascript-name URL [^URL url]
  (if url (.getPath url) "cljs/user.js"))

(defmethod javascript-name String [s]
  (if-let [name (first (deps/-provides s))] name "cljs/user.js"))

(defmethod javascript-name JavaScriptFile [js] (javascript-name (deps/-url js)))

(defn build-provides
  "Given a vector of provides, builds required goog.provide statements"
  [provides]
  (apply str (map #(str "goog.provide('" % "');\n") provides)))

(defmethod js-source-file JavaScriptFile [_ js]
  (when-let [url (deps/-url js)]
    (js-source-file (javascript-name url) (io/input-stream url))))

(defn add-cljs-base-module
  ([modules] (add-cljs-base-module modules nil))
  ([modules opts]
   (reduce
     (fn [modules module-name]
       (if-not (= module-name :cljs-base)
         (update-in modules [module-name :depends-on]
           (fnil identity #{:cljs-base}))
         modules))
     (update-in modules [:cljs-base :output-to]
       (fnil io/file
         (io/file
           (util/output-directory opts)
           "cljs_base.js")))
     (keys modules))))

(comment
  (add-cljs-base-module
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

(defn sort-modules [modules-with-base]
  (letfn [(get-deps [module]
            (reduce
              (fn [ret [name {:keys [depends-on] :as module-desc}]]
                (cond-> ret
                  (contains? depends-on module) (conj name)))
              [] modules-with-base))]
    (vec (map (fn [module-name]
                [module-name (module-name modules-with-base)])
           (into [:cljs-base] (util/topo-sort :cljs-base get-deps))))))

(comment
  (sort-modules
    (add-cljs-base-module
      {:cljs-base
       {:output-to "out/module/base.js"}
       :core
       {:output-to "out/modules/core.js"
        :entries   '#{cljs.core}}
       :landing
       {:output-to  "out/modules/reader.js"
        :entries    '#{cljs.reader}
        :depends-on #{:core}}}))
  )

(defn build-modules
  "Given a list of IJavaScript sources in dependency order and compiler options
   return a dependency sorted list of module name / description tuples. The
   module descriptions will be augmented with a :closure-module entry holding
   the Closure JSModule. Each module description will also be augmented with
   a :foreign-deps vector containing foreign IJavaScript sources in dependency
   order."
  [sources opts]
  (let [find-entry (fn [sources entry]
                     (some
                       (fn [source]
                         (let [matcher
                               (into #{}
                                 [(name entry) (name (comp/munge entry))])]
                           (when (some matcher (:provides source))
                             source)))
                       sources))
        used (atom {})
        [sources' modules]
        (reduce
          (fn [[sources ret] [name {:keys [entries output-to depends-on] :as module-desc}]]
            (assert (or (= name :cljs-base) (not (empty? entries)))
              (str "Module " name " does not define any :entries"))
            (when (:verbose opts)
              (util/debug-prn "Building module" name))
            (let [js-module (JSModule. (clojure.core/name name))
                  [sources' module-sources]
                  ;; compute inputs for a closure module
                  ;; as well as sources difference
                  (reduce
                    (fn [[sources ret] entry-sym]
                      (if-let [entry (find-entry sources entry-sym)]
                        (do
                          (swap! used assoc entry-sym name)
                          [(remove #{entry} sources) (conj ret entry)])
                        (if (contains? @used entry-sym)
                          (throw
                            (IllegalArgumentException.
                              (str "Already used namespace " entry-sym " "
                                   "in module " (get @used entry-sym))))
                          (throw
                            (IllegalArgumentException.
                             (str "Could not find namespace " entry-sym))))))
                    [sources []] entries)
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
                    (.addDependency js-module ^JSModule parent-module))
                  (throw (IllegalArgumentException.
                           (str "Parent module " dep " does not exist")))))
              [sources'
               (conj ret
                 [name (assoc module-desc
                         :closure-module js-module
                         :foreign-deps @foreign-deps)])]))
          [sources []] (sort-modules (add-cljs-base-module (:modules opts) opts)))
        cljs-base-closure-module (get-in (into {} modules) [:cljs-base :closure-module])
        foreign-deps (atom [])]
    (when (:verbose opts)
      (util/debug-prn "Adding remaining namespaces to" :cljs-base))
    ;; add anything left to :cljs-base module
    (doseq [source sources']
      (when (:verbose opts)
        (util/debug-prn "  adding entry" (:provides source)))
      (if-not (deps/-foreign? source)
        (.add ^JSModule cljs-base-closure-module
          (js-source-file (javascript-name source) source))
        (swap! foreign-deps conj source)))
    (assoc-in modules [0 1 :foreign-deps] @foreign-deps)))

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

(defn optimize-modules
  "Use the Closure Compiler to optimize one or more Closure JSModules. Returns
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
        _ (doseq [^JSModule input inputs]
            (.sortInputsByDeps input closure-compiler))
        ^Result result (.compileModules closure-compiler externs inputs compiler-options)
        ^SourceMap source-map (when (:source-map opts)
                                (.getSourceMap closure-compiler))]
    (assert (or (nil? (:source-map opts)) source-map)
      "Could not create source maps for modules")
    (if (.success result)
      (vec
        (for [[name {:keys [output-to closure-module] :as module}] modules]
          [name
           (merge
             (assoc module
               :source
               (do
                 (when source-map (.reset source-map))
                 (.toSource closure-compiler ^JSModule closure-module)))
             (when source-map
               (let [sw (StringWriter.)
                     source-map-name (str output-to ".map.closure")]
                 (.appendTo source-map sw source-map-name)
                 {:source-map-json (.toString sw)
                  :source-map-name source-map-name})))]))
      (report-failure result))))

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
        ^List inputs (map #(js-source-file (javascript-name %) %) sources)
        ^Result result (.compile closure-compiler externs inputs compiler-options)]
    (if (.success result)
      ;; compiler.getSourceMap().reset()
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
        source)
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
  
  ;; optimize a project
  (println (->> (-compile "samples/hello/src" {})
                (apply add-dependencies {})
                (apply optimize {:optimizations :simple :pretty-print true})))
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
         (ns-list (deps/-requires input))
         "]);\n")))

(defn deps-file
  "Return a deps file string for a sequence of inputs."
  [opts sources]
  (apply str (map #(add-dep-string opts %) sources)))

(comment
  (path-relative-to (io/file "out/goog/base.js") {:url (deps/to-url "out/cljs/core.js")})
  (add-dep-string {} {:url (deps/to-url "out/cljs/core.js") :requires ["goog.string"] :provides ["cljs.core"]})
  (deps-file {} [{:url (deps/to-url "out/cljs/core.js") :requires ["goog.string"] :provides ["cljs.core"]}])
  )

(defn output-one-file [{:keys [output-to] :as opts} js]
  (cond
    (nil? output-to) js

    (string? output-to)
    (let [f (io/file output-to)]
      (util/mkdirs f)
      (spit f js))

    :else (println js)))

(defn output-deps-file [opts sources]
  (output-one-file opts (deps-file opts sources)))

(declare foreign-deps-str add-header add-source-map-link)

(defn output-main-file [opts]
  (let [asset-path (or (:asset-path opts)
                       (util/output-directory opts))
        closure-defines (json/write-str (:closure-defines opts))]
    (case (:target opts)
      :nodejs
      (output-one-file opts
        (add-header opts
          (str "var path = require(\"path\");\n"
               "try {\n"
               "    require(\"source-map-support\").install();\n"
               "} catch(err) {\n"
               "}\n"
               "require(path.join(path.resolve(\".\"),\"" asset-path "\",\"goog\",\"bootstrap\",\"nodejs.js\"));\n"
               "require(path.join(path.resolve(\".\"),\"" asset-path "\",\"cljs_deps.js\"));\n"
               "goog.global.CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
               "goog.require(\"" (comp/munge (:main opts)) "\");\n"
               "goog.require(\"cljs.nodejscli\");\n")))
      (output-one-file opts
        (str "var CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
             "if(typeof goog == \"undefined\") document.write('<script src=\"" asset-path "/goog/base.js\"></script>');\n"
             "document.write('<script src=\"" asset-path "/cljs_deps.js\"></script>');\n"
             "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"" (comp/munge (:main opts))
             "\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n")))))

(defn output-modules
  "Given compiler options, original IJavaScript sources and a sequence of
   module name and module description tuples output module sources to disk.
   Modules description must define :output-to and supply :source entry with
   the JavaScript source to write to disk."
  [opts js-sources modules]
  (doseq [[name {:keys [output-to source foreign-deps] :as module-desc}] modules]
    (assert (not (nil? output-to))
      (str "Module " name " does not define :output-to"))
    (assert (not (nil? source))
      (str "Module " name " did not supply :source"))
    (let [fdeps-str (when-not (empty? foreign-deps)
                      (foreign-deps-str opts foreign-deps))
          sm-name (when (:source-map opts)
                    (str output-to ".map"))
          out-file (io/file output-to)]
      (util/mkdirs out-file)
      (spit out-file
        (as-> source source
          (if (= name :cljs-base)
            (add-header opts source)
            source)
          (if fdeps-str
            (str fdeps-str "\n" source)
            source)
          (if sm-name
            (add-source-map-link
              (assoc opts
                :output-to output-to
                :source-map sm-name)
              source)
            source)))
      (when (:source-map opts)
        (let [sm-json-str (:source-map-json module-desc)
              sm-json     (json/read-str sm-json-str :key-fn keyword)]
          (when (true? (:closure-source-map opts))
            (spit (io/file (:source-map-name module-desc)) sm-json-str))
          (emit-optimized-source-map sm-json js-sources sm-name
            (merge opts
              {:source-map sm-name
               :preamble-line-count
               (if (= name :cljs-base)
                 (+ (- (count (.split #"\r?\n" (make-preamble opts) -1)) 1)
                    (if (:output-wrapper opts) 1 0))
                 0)
               :foreign-deps-line-count
               (if fdeps-str
                 (- (count (.split #"\r?\n" fdeps-str -1)) 1)
                 0)})))))))

(defn lib-rel-path [{:keys [lib-path url provides] :as ijs}]
  (if (nil? lib-path)
    (util/ns->relpath (first provides) "js")
    (if (.endsWith lib-path ".js")
      (util/get-name url)
      (let [path (util/path url)]
        (string/replace
          path
          (str (io/file (System/getProperty "user.dir") lib-path) File/separator)
          "")))))

(defn ^String rel-output-path
  "Given an IJavaScript which is either in memory, in a jar file,
  or is a foreign lib, return the path relative to the output
  directory."
  ([js] (rel-output-path js nil))
  ([js opts]
   (let [url (deps/-url js)]
     (cond
       url
       (cond
         (deps/-closure-lib? js) (lib-rel-path js)
         (deps/-foreign? js) (util/get-name url)
         :else (path-from-jarfile url))

       (string? js)
       (let [digest (MessageDigest/getInstance "SHA-1")]
         (.reset digest)
         (.update digest (.getBytes ^String js "utf8"))
         (str
           (->> (DatatypeConverter/printHexBinary (.digest digest))
             (take 7)
             (apply str))
           ".js"))

       :else (str (random-string 5) ".js")))))

(defn get-js-module-root [js-file]
  (let [path (.getParent (io/file js-file))]
    (cond->> path
      (.startsWith path File/separator) (str ".")
      (not (.startsWith path (str "." File/separator))) (str "." File/separator)
      (not (.endsWith path File/separator)) (#(str % File/separator)))))

(util/compile-if is-new-es6-loader?
  (defn make-es6-loader [source-files]
    (let [^List module-roots (list default-module-root)
          ^List compiler-inputs (map #(CompilerInput. %) source-files)]
      (ES6ModuleLoader. module-roots compiler-inputs)))
  (defn make-es6-loader [closure-compiler file]
    (let [module-root (get-js-module-root file)]
      (ES6ModuleLoader. closure-compiler module-root))))

(defn ^Node get-root-node [ijs closure-compiler]
  (let [^CompilerInput input (->> (deps/-source ijs)
                                  (js-source-file (:file ijs))
                                  (CompilerInput.))]
    (.getAstRoot input closure-compiler)))

(defn get-source-files [opts]
  (->> (:foreign-libs opts)
       (filter #(let [module-type (:module-type %)]
                  (or (= module-type :amd)
                      (= module-type :commonjs)
                      (= module-type :es6))))
       (map (fn [lib]
              (let [lib (deps/load-foreign-library lib)]
                (js-source-file (:file lib) (deps/-source lib)))))))

(defmulti convert-js-module
  "Takes a JavaScript module as an IJavaScript and rewrites it into a Google
  Closure-compatible form. Returns an IJavaScript with the converted module
  code set as source."
  (fn [{module-type :module-type :as ijs} opts]
    (if (and (= module-type :amd) can-convert-amd?)
      ;; AMD modules are converted via CommonJS modules
      :commonjs
      module-type)))

(defn make-convert-js-module-options [opts]
  (-> opts
    (select-keys
      [:closure-warnings :closure-extra-annotations :pretty-print
       :language-in :language-out])
    (set-options (CompilerOptions.))))

(util/compile-if can-convert-commonjs?
  (defmethod convert-js-module :commonjs [ijs opts]
    (let [{:keys [file module-type]} ijs
          ^List externs '()
          ^List source-files (get-source-files opts)
          ^CompilerOptions options (make-convert-js-module-options opts)
          closure-compiler (doto (make-closure-compiler)
                             (.init externs source-files options))
          es6-loader (if is-new-es6-loader?
                       (make-es6-loader source-files)
                       (make-es6-loader closure-compiler file))
          cjs (ProcessCommonJSModules. closure-compiler es6-loader)
          ^Node root (get-root-node ijs closure-compiler)]
      (util/compile-if can-convert-amd?
        (when (= module-type :amd)
          (.process (TransformAMDToCJSModule. closure-compiler) nil root)))
      (.process cjs nil root)
      (report-failure (.getResult closure-compiler))
      (assoc ijs :source (.toSource closure-compiler root)))))

(util/compile-if can-convert-es6?
  (defmethod convert-js-module :es6 [ijs opts]
    (let [{:keys [file]} ijs
          ^List externs '()
          ^List source-files (get-source-files opts)
          ^CompilerOptions options (doto (make-convert-js-module-options opts)
                                     (.setLanguageIn CompilerOptions$LanguageMode/ECMASCRIPT6)
                                     (.setLanguageOut CompilerOptions$LanguageMode/ECMASCRIPT5))
          closure-compiler (doto (make-closure-compiler)
                             (.init externs source-files options))
          es6-loader (if is-new-es6-loader?
                       (make-es6-loader source-files)
                       (make-es6-loader closure-compiler file))
          cjs (ProcessEs6Modules. closure-compiler es6-loader true)
          ^Node root (get-root-node ijs closure-compiler)]
      (.processFile cjs root)
      (report-failure (.getResult closure-compiler))
      (assoc ijs :source (.toSource closure-compiler root)))))

(defmethod convert-js-module :default [ijs opts]
  (ana/warning :unsupported-js-module-type @env/*compiler* ijs)
  ijs)

(defmulti js-transforms
  "Takes an IJavaScript with the source code set as source, transforms the
  source code and returns an IJavascript with the new code set as source."
  (fn [ijs opts]
    (:preprocess ijs)))

(defmethod js-transforms :default [ijs opts]
  (ana/warning :unsupported-preprocess-value @env/*compiler* ijs)
  ijs)

(defn write-javascript
  "Write or copy a JavaScript file to output directory. Only write if the file
   does not already exist. Return IJavaScript for the file on disk at the new
   location."
  [opts js]
  (let [out-dir  (io/file (util/output-directory opts))
        out-name (rel-output-path js opts)
        out-file (io/file out-dir out-name)
        ijs      {:url      (deps/to-url out-file)
                  :out-file (.toString out-file)
                  :requires (deps/-requires js)
                  :provides (deps/-provides js)
                  :group    (:group js)}
        res      (or (:url js) (:source-file js))]
    (when (or (not (.exists out-file))
              (and res (util/changed? out-file res)))
      (when (and res (or ana/*verbose* (:verbose opts)))
        (util/debug-prn "Copying" (str res) "to" (str out-file)))
      (util/mkdirs out-file)
      (spit out-file
        (cond-> js
          (map? js) (assoc :source (deps/-source js))
          (:preprocess js) (js-transforms opts)
          (:module-type js) (convert-js-module opts)
          true deps/-source))
      (when res
        (.setLastModified ^File out-file (util/last-modified res))))
    (if (map? js)
      (merge js ijs)
      ijs)))

(defn write-js?
  "Returns true if IJavaScript instance needs to be written/copied to output
  directory. True when in memory, in a JAR, or if foreign library."
  [js]
  (let [url ^URL (deps/-url js)]
    (or (not url)
        (= (.getProtocol url) "jar")
        (deps/-closure-lib? js)
        (deps/-foreign? js))))

(defn source-on-disk
  "Ensure that the given IJavaScript exists on disk in the output directory.
   Return updated IJavaScript with the new location if necessary."
  [opts js]
  (if (write-js? js)
    (write-javascript opts js)
    ;; always copy original ClojureScript sources to the output directory
    ;; when source maps enabled
    (let [out-file (when-let [ns (and (:source-map opts)
                                      (:source-url js)
                                      (first (:provides js)))]
                     (io/file (io/file (util/output-directory opts))
                       (util/ns->relpath ns (util/ext (:source-url js)))))
          source-url (:source-url js)]
      (when (and out-file source-url
                 (or (not (.exists ^File out-file))
                     (util/changed? (io/file source-url) out-file)))
        (when (or ana/*verbose* (:verbose opts))
          (util/debug-prn "Copying" (str source-url) "to" (str out-file)))
        (spit out-file (slurp source-url))
        (.setLastModified ^File out-file (util/last-modified source-url)))
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
  [opts & sources]
  (let [disk-sources (remove #(= (:group %) :goog)
                       (map #(source-on-disk opts %) sources))
        goog-deps    (io/file (util/output-directory opts)
                       "goog" "deps.js")
        main         (:main opts)]
    (util/mkdirs goog-deps)
    (spit goog-deps (slurp (io/resource "goog/deps.js")))
    (if main
      (do
        (output-deps-file
          (assoc opts :output-to
            (str (util/output-directory opts)
                 File/separator "cljs_deps.js"))
          disk-sources)
        (output-main-file opts))
      (output-deps-file opts disk-sources))))

(comment
  
  ;; output unoptimized alone
  (output-unoptimized {} "goog.provide('test');\ngoog.require('cljs.core');\nalert('hello');\n")
  ;; output unoptimized with all dependencies
  (apply output-unoptimized {}
         (add-dependencies {}
                           "goog.provide('test');\ngoog.require('cljs.core');\nalert('hello');\n"))
  ;; output unoptimized with external library
  (apply output-unoptimized {}
         (add-dependencies {:libs ["closure/library/third_party/closure"]}
                           "goog.provide('test');\ngoog.require('cljs.core');\ngoog.require('goog.dom.query');\n"))
  ;; output unoptimized and write deps file to 'out/test.js'
  (output-unoptimized {:output-to "out/test.js"}
                      "goog.provide('test');\ngoog.require('cljs.core');\nalert('hello');\n")
  )


(defn get-upstream-deps* 
  "returns a merged map containing all upstream dependencies defined
  by libraries on the classpath."
  ([]
   (get-upstream-deps* (. (Thread/currentThread) (getContextClassLoader))))
  ([classloader]
   (let [upstream-deps (map #(read-string (slurp %))
                         (enumeration-seq (. classloader (getResources "deps.cljs"))))]
     (apply merge-with concat upstream-deps))))

(def get-upstream-deps (memoize get-upstream-deps*))

(defn add-header [opts js]
  (str (make-preamble opts) js))

(defn foreign-deps-str [opts sources]
  (letfn [(to-js-str [ijs]
            (let [url (or (and (#{:advanced :simple} (:optimizations opts))
                               (:url-min ijs))
                          (:url ijs))]
              (slurp url)))]
    (str (string/join "\n" (map to-js-str sources)) "\n")))

(defn add-wrapper [{:keys [output-wrapper] :as opts} js]
  (if output-wrapper
   (str ";(function(){\n" js "\n})();\n")
   js))

(defn add-source-map-link [{:keys [source-map output-to] :as opts} js]
  (if source-map
      (if (= output-to :print)
        (str js "\n//# sourceMappingURL=" source-map)
        (str js "\n//# sourceMappingURL=" (path-relative-to (io/file output-to) {:url source-map})))
    js))

(defn absolute-path? [path]
  (.isAbsolute (io/file path)))

(defn absolute-parent [path]
  (.getParent (.getAbsoluteFile (io/file path))))

(defn in-same-dir? [path-1 path-2]
  "Checks that path-1 and path-2 are siblings in the same logical directory."
  (= (absolute-parent path-1)
     (absolute-parent path-2)))

(defn same-or-subdirectory-of? [dir path]
  "Checks that path names a file or directory that is the dir or a subdirectory there of."
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

(defn check-source-map [{:keys [output-to source-map output-dir optimizations] :as opts}]
  "When :source-map is specified in opts, "
  (when (and (contains? opts :source-map)
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

(defn check-node-target [{:keys [target optimizations] :as opts}]
  (assert (not (and (= target :nodejs) (= optimizations :whitespace)))
    (format ":nodejs target not compatible with :whitespace optimizations")))

(defn foreign-source? [js]
  (and (satisfies? deps/IJavaScript js)
       (deps/-foreign? js)))

(defn add-implicit-options
  [{:keys [optimizations output-dir]
    :or {optimizations :none
         output-dir "out"}
    :as opts}]
  (let [opts (cond-> opts
               (:closure-defines opts)
               (assoc :closure-defines
                 (into {}
                   (map (fn [[k v]]
                          [(if (symbol? k) (str (comp/munge k)) k) v])
                     (:closure-defines opts)))))
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
          :ups-foreign-libs foreign-libs
          :ups-externs externs
          :emit-constants emit-constants)
        (update-in [:preamble] #(into (or % []) ["cljs/imul.js"])))

      (:target opts)
      (assoc-in [:closure-defines (str (comp/munge 'cljs.core/*target*))]
        (name (:target opts)))

      (= optimizations :none)
      (assoc
        :cache-analysis (:cache-analysis opts true)
        :source-map (:source-map opts true))

      (= optimizations :advanced)
      (cond->
        (not (false? (:static-fns opts))) (assoc :static-fns true)
        (not (false? (:optimize-constants opts))) (assoc :optimize-constants true)))))

(defn process-js-modules
  "Given the current compiler options, converts JavaScript modules to Google
  Closure modules and writes them to disk. Adds mapping from original module
  namespace to new module namespace to compiler env. Returns modified compiler
  options where new modules are passed with :libs option."
  [opts]
  (let [js-modules (filter :module-type (:foreign-libs opts))]
    (reduce (fn [new-opts {:keys [file module-type] :as lib}]
              (if (or (and (= module-type :commonjs) can-convert-commonjs?)
                      (and (= module-type :amd) can-convert-amd?)
                      (and (= module-type :es6) can-convert-es6?))
                (let [ijs (write-javascript opts (deps/load-foreign-library lib))
                      module-name (-> (deps/load-library (:out-file ijs)) first :provides first)]
                  (doseq [provide (:provides ijs)]
                    (swap! env/*compiler*
                      #(update-in % [:js-module-index] assoc provide module-name)))
                  (-> new-opts
                      (update-in [:libs] (comp vec conj) (:out-file ijs))
                      (update-in [:foreign-libs]
                        (comp vec (fn [libs] (remove #(= (:file %) file) libs))))))
                new-opts))
            opts js-modules)))

(defn build
  "Given a source which can be compiled, produce runnable JavaScript."
  ([source opts]
    (build source opts
      (if-not (nil? env/*compiler*)
        env/*compiler*
        (env/default-compiler-env opts))))
  ([source opts compiler-env]
     (env/with-compiler-env compiler-env
       (let [compiler-stats (:compiler-stats opts)
             all-opts (-> opts
                          add-implicit-options
                          process-js-modules)]
         (check-output-to opts)
         (check-output-dir opts)
         (check-source-map opts)
         (check-source-map-path opts)
         (check-output-wrapper opts)
         (check-node-target opts)
         (swap! compiler-env
           #(-> %
             (update-in [:options] merge all-opts)
             (assoc :target (:target opts))
             (assoc :js-dependency-index (deps/js-dependency-index all-opts))
             ;; Save list of sources for cljs.analyzer/locate-src - Juho Teperi
             (assoc :sources (-find-sources source all-opts))))
         (binding [comp/*recompiled* (when-not (false? (:recompile-dependents opts))
                                       (atom #{}))
                   ana/*cljs-static-fns*
                   (or (and (= (:optimizations opts) :advanced)
                            (not (false? (:static-fns opts))))
                       (:static-fns opts)
                       ana/*cljs-static-fns*)
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
           (let [one-file? (and (:main all-opts)
                                (#{:advanced :simple} (:optimizations all-opts)))
                 source (if one-file?
                          (let [main (:main all-opts)
                                uri  (:uri (cljs-source-for-namespace main))]
                            (assert uri (str "No file for namespace " main " exists"))
                            uri)
                          source)
                 compile-opts (if one-file?
                                (assoc all-opts :output-file (:output-to all-opts))
                                all-opts)
                 js-sources (-> (-find-sources source all-opts)
                                (add-dependency-sources compile-opts)
                                deps/dependency-order
                                (compile-sources compiler-stats compile-opts)
                                (#(map add-core-macros-if-cljs-js %))
                                (add-js-sources all-opts)
                                (cond-> (= :nodejs (:target all-opts)) (concat [(-compile (io/resource "cljs/nodejs.cljs") all-opts)]))
                                deps/dependency-order
                                add-goog-base
                                (cond-> (= :nodejs (:target all-opts)) (concat [(-compile (io/resource "cljs/nodejscli.cljs") all-opts)])))
                 _ (when (:emit-constants all-opts)
                     (comp/emit-constants-table-to-file
                       (::ana/constant-table @env/*compiler*)
                       (str (util/output-directory all-opts) "/constants_table.js")))
                 optim (:optimizations all-opts)
                 ret (if (and optim (not= optim :none))
                       (do
                         (when-let [fname (:source-map all-opts)]
                           (assert (or (nil? (:output-to all-opts)) (:modules opts) (string? fname))
                             (str ":source-map must name a file when using :whitespace, "
                                  ":simple, or :advanced optimizations with :output-to"))
                           (doall (map #(source-on-disk all-opts %) js-sources)))
                         (if (:modules all-opts)
                           (->>
                             (apply optimize-modules all-opts js-sources)
                             (output-modules all-opts js-sources))
                           (let [fdeps-str (foreign-deps-str all-opts
                                             (filter foreign-source? js-sources))
                                 all-opts  (assoc all-opts
                                             :foreign-deps-line-count
                                             (- (count (.split #"\r?\n" fdeps-str -1)) 1))]
                             (->>
                               (util/measure compiler-stats
                                 (str "Optimizing " (count js-sources) " sources")
                                 (apply optimize all-opts
                                   (remove foreign-source? js-sources)))
                               (add-wrapper all-opts)
                               (add-source-map-link all-opts)
                               (str fdeps-str)
                               (add-header all-opts)
                               (output-one-file all-opts)))))
                       (apply output-unoptimized all-opts js-sources))]
             ;; emit Node.js bootstrap script for :none & :whitespace optimizations
             (when (and (= (:target opts) :nodejs)
                        (not= (:optimizations opts) :whitespace))
               (let [outfile (io/file (util/output-directory opts)
                               "goog" "bootstrap" "nodejs.js")]
                 (util/mkdirs outfile)
                 (spit outfile (slurp (io/resource "cljs/bootstrap_node.js")))))
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
  )

(defn watch
  "Given a source directory, produce runnable JavaScript. Watch the source
   directory for changes rebuliding when necessary. Takes the same arguments as
   cljs.closure/build in addition to :watch-fn, a function of no arguments to
   run after a successful build."
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
                  (let [start (System/nanoTime)]
                    (build source opts compiler-env)
                    (println "... done. Elapsed"
                      (/ (unchecked-subtract (System/nanoTime) start) 1e9) "seconds")
                    (flush))
                  (when-let [f (:watch-fn opts)]
                    (f))
                  (catch Throwable e
                    (binding [*out* *err*]
                      (println (Throwables/getStackTraceAsString e))))))
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
            (let [key (. srvc (poll 300 TimeUnit/MILLISECONDS))]
              (when (and key
                         (some
                           (fn [^WatchEvent e]
                             (let [fstr (.. e context toString)]
                               (and (or (. fstr (endsWith "cljc"))
                                        (. fstr (endsWith "cljs"))
                                        (. fstr (endsWith "js")))
                                    (not (. fstr (startsWith ".#"))))))
                           (seq (.pollEvents key))))
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
  ([src] (src-file->target-file src nil))
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
              (IllegalArgumentException.
                (str "Can't create goog.require expression for " src))))]
      (if (and (not all-provides) wrap)
        (if (:reload options)
          (str "goog.require(\"" goog-ns "\", true);")
          (str "goog.require(\"" goog-ns "\");"))
        (if (vector? goog-ns)
          goog-ns
          (str goog-ns))))))

(defn aot-cache-core []
  (let [base-path (io/file "src" "main" "cljs" "cljs")
        src       (io/file base-path "core.cljs")
        dest      (io/file base-path "core.aot.js")
        cache     (io/file base-path "core.cljs.cache.aot.edn")
        tcache    (io/file base-path "core.cljs.cache.aot.json")]
    (util/mkdirs dest)
    (env/with-compiler-env (env/default-compiler-env)
      (comp/compile-file src dest
        {:source-map true
         :source-map-url "core.js.map"
         :output-dir (str "src" File/separator "main" File/separator "cljs")})
      (ana/write-analysis-cache 'cljs.core cache src)
      (ana/write-analysis-cache 'cljs.core tcache src))))

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
