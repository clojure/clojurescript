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

   build = compile -> add-dependencies -> optimize -> output

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
            [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.source-map :as sm]
            [cljs.env :as env]
            [cljs.js-deps :as deps]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json])
  (:import [java.io File BufferedInputStream StringWriter]
           [java.net URL]
           [java.util.logging Level]
           [java.util List Random]
           [com.google.javascript.jscomp CompilerOptions CompilationLevel
              CompilerOptions$LanguageMode SourceMap$Format
              SourceMap$DetailLevel ClosureCodingConvention SourceFile
              Result JSError CheckLevel DiagnosticGroups
              CommandLineRunner AnonymousFunctionNamingPolicy
              JSModule JSModuleGraph SourceMap]
           [java.security MessageDigest]
           [javax.xml.bind DatatypeConverter]
           [java.nio.file Path Paths Files StandardWatchEventKinds WatchKey
                          WatchEvent FileVisitor FileVisitResult]
           [com.sun.nio.file SensitivityWatchEventModifier]))

(def name-chars (map char (concat (range 48 57) (range 65 90) (range 97 122))))

(defn random-char []
  (nth name-chars (.nextInt (Random.) (count name-chars))))

(defn random-string [length]
  (apply str (take length (repeatedly random-char))))

;; Closure API
;; ===========

(defmulti js-source-file (fn [_ source] (class source)))

(defmethod js-source-file String [^String name ^String source]
  (SourceFile/fromCode name source))

(defmethod js-source-file File [_ ^File source]
  (SourceFile/fromFile source))

(defmethod js-source-file BufferedInputStream [^String name ^BufferedInputStream source]
  (SourceFile/fromInputStream name source))

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
      :ecmascript5        (.setLanguageIn compiler-options CompilerOptions$LanguageMode/ECMASCRIPT5)
      :ecmascript5-strict (.setLanguageIn compiler-options CompilerOptions$LanguageMode/ECMASCRIPT5_STRICT)
      :ecmascript3        (.setLanguageIn compiler-options CompilerOptions$LanguageMode/ECMASCRIPT3)))

  (when (contains? opts :language-out)
    (case (:language-out opts)
      :ecmascript5        (.setLanguageOut compiler-options CompilerOptions$LanguageMode/ECMASCRIPT5)
      :ecmascript5-strict (.setLanguageOut compiler-options CompilerOptions$LanguageMode/ECMASCRIPT5_STRICT)
      :ecmascript3        (.setLanguageOut compiler-options CompilerOptions$LanguageMode/ECMASCRIPT3)))

  (when (contains? opts :print-input-delimiter)
    (set! (.printInputDelimiter compiler-options)
      (:print-input-delimiter opts))))

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
    (doseq [[type level] (:closure-warnings opts)]
      (. compiler-options
        (setWarningLevel (type warning-types) (level check-level))))
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
  (let [filter-cp-js (fn [paths]
                       (for [p paths u (deps/find-js-classpath p)] u))
        filter-js (fn [paths]
                    (for [p paths u (deps/find-js-resources p)] u))
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
  (-url [this] nil)
  (-provides [this] (:provides (deps/parse-js-ns (string/split-lines this))))
  (-requires [this] (:requires (deps/parse-js-ns (string/split-lines this))))
  (-source [this] this)
  
  clojure.lang.IPersistentMap
  (-foreign? [this] (:foreign this))
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
  (javascript-file
    (:foreign m)
    (when-let [f (:file m)]
      (deps/to-url f))
    (when-let [sf (:source-file m)]
      (deps/to-url sf))
    (:provides m)
    (:requires m)
    (:lines m)
    (:source-map m)))

(defn read-js
  "Read a JavaScript file returning a map of file information."
  [f]
  (let [source (slurp f)
        m (deps/parse-js-ns (string/split-lines source))]
    (map->javascript-file (assoc m :file f))))


;; Compile
;; =======

(defprotocol Compilable
  (-compile [this opts] "Returns one or more IJavaScripts."))

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
      (binding [ana/*cljs-file* (.getPath ^File file)]
        (compile-form-seq (ana/forms-seq file)))))

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
  [url out-dir]
  (let [out-file (io/file out-dir (path-from-jarfile url))
        content  (with-open [reader (io/reader url)]
                   (slurp reader))]
    (util/mkdirs out-file)
    (spit out-file content)
    out-file))

;; TODO: it would be nice if we could consolidate requires-compilation?
;; logic - David
(defn compile-from-jar
  "Compile a file from a jar."
  [this {:keys [output-file] :as opts}]
  (or (when output-file
        (let [out-file (io/file (util/output-directory opts) output-file)]
          (when (and (.exists out-file)
                     (= (util/compiled-by-version out-file)
                        (util/clojurescript-version)))
            (compile-file
              (io/file (util/output-directory opts)
                (last (string/split (.getPath ^URL this) #"\.jar!/")))
              opts))))
      (let [file-on-disk (jar-file-to-disk this (util/output-directory opts))]
        (-compile file-on-disk opts))))

(extend-protocol Compilable

  File
  (-compile [this opts]
    (if (.isDirectory this)
      (compile-dir this opts)
      (compile-file this opts)))

  URL
  (-compile [this opts]
    (case (.getProtocol this)
      "file" (-compile (io/file this) opts)
      "jar" (compile-from-jar this opts)))
  
  clojure.lang.PersistentList
  (-compile [this opts]
    (compile-form-seq [this]))
  
  String
  (-compile [this opts] (-compile (io/file this) opts))
  
  clojure.lang.PersistentVector
  (-compile [this opts] (compile-form-seq this))
  )

(comment
  ;; compile a file in memory
  (-compile "samples/hello/src/hello/core.cljs" {})
  ;; compile a file to disk - see file @ 'out/clojure/set.js'
  (-compile (io/resource "clojure/set.cljs") {:output-file "clojure/set.js"})
  ;; compile a project
  (-compile (io/file "samples/hello/src") {})
  ;; compile a project with a custom output directory
  (-compile (io/file "samples/hello/src") {:output-dir "my-output"})
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

(defn get-compiled-cljs
  "Return an IJavaScript for this file. Compiled output will be
   written to the working directory."
  [opts {:keys [relative-path uri]}]
  (let [js-file (comp/rename-to-js relative-path)]
    (-compile uri (merge opts {:output-file js-file}))))

(defn cljs-source-for-namespace
  "Returns a map containing :relative-path, :uri referring to the resource that
should contain the source for the given namespace name."
  [ns]
  (as-> (munge ns) %
    (string/replace % \. \/)
    (str % ".cljs")
    {:relative-path % :uri (io/resource %)}))

(defn source-for-namespace
  [ns compiler-env]
  (let [ns-str  (str (comp/munge ns))
        path    (string/replace ns-str \. \/)
        relpath (str path ".cljs")]
    (if-let [cljs-res (io/resource relpath)]
      {:relative-path relpath :uri cljs-res}
      (let [relpath (:file (get-in @compiler-env [:js-dependency-index ns-str]))]
        (if-let [js-res (io/resource relpath)]
          {:relative-path relpath :uri js-res}
         (throw
           (IllegalArgumentException. (str "Namespace " ns " does not exist"))))))))

(defn cljs-dependencies
  "Given a list of all required namespaces, return a list of
  IJavaScripts which are the cljs dependencies. The returned list will
  not only include the explicitly required files but any transitive
  dependencies as well. JavaScript files will be compiled to the
  working directory if they do not already exist.

  Only load dependencies from the classpath."
  [opts requires]
  (let [cljs-deps (fn [lib-names]
                    (->> (remove #(or ((@env/*compiler* :js-dependency-index) %)
                                    (deps/find-classpath-lib %))
                           lib-names)
                      (map cljs-source-for-namespace)
                      (remove (comp nil? :uri))))]
    (loop [required-files (cljs-deps requires)
           visited (set required-files)
           js-deps #{}]
      (if (seq required-files)
        (let [next-file (first required-files)
              js (get-compiled-cljs opts next-file)
              new-req (remove #(contains? visited %) (cljs-deps (deps/-requires js)))]
          (recur (into (rest required-files) new-req)
                 (into visited new-req)
                 (conj js-deps js)))
        (remove nil? js-deps)))))

(comment
  ;; only get cljs deps
  (cljs-dependencies {} ["goog.string" "cljs.core"])
  ;; get transitive deps
  (cljs-dependencies {} ["clojure.string"])
  ;; don't get cljs.core twice
  (cljs-dependencies {} ["cljs.core" "clojure.string"])
  )

(defn add-dependencies
  "Given one or more IJavaScript objects in dependency order, produce
  a new sequence of IJavaScript objects which includes the input list
  plus all dependencies in dependency order."
  [opts & inputs]
  (let [requires      (mapcat deps/-requires inputs)
        required-cljs (remove (set inputs) (cljs-dependencies opts requires))
        required-js   (js-dependencies opts (set (concat (mapcat deps/-requires required-cljs) requires)))
        provided      (mapcat deps/-provides (concat inputs required-cljs required-js))
        unprovided    (clojure.set/difference (set requires) (set provided) #{"constants-table"})]
    (when (seq unprovided)
      (ana/warning :unprovided @env/*compiler* {:unprovided (sort unprovided)}))
    (cons
      (javascript-file nil (deps/goog-resource "goog/base.js") ["goog"] nil)
      (deps/dependency-order
        (concat
          (map
            (fn [{:keys [foreign url file provides requires] :as js-map}]
              (let [url (or url (io/resource file))]
                (merge
                  (javascript-file foreign url provides requires)
                  js-map)))
            required-js)
          [(when (-> @env/*compiler* :opts :emit-constants)
             (let [url (deps/to-url (str (util/output-directory opts) "/constants_table.js"))]
               (javascript-file nil url url ["constants-table"] ["cljs.core"] nil nil)))]
          required-cljs
          inputs)))))

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
           (fnil conj #{}) :cljs-base)
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
                (if-not (deps/-foreign? ijs)
                  (.add js-module
                    ^SourceFile (js-source-file (javascript-name ijs) ijs))
                  (swap! foreign-deps conj ijs)))
              ;; add module dependencies, will always work
              ;; since modules are already in dependency order
              (doseq [dep depends-on]
                (if-let [parent-module (get-in (into {} ret) [dep :closure-module])]
                  (.addDependency js-module ^JSModule parent-module)
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
    ;; add anything left to :cljs-base module
    (doseq [source sources']
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
                  (util/ns->relpath (first provides)))
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
                (- (count (.split #"\r?\n" (make-preamble opts) -1)) 1)))))
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
        input-path (util/path-seq (.getCanonicalPath (io/file ^URL (deps/-url input))))
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
    (spit output-to js)

    :else (println js)))

(defn output-deps-file [opts sources]
  (output-one-file opts (deps-file opts sources)))

(defn output-main-file [opts]
  (let [asset-path (or (:asset-path opts)
                       (util/output-directory opts))]
    (case (:target opts)
      :nodejs
      (output-one-file opts
        (str "var path = require(\"path\");\n"
             "try {\n"
             "    require(\"source-map-support\").install();\n"
             "} catch(err) {\n"
             "}\n"
             "require(path.join(path.resolve(\".\"),\"" asset-path "\",\"goog\",\"bootstrap\",\"nodejs.js\"));\n"
             "require(path.join(path.resolve(\".\"),\"" asset-path "\",\"cljs_deps.js\"));\n"
             "goog.require(\"" (comp/munge (:main opts)) "\");\n"
             "goog.require(\"cljs.nodejscli\");\n"))
      (output-one-file opts
        (str "if(typeof goog == \"undefined\") document.write('<script src=\"" asset-path "/goog/base.js\"></script>');\n"
             "document.write('<script src=\"" asset-path "/cljs_deps.js\"></script>');\n"
             "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"" (comp/munge (:main opts))
             "\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n")))))

(declare foreign-deps-str add-header add-source-map-link)

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
                    (str output-to ".map"))]
      (spit (io/file output-to)
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
                 (- (count (.split #"\r?\n" (make-preamble opts) -1)) 1)
                 0)
               :foreign-deps-line-count
               (if fdeps-str
                 (- (count (.split #"\r?\n" fdeps-str -1)) 1)
                 0)})))))))

(defn ^String rel-output-path
  "Given an IJavaScript which is either in memory, in a jar file,
  or is a foreign lib, return the path relative to the output
  directory."
  [js]
  (let [url (deps/-url js)]
    (cond
      url
      (if (deps/-foreign? js)
        (util/get-name url)
        (path-from-jarfile url))

      (string? js)
      (let [digest (MessageDigest/getInstance "SHA-1")]
        (.reset digest)
        (.update digest (.getBytes ^String js "utf8"))
        (str
          (->> (DatatypeConverter/printHexBinary (.digest digest))
            (take 7)
            (apply str))
          ".js"))

      :else (str (random-string 5) ".js"))))

(defn write-javascript
  "Write or copy a JavaScript file to output directory. Only write if the file
  does not already exist. Return IJavaScript for the file on disk at the new
  location."
  [opts js]
  (let [out-dir  (io/file (util/output-directory opts))
        out-name (rel-output-path js)
        out-file (io/file out-dir out-name)
        ijs      {:url      (deps/to-url out-file)
                  :requires (deps/-requires js)
                  :provides (deps/-provides js)
                  :group    (:group js)}]
    (when-not (.exists out-file)
      (util/mkdirs out-file)
      (spit out-file (deps/-source js)))
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
        (deps/-foreign? js))))

(defn source-on-disk
  "Ensure that the given IJavaScript exists on disk in the output directory.
  Return updated IJavaScript with the new location if necessary."
  [opts js]
  (if (write-js? js)
    (write-javascript opts js)
    ;; always copy original ClojureScript sources to the output directory
    ;; when source maps enabled
    (let [out-file (if-let [ns (and (:source-map opts) (first (:provides js)))]
                     (io/file (io/file (util/output-directory opts))
                       (util/ns->relpath ns)))
          source-url (:source-url js)]
      (when (and out-file source-url
                 (or (not (.exists ^File out-file))
                     (> (.lastModified (io/file source-url))
                        (.lastModified out-file))))
        (spit out-file (slurp source-url)))
      js)))

(comment
  (write-javascript {} "goog.provide('demo');\nalert('hello');\n")
  ;; write something from a jar file to disk
  (source-on-disk {}
                  {:url (deps/goog-resource "goog/base.js")
                   :source (with-open [reader (io/reader (deps/goog-resource "goog/base.js"))]
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
  by libraries on the classpath. Should be run in the main thread. If
  not, pass (java.lang.ClassLoader/getSystemClassLoader) to use the
  system classloader."
  ([]
   (get-upstream-deps* (. (Thread/currentThread) (getContextClassLoader))))
  ([classloader]
   (let [upstream-deps (map #(read-string (slurp %)) (enumeration-seq (. classloader (getResources "deps.cljs"))))]
     #_(doseq [dep upstream-deps]
         (println (str "Upstream deps.cljs found on classpath. " dep " This is an EXPERIMENTAL FEATURE and is not guarenteed to remain stable in future versions.")))
     (apply merge-with concat upstream-deps))))

(def get-upstream-deps (memoize get-upstream-deps*))

(defn add-header [opts js]
  (str (make-preamble opts) js))

(defn foreign-deps-str [opts sources]
  (letfn [(to-js-str [ijs]
            (let [url (or (and (= (:optimizations opts) :advanced)
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

(defn check-source-map [{:keys [output-to source-map output-dir] :as opts}]
  "When :source-map is specified in opts, "
  (when (and (contains? opts :source-map)
             (not (= (:optimizations opts) :none)))
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

(defn add-implicit-options [opts]
  (let [{:keys [libs foreign-libs externs]} (get-upstream-deps)]
    (cond->
      (-> opts
        (assoc
          :ups-libs libs
          :ups-foreign-libs foreign-libs
          :ups-externs externs)
        (update-in [:preamble] #(into (or % []) ["cljs/imul.js"])))
      (:target opts)
      (assoc-in [:closure-defines (str (comp/munge 'cljs.core/*target*))]
        (name (:target opts)))
      (nil? (:optimizations opts))
      (assoc :optimizations :none))))

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
             all-opts (add-implicit-options opts)
             emit-constants (or (and (= (:optimizations opts) :advanced)
                                     (not (false? (:optimize-constants opts))))
                                (:optimize-constants opts))]
         (check-output-to opts)
         (check-output-dir opts)
         (check-source-map opts)
         (check-source-map-path opts)
         (check-output-wrapper opts)
         (check-node-target opts)
         (swap! compiler-env
           #(-> %
             (assoc-in [:opts :emit-constants] emit-constants)
             (assoc :target (:target opts))
             (assoc :js-dependency-index (deps/js-dependency-index all-opts))))
         (binding [comp/*dependents* (when-not (false? (:recompile-dependents opts))
                                       (atom {:recompile #{} :visited #{}}))
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
           (let [compiled (util/measure compiler-stats
                            "Compile basic sources"
                            (doall (-compile source all-opts)))
                 ;; the constants_table.js file is not used directly here, is picked up by
                 ;; add-dependencies below
                 _ (when emit-constants
                     (comp/emit-constants-table-to-file
                       (::ana/constant-table @env/*compiler*)
                       (str (util/output-directory all-opts) "/constants_table.js")))
                 js-sources (util/measure compiler-stats
                              "Add dependencies"
                              (doall
                                (concat
                                 (apply add-dependencies all-opts
                                   (concat
                                     (if (coll? compiled) compiled [compiled])
                                     (when (= :nodejs (:target all-opts))
                                       [(-compile (io/resource "cljs/nodejs.cljs") all-opts)])))
                                 (when (= :nodejs (:target all-opts))
                                   [(-compile (io/resource "cljs/nodejscli.cljs") all-opts)]))))
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
                                 "Optimize sources"
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
                        (#{:none :whitespace} (:optimizations opts)))
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
   cljs.closure/build."
  ([source opts]
    (watch source opts
      (if-not (nil? env/*compiler*)
        env/*compiler*
        (env/default-compiler-env opts))))
  ([source opts compiler-env]
    (let [path (Paths/get (.toURI (io/file source)))
          fs (.getFileSystem path)
          service (.newWatchService fs)]
      (letfn [(buildf []
                (let [start (System/nanoTime)]
                  (build source opts compiler-env)
                  (println "... done. Elapsed"
                    (/ (unchecked-subtract (System/nanoTime) start) 1e9) "seconds")
                  (flush)))
              (watch-all [^Path root]
                (Files/walkFileTree root
                  (reify
                    FileVisitor
                    (preVisitDirectory [_ dir _]
                      (let [^Path dir dir]
                        (. dir
                          (register service
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
        (println "Building...")
        (flush)
        (buildf)
        (println "Watching path:" source)
        (watch-all path)
        (loop [key nil]
          (when (or (nil? key) (. ^WatchKey key reset))
            (let [key (. service take)]
              (when (some (fn [^WatchEvent e]
                            (let [fstr (.. e context toString)]
                              (and (or (. fstr (endsWith "cljs"))
                                       (. fstr (endsWith "js")))
                                   (not (. fstr (startsWith ".#"))))))
                      (seq (.pollEvents key)))
                (println "Change detected, recompiling...")
                (flush)
                (try
                  (buildf)
                  (catch Exception e
                    (.printStackTrace e))))
              (recur key))))))))

(comment
  (watch "samples/hello/src"
    {:optimizations :none
     :output-to "samples/hello/out/hello.js"
     :output-dir "samples/hello/out"
     :cache-analysis true
     :source-map true
     :verbose true})
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
  ([src {:keys [wrap all-provides :as options]}]
    (let [goog-ns
          (case (util/ext src)
            "cljs" (comp/munge (:ns (ana/parse-ns src)))
            "js"   (cond-> (:provides (parse-js-ns src))
                     (not all-provides) first)
            (throw
              (IllegalArgumentException.
                (str "Can't create goog.require expression for " src))))]
      (if (and (not all-provides) wrap)
        (str "goog.require(\"" goog-ns "\");")
        (if (vector? goog-ns)
          goog-ns
          (str goog-ns))))))

(comment

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
