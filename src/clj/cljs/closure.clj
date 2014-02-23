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
  (:require [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.source-map :as sm]
            [cljs.env :as env]
            [cljs.js-deps :as deps]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json])
  (:import java.io.File
           java.io.BufferedInputStream
           java.net.URL
           java.util.logging.Level
           java.util.jar.JarFile
           java.util.List
           com.google.common.collect.ImmutableList
           com.google.javascript.jscomp.CompilerOptions
           com.google.javascript.jscomp.CompilationLevel
           com.google.javascript.jscomp.SourceMap$Format
           com.google.javascript.jscomp.SourceMap$DetailLevel
           com.google.javascript.jscomp.ClosureCodingConvention
           com.google.javascript.jscomp.JSSourceFile
           com.google.javascript.jscomp.Result
           com.google.javascript.jscomp.JSError
           com.google.javascript.jscomp.CheckLevel
           com.google.javascript.jscomp.DiagnosticGroups
           com.google.javascript.jscomp.CommandLineRunner))

(defmacro ^:private debug-prn
  [& args]
  `(.println System/err (str ~@args)))

(def name-chars (map char (concat (range 48 57) (range 65 90) (range 97 122))))

(defn random-char []
  (nth name-chars (.nextInt (java.util.Random.) (count name-chars))))

(defn random-string [length]
  (apply str (take length (repeatedly random-char))))

;; Closure API
;; ===========

(defmulti js-source-file (fn [_ source] (class source)))

(defmethod js-source-file String [^String name ^String source]
  (JSSourceFile/fromCode name source))

(defmethod js-source-file File [_ ^File source]
  (JSSourceFile/fromFile source))

(defmethod js-source-file BufferedInputStream [^String name ^BufferedInputStream source]
  (JSSourceFile/fromInputStream name source))

(defn set-options
  "TODO: Add any other options that we would like to support."
  [opts ^CompilerOptions compiler-options]
  (when (contains? opts :pretty-print)
    (set! (.prettyPrint compiler-options) (:pretty-print opts)))
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
    (when (contains? opts :source-map)
      (set! (.sourceMapOutputPath compiler-options)
            (:source-map opts))
      (set! (.sourceMapDetailLevel compiler-options)
            SourceMap$DetailLevel/ALL)
      (set! (.sourceMapFormat compiler-options)
            SourceMap$Format/V3))
    (do (.setOptionsForCompilationLevel level compiler-options)
        (set-options opts compiler-options)
        compiler-options)))

(defn load-externs
  "Externs are JavaScript files which contain empty definitions of
  functions which will be provided by the envorinment. Any function in
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
                     (if (= :nodejs target)
                       (cons (io/resource "cljs/nodejs_externs.js")
                             (or ext []))
                       ext))
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
    (do (com.google.javascript.jscomp.Compiler/setLoggingLevel Level/WARNING)
        compiler)))

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
  (comp/with-core-cljs
    (with-out-str
      (binding [ana/*cljs-ns* 'cljs.user]
        (doseq [form forms]
          (comp/emit (ana/analyze (ana/empty-env) form)))))))

(defn output-directory [opts]
  (or (:output-dir opts) "out"))

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
    (do (swap! env/*compiler* update-in [::compiled-cljs] assoc path js)
        js)))

(defn compile-file
  "Compile a single cljs file. If no output-file is specified, returns
  a string of compiled JavaScript. With an output-file option, the
  compiled JavaScript will written to this location and the function
  returns a JavaScriptFile. In either case the return value satisfies
  IJavaScript."
  [^File file {:keys [output-file] :as opts}]
    (if output-file
      (let [out-file (io/file (output-directory opts) output-file)]
        (compiled-file (comp/compile-file file out-file opts)))
      (binding [ana/*cljs-file* (.getPath ^java.io.File file)]
        (compile-form-seq (ana/forms-seq file)))))

(defn compile-dir
  "Recursively compile all cljs files under the given source
  directory. Return a list of JavaScriptFiles."
  [^File src-dir opts]
  (let [out-dir (output-directory opts)]
    (map compiled-file
         (comp/compile-root src-dir out-dir opts))))

(defn path-from-jarfile
  "Given the URL of a file within a jar, return the path of the file
  from the root of the jar."
  [^URL url]
  (last (string/split (.getFile url) #"\.jar!/")))

(defn jar-file-to-disk
  "Copy a file contained within a jar to disk. Return the created file."
  [url out-dir]
  (let [out-file (io/file out-dir (path-from-jarfile url))
        content (with-open [reader (io/reader url)]
                  (slurp reader))]
    (do (comp/mkdirs out-file)
        (spit out-file content)
        out-file)))

;; TODO: it would be nice if we could consolidate requires-compilation?
;; logic - David
(defn compile-from-jar
  "Compile a file from a jar."
  [this {:keys [output-file] :as opts}]
  (or (when output-file
        (let [out-file (io/file (output-directory opts) output-file)]
          (when (and (.exists out-file)
                     (= (comp/compiled-by-version out-file)
                        (comp/clojurescript-version)))
            (compile-file
              (io/file (output-directory opts)
                (last (string/split (.getPath ^URL this) #"\.jar!/")))
              opts))))
      (let [file-on-disk (jar-file-to-disk this (output-directory opts))]
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
      (let [node (get (@env/*compiler* :js-dependency-index) (first requires))
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

(defn cljs-dependencies
  "Given a list of all required namespaces, return a list of
  IJavaScripts which are the cljs dependencies. The returned list will
  not only include the explicitly required files but any transitive
  depedencies as well. JavaScript files will be compiled to the
  working directory if they do not already exist.

  Only load dependencies from the classpath."
  [opts requires]
  (letfn [(ns->cp [s] (str (string/replace (munge s) \. \/) ".cljs"))
          (cljs-deps [coll]
                     (->> coll
                          (remove (@env/*compiler* :js-dependency-index))
                          (map #(let [f (ns->cp %)] (hash-map :relative-path f :uri (io/resource f))))
                          (remove #(nil? (:uri %)))))]
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
  (let [requires (mapcat deps/-requires inputs)
        required-cljs (remove (set inputs) (cljs-dependencies opts requires))
        required-js (js-dependencies opts (set (concat (mapcat deps/-requires required-cljs) requires)))
        provided (mapcat deps/-provides (concat inputs required-cljs required-js))
        unprovided (clojure.set/difference (set requires) (set provided) #{"constants-table"})]
    (when (seq unprovided)
      (ana/warning :unprovided @env/*compiler* {:unprovided (sort unprovided)}))
    (cons (javascript-file nil (io/resource "goog/base.js") ["goog"] nil)
          (deps/dependency-order
           (concat (map #(-> (javascript-file (:foreign %)
                                              (or (:url %) (io/resource (:file %)))
                                              (:provides %)
                                              (:requires %))
                             (assoc :group (:group %))) required-js)
                   [(when (-> @env/*compiler* :opts :emit-constants)
                      (let [url (deps/to-url (str (output-directory opts) "/constants_table.js"))]
                        (javascript-file nil url url ["constants-table"] ["cljs.core"] nil nil)))]
                   required-cljs
                   inputs)))))

(defn preamble-from-paths [paths]
  (str (apply str (map #(slurp (io/resource %)) paths)) "\n"))

(defn make-preamble [{:keys [target preamble hashbang]}]
  (str (when (= :nodejs target)
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
             (js-source-file (javascript-name url)
                             (if (deps/-foreign? js)
                               (str (build-provides (deps/-provides js)) (slurp url))
                               (io/input-stream url)))))

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
        result ^Result (.compile closure-compiler externs inputs compiler-options)
        preamble (make-preamble opts)
        preamble-line-count (- (count (.split #"\r?\n" preamble -1)) 1)]
    (if (.success result)
      ;; compiler.getSourceMap().reset()
      (let [source (.toSource closure-compiler)]
        (when-let [name (:source-map opts)]
          (with-open [out (io/writer name)]
            (.appendTo (.getSourceMap closure-compiler) out name))
          (let [sm-json (-> (io/file name) slurp
                            (json/read-str :key-fn keyword))
                closure-source-map (sm/decode sm-json)]
            (loop [sources  (seq sources)
                   relpaths {}
                   merged   (sorted-map-by
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
                        (assoc relpaths (.getPath ^URL source-url)
                          (ana/ns->relpath (first provides)))
                        relpaths))
                    (if-let [url (:url source)]
                      (let [path (.getPath ^URL url)]
                        (if-let [compiled (get-in @env/*compiler* [::comp/compiled-cljs path])]
                          (if-let [source-url (:source-url source)]
                            (assoc merged (.getPath ^URL source-url)
                              (sm/merge-source-maps
                                (:source-map compiled)
                                (get closure-source-map path)))
                            merged)
                          (assoc merged path (get closure-source-map path))))
                      merged)))
                (spit (io/file name)
                      (sm/encode merged
                                 {:preamble-line-count preamble-line-count
                                  :lines (+ (:lineCount sm-json) preamble-line-count 2)
                                  :file (:file sm-json)
                                  :output-dir (output-directory opts)
                                  :source-map-path (:source-map-path opts)
                                  :source-map (:source-map opts)
                                  :relpaths relpaths}))))))
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

(defn path-relative-to
  "Generate a string which is the path to input relative to base."
  [^File base input]
  (let [base-path (comp/path-seq (.getCanonicalPath base))
        input-path (comp/path-seq (.getCanonicalPath (io/file ^URL (deps/-url input))))
        count-base (count base-path)
        common (count (take-while true? (map #(= %1 %2) base-path input-path)))
        prefix (repeat (- count-base common 1) "..")]
    (if (= count-base common)
      (last input-path) ;; same file
      (comp/to-path (concat prefix (drop common input-path)) "/"))))

(defn add-dep-string
  "Return a goog.addDependency string for an input."
  [opts input]
  (letfn [(ns-list [coll] (when (seq coll) (apply str (interpose ", " (map #(str "'" (comp/munge %) "'") coll)))))]
    (str "goog.addDependency(\""
         (path-relative-to (io/file (output-directory opts) "goog/base.js") input)
         "\", ["
         (ns-list (deps/-provides input))
         "], ["
         (ns-list (deps/-requires input))
         "]);")))

(defn deps-file
  "Return a deps file string for a sequence of inputs."
  [opts sources]
  (apply str (interpose "\n" (map #(add-dep-string opts %) sources))))

(comment
  (path-relative-to (io/file "out/goog/base.js") {:url (deps/to-url "out/cljs/core.js")})
  (add-dep-string {} {:url (deps/to-url "out/cljs/core.js") :requires ["goog.string"] :provides ["cljs.core"]})
  (deps-file {} [{:url (deps/to-url "out/cljs/core.js") :requires ["goog.string"] :provides ["cljs.core"]}])
  )

(defn output-one-file [{:keys [output-to]} js]
  (cond (nil? output-to) js
        (string? output-to) (spit output-to js)
        :else (println js)))

(defn output-deps-file [opts sources]
  (output-one-file opts (deps-file opts sources)))

(defn ^String output-path
  "Given an IJavaScript which is either in memory or in a jar file,
  return the output path for this file relative to the working
  directory."
  [js]
  (if-let [url ^URL (deps/-url js)]
    (path-from-jarfile url)
    (str (random-string 5) ".js")))


(defn write-javascript
  "Write a JavaScript file to disk. Only write if the file does not
  already exist. Return IJavaScript for the file on disk."
  [opts js]
  (let [out-dir (io/file (output-directory opts))
        out-name (output-path js)
        out-file (io/file out-dir out-name)]
    (do (when-not (.exists out-file)
          (do (comp/mkdirs out-file)
              (spit out-file (deps/-source js))))
        {:url (deps/to-url out-file) :requires (deps/-requires js)
         :provides (deps/-provides js) :group (:group js)})))

(defn source-on-disk
  "Ensure that the given JavaScript exists on disk. Write in memory
  sources and files contained in jars to the working directory. Return
  updated IJavaScript with the new location."
  [opts js]
  (let [url ^URL (deps/-url js)]
    (if (or (not url)
            (= (.getProtocol url) "jar"))
      (write-javascript opts js)
      ;; always copy original sources to the output directory
      ;; when source maps enabled
      (let [out-file (if-let [ns (and (:source-map opts)
                                      (first (:provides js)))]
                       (io/file (io/file (output-directory opts))
                         (ana/ns->relpath ns)))
            source-url (:source-url js)]
        (when (and out-file source-url
                   (or (not (.exists ^File out-file))
                       (> (.lastModified (io/file source-url))
                          (.lastModified out-file))))
          (spit out-file (slurp source-url)))
        js))))

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
  (let [disk-sources (map #(source-on-disk opts %) sources)]
    (let [goog-deps (io/file (output-directory opts) "goog/deps.js")]
      (do (comp/mkdirs goog-deps)
          (spit goog-deps (deps-file opts (filter #(= (:group %) :goog) disk-sources)))
          (output-deps-file opts (remove #(= (:group %) :goog) disk-sources))))))

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
  "returns a merged map containing all upstream dependencies defined by libraries on the classpath"
  []
  (let [classloader (. (Thread/currentThread) (getContextClassLoader))
        upstream-deps (map #(read-string (slurp %)) (enumeration-seq (. classloader (findResources "deps.cljs"))))]
    (doseq [dep upstream-deps]
      (println (str "Upstream deps.cljs found on classpath. " dep " This is an EXPERIMENTAL FEATURE and is not guarenteed to remain stable in future versions.")))
    (apply merge-with concat upstream-deps)))

(def get-upstream-deps (memoize get-upstream-deps*))

(defn add-header [opts js]
  (str (make-preamble opts) js))

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
    (assert (and (contains? opts :output-to)
                 (contains? opts :output-dir))
      ":source-map cannot be specied without also specifying :output-to and :output-dir if optimization setting applied")
    (assert (string? source-map)
      (format ":source-map %s must specify a file in the same directory as :output-to %s if optimization setting applied"
        (pr-str source-map)
        (pr-str output-to)))
    (assert (in-same-dir? source-map output-to)
      (format ":source-map %s must specify a file in the same directory as :output-to %s if optimization setting applied"
        (pr-str source-map)
        (pr-str output-to)))
    (assert (same-or-subdirectory-of? (absolute-parent output-to) output-dir)
      (format ":output-dir %s must specify a directory in :output-to's parent %s if optimization setting applied"
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
        ":source-map-path cannot be specified without also specifying :output-to and :source-map if optimization setting applied")))
  true)

(defn build
  "Given a source which can be compiled, produce runnable JavaScript."
  ([source opts]
    (build source opts
      (if-not (nil? env/*compiler*)
        env/*compiler*
        (env/default-compiler-env opts))))
  ([source opts compiler-env]
     (env/with-compiler-env compiler-env
       (let [opts (if (= :nodejs (:target opts))
                    (merge {:optimizations :simple} opts)
                    opts)
             ups-deps (get-upstream-deps)
             all-opts (assoc opts 
                        :ups-libs (:libs ups-deps)
                        :ups-foreign-libs (:foreign-libs ups-deps)
                        :ups-externs (:externs ups-deps))
             emit-constants (or (and (= (:optimizations opts) :advanced)
                                     (not (false? (:optimize-constants opts))))
                                (:optimize-constants opts))]
         (check-output-to opts)
         (check-output-dir opts)
         (check-source-map opts)
         (check-source-map-path opts)
         (swap! compiler-env assoc-in [:opts :emit-constants] emit-constants)
         (binding [ana/*cljs-static-fns*
                   (or (and (= (:optimizations opts) :advanced)
                            (not (false? (:static-fns opts))))
                       (:static-fns opts)
                       ana/*cljs-static-fns*)
                   *assert* (not= (:elide-asserts opts) true)
                   ana/*cljs-warnings*
                   (let [enabled? (true? (opts :warnings true))]
                     (merge ana/*cljs-warnings*
                            {:unprovided enabled?
                             :undeclared-var enabled?
                             :undeclared-ns enabled?
                             :undeclared-ns-form enabled?}))]
           (let [compiled (-compile source all-opts)

                 ; the constants_table.js file is not used directly here, is picked up by
                 ; add-dependencies below
                 _ (when emit-constants
                     (comp/emit-constants-table-to-file (::ana/constant-table @env/*compiler*)
                                                        (str (output-directory all-opts) "/constants_table.js")))
                 js-sources (concat
                             (apply add-dependencies all-opts
                                    (concat (if (coll? compiled) compiled [compiled])
                                            (when (= :nodejs (:target all-opts))
                                              [(-compile (io/resource "cljs/nodejs.cljs") all-opts)])))
                             (when (= :nodejs (:target all-opts))
                               [(-compile (io/resource "cljs/nodejscli.cljs") all-opts)]))
                 optim (:optimizations all-opts)]
             (if (and optim (not= optim :none))
               (do
                 (when-let [fname (:source-map all-opts)]
                   (assert (string? fname)
                           (str ":source-map must name a file when using :whitespace, "
                                ":simple, or :advanced optimizations"))
                   (doall (map #(source-on-disk all-opts %) js-sources)))
                 (->> js-sources
                      (apply optimize all-opts)
                      (add-wrapper all-opts)
                      (add-source-map-link all-opts)
                      (add-header all-opts)
                      (output-one-file all-opts)))
               (apply output-unoptimized all-opts js-sources))))))))
  

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
