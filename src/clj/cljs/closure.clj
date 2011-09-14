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
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import java.io.File
           java.io.BufferedInputStream
           java.net.URL
           java.util.logging.Level
           java.util.jar.JarFile
           com.google.common.collect.ImmutableList
           com.google.javascript.jscomp.CompilerOptions
           com.google.javascript.jscomp.CompilationLevel
           com.google.javascript.jscomp.ClosureCodingConvention
           com.google.javascript.jscomp.JSSourceFile
           com.google.javascript.jscomp.Result
           com.google.javascript.jscomp.JSError
           com.google.javascript.jscomp.CommandLineRunner))

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

(defn make-options
  "Create a CompilerOptions object and set options from opts map."
  [opts]
  (let [level (case (:optimizations opts)
                    :advanced CompilationLevel/ADVANCED_OPTIMIZATIONS
                    :whitespace CompilationLevel/WHITESPACE_ONLY
                    :simple CompilationLevel/SIMPLE_OPTIMIZATIONS)
        compiler-options (doto (CompilerOptions.)
                           (.setCodingConvention (ClosureCodingConvention.)))]
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
  [{:keys [externs use-only-custom-externs target]}]
  (letfn [(filter-js [paths]
            (for [p paths f (file-seq (io/file p))
                  :when (.endsWith (.toLowerCase (.getName f)) ".js")]
              (.getAbsolutePath f)))
          (add-target [ext]
            (if (= :nodejs target)
              (cons (.getFile (io/resource "cljs/nodejs_externs.js"))
                    (or ext []))
              ext))
          (load-js [ext]
            (map #(js-source-file % (io/input-stream %)) ext))]
    (let [js-sources (-> externs filter-js add-target load-js)]
      (if use-only-custom-externs
        js-sources
        (into js-sources (CommandLineRunner/getDefaultExterns))))))

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

(defn parse-js-ns
  "Given the lines from a JavaScript source file, parse the provide
  and require statements and return them in a map. Assumes that all
  provide and require statements appear before the first function
  definition."
  [lines]
  (letfn [(conj-in [m k v] (update-in m [k] (fn [old] (conj old v))))]
    (->> (for [line lines x (string/split line #";")] x)
         (map string/trim)
         (take-while #(not (re-matches #".*=[\s]*function\(.*\)[\s]*[{].*" %)))
         (map #(re-matches #".*goog\.(provide|require)\('(.*)'\)" %))
         (remove nil?)
         (map #(drop 1 %))
         (reduce (fn [m ns]
                   (if (= (first ns) "require")
                     (conj-in m :requires (last ns))
                     (conj-in m :provides (last ns))))
                 {:requires [] :provides []}))))

;; Protocols for IJavaScript and Compilable
;; ========================================

(defmulti to-url class)

(defmethod to-url File [^File f] (.toURL (.toURI f)))

(defmethod to-url String [s] (to-url (io/file s)))

(defprotocol IJavaScript
  (-url [this] "The URL where this JavaScript is located. Returns nil
  when JavaScript exists in memory only.")
  (-provides [this] "A list of namespaces that this JavaScript provides.")
  (-requires [this] "A list of namespaces that this JavaScript requires.")
  (-source [this] "The JavaScript source string."))

(extend-protocol IJavaScript
  
  String
  (-url [this] nil)
  (-provides [this] (:provides (parse-js-ns (string/split-lines this))))
  (-requires [this] (:requires (parse-js-ns (string/split-lines this))))
  (-source [this] this)
  
  clojure.lang.IPersistentMap
  (-url [this] (or (:url this)
                   (to-url (:file this))))
  (-provides [this] (map name (:provides this)))
  (-requires [this] (map name (:requires this)))
  (-source [this] (if-let [s (:source this)]
                    s
                    (slurp (io/reader (-url this))))))

(defrecord JavaScriptFile [^URL url provides requires]
  IJavaScript
  (-url [this] url)
  (-provides [this] provides)
  (-requires [this] requires)
  (-source [this] (slurp (io/reader url))))

(defn javascript-file [^URL url provides requires]
  (JavaScriptFile. url (map name provides) (map name requires)))

(defn map->javascript-file [m]
  (javascript-file (to-url (:file m))
                   (:provides m)
                   (:requires m)))

(defn read-js
  "Read a JavaScript file returning a map of file information."
  [f]
  (let [source (slurp f)
        m (parse-js-ns (string/split-lines source))]
    (map->javascript-file (assoc m :file f))))

(defprotocol Compilable
  (-compile [this opts] "Returns one or more IJavaScripts."))

(defn build-index
  "Index a list of dependencies by namespace and file name. There can
  be zero or more namespaces provided per file."
  [deps]
  (reduce (fn [m next]
            (let [provides (:provides next)]
              (-> (if (seq provides)
                    (reduce (fn [m* provide]
                              (assoc m* provide next))
                            m
                            provides)
                    m)
                  (assoc (:file next) next))))
          {}
          deps))

(defn dependency-order-visit
  [state ns-name]
  (let [file (get state ns-name)]
    (if (or (:visited file) (nil? file))
      state
      (let [state (assoc-in state [ns-name :visited] true)
            deps (:requires file)
            state (reduce dependency-order-visit state deps)]
        (assoc state :order (conj (:order state) file))))))

(defn dependency-order
  "Topologically sort a collection of dependencies."
  [coll]
  (let [state (build-index coll)]
    (distinct (:order (reduce dependency-order-visit (assoc state :order []) (keys state))))))

;; Compile
;; =======

(defn empty-env []
  {:ns (@comp/namespaces comp/*cljs-ns*) :context :statement :locals {}})

(defn compile-form-seq
  "Compile a sequence of forms to a JavaScript source string."
  [forms]
  (comp/with-core-cljs
    (with-out-str
      (binding [comp/*cljs-ns* 'cljs.user]
        (doseq [form forms]
          (comp/emit (comp/analyze (empty-env) form)))))))

(defn output-directory [opts]
  (or (:output-dir opts) "out"))

(def compiled-cljs (atom {}))

(defn compiled-file
  "Given a map with at least a :file key, return a map with
   {:file .. :provides .. :requires ..}.

   Compiled files are cached so they will only be read once."
  [m]
  (let [path (.getAbsolutePath (:file m))
        js (if (:provides m)
             (map->javascript-file m)
             (if-let [js (get @compiled-cljs path)]
               js
               (read-js (:file m))))]
    (do (swap! compiled-cljs (fn [old] (assoc old path js)))
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
      (compiled-file (comp/compile-file file out-file)))
    (compile-form-seq (comp/forms-seq file))))

(defn compile-dir
  "Recursively compile all cljs files under the given source
  directory. Return a list of JavaScriptFiles in dependency order."
  [^File src-dir opts]
  (let [out-dir (output-directory opts)]
    (dependency-order
     (map compiled-file
          (comp/compile-root src-dir out-dir)))))

(defn path-from-jarfile
  "Given the URL of a file within a jar, return the path of the file
  from the root of the jar."
  [^URL url]
  (last (string/split (.getFile url) #"\.jar!/")))

(defn jar-file-to-disk
  "Copy a file contained within a jar to disk. Return the created file."
  [url out-dir]
  (let [out-file (io/file out-dir (path-from-jarfile url)) 
        content (slurp (io/reader url))]
    (do (comp/mkdirs out-file)
        (spit out-file content)
        out-file)))

(defn compile-from-jar
  "Compile a file from a jar."
  [this {:keys [output-file] :as opts}]
  (or (when output-file
        (let [out-file (io/file (output-directory opts) output-file)]
          (when (.exists out-file)
            (compiled-file {:file out-file}))))
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

;; Dependencies
;; ============
;;
;; Find all dependencies from files on the classpath. Eliminates the
;; need for closurebuilder. cljs dependencies will be compiled as
;; needed.

(defn load-library*
  "Given a path to a JavaScript library, which is a directory
  containing Javascript files, return a list of maps
  containing :provides, :requires, :file and :url."
  [path]
  (letfn [(graph-node [f]
            (-> (io/reader f)
                line-seq
                parse-js-ns
                (assoc :file (.getPath f) :url (to-url f))))]
    (let [js-sources (filter #(.endsWith (.getName %) ".js") (file-seq (io/file path)))]
      (filter #(seq (:provides %)) (map graph-node js-sources)))))

(def load-library (memoize load-library*))

(defn library-dependencies [{:keys [libs]}]
  (mapcat load-library libs))

(comment
  ;; load one library
  (load-library* "closure/library/third_party/closure")
  ;; load all library dependencies
  (library-dependencies {:libs ["closure/library/third_party/closure"]})
  )

(defn goog-dependencies*
  "Create an index of Google dependencies by namespace and file name."
  []
  (letfn [(parse-list [s] (when (> (count s) 0)
                            (-> (.substring s 1 (dec (count s)))
                                (string/split #"'\s*,\s*'"))))]
    (->> (line-seq (io/reader (io/resource "goog/deps.js")))
         (map #(re-matches #"^goog\.addDependency\(['\"](.*)['\"],\s*\[(.*)\],\s*\[(.*)\]\);.*" %))
         (remove nil?)
         (map #(drop 1 %))
         (remove #(.startsWith (first %) "../../third_party"))
         (map #(hash-map :file (str "goog/"(first %))
                         :provides (parse-list (second %))
                         :requires (parse-list (last %))
                         :group :goog)))))

(def goog-dependencies (memoize goog-dependencies*))

(defn js-dependency-index
  "Returns the index for all JavaScript dependencies. Lookup by
  namespace or file name."
  [opts]
  (build-index (concat (goog-dependencies) (library-dependencies opts))))

(defn js-dependencies
  "Given a sequence of Closure namespace strings, return the list of
  all dependencies in dependency order. The returned list includes all
  Google and third-party library dependencies.

  Third-party libraries are configured using the :libs option where
  the value is a list of directories containing third-party
  libraries."
  [opts requires]
  (let [index (js-dependency-index opts)]
    (loop [requires requires
           visited requires
           deps #{}]
      (if (seq requires)
        (let [node (get index (first requires))
              new-req (remove #(contains? visited %) (:requires node))]
          (recur (into (rest requires) new-req)
                 (into visited new-req)
                 (conj deps node)))
        (cons (get index "goog/base.js") (dependency-order deps))))))

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
  IJavaScripts which are the cljs dependencies in dependency
  order. The returned list will not only include the explicitly
  required files but any transitive depedencies as well. JavaScript
  files will be compiled to the working directory if they do not
  already exist.

  Only load dependencies from the classpath."
  [opts requires]
  (let [index (js-dependency-index opts)]
    (letfn [(ns->cp [s] (str (string/replace (munge s) \. \/) ".cljs"))
            (cljs-deps [coll]
              (->> coll
                   (remove #(contains? index %))
                   (map #(let [f (ns->cp %)] (hash-map :relative-path f :uri (io/resource f))))
                   (remove #(nil? (:uri %)))))]
      (loop [required-files (cljs-deps requires)
             visited (set required-files)
             js-deps #{}]
        (if (seq required-files)
          (let [next-file (first required-files)
                js (get-compiled-cljs opts next-file)
                new-req (remove #(contains? visited %) (cljs-deps (-requires js)))]
            (recur (into (rest required-files) new-req)
                   (into visited new-req)
                   (conj js-deps js)))
          (dependency-order js-deps))))))

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
  (let [requires (mapcat -requires inputs)
        required-cljs (cljs-dependencies opts requires)
        required-js (js-dependencies opts (set (concat (mapcat -requires required-cljs) requires)))]
    (concat (map #(-> (javascript-file (or (:url %) (io/resource (:file %)))
                                       (:provides %)
                                       (:requires %))
                      (assoc :group (:group %))) required-js)
            required-cljs
            inputs)))

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
  ;; add dependencies to a JavaScriptFile record
  (add-dependencies {} (javascript-file (to-url "samples/hello/src/hello/core.cljs")
                                     ["hello.core"]
                                     ["goog.array"]))
  )

;; Optimize
;; ========

(defmulti javascript-name class)

(defmethod javascript-name URL [^URL url]
  (if url (.getPath url) "cljs/user.js"))

(defmethod javascript-name String [s]
  (if-let [name (first (-provides s))] name "cljs/user.js"))

(defmethod javascript-name JavaScriptFile [js] (javascript-name (-url js)))

(defmethod js-source-file JavaScriptFile [_ js]
  (when-let [url (-url js)]
    (js-source-file (javascript-name url) (io/input-stream url))))

(defn optimize
  "Use the Closure Compiler to optimize one or more JavaScript files."
  [opts & sources]
  (let [closure-compiler (make-closure-compiler)
        externs (load-externs opts)
        compiler-options (make-options opts)
        inputs (map #(js-source-file (javascript-name %) %) sources)
        result ^Result (.compile closure-compiler externs inputs compiler-options)]
    (if (.success result)
      (.toSource closure-compiler)
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
        input-path (comp/path-seq (.getCanonicalPath (io/file ^URL (-url input))))
        count-base (count base-path)
        common (count (take-while true? (map #(= %1 %2) base-path input-path)))
        prefix (repeat (- count-base common 1) "..")]
    (if (= count-base common)
      (last input-path) ;; same file
      (comp/to-path (concat prefix (drop common input-path)) "/"))))

(defn add-dep-string
  "Return a goog.addDependency string for an input."
  [opts input]
  (letfn [(ns-list [coll] (when (seq coll) (apply str (interpose ", " (map #(str "'" (munge %) "'") coll)))))]
    (str "goog.addDependency(\""
         (path-relative-to (io/file (output-directory opts) "goog/base.js") input)
         "\", ["
         (ns-list (-provides input))
         "], ["
         (ns-list (-requires input))
         "]);")))

(defn deps-file
  "Return a deps file string for a sequence of inputs."
  [opts sources]
  (apply str (interpose "\n" (map #(add-dep-string opts %) sources))))

(comment
  (path-relative-to (io/file "out/goog/base.js") {:url (to-url "out/cljs/core.js")})
  (add-dep-string {} {:url (to-url "out/cljs/core.js") :requires ["goog.string"] :provides ["cljs.core"]})
  (deps-file {} [{:url (to-url "out/cljs/core.js") :requires ["goog.string"] :provides ["cljs.core"]}])
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
  (if-let [url ^URL (-url js)]
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
              (spit out-file (-source js))))
        {:url (to-url out-file) :requires (-requires js) :provides (-provides js) :group (:group js)})))

(defn source-on-disk
  "Ensure that the given JavaScript exists on disk. Write in memory
  sources and files contained in jars to the working directory. Return
  updated IJavaScript with the new location."
  [opts js]
  (let [url ^URL (-url js)]
    (if (or (not url) (= (.getProtocol url) "jar"))
      (write-javascript opts js)
      js)))

(comment
  (write-javascript {} "goog.provide('demo');\nalert('hello');\n")
  ;; write something from a jar file to disk
  (source-on-disk {}
                  {:url (io/resource "goog/base.js")
                   :source (slurp (io/reader (io/resource "goog/base.js")))})
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

(defn add-header [{:keys [hashbang target]} js]
  (if (= :nodejs target)
    (str "#!" (or hashbang "/usr/bin/nodejs") "\n" js)
    js))

(defn build
  "Given a source which can be compiled, produce runnable JavaScript."
  [source opts]
  (let [opts (if (= :nodejs (:target opts))
               (merge {:optimizations :simple} opts)
               opts)
        compiled (-compile source opts)
        compiled (concat
                   (if (coll? compiled) compiled [compiled])
                   (when (= :nodejs (:target opts))
                     [(-compile (io/resource "cljs/nodejscli.cljs") opts)]))
        js-sources (if (coll? compiled)
                     (apply add-dependencies opts compiled)
                     (add-dependencies opts compiled))]
    (if (:optimizations opts)
      (->> js-sources
           (apply optimize opts)
           (add-header opts)
           (output-one-file opts))
      (apply output-unoptimized opts js-sources))))

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
