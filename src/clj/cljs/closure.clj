;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(set! *warn-on-reflection* true)

(ns cljs.closure
  "Compile ClojureScript to JavaScript with optimizations from Google
   Closure Compiler producing runnable JavaScript.

   Use the 'build' function for end-to-end compilation.

   build = compile -> add-dependencies -> optimize -> output

   See comments for example usage. core.js and the Closure
   compiler.jar must be on the classpath.

   Two protocols are defined: IJavaScript and Compilable. The
   Compilable protocol is satisfied by something which can return one
   or more IJavaScripts.

   With IJavaScriipt objects in hand, calling add-dependencies will
   produce a sequence of IJavaScript objects which includes all
   required dependencies from the Closure library plus cljs.core, in
   dependency order. This function replaces the closurebuilder tool.

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

;; from clojure.contrib.jar

(defn jar-file?
  "Returns true if file is a normal file with a .jar or .JAR extension."
  [^File file]
  (and (.isFile file)
       (or (.endsWith (.getName file) ".jar")
           (.endsWith (.getName file) ".JAR"))))

(defn filenames-in-jar
  "Returns a sequence of Strings naming the non-directory entries in
the JAR file."
  [^JarFile jar-file]
  (map #(.getName %)
       (filter #(not (.isDirectory %))
               (enumeration-seq (.entries jar-file)))))

;; from clojure.contrib.classpath

(defn classpath
  "Returns a sequence of File objects of the elements on CLASSPATH."
  []
  (map #(File. %)
       (.split (System/getProperty "java.class.path")
               (System/getProperty "path.separator"))))

(defn classpath-jarfiles
  "Returns a sequence of JarFile objects for the JAR files on classpath."
  []
  (map #(JarFile. %) (filter jar-file? (classpath))))

;; End of functions from clojure.contrib

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
  (doseq [flag (:flags opts)]
    (case flag
          :pretty-print (set! (. compiler-options prettyPrint) true))))

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
  the default externs should be excluded.

  TODO: Implement options described above."
  [opts]
  (CommandLineRunner/getDefaultExterns))

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
  (-url [this] (:url this))
  (-provides [this] (:provides this))
  (-requires [this] (:requires this))
  (-source [this] (:source this)))

(defrecord JavaScriptFile [^URL url provides requires]
  IJavaScript
  (-url [this] url)
  (-provides [this] provides)
  (-requires [this] requires)
  (-source [this] (slurp (io/reader url))))

(defn javascript-file [^URL url provides requires]
  (JavaScriptFile. url (map name provides) (map name requires)))

(defprotocol Compilable
  (-compile [this opts] "Returns one or more IJavaScripts."))

;; Compile
;; =======

;; TODO: Most of the code below assumes that core is the only file in
;; ClojureScript. There are others and this will need to be changed to
;; load them both as cljs source and as compiled JavaScript.

(defmacro with-core
  "Ensure that core.cljs has been loaded."
  [& body]
  `(do (when-not (:defs (get @comp/namespaces 'cljs.core))
         (comp/load-file (comp/repl-env) "cljs/core.cljs"))
       ~@body))

(defn empty-env []
  {:ns (@comp/namespaces comp/*cljs-ns*) :context :statement :locals {}})

(defn compile-form-seq
  "Compile a sequence of forms to a JavaScript source string."
  [forms]
  (with-core
    (with-out-str
      (binding [comp/*cljs-ns* 'cljs.user]
        (doseq [form forms]
          (comp/emit (comp/analyze (empty-env) form)))))))

(defn compile-file
  "Compile a single cljs file in memory returning the JavaScript
  source as a String. The IJavaScript protocol is extended to String so
  this result is suitable to be passed to anything that expects to see
  IJavaScript."
  [^File file]
  (compile-form-seq (comp/forms-seq file)))

(defn output-directory [opts]
  (or (:output-dir opts) "out"))

(defn compile-dir
  "Recursively compile all cljs files under the given source
  directory. Return a list of JavaScriptFiles in dependency order."
  [^File src-dir opts]
  (let [out-dir (output-directory opts)
        compiled-info (comp/compile-root src-dir out-dir)]
    (map (fn [{:keys [file ns requires]}]
           (javascript-file (to-url file) [ns] requires))
         compiled-info)))

(extend-protocol Compilable

  File
  (-compile [this opts]
    (if (.isDirectory this)
      (compile-dir this opts)
      (compile-file this)))
  
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
;; Find dependencies from goog js files on the classpath. Eliminates
;; the need for closurebuilder.

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

(defn dependency-index*
  "Create an indexed list of all namespaces in the Closure library
  plus cljs.core."
  []
  (letfn [(graph-node [res]
                      (-> (io/resource res)
                          io/reader
                          line-seq
                          parse-js-ns
                          (assoc :file res)))]
    (let [goog (->> (classpath-jarfiles)
                    (filter #(.endsWith (.getName %) "goog.jar"))
                    first
                    filenames-in-jar
                    (filter #(.endsWith % ".js")))
          all-nodes (map graph-node (conj goog "core.js"))]
      (build-index all-nodes))))

(def dependency-index (memoize dependency-index*))

(defn dependency-order
  "Topologically sort a collection of dependencies."
  [coll]
  (let [state (build-index coll)]
    (:order (reduce comp/dependency-order-visit (assoc state :order []) (keys state)))))

(defn dependencies
  "Given a sequence of Closure namespace strings, return the list of
  all dependencies in dependency order. The returned list will contain
  file names relative to the classpath suitable to be passed to io/resource."
  [requires]
  (let [index (dependency-index)]
    (loop [requires requires
           deps []]
      (if (seq requires)
        (let [node (get index (first requires))]
          (recur (concat (rest requires) (:requires node)) (conj deps node)))
        (cons "goog/base.js" (map :file (distinct (dependency-order deps))))))))

(comment
  ;; find dependencies
  (dependencies ["cljs.core"])
  (dependencies ["goog.array"])
  (dependencies ["cljs.core" "goog.array"])
  )

(defn add-dependencies
  "Given one or more IJavaScript objects in dependency order, produce
  a new sequence of IJavaScript objects which includes all dependencies
  from the Closure library, plus cljs.core, in dependency order."
  [& inputs]
  (let [required (dependencies (mapcat -requires inputs))
        index (dependency-index)]
    (concat (map #(javascript-file (io/resource %)
                                   (:provides (get index %))
                                   (:requires (get index %))) required)
            inputs)))

(comment
  ;; add dependencies to literal js
  (add-dependencies "goog.provide('test.app');\ngoog.require('goog.array');")
  ;; add dependencies to a JavaScriptFile record
  (add-dependencies (javascript-file (to-url "samples/hello/src/hello/core.cljs")
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
                (apply add-dependencies)
                (apply optimize {:optimizations :simple :flags #{:pretty-print}})))
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
;; For unoptimized output the string will be a Closure deps file
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
        input-path (comp/path-seq (.getFile ^URL (-url input)))
        count-base (count base-path)
        common (count (take-while true? (map #(= %1 %2) base-path input-path)))
        prefix (repeat (- count-base common 1) "..")]
    (comp/to-path (concat prefix (drop common input-path)))))

(defn add-dep-string
  "Return a goog.addDependency string for an input."
  [opts input]
  (letfn [(ns-list [coll] (when (seq coll) (apply str (interpose ", " (map #(str "'" % "'") coll)))))]
    (str "goog.addDependency(\""
         (path-relative-to (io/file (output-directory opts)"goog/base.js") input)
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
  (path-relative-to (io/file "out/goog/base.js") {:url (io/resource "core.js")})
  (add-dep-string {} {:url (io/resource "core.js") :requires ["goog.string"] :provides ["cljs.core"]})
  (deps-file {} [{:url (io/resource "core.js") :requires ["goog.string"] :provides ["cljs.core"]}])  
  )

(defn output-one-file [{:keys [output-to]} js]
  (if output-to (spit output-to js) js))

(defn output-deps-file [opts sources]
  (output-one-file opts (deps-file opts sources)))

(defn ^String output-path
  "Given an IJavaScript which is either in memory or in a jar file,
  return the output path for this file relative to the working
  directory."
  [js]
  (if-let [url ^URL (-url js)]
    (last (string/split (.getFile url) #"/goog.jar!/"))
    (str (random-string 5) ".js")))

(defn write-javascript
  "Write a JavaScript file to disk. Only write if the file does not
  already exist. Return IJavaScript for the file on disk."
  [opts js]
  (let [out-dir (io/file (output-directory opts))
        out-name (output-path js)
        out-file (io/file out-dir out-name)]
    (do (when-not (.exists out-file)
          (do (.mkdirs (.getParentFile (.getCanonicalFile out-file)))
              (spit out-file (-source js))))
        {:url (to-url out-file) :requires (-requires js) :provides (-provides js)
         :library (when (.startsWith out-name "goog/") :goog)})))

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
  (source-on-disk {} {:url (io/resource "core.js")})
  )

(defn output-unoptimized [opts & sources]
  (let [disk-sources (map #(source-on-disk opts %) sources)
        goog-deps-url (io/resource "goog/deps.js")]
    (do (write-javascript opts {:url goog-deps-url :source (slurp (io/reader goog-deps-url))})
        (output-deps-file opts (remove #(= (:library %) :goog) disk-sources)))))

(comment
  
  ;; output unoptimized alone
  (output-unoptimized {} "goog.provide('test');\ngoog.require('cljs.core');\nalert('hello');\n")
  ;; output unoptimized with all dependencies
  (apply output-unoptimized {} (add-dependencies
                                "goog.provide('test');\ngoog.require('cljs.core');\nalert('hello');\n"))
  ;; output unoptimized and write deps file to 'out/test.js'
  (output-unoptimized {:output-to "out/test.js"}
                      "goog.provide('test');\ngoog.require('cljs.core');\nalert('hello');\n")
  )

(defn build
  "Given a source which can be compiled, produce runnable JavaScript."
  [source opts]
  (let [compiled (-compile source opts)
        js-sources (if (coll? compiled)
                     (apply add-dependencies compiled)
                     (add-dependencies compiled))]
    (if (:optimizations opts)
      (->> js-sources
           (apply optimize opts)
           (output-one-file opts))
      (apply output-unoptimized opts js-sources))))

(comment

  (println (build '[(ns hello.core)
                    (defn ^{:export greet} greet [n] (str "Hola " n))
                    (defn ^:export sum [xs] 42)]
                  {:optimizations :simple :flags #{:pretty-print}}))
  
  ;; build a project with optimizations
  (build "samples/hello/src" {:optimizations :advanced})
  (build "samples/hello/src" {:optimizations :advanced :output-to "samples/hello/hello.js"})
  ;; open 'samples/hello/hello.html' to see the result in action

  ;; build a project without optimizations
  (build "samples/hello/src" {:output-to "samples/hello/hello.js"})
  ;; open 'samples/hello/hello-dev.html' to see the result in action
  ;; notice how each script was loaded individually
  
  ;; build unoptimized from raw ClojureScript
  (build '[(ns hello.core)
           (defn ^{:export greet} greet [n] (str "Hola " n))
           (defn ^:export sum [xs] 42)]
         {:output-to "samples/hello/hello.js"})
  ;; open 'samples/hello/hello-dev.html' to see the result in action
  )
