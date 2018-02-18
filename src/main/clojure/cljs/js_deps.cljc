;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.js-deps
  (:require [cljs.util :refer [distinct-by]]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.io File]
           [java.net URL URLClassLoader]
           [java.util.zip ZipFile ZipEntry]))

(def ^:private java-8? (-> (System/getProperty "java.version") (string/starts-with? "1.8.")))

(defn- classpath-files
  "Returns a list of classpath files. Under Java 8, walks up the parentage
  chain of RT/baseLoader, concatenating any URLs it retrieves. Under Java 9 and
  later, builds file list from the java.class.path system property."
  []
  (->>
    (if java-8?
      ; taken from pomegranate/dynapath
      ; https://github.com/tobias/dynapath/blob/master/src/dynapath/util.clj
      (->> (clojure.lang.RT/baseLoader)
        (iterate #(.getParent ^ClassLoader %))
        (take-while identity)
        reverse
        (filter (partial instance? URLClassLoader))
        (mapcat #(.getURLs ^URLClassLoader %)))
      (-> (System/getProperty "java.class.path")
        (string/split (re-pattern File/pathSeparator))))
    distinct
    (map io/file)))

(defn ^ZipFile zip-file [jar-path]
  (try
    (cond
      (instance? File jar-path) (ZipFile. ^File jar-path)
      (string? jar-path) (ZipFile. ^String jar-path))
    (catch Exception _
      nil)))

(defn jar-entry-names* [jar-path]
  (when-let [zf (zip-file jar-path)]
    (with-open [z zf]
      (doall (map #(.getName ^ZipEntry %) (enumeration-seq (.entries ^ZipFile z)))))))

(def jar-entry-names (memoize jar-entry-names*))

(defn find-js-jar
  "Returns a seq of URLs of all JavaScript resources in the given jar"
  [jar-path lib-path]
  (map io/resource
    (filter #(and
               (.endsWith ^String % ".js")
               (.startsWith ^String % lib-path))
      (jar-entry-names jar-path))))

(defmulti to-url class)

(defmethod to-url File [^File f] (.toURL (.toURI f)))

(defmethod to-url URL [^URL url] url)

(defmethod to-url String [s] (to-url (io/file s)))

(defn find-js-fs
  "finds js resources from a path on the files system"
  [path]
  (let [file (io/file path)]
    (when (.exists file)
      (map to-url (filter #(.endsWith ^String (.getName ^File %) ".js") (file-seq (io/file path)))))))

(defn find-js-classpath
  "Returns a seq of URLs of all JavaScript files on the classpath."
  [path]
  (->> (classpath-files)
    (reduce
      (fn [files jar-or-dir]
        (let [name (.toLowerCase (.getName ^File jar-or-dir))
              ext  (.substring name (inc (.lastIndexOf name ".")))]
          (->> (when (.exists ^File jar-or-dir)
                 (cond
                   (.isDirectory ^File jar-or-dir)
                   (find-js-fs (str (.getAbsolutePath ^File jar-or-dir) "/" path))

                   (#{"jar" "zip"} ext)
                   (find-js-jar jar-or-dir path)

                   :else nil))
            (remove nil?)
            (into files))))
      [])))

(defn find-js-resources [path]
  "Returns a seq of URLs to all JavaScript resources on the classpath or within
a given (directory) path on the filesystem. [path] only applies to the latter
case."
  (let [file (io/file path)]
    (if (.exists file)
      (find-js-fs path)
      (find-js-classpath path))))

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
         (map #(re-matches #".*goog\.(provide|require)\(['\"](.*)['\"]\)" %))
         (remove nil?)
         (map #(drop 1 %))
         (reduce (fn [m ns]
                   (let [munged-ns (string/replace (last ns) "_" "-")]
                     (if (= (first ns) "require")
                       (conj-in m :requires munged-ns)
                       (conj-in m :provides munged-ns))))
                 {:requires [] :provides []}))))

(defprotocol IJavaScript
  (-foreign? [this] "Whether the Javascript represents a foreign
  library (a js file that not have any goog.provide statement")
  (-closure-lib? [this] "Whether the Javascript represents a Closure style
  library")
  (-url [this] [this opts] "The URL where this JavaScript is located. Returns nil
  when JavaScript exists in memory only.")
  (-relative-path [this] [this opts] "Relative path for this JavaScript.")
  (-provides [this] "A list of namespaces that this JavaScript provides.")
  (-requires [this] "A list of namespaces that this JavaScript requires.")
  (-source [this] [this opts] "The JavaScript source string."))

(defn get-file [lib-spec index]
  (or (:file lib-spec)
      (some (fn [provide] (get-in index [provide :file]))
        (:provides lib-spec))))

(defn lib-spec-merge [a b]
  (merge a
    (cond-> b
      (contains? a :provides) (dissoc :provides))))

(defn build-index
  "Index a list of dependencies by namespace and file name. There can
  be zero or more namespaces provided per file. Upstream foreign libraies
  will have their options merged with local foreign libraries to support
  fine-grained overriding."
  [deps]
  (reduce
    (fn [index dep]
      (let [provides (:provides dep)
            index'   (if (seq provides)
                       (reduce
                         (fn [index' provide]
                           (if (:foreign dep)
                             (update-in index' [provide] lib-spec-merge dep)
                             ;; when building the dependency index, we need to
                             ;; avoid overwriting a CLJS dep with a CLJC dep of
                             ;; the same namespace - António Monteiro
                             (let [file (when-let [f (or (:source-file dep) (:file dep))]
                                          (str f))
                                   ext (when file
                                         (subs file (inc (string/last-index-of file "."))))]
                               (update-in index' [provide]
                                 (fn [d]
                                   (if (and (= ext "cljc") (some? d))
                                     d
                                     dep))))))
                         index provides)
                       index)]
        (if (:foreign dep)
          (if-let [file (get-file dep index')]
            (update-in index' [file] lib-spec-merge dep)
            (throw
              (Exception.
                (str "No :file provided for :foreign-libs spec " (pr-str dep)))))
          (assoc index' (:file dep) dep))))
    {} deps))

(defn dependency-order-visit
  ([state ns-name]
    (dependency-order-visit state ns-name []))
  ([state ns-name seen]
   #_(assert (not (some #{ns-name} seen))
       (str "Circular dependency detected, "
         (apply str (interpose " -> " (conj seen ns-name)))))
   (if-not (some #{ns-name} seen)
     (let [file (get state ns-name)]
       (if (or (:visited file) (nil? file))
         state
         (let [state (assoc-in state [ns-name :visited] true)
               deps (:requires file)
               state (reduce #(dependency-order-visit %1 %2 (conj seen ns-name)) state deps)]
           (assoc state :order (conj (:order state) file)))))
     state)))

(defn- pack-string [s]
  (if (string? s)
    {:provides (-provides s)
     :requires (-requires s)
     :file (str "from_source_" (gensym) ".clj")
     ::original s}
    s))

(defn- unpack-string [m]
  (or (::original m) m))

(defn dependency-order
  "Topologically sort a collection of dependencies."
  [coll]
  (let [state (build-index (map pack-string coll))]
    (map unpack-string
      (distinct-by :provides
        (:order (reduce dependency-order-visit (assoc state :order []) (keys state)))))))


;; Dependencies
;; ============
;;
;; Find all dependencies from files on the classpath. Eliminates the
;; need for closurebuilder. cljs dependencies will be compiled as
;; needed.

(defn find-url
  "Given a string, returns a URL. Attempts to resolve as a classpath-relative
  path, then as a path relative to the working directory or a URL string"
  [path-or-url]
  (or (io/resource path-or-url)
      (try (io/as-url path-or-url)
           (catch java.net.MalformedURLException e
             false))
      (io/as-url (io/as-file path-or-url))))

(defn load-foreign-library*
  "Given a library spec (a map containing the keys :file
  and :provides), returns a map containing :provides, :requires, :file
  and :url"
  ([lib-spec] (load-foreign-library* lib-spec false))
  ([lib-spec cp-only?]
    (let [find-func (if cp-only? io/resource find-url)]
      (cond-> (assoc lib-spec :foreign true)
        (:file lib-spec)
        (assoc :url (find-func (:file lib-spec)))

        (:file-min lib-spec)
        (assoc :url-min (find-func (:file-min lib-spec)))))))

(def load-foreign-library (memoize load-foreign-library*))

(defn- library-graph-node
  "Returns a map of :provides, :requires, and :url given a URL to a goog-style
JavaScript library containing provide/require 'declarations'."
  ([url] (library-graph-node url nil))
  ([url lib-path]
   (with-open [reader (io/reader url)]
     (-> reader line-seq parse-js-ns
       (merge
         {:url url}
         (when lib-path
           {:closure-lib true :lib-path lib-path}))))))

(defn load-library*
  "Given a path to a JavaScript library, which is a directory
  containing Javascript files, return a list of maps
  containing :provides, :requires, :file and :url."
  [path]
  (->> (find-js-resources path)
    (map #(library-graph-node % path))
    (filter #(seq (:provides %)))))

(def load-library (memoize load-library*))

(defn library-dependencies
  [{libs :libs foreign-libs :foreign-libs
    ups-libs :ups-libs ups-flibs :ups-foreign-libs}]
  (concat
    (mapcat load-library ups-libs) ;upstream deps
    ; :libs are constrained to filesystem-only at this point; see
    ; `find-classpath-lib` for goog-style JS library lookup
    (mapcat load-library (filter #(.exists (io/file %)) libs))
    (map #(load-foreign-library % true) ups-flibs) ;upstream deps
    (map load-foreign-library foreign-libs)))

(comment
  ;; load one library
  (load-library* "closure/library/third_party/closure")
  ;; load all library dependencies
  (library-dependencies {:libs ["closure/library/third_party/closure"]})
  (library-dependencies {:foreign-libs [{:file "http://example.com/remote.js"
                                          :provides ["my.example"]}]})
  (library-dependencies {:foreign-libs [{:file "local/file.js"
                                            :provides ["my.example"]}]})
  (library-dependencies {:foreign-libs [{:file "cljs/nodejs_externs.js"
                                          :provides ["my.example"]}]}))

;; NO LONGER NEEDED, deps.js and base.js now removed from build
;(defn goog-resource
;  "Helper to disambiguate Google Closure Library resources from Google
;   Closure Library Third Party resoures."
;  [path]
;  (first
;    (filter
;      (fn [res]
;        (re-find #"(\/google-closure-library-0.0*|\/google-closure-library\/)" (.getPath ^URL res)))
;      (enumeration-seq (.getResources (.getContextClassLoader (Thread/currentThread)) path)))))

(defn goog-dependencies*
  "Create an index of Google dependencies by namespace and file name."
  []
  (letfn [(parse-list [s] (when (> (count s) 0)
                            (-> (.substring ^String s 1 (dec (count s)))
                                (string/split #"'\s*,\s*'"))))]
    (with-open [reader (io/reader (io/resource "goog/deps.js"))]
      (->> (line-seq reader)
           (map #(re-matches #"^goog\.addDependency\(['\"](.*)['\"],\s*\[(.*)\],\s*\[(.*)\],.*\);.*" %))
           (remove nil?)
           (map #(drop 1 %))
           (remove #(.startsWith ^String (first %) "../../third_party"))
           (map #(hash-map :file (str "goog/" (nth % 0))
                           :provides (parse-list (nth % 1))
                           :requires (parse-list (nth % 2))
                           :group :goog))
           (doall)))))

(def goog-dependencies (memoize goog-dependencies*))

(defn js-dependency-index
  "Returns the index for all JavaScript dependencies. Lookup by
  namespace or file name."
  [opts]
  ; (library-dependencies) will find all of the same libs returned by
  ; (goog-dependencies), but the latter returns some additional/different
  ; information (:file instead of :url, :group), so they're folded in last to
  ; take precedence in the returned index.  It is likely that
  ; (goog-dependencies), special-casing of them, goog/deps.js, etc can be
  ; removed entirely, but verifying that can be a fight for another day.
  (build-index (concat (library-dependencies opts) (goog-dependencies))))

(defn find-classpath-lib
  "Given [lib], a string or symbol naming a goog-style JavaScript library
  (i.e. one that uses goog.provide and goog.require), look for a resource on the
  classpath corresponding to [lib] and return a map via `library-graph-node`
  that contains its relevant metadata.  The library found on the classpath
  _must_ contain a `goog.provide` that matches [lib], or this fn will return nil
  and print a warning."
  [lib]
  (when-let [lib-resource (some-> (name lib)
                            (.replace \. \/)
                            (.replace \- \_)
                            (str ".js")
                            io/resource)]
    (let [{:keys [provides] :as lib-info} (library-graph-node lib-resource)]
      (if (some #{(name lib)} provides)
        (assoc lib-info :closure-lib true)
        (binding [*out* *err*]
          (println
            (format
              (str "WARNING: JavaScript file found on classpath for library `%s`, "
                   "but does not contain a corresponding `goog.provide` declaration: %s")
              lib lib-resource)))))))

(def native-node-modules
  #{"assert" "buffer_ieee754" "buffer" "child_process" "cluster" "console"
    "constants" "crypto" "_debugger" "dgram" "dns" "domain" "events" "freelist"
    "fs" "http" "https" "_linklist" "module" "net" "os" "path" "punycode"
    "querystring" "readline" "repl" "stream" "string_decoder" "sys" "timers"
    "tls" "tty" "url" "util" "vm" "zlib" "_http_server" "process" "v8"})
