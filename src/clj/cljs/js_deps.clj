(ns cljs.js-deps
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import java.io.File
           java.net.URL
           java.net.URLClassLoader
           java.util.zip.ZipFile
           java.util.zip.ZipEntry))

; taken from pomegranate/dynapath
; https://github.com/tobias/dynapath/blob/master/src/dynapath/util.clj
(defn- all-classpath-urls
  "Walks up the parentage chain for a ClassLoader, concatenating any URLs it retrieves.
If no ClassLoader is provided, RT/baseLoader is assumed."
  ([] (all-classpath-urls (clojure.lang.RT/baseLoader)))
  ([cl]
     (->> (iterate #(.getParent ^ClassLoader %) cl)
       (take-while identity)
       reverse
       (filter (partial instance? URLClassLoader))
       (mapcat #(.getURLs ^URLClassLoader %))
       distinct)))

(defn ^ZipFile zip-file [jar-path]
  (cond
    (instance? File jar-path) (ZipFile. ^File jar-path)
    (string? jar-path) (ZipFile. ^String jar-path)
    :else
    (throw
      (IllegalArgumentException. (str "Cannot construct zipfile from " jar-path)))))

(defn jar-entry-names* [jar-path]
  (with-open [z (zip-file jar-path)]
    (doall (map #(.getName ^ZipEntry %) (enumeration-seq (.entries ^ZipFile z))))))

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
  (->> (all-classpath-urls)
    (map io/file)
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
  (-url [this] "The URL where this JavaScript is located. Returns nil
  when JavaScript exists in memory only.")
  (-provides [this] "A list of namespaces that this JavaScript provides.")
  (-requires [this] "A list of namespaces that this JavaScript requires.")
  (-source [this] "The JavaScript source string."))

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
                             (update-in index' [provide] merge dep)
                             (assoc index' provide dep)))
                         index provides)
                       index)]
        (if (:foreign dep)
          (update-in index' [(:file dep)] merge dep)
          (assoc index' (:file dep) dep))))
    {} deps))

(defn dependency-order-visit
  [state ns-name]
  (let [file (get state ns-name)]
    (if (or (:visited file) (nil? file))
      state
      (let [state (assoc-in state [ns-name :visited] true)
            deps (:requires file)
            state (reduce dependency-order-visit state deps)]
        (assoc state :order (conj (:order state) file))))))

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
         (distinct
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
      (cond->
        (merge lib-spec
          {:foreign true
           :url     (find-func (:file lib-spec))})
        (:file-min lib-spec)
        (assoc :url-min (find-func (:file-min lib-spec)))))))

(def load-foreign-library (memoize load-foreign-library*))

(defn- library-graph-node
  "Returns a map of :provides, :requires, and :url given a URL to a goog-style
JavaScript library containing provide/require 'declarations'."
  [url]
  (with-open [reader (io/reader url)]
    (-> reader line-seq parse-js-ns
      (assoc :url url))))

(defn load-library*
  "Given a path to a JavaScript library, which is a directory
  containing Javascript files, return a list of maps
  containing :provides, :requires, :file and :url."
  [path]
  (->> (find-js-resources path)
    (map library-graph-node)
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

(defn goog-resource
  "Helper to disambiguate Google Closure Library resources from Google
   Closure Library Third Party resoures."
  [path]
  (first
    (filter
      (fn [res]
        (re-find #"(\/google-closure-library-0.0*|\/google-closure-library\/)" (.getPath ^URL res)))
      (enumeration-seq (.getResources (.getContextClassLoader (Thread/currentThread)) path)))))

(defn goog-dependencies*
  "Create an index of Google dependencies by namespace and file name."
  []
  (letfn [(parse-list [s] (when (> (count s) 0)
                            (-> (.substring ^String s 1 (dec (count s)))
                                (string/split #"'\s*,\s*'"))))]
    (with-open [reader (io/reader (goog-resource "goog/deps.js"))]
      (->> (line-seq reader)
           (map #(re-matches #"^goog\.addDependency\(['\"](.*)['\"],\s*\[(.*)\],\s*\[(.*)\]\);.*" %))
           (remove nil?)
           (map #(drop 1 %))
           (remove #(.startsWith ^String (first %) "../../third_party"))
           (map #(hash-map :file (str "goog/"(first %))
                           :provides (parse-list (second %))
                           :requires (parse-list (last %))
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
        lib-info
        (println (format "WARNING: JavaScript file found on classpath for library `%s`, but does not contain a corresponding `goog.provide` declaration: %s"
                   lib lib-resource))))))
