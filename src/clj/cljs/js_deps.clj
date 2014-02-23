(ns cljs.js-deps
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import java.io.File))

(defn jar-entry-names* [jar-path]
  (with-open [z (java.util.zip.ZipFile. jar-path)]
    (doall (map #(.getName %) (enumeration-seq (.entries z))))))

(def jar-entry-names (memoize jar-entry-names*))

(defn find-js-jar
  "finds js resources from a given path in a jar file"
  [jar-path lib-path]
  (doall
    (map #(io/resource %)
         (filter #(do
                    (and 
                      (.startsWith ^String % lib-path)
                      (.endsWith ^String % ".js")))
                 (jar-entry-names jar-path)))))

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
  "finds all js files on the classpath matching the path provided"
  [path]
  (let [process-entry #(if (.endsWith ^String % ".jar")
                           (find-js-jar % path)
                           (find-js-fs (str % "/" path)))
        cpath-list (let [sysp (System/getProperty  "java.class.path" )]
                     (if (.contains sysp ";")
                       (string/split sysp #";")
                       (string/split sysp #":")))]
    (doall (reduce #(let [p (process-entry %2)]
                      (if p (concat %1 p) %1)) [] cpath-list))))

(defn find-js-resources [path]
  "finds js resources in a given path on either the file system or
   the classpath"
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
      (merge lib-spec {:foreign true
                       :url (find-func (:file lib-spec))}))))

(def load-foreign-library (memoize load-foreign-library*))

(defn load-library*
  "Given a path to a JavaScript library, which is a directory
  containing Javascript files, return a list of maps
  containing :provides, :requires, :file and :url."
  ([path] (load-library* path false))
  ([path cp-only?]
    (let [find-func (if cp-only? find-js-classpath find-js-resources)
          graph-node (fn [u]
                       (with-open [reader (io/reader u)]
                         (-> reader line-seq parse-js-ns
                             (assoc :url u))))]
    (let [js-sources (find-js-resources path)]
      (filter #(seq (:provides %)) (map graph-node js-sources))))))

(def load-library (memoize load-library*))

(defn library-dependencies [{libs :libs foreign-libs :foreign-libs
                             ups-libs :ups-libs ups-flibs :ups-foreign-libs}]
  (concat
   (mapcat #(load-library % true) ups-libs) ;upstream deps
   (mapcat load-library libs)
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

(defn goog-dependencies*
  "Create an index of Google dependencies by namespace and file name."
  []
  (letfn [(parse-list [s] (when (> (count s) 0)
                            (-> (.substring ^String s 1 (dec (count s)))
                                (string/split #"'\s*,\s*'"))))]
    (with-open [reader (io/reader (io/resource "goog/deps.js"))]
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
  (build-index (concat (goog-dependencies) (library-dependencies opts))))
