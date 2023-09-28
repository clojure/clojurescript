(ns cljs.foreign.node
  (:require [cljs.vendor.clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn package-json-entries
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

(defn- package-json? [path]
  (= "package.json" (.getName (io/file path))))

(defn- top-level-package-json? [path]
  (boolean (re-find #"node_modules[/\\](@[^/\\]+?[/\\])?[^/\\]+?[/\\]package\.json$" path)))

;; the path sans the package.json part
;; i.e. some_lib/package.json -> some_lib
(defn- trim-package-json [s]
  (if (string/ends-with? s "package.json")
    (subs s 0 (- (count s) 12))
    s))

(defn- trim-relative [path]
  (cond-> path
    (string/starts-with? path "./")
    (subs 2)))

(defn- ->export-pkg-json [package-path export]
  (io/file
    (trim-package-json package-path)
    (trim-relative export)
    "package.json"))

(defn resolve-export
  "Given an export value, find the entry point based on the
  :package-json-resolution value, defaults to :nodejs. Returns nil
  if we couldn't resolve it."
  [export opts]
  (if (string? export)
    export
    ;; we check for require to attempt to filter out
    ;; strange cases, i.e. import but no require etc.
    (when (and (map? export) (contains? export "require"))
      (let [resolve (:package-json-resolution opts :nodejs)
            lookup  (if (sequential? resolve)
                      (or (some #{"import" "require"} resolve) "require")
                      ({:webpack "import" :nodejs "require"} resolve))
            entry   (get export lookup)]
        (if (map? entry)
          (get entry "default")
          entry)))))

(defn- export-subpaths
  "Examine the export subpaths to compute subpackages. Add them to pkg-json
  parameter (this is a reduce-kv helper)."
  [pkg-jsons export-subpath export package-path pkg-name opts]
  ;; NOTE: ignore "." exports for now
  (if (= "." export-subpath)
    (if-let [resolved (resolve-export export opts)]
      (assoc-in pkg-jsons [package-path "main"] resolved)
      pkg-jsons)
    ;; technically the following logic is a bit brittle since `exports` is
    ;; supposed to be used to hide the package structure.
    ;; instead, here we assume the export subpath does match the library structure
    ;; on disk, if we find a package.json we add it to pkg-jsons map
    ;; and we synthesize "name" key based on subpath
    (let [export-pkg-json-file (->export-pkg-json package-path export-subpath)]
      ;; note this will ignore export wildcards etc.
      (cond-> pkg-jsons
        (.exists export-pkg-json-file)
        (-> (assoc
              (.getAbsolutePath export-pkg-json-file)
              (merge
                (json/read-str (slurp export-pkg-json-file))
                ;; add the name field so that path->main-name works later
                (when (and (map? export)
                           (contains? export "require"))
                  {"name" (str pkg-name (string/replace export-subpath "./" "/"))}))))))))

(defn- add-exports
  "Given a list of pkg-jsons examine them for the `exports` field. `exports`
  is now the preferred way to declare an entrypoint to a Node.js library. However,
  for backwards compatibility it is often combined with `main`.

  `export` can also be a JS object - if so, it can define subpaths. `.` points
  to main and other subpaths can be defined relative to that.

  See https://nodejs.org/api/packages.html#main-entry-point-export for more
  detailed information."
  [pkg-jsons opts]
  (reduce-kv
    (fn [pkg-jsons package-path {:strs [exports] :as pkg-json}]
      (if (string? exports)
        pkg-jsons
        ;; map case
        (reduce-kv
          (fn [pkg-jsons export-subpath export]
            (export-subpaths pkg-jsons export-subpath
              export package-path (get pkg-json "name") opts))
          pkg-jsons exports)))
    pkg-jsons pkg-jsons))

(defn path->main-name
  "Determine whether a path is a main entrypoint in the provided package.json.
  If so return the name entry provided in the package.json file."
  [path [pkg-json-path {:as pkg-json :strs [name]}] opts]
  (let [entries (package-json-entries opts)
        entry   (first (keep (partial get pkg-json) entries))]
    (when-not (nil? entry)
      ;; should be the only edge case in
      ;; the package.json main field - Antonio
      (let [entry      (trim-relative entry)
            entry-path (-> pkg-json-path (string/replace \\ \/)
                         trim-package-json (str entry))]
        ;; find a package.json entry point that matches
        ;; the `path`
        (some (fn [candidate]
                (when (= candidate (string/replace path \\ \/)) name))
          (cond-> [entry-path]
            ;; if we have an entry point that doesn't end in .js or .json
            ;; try to match some alternatives
            (not (or (string/ends-with? entry-path ".js")
                     (string/ends-with? entry-path ".json")))
            (into [(str entry-path ".js") (str entry-path "/index.js") (str entry-path ".json")
                   (string/replace entry-path #"\.cjs$" ".js")])))))))

(defn- path->rel-name [path]
  (-> (subs path (.lastIndexOf path "node_modules"))
    (string/replace \\ \/)
    (string/replace #"node_modules[\\\/]" "")))

(defn path->provides
  "For a given path in node_modules, determine what namespaces that file would
  provide to ClojureScript. Note it is assumed that we *already* processed all
  package.json files and they are present via pkg-jsons parameter as we need them
  to figure out the provides."
  [path pkg-jsons opts]
  (merge
    {:file        path
     :module-type :es6}
    ;; if the file is *not* a package.json, then compute what
    ;; namespaces it :provides to ClojureScript
    (when-not (package-json? path)
      ;; given some path search the package.json to determine whether it is a
      ;; main entry point or not
      (let [pkg-json-main (some #(path->main-name path % opts) pkg-jsons)]
        {:provides (let [module-rel-name (path->rel-name path)
                         provides        (cond-> [module-rel-name (string/replace module-rel-name #"\.js(on)?$" "")]
                                           (some? pkg-json-main) (conj pkg-json-main))
                         index-replaced  (string/replace module-rel-name #"[\\\/]index\.js(on)?$" "")]
                     (cond-> provides
                       (and (boolean (re-find #"[\\\/]index\.js(on)?$" module-rel-name))
                            (not (some #{index-replaced} provides)))
                       (conj index-replaced)))}))))

(defn get-pkg-jsons
  "Given all a seq of files in node_modules return a map of all package.json
  files indexed by path. Includes any `export` package.json files as well"
  ([module-fseq]
   (get-pkg-jsons module-fseq nil))
  ([module-fseq opts]
   (add-exports
     (into {}
       (comp (map #(.getAbsolutePath %))
         (filter top-level-package-json?)
         (map (fn [path] [path (json/read-str (slurp path))])))
       module-fseq) opts)))

(defn node-file-seq->libs-spec*
  "Given a sequence of non-nested node_module paths where the extension ends in
  `.js/.json`, return lib-spec maps for each path containing at least :file,
   :module-type, and :provides."
  [module-fseq opts]
  (let [;; a map of all the *top-level* package.json paths and their exports
        ;; to the package.json contents as EDN
        pkg-jsons (get-pkg-jsons module-fseq opts)]
    (into []
      (comp
        (map #(.getAbsolutePath %))
        ;; for each file, figure out what it will provide to ClojureScript
        (map #(path->provides % pkg-jsons opts)))
      module-fseq)))
