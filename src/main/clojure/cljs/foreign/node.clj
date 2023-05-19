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

(defn- ->export-pkg-json [path export]
  (io/file
    (trim-package-json path)
    (trim-relative export)
    "package.json"))

(defn- export-subpaths
  [pkg-jsons export path]
  ;; NOTE: ignore "." exports for now
  (if (= "." export)
    pkg-jsons
    (let [export-pkg-json (->export-pkg-json path export)]
      (cond-> pkg-jsons
        (.exists export-pkg-json)
        (assoc
          (.getAbsolutePath export-pkg-json)
          (json/read-str (slurp export-pkg-json)))))))

(defn- add-exports
  "Given a list of pkg-jsons examine them for the `exports` field. `exports`
  is now the preferred way to declare an entrypoint to a Node.js library. However,
  for backwards compatibility it is often combined with `main`.

  `export` can also be a JS object - if so, it can define subpaths. `.` points
  to main and other subpaths can be defined relative to that.

  See https://nodejs.org/api/packages.html#main-entry-point-export for more
  detailed information."
  [pkg-jsons]
  (reduce-kv
    (fn [pkg-jsons path {:strs [exports] :as pkg-json}]
      ;; "exports" can just be a dupe of "main", i.e. a string - ignore
      (if (string? exports)
        pkg-jsons
        (reduce-kv
          (fn [acc k _] (export-subpaths acc k path)) pkg-jsons exports)))
    pkg-jsons pkg-jsons))

(defn pkg-json->main
  [[pkg-json-path {:as pkg-json :strs [name]}] path opts]
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
            (not (or (string/ends-with? entry-path ".js")
                     (string/ends-with? entry-path ".json")))
            (into [(str entry-path ".js") (str entry-path "/index.js") (str entry-path ".json")
                   (string/replace entry-path #"\.cjs$" ".js")])))))))

(defn- path->rel-name [path]
  (-> (subs path (.lastIndexOf path "node_modules"))
    (string/replace \\ \/)
    (string/replace #"node_modules[\\\/]" "")))

(defn path->provides
  [path pkg-jsons opts]
  (merge
    {:file        path
     :module-type :es6}
    ;; if the file is *not* a package.json, then compute what
    ;; namespaces it :provides to ClojureScript
    (when-not (package-json? path)
      (let [pkg-json-main (some #(pkg-json->main % path opts) pkg-jsons)]
        {:provides (let [module-rel-name (path->rel-name path)
                         provides        (cond-> [module-rel-name (string/replace module-rel-name #"\.js(on)?$" "")]
                                           (some? pkg-json-main) (conj pkg-json-main))
                         index-replaced  (string/replace module-rel-name #"[\\\/]index\.js(on)?$" "")]
                     (cond-> provides
                       (and (boolean (re-find #"[\\\/]index\.js(on)?$" module-rel-name))
                            (not (some #{index-replaced} provides)))
                       (conj index-replaced)))}))))

(defn node-file-seq->libs-spec*
  "Given a sequence of non-nested node_module paths where the extension ends in
  `.js/.json`, return lib-spec maps for each path containing at least :file,
   :module-type, and :provides."
  [module-fseq opts]
  (let [;; a map of all the *top-level* package.json paths and their exports
        ;; to the package.json contents as EDN
        pkg-jsons         (into {}
                            (comp (map #(.getAbsolutePath %))
                              (filter top-level-package-json?)
                              (map (fn [path] [path (json/read-str (slurp path))])))
                            module-fseq)
        pkg-jsons+exports (add-exports pkg-jsons)]
    (into []
      (comp
        (map #(.getAbsolutePath %))
        (map #(path->provides % pkg-jsons+exports opts)))
      module-fseq)))
