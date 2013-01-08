(ns cljs.io)

(def is-node? (if (try js/require (catch js/Error e nil)) true false))
(def node-fs (if is-node? (js/require "fs") nil))
(def node-path (if is-node? (js/require "path") nil))

(def path-separator (if is-node? (.-sep node-path) "/"))
(defprotocol IFile "Marker interface indicating a File")

(defn file-read
  [f]
  (let [path (if (satisfies? IFile f) (.getPath f) f)]
    (if is-node?
      (.toString (.readFileSync node-fs path))
      (let [req (doto (js/XMLHttpRequest.)
                      (.open "GET" path false))]
        (.send req) ;; false above mean synchronous/blocking      
        (if (= 200 (.-status req))
          (.-responseText req)
          (throw (js/Error. (str "Could not file-read: " path))))))))

(defn file-write
  [f data]
  (let [path (if (satisfies? IFile f) (.getPath f) f)]
    (if is-node?
      (.writeFileSync node-fs path data)
      (throw (js/Error. "No file-write capability defined for this JS environment")))))

(defn- file-exists?
  [path]
  (if is-node?
    (.existsSync node-fs path)
    (throw (js/Error. "No file-exists? capability defined for this JS environment"))))

(defn- file-is-dir?
  [path]
  (if is-node?
    (.isDirectory (.statSync node-fs path))
    (throw (js/Error. "No file-is-dir? capability defined for this JS environment"))))

(defn- file-readdir
  [path]
  (if is-node?
    (.readdirSync node-fs path)
    (throw (js/Error. "No file-readdir capability defined for this JS environment"))))

(defn- file-stat
  [path]
  (if is-node?
    (.statSync node-fs path)
    (throw (js/Error. "No file-stat capability defined for this JS environment"))))

(defn- path-normalize
  [path]
  (if is-node?
    (.normalize node-path path)
    (throw (js/Error. "No path-normalize capability defined for this JS environment"))))

(defn- path-resolve
  [path]
  (if is-node?
    (.resolve node-path path)
    (throw (js/Error. "No path-resolve capability defined for this JS environment"))))

(defn- path-dirname
  [path]
  (if is-node?
    (.dirname node-path path)
    (throw (js/Error. "No path-dirname capability defined for this JS environment"))))

(defn- path-basename
  [path]
  (if is-node?
    (.basename node-path path)
    (throw (js/Error. "No path-basename capability defined for this JS environment"))))

(defn- mkdir
  [path]
  (if is-node?
    (.mkdirSync node-fs path)
    (throw (js/Error. "No mkdir capability defined for this JS environment"))))

(defn- mkdirs
  [path]
  (let [components (.split path "/")]
    (loop [cur (str (first components) "/")
           left (next components)]
      (when (not (file-exists? cur))
        (mkdir cur))
      (when left
        (recur (str cur (first left) "/") (next left))))))

(deftype File [pathname]
  IFile

  Object
  (toString [me]
    (.getPath me))

  ;; Tests whether the file or directory denoted by this abstract
  ;; pathname exists
  (exists [me]
    (file-exists? (.getPath me)))

  ;; Returns the absolute form of this abstract pathname
  (getAbsoluteFile [me]
    (File. (.getAbsolutePath me)))

  ;; Returns the absolute pathname string of this abstract pathname
  (getAbsolutePath [me]
    (path-resolve (.getPath me)))

  ;; Returns the canonical form of this abstract pathname
  (getCanonicalFile [me]
    (.getAbsoluteFile me))

  ;; Returns the canonical pathname string of this abstract pathname
  (getCanonicalPath [me]
    (.getAbsolutePath me))

  ;; Returns the name of the file or directory denoted by this abstract
  ;; pathname.
  (getName [me]
    (path-basename (.getPath me)))

  ;; Returns the pathname string of this abstract pathname's parent,
  ;; or null if this pathname does not name a parent directory.
  (getParent [me]
    (path-dirname (.getPath me)))
        
  ;; Returns the abstract pathname of this abstract pathname's parent,
  ;; or null if this pathname does not name a parent directory.
  (getParentFile [me]
    (File. (.getParent me)))

  ;; Converts this abstract pathname into a pathname string
  (getPath [_]
    (path-normalize pathname))

  ;; Returns the time that the file denoted by this abstract pathname
  ;; was last modified
  (lastModified [_]
    (.getTime (.-mtime (file-stat pathname))))

  ;; Creates the directory named by this abstract pathname, including
  ;; any necessary but nonexistent parent directories
  (mkdirs [_]
    (mkdirs pathname))

  IPrintWithWriter
  (-pr-writer [me writer opts]
    (-write writer (str "#<File " (.getPath me) ">"))))

(defn file
  ([path]
    (let [path (if (satisfies? IFile path) (.getPath path) path)]
      (File. path)))
  ([parent path]
    (let [parent (if (satisfies? IFile parent) (.getPath parent) parent)
          path (if (satisfies? IFile path) (.getPath path) path)]
      (File. (str parent path-separator path)))))

(defn file-seq
  "A tree seq on cljs.io.Files"
  [^cljs.io.File dir]
  (tree-seq
   (fn [^cljs.io.File f]
     (file-is-dir? (.getPath f)))
   (fn [^cljs.io.File d]
     (map #(cljs.io.File. (str d path-separator %)) (file-readdir (.getPath d))))
   dir))


