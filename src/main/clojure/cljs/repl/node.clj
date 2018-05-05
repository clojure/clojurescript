;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl.node
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [cljs.util :as util]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.repl :as repl]
            [cljs.cli :as cli]
            [cljs.closure :as closure]
            [clojure.data.json :as json])
  (:import [java.net Socket]
           [java.lang StringBuilder]
           [java.io File BufferedReader BufferedWriter IOException]
           [java.lang ProcessBuilder Process]
           [java.util.concurrent ConcurrentHashMap LinkedBlockingQueue]))

(def lock (Object.))
(def results (ConcurrentHashMap.))
(def outs (ConcurrentHashMap.))
(def errs (ConcurrentHashMap.))

(defn thread-name []
  (let [name (.getName (Thread/currentThread))]
    (if (string/starts-with? name "nREPL") "main" name)))

(defn create-socket [^String host port]
  (let [socket (Socket. host (int port))
        in     (io/reader socket)
        out    (io/writer socket)]
    {:socket socket :in in :out out}))

(defn close-socket [s]
  (.close (:in s))
  (.close (:out s))
  (.close (:socket s)))

(defn write [^BufferedWriter out ^String js]
  (.write out js)
  (.write out (int 0)) ;; terminator
  (.flush out))

(defn ^String read-response [^BufferedReader in]
  (let [sb (StringBuilder.)]
    (loop [sb sb c (.read in)]
      (case c
        -1 (throw (IOException. "Stream closed"))
         0 (str sb)
         (do
           (.append sb (char c))
           (recur sb (.read in)))))))

(defn node-eval
  "Evaluate a JavaScript string in the Node REPL process."
  [repl-env js]
  (let [tname (thread-name)
        {:keys [out]} @(:socket repl-env)]
    (write out (json/write-str {:type "eval" :repl tname :form js}))
    (let [result (.take ^LinkedBlockingQueue (.get results tname))]
      (condp = (:status result)
        "success"
        {:status :success
         :value (:value result)}

        "exception"
        {:status :exception
         :value (:value result)}))))

(defn load-javascript
  "Load a Closure JavaScript file into the Node REPL process."
  [repl-env provides url]
  (node-eval repl-env
    (str "goog.require('" (comp/munge (first provides)) "')")))

(defn seq->js-array [v]
  (str "[" (apply str (interpose ", " (map pr-str v))) "]"))

(defn platform-path [v]
  (str "path.join.apply(null, " (seq->js-array v) ")"))

(defn- alive? [proc]
  (try (.exitValue proc) false (catch IllegalThreadStateException _ true)))

(defn- event-loop [^Process proc in]
  ;; we really do want system-default encoding here
  (while (alive? proc)
    (try
      (let [res (read-response in)]
        (try
          (let [{:keys [type repl value] :or {repl "main"} :as event}
                (json/read-str res :key-fn keyword)]
            (case type
              "result"
              (.offer (.get results repl) event)
              (when-let [stream (.get (if (= type "out") outs errs) repl)]
                (.write stream value 0 (.length ^String value))
                (.flush stream))))
          (catch Throwable _
            (.write *out* res 0 (.length res))
            (.flush *out*))))
      (catch IOException e
        (when (and (alive? proc) (not (.contains (.getMessage e) "Stream closed")))
          (.printStackTrace e *err*))))))

(defn- build-process
  [opts repl-env input-src]
  (let [xs   (cond-> [(get opts :node-command "node")]
               (:debug-port repl-env) (conj (str "--inspect=" (:debug-port repl-env))))
        proc (-> (ProcessBuilder. (into-array xs)) (.redirectInput input-src))]
    (when-let [path-fs (:path repl-env)]
      (.put (.environment proc)
            "NODE_PATH"
            (string/join File/pathSeparator
                         (map #(.getAbsolutePath (io/as-file %)) path-fs))))
    proc))

(defn setup
  ([repl-env] (setup repl-env nil))
  ([{:keys [host port socket state] :as repl-env} opts]
   (let [tname (thread-name)]
     (.put results tname (LinkedBlockingQueue.))
     (.put outs tname *out*)
     (.put errs tname *err*))
   (locking lock
     (when-not @socket
       (let [output-dir   (io/file (util/output-directory opts))
             _            (.mkdirs output-dir)
             of           (io/file output-dir "node_repl.js")
             _            (spit of
                            (string/replace (slurp (io/resource "cljs/repl/node_repl.js"))
                              "var PORT = 5001;"
                              (str "var PORT = " (:port repl-env) ";")))
             proc         (.start (build-process opts repl-env of))
             env          (ana/empty-env)
             core         (io/resource "cljs/core.cljs")
             ;; represent paths as vectors so we can emit JS arrays, this is to
             ;; paper over Windows issues with minimum hassle - David
             path         (.getPath (.getCanonicalFile output-dir))
             [fc & cs]    (rest (util/path-seq path)) ;; remove leading empty string
             root         (.substring path 0 (+ (.indexOf path fc) (count fc)))
             root-path    (vec (cons root cs))
             rewrite-path (conj root-path "goog")]
         (reset! (:proc repl-env) proc)
         (loop [r nil]
           (when-not (= r "ready")
             (Thread/sleep 50)
             (try
               (reset! socket (create-socket host port))
               (catch Exception e))
             (if @socket
               (recur (read-response (:in @socket)))
               (recur nil))))
         (.start (Thread. (bound-fn [] (event-loop proc (:in @socket)))))
         ;; compile cljs.core & its dependencies, goog/base.js must be available
         ;; for bootstrap to load, use new closure/compile as it can handle
         ;; resources in JARs
         (let [core-js (closure/compile core
                         (assoc opts :output-file
                                     (closure/src-file->target-file
                                       core (dissoc opts :output-dir))))
               deps    (closure/add-dependencies opts core-js)]
           ;; output unoptimized code and the deps file
           ;; for all compiled namespaces
           (apply closure/output-unoptimized
             (assoc opts
               :output-to (.getPath (io/file output-dir "node_repl_deps.js")))
             deps))
         ;; bootstrap, replace __dirname as __dirname won't be set
         ;; properly due to how we are running it - David
         (node-eval repl-env
           (-> (slurp (io/resource "cljs/bootstrap_nodejs.js"))
             (string/replace "path.resolve(__dirname, '..', 'base.js')"
               (platform-path (conj rewrite-path "bootstrap" ".." "base.js")))
             (string/replace
               "path.join(\".\", \"..\", src)"
               (str "path.join(" (platform-path rewrite-path) ", src)"))
             (string/replace
               "var CLJS_ROOT = \".\";"
               (str "var CLJS_ROOT = " (platform-path root-path) ";"))))
         ;; load the deps file so we can goog.require cljs.core etc.
         (node-eval repl-env
           (str "require("
             (platform-path (conj root-path "node_repl_deps.js"))
             ")"))
         ;; monkey-patch isProvided_ to avoid useless warnings - David
         (node-eval repl-env
           (str "goog.isProvided_ = function(x) { return false; };"))
         ;; monkey-patch goog.require, skip all the loaded checks
         (repl/evaluate-form repl-env env "<cljs repl>"
           '(set! (.-require js/goog)
              (fn [name]
                (js/CLOSURE_IMPORT_SCRIPT
                  (if (some? goog/debugLoader_)
                    (.getPathFromDeps_ goog/debugLoader_ name)
                    (unchecked-get (.. js/goog -dependencies_ -nameToPath) name))))))
         ;; load cljs.core, setup printing
         (repl/evaluate-form repl-env env "<cljs repl>"
           '(do
              (.require js/goog "cljs.core")
              (enable-console-print!)))
         ;; redef goog.require to track loaded libs
         (repl/evaluate-form repl-env env "<cljs repl>"
           '(do
              (set! *target* "nodejs")
              (set! *loaded-libs* #{"cljs.core"})
              (set! (.-require js/goog)
                (fn [name reload]
                  (when (or (not (contains? *loaded-libs* name)) reload)
                    (set! *loaded-libs* (conj (or *loaded-libs* #{}) name))
                    (js/CLOSURE_IMPORT_SCRIPT
                      (if (some? goog/debugLoader_)
                        (.getPathFromDeps_ goog/debugLoader_ name)
                        (unchecked-get (.. js/goog -dependencies_ -nameToPath) name))))))))
         (node-eval repl-env
           (str "goog.global.CLOSURE_UNCOMPILED_DEFINES = "
             (json/write-str (:closure-defines opts)) ";")))))
   (swap! state update :listeners inc)))

(defrecord NodeEnv [host port path socket proc state]
  repl/IReplEnvOptions
  (-repl-options [this]
    {:output-dir ".cljs_node_repl"
     :target :nodejs})
  repl/IParseError
  (-parse-error [_ err _]
    (assoc err :value nil))
  repl/IJavaScriptEnv
  (-setup [this opts]
    (setup this opts))
  (-evaluate [this filename line js]
    (node-eval this js))
  (-load [this provides url]
    (load-javascript this provides url))
  (-tear-down [this]
    (swap! state update :listeners dec)
    (let [tname (thread-name)]
      (.remove results tname)
      (.remove outs tname)
      (.remove errs tname))
    (locking lock
      (when (zero? (:listeners @state))
        (let [sock @socket]
          (when-not (.isClosed (:socket sock))
            (write (:out sock) ":cljs/quit")
            (while (alive? @proc) (Thread/sleep 50))
            (close-socket sock)))))))

(defn repl-env* [options]
  (let [{:keys [host port path debug-port]}
        (merge
          {:host "localhost"
           :port (+ 49000 (rand-int 10000))}
          options)]
    (assoc
      (NodeEnv. host port path
        (atom nil) (atom nil) (atom {:listeners 0}))
      :debug-port debug-port)))

(defn repl-env
  "Construct a Node.js evalution environment. Can supply :host, :port
  and :path (a vector used as the NODE_PATH)."
  [& {:as options}]
  (repl-env* options))

(defn -main [& args]
  (apply cli/main repl-env args))
