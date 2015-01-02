;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl
  (:refer-clojure :exclude [load-file])
  (:import java.io.File
           javax.xml.bind.DatatypeConverter)
  (:require [clojure.java.io :as io]
            [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.env :as env]
            [cljs.tagged-literals :as tags]
            [cljs.closure :as cljsc]
            [cljs.source-map :as sm]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [cljs.util :as util]))

(def ^:dynamic *cljs-verbose* false)

(defprotocol IJavaScriptEnv
  (-setup [this] [this opts] "initialize the environment")
  (-evaluate [this filename line js] "evaluate a javascript string")
  (-load [this provides url] "load code at url into the environment")
  (-tear-down [this] "dispose of the environment"))

(defn- env->opts
  "Returns a hash-map containing all of the entries in [repl-env], translating
:working-dir to :output-dir."
  [repl-env]
  ; some bits in cljs.closure use the options value as an ifn :-/
  (-> (into {} repl-env)
      (assoc :optimizations (get repl-env :optimizations :none))
      (assoc :output-dir (get repl-env :working-dir ".repl"))))

(defn load-namespace
  "Load a namespace and all of its dependencies into the evaluation environment.
  The environment is responsible for ensuring that each namespace is loaded once and
  only once."
  ([repl-env sym] (load-namespace repl-env sym nil))
  ([repl-env sym opts]
   (let [sym      (if (and (seq? sym)
                        (= (first sym) 'quote))
                    (second sym)
                    sym)
         ;; TODO: add pre-condition to source-on-disk, the
         ;; source must supply at least :url - David
         sources  (cljsc/add-dependencies
                    (merge (env->opts repl-env) opts)
                    {:requires [(name sym)] :type :seed
                     :url (:uri (cljsc/source-for-namespace
                                  sym env/*compiler*))})
         deps     (->> sources
                    (remove (comp #{["goog"]} :provides))
                    (remove (comp #{:seed} :type))
                    (map #(select-keys % [:provides :url])))]
     (if (:output-dir opts)
       ;; REPLs that read from :output-dir just need to add deps,
       ;; environment will handle actual loading - David
       (doseq [source (map #(cljsc/source-on-disk opts %) sources)]
         (-evaluate repl-env "<cljs repl>" 1
           (cljsc/add-dep-string opts source)))
       ;; REPLs that stream must manually load each dep - David
       (doseq [{:keys [url provides]} deps]
         (-load repl-env provides url))))))

(defn- load-dependencies
  ([repl-env requires] (load-dependencies repl-env requires nil))
  ([repl-env requires opts]
   (doseq [ns (distinct requires)]
     (load-namespace repl-env ns opts))))

(defn- display-error
  ([ret form]
     (display-error ret form (constantly nil)))
  ([ret form f]
     (when-not (and (seq? form) (= 'ns (first form)))
       (f)
       (println (:value ret))
       (when-let [st (:stacktrace ret)]
         (println st)))))

(defn evaluate-form
  "Evaluate a ClojureScript form in the JavaScript environment. Returns a
  string which is the ClojureScript return value. This string may or may
  not be readable by the Clojure reader."
  ([repl-env env filename form]
    (evaluate-form repl-env env filename form identity))
  ([repl-env env filename form wrap]
    (evaluate-form repl-env env filename form wrap nil))
  ([repl-env env filename form wrap opts]
    (try
      (binding [ana/*cljs-file* filename]
        (let [ast (ana/analyze env form opts)
              js (comp/emit-str ast)
              wrap-js
              ;; TODO: check opts as well - David
              (if (:source-map repl-env)
                (binding [comp/*source-map-data*
                          (atom {:source-map (sorted-map)
                                 :gen-col 0
                                 :gen-line 0})]
                  (let [js (comp/emit-str (ana/no-warn (ana/analyze env (wrap form) opts)))
                        t (System/currentTimeMillis)]
                    (str js
                      "\n//# sourceURL=repl-" t ".js"
                      "\n//# sourceMappingURL=data:application/json;base64,"
                      (DatatypeConverter/printBase64Binary
                        (.getBytes
                          (sm/encode
                            {(str "repl-" t ".cljs")
                             (:source-map @comp/*source-map-data*)}
                            {:lines (+ (:gen-line @comp/*source-map-data*) 3)
                             :file  (str "repl-" t ".js")
                             :sources-content
                                    [(or (:source (meta form))
                                       ;; handle strings / primitives without metadata
                                       (with-out-str (pr form)))]})
                          "UTF-8")))))
                (comp/emit-str (ana/no-warn (ana/analyze env (wrap form) opts))))]
          (when (= (:op ast) :ns)
            (load-dependencies repl-env
              (into (vals (:requires ast))
                (distinct (vals (:uses ast))))
              opts))
          (when *cljs-verbose*
            (print js))
          (let [ret (-evaluate repl-env filename (:line (meta form)) wrap-js)]
            (case (:status ret)
              ;;we eat ns errors because we know goog.provide() will throw when reloaded
              ;;TODO - file bug with google, this is bs error
              ;;this is what you get when you try to 'teach new developers'
              ;;via errors (goog/base.js 104)
              :error (display-error ret form)
              :exception (display-error ret form
                           #(prn "Error evaluating:" form :as js))
              :success (:value ret)))))
      (catch Throwable ex
        (.printStackTrace ex)
        (println (str ex))))))

(defn load-stream [repl-env filename res]
  (let [env (ana/empty-env)]
    (doseq [form (ana/forms-seq res filename)]
      (let [env (assoc env :ns (ana/get-namespace ana/*cljs-ns*))]
        (evaluate-form repl-env env filename form)))))

;; TODO: this should probably compile dependencies - David

(defn load-file
  ([repl-env f] (load-file repl-env f nil))
  ([repl-env f opts]
    (if (:output-dir opts)
      (let [src (if (util/url? f) f (io/resource f))
            compiled (cljsc/compile src
                       (assoc opts
                         :output-file
                         (cljsc/src-file->target-file src)))]
        (-evaluate repl-env f 1 (cljsc/add-dep-string opts compiled))
        (-evaluate repl-env f 1 (cljsc/src-file->goog-require src)))
      (binding [ana/*cljs-ns* 'cljs.user]
        (let [res (if (= \/ (first f)) f (io/resource f))]
          (assert res (str "Can't find " f " in classpath"))
          (load-stream repl-env f res))))))

(defn- wrap-fn [form]
  (cond (and (seq? form) (= 'ns (first form))) identity
        ('#{*1 *2 *3} form) (fn [x] `(cljs.core.pr-str ~x))
        :else (fn [x] `(cljs.core.pr-str
                       (let [ret# ~x]
                         (do (set! *3 *2)
                             (set! *2 *1)
                             (set! *1 ret#)
                             ret#))))))

(defn- eval-and-print [repl-env env form]
  (println
    (evaluate-form repl-env
      (assoc env :ns (ana/get-namespace ana/*cljs-ns*))
      "<cljs repl>"
      form
      (wrap-fn form))))

(def spec-sort
  {:as 0
   :refer 1
   :refer-macros 2
   :include-macros 3})

(defn merge-spec [[lib & {:as aindex}] [_ & {:as bindex}]]
  (let [merged-map
        (merge-with
          (fn [x y]
            (if (vector? x)
              (vec (distinct (into x y)))
              y))
          aindex bindex)]
    (apply vector lib
      (apply concat
        (sort
          (fn [[sa] [sb]]
            (compare (spec-sort sa) (spec-sort sb)))
          merged-map)))))

(defn merge-require [requires [lib :as spec]]
  (let [[before [match & after]]
        (split-with
          (fn [[lib' & _]]
            (not= lib lib'))
          requires)]
    (if (nil? match)
      ;; no match, append to end
      (concat requires [spec])
      (if (= match spec)
        ;; dupe
        requires
        ;; merge
        (concat before [(merge-spec match spec)] after)))))

(defn update-require-spec
  "Given the specification portion of a ns form and require spec additions
  return an updated specification."
  [specs & additions]
  (let [[before [requires & other-specs]]
        (split-with
          (fn [[x _]] (not= :require x))
          specs)
        requires'
        `(:require
           ~@(reduce
               (fn [requires spec]
                 (merge-require requires spec))
               (rest requires)
               additions))]
    (concat before [requires'] other-specs)))

(def default-special-fns
  (let [load-file-fn
        (fn self
          ([repl-env env form]
            (self repl-env env form nil))
          ([repl-env env [_ file :as form] opts]
            (load-file repl-env file opts)))]
    {'in-ns
     (fn self
       ([repl-env env form]
         (self repl-env env form nil))
       ([repl-env env [_ [quote ns-name] :as form] _]
         (when-not (ana/get-namespace ns-name)
           (swap! env/*compiler* assoc-in [::ana/namespaces ns-name] {:name ns-name})
           (-evaluate repl-env "<cljs repl>" 1
             (str "goog.provide('" (comp/munge ns-name) "');")))
         (set! ana/*cljs-ns* ns-name)))
     'require
     (fn self
       ([repl-env env form]
         (self repl-env env form nil))
       ([repl-env env [_ & specs :as form] opts]
         (let [new-specs
               (reduce
                 (fn [requires [quote spec]]
                   (update-require-spec requires
                     (if (symbol? spec) [spec] spec)))
                 (ana-api/ns-specs ana/*cljs-ns*) specs)]
           (evaluate-form repl-env env "<cljs repl>"
             (with-meta
               `(~'ns ~ana/*cljs-ns*
                  ~@new-specs)
               {:line 1 :column 1})
             identity opts))))
     'load-file load-file-fn
     'clojure.core/load-file load-file-fn
     'load-namespace
     (fn self
       ([repl-env env form]
         (self env repl-env form nil))
       ([repl-env env [_ ns :as form] opts]
         (load-namespace repl-env ns opts)))}))

(defn analyze-source
  "Given a source directory, analyzes all .cljs files. Used to populate
  (:cljs.analyzer/namespaces compiler-env) so as to support code reflection."
  [src-dir]
  (if-let [src-dir (and (not (empty? src-dir))
                     (File. src-dir))]
    (doseq [file (comp/cljs-files-in src-dir)]
      (ana/analyze-file (str "file://" (.getAbsolutePath file))))))

(defn repl*
  [repl-env {:keys [analyze-path verbose-repl warn-on-undeclared special-fns static-fns] :as opts
             :or {warn-on-undeclared true}}]
  (print "To quit, type: ")
  (prn :cljs/quit)
  (env/with-compiler-env
    (or (::env/compiler repl-env) (env/default-compiler-env opts))
    (binding [ana/*cljs-ns* 'cljs.user
              *cljs-verbose* verbose-repl
              ana/*cljs-warnings* (assoc ana/*cljs-warnings*
                                    :unprovided warn-on-undeclared
                                    :undeclared-var warn-on-undeclared
                                    :undeclared-ns warn-on-undeclared
                                    :undeclared-ns-form warn-on-undeclared)
              ana/*cljs-static-fns* static-fns]
      ;; TODO: the follow should become dead code when the REPL is
      ;; sufficiently enhanced to understand :cache-analysis - David
      (when analyze-path
        (analyze-source analyze-path))
      (let [env {:context :expr :locals {}}
            special-fns (merge default-special-fns special-fns)
            is-special-fn? (set (keys special-fns))
            read-error (Object.)]
        (if-not (nil? opts)
          (-setup repl-env opts)
          (-setup repl-env))
        (evaluate-form repl-env env "<cljs repl>"
          (with-meta
            '(ns cljs.user
               (:require [cljs.repl :refer-macros [doc]]))
            {:line 1 :column 1})
          identity opts)
        (loop []
          (print (str "ClojureScript:" ana/*cljs-ns* "> "))
          (flush)
          (let [rdr (readers/source-logging-push-back-reader
                      (java.io.PushbackReader. (io/reader *in*))
                      1
                      "NO_SOURCE_FILE")
                form (try
                       (binding [*ns* (create-ns ana/*cljs-ns*)
                                 reader/*data-readers* tags/*cljs-data-readers*
                                 reader/*alias-map*
                                 (apply merge
                                   ((juxt :requires :require-macros)
                                     (ana/get-namespace ana/*cljs-ns*)))]
                         (reader/read rdr nil read-error))
                       (catch Exception e
                         (println (.getMessage e))
                         read-error))]
            ;; TODO: need to catch errors here too - David
            (cond
              (identical? form read-error) (recur)
              (= form :cljs/quit) :quit

              (and (seq? form) (is-special-fn? (first form)))
              (do
                ((get special-fns (first form))
                  repl-env env form opts)
                (newline)
                (recur))

              :else
              (do (eval-and-print repl-env env form)
                  (recur)))))
        (-tear-down repl-env)))))

(defn repl
  "Note - repl will reload core.cljs every time, even if supplied old repl-env"
  [repl-env & {:as opts}]
  (repl* repl-env opts))

;; =============================================================================
;; ClojureScript REPL interaction support

(defmacro doc
  "Prints documentation for a var or special form given its name"
  [sym]
  `(cljs.repl/print-doc (meta (var ~sym))))
