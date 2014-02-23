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
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.env :as env]
            [cljs.tagged-literals :as tags]
            [cljs.closure :as cljsc]
            [cljs.source-map :as sm]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]))

(def ^:dynamic *cljs-verbose* false)

(defprotocol IJavaScriptEnv
  (-setup [this] "initialize the environment")
  (-evaluate [this filename line js] "evaluate a javascript string")
  (-load [this ns url] "load code at url into the environment")
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
  [repl-env sym]
  (let [sym (if (and (seq? sym)
                     (= (first sym) 'quote))
              (second sym)
              sym)
        deps (->> (cljsc/add-dependencies (env->opts repl-env)
                                          {:requires [(name sym)] :type :seed})
                  (remove (comp #{["goog"]} :provides))
                  (remove (comp #{:seed} :type))
                  (map #(select-keys % [:provides :url])))]
    (doseq [{:keys [url provides]} deps]
      (-load repl-env provides url))))

(defn- load-dependencies
  [repl-env requires]
  (doseq [ns requires]
    (load-namespace repl-env ns)))

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
     (try
       (binding [ana/*cljs-file* filename]
         (let [ast (ana/analyze env form)
               js (comp/emit-str ast)
               wrap-js
               (if (:source-map repl-env)
                 (binding [comp/*source-map-data*
                           (atom {:source-map (sorted-map)
                                  :gen-col 0
                                  :gen-line 0})]
                   (let [js (comp/emit-str (ana/no-warn (ana/analyze env (wrap form))))
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
                 (comp/emit-str (ana/no-warn (ana/analyze env (wrap form)))))]
           (when (= (:op ast) :ns)
             (load-dependencies repl-env (into (vals (:requires ast))
                                               (distinct (vals (:uses ast))))))
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

(defn load-file
  [repl-env f]
  (binding [ana/*cljs-ns* 'cljs.user]
    (let [res (if (= \/ (first f)) f (io/resource f))]
      (assert res (str "Can't find " f " in classpath"))
      (load-stream repl-env f res))))

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
  (let [ret (evaluate-form repl-env
                           (assoc env :ns (ana/get-namespace ana/*cljs-ns*))
                           "<cljs repl>"
                           form
                           (wrap-fn form))]
    (try (prn (read-string ret))
         (catch Exception e
           (if (string? ret)
             (println ret)
             (prn nil))))))

(def default-special-fns
  (let [load-file-fn (fn [repl-env file] (load-file repl-env file))]
    {'in-ns (fn [_ quoted-ns]
              (let [ns-name (second quoted-ns)]
                (when-not (ana/get-namespace ns-name)
                  (swap! env/*compiler* update-in [::ana/namespaces ns-name] {:name ns-name}))
                (set! ana/*cljs-ns* ns-name)))
     'load-file load-file-fn
     'clojure.core/load-file load-file-fn
     'load-namespace (fn [repl-env ns] (load-namespace repl-env ns))}))

(defn analyze-source
  "Given a source directory, analyzes all .cljs files. Used to populate
  (:cljs.analyzer/namespaces compiler-env) so as to support code reflection."
  [src-dir]
  (if-let [src-dir (and (not (empty? src-dir))
                     (File. src-dir))]
    (doseq [file (comp/cljs-files-in src-dir)]
      (ana/analyze-file (str "file://" (.getAbsolutePath file))))))

(defn repl
  "Note - repl will reload core.cljs every time, even if supplied old repl-env"
  [repl-env & {:keys [analyze-path verbose warn-on-undeclared special-fns static-fns] :as opts
               :or {warn-on-undeclared true}}]
  (print "To quit, type: ")
  (prn :cljs/quit)
  (env/with-compiler-env
    (or (::env/compiler repl-env) (env/default-compiler-env opts))
    (binding [ana/*cljs-ns* 'cljs.user
              *cljs-verbose* verbose
              ana/*cljs-warnings* (assoc ana/*cljs-warnings*
                                    :unprovided warn-on-undeclared
                                    :undeclared-var warn-on-undeclared
                                    :undeclared-ns warn-on-undeclared
                                    :undeclared-ns-form warn-on-undeclared)
              ana/*cljs-static-fns* static-fns]
      (when analyze-path
        (analyze-source analyze-path))
      (let [env {:context :expr :locals {}}
            special-fns (merge default-special-fns special-fns)
            is-special-fn? (set (keys special-fns))
            read-error (Object.)]
        (-setup repl-env)
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
            (cond
             (identical? form read-error) (recur)
             (= form :cljs/quit) :quit

             (and (seq? form) (is-special-fn? (first form)))
             (do (apply (get special-fns (first form)) repl-env (rest form))
                 (newline)
                 (recur))

             :else
             (do (eval-and-print repl-env env form)
                 (recur)))))
        (-tear-down repl-env)))))
