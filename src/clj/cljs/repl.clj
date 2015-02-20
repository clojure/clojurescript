;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl
  (:refer-clojure :exclude [load-file])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.stacktrace :as trace]
            [clojure.repl :as cljrepl]
            [cljs.util :as util]
            [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.env :as env]
            [cljs.tagged-literals :as tags]
            [cljs.closure :as cljsc]
            [cljs.source-map :as sm])
  (:import [java.io File PushbackReader FileWriter]
           [java.net URL]
           [javax.xml.bind DatatypeConverter]
           [clojure.lang LineNumberingPushbackReader]))

(def ^:dynamic *cljs-verbose* false)
(def ^:dynamic *repl-opts* nil)

;; =============================================================================
;; Copied over from clojure.main

(defn skip-if-eol
  "If the next character on stream s is a newline, skips it, otherwise
  leaves the stream untouched. Returns :line-start, :stream-end, or :body
  to indicate the relative location of the next character on s. The stream
  must either be an instance of LineNumberingPushbackReader or duplicate
  its behavior of both supporting .unread and collapsing all of CR, LF, and
  CRLF to a single \\newline."
  [s]
  (let [c (.read s)]
    (case c
      \newline :line-start
      nil :stream-end
      (do (.unread s c) :body))))

(defn skip-whitespace
  "Skips whitespace characters on stream s. Returns :line-start, :stream-end,
  or :body to indicate the relative location of the next character on s.
  Interprets comma as whitespace and semicolon as comment to end of line.
  Does not interpret #! as comment to end of line because only one
  character of lookahead is available. The stream must either be an
  instance of LineNumberingPushbackReader or duplicate its behavior of both
  supporting .unread and collapsing all of CR, LF, and CRLF to a single
  \\newline."
  [s]
  (loop [c (.read s)]
    (case c
      \newline :line-start
      nil :stream-end
      \; (do (.readLine s) :line-start)
      (if (or (Character/isWhitespace c) (identical? c \,))
        (recur (.read s))
        (do (.unread s c) :body)))))

(defn repl-read
  "Default :read hook for repl. Reads from *in* which must either be an
  instance of LineNumberingPushbackReader or duplicate its behavior of both
  supporting .unread and collapsing all of CR, LF, and CRLF into a single
  \\newline. repl-read:
    - skips whitespace, then
      - returns request-prompt on start of line, or
      - returns request-exit on end of stream, or
      - reads an object from the input stream, then
        - skips the next input character if it's end of line, then
        - returns the object."
  [request-prompt request-exit]
  (or ({:line-start request-prompt :stream-end request-exit}
        (skip-whitespace *in*))
    (let [input (read)]
      (skip-if-eol *in*)
      input)))

(defmacro with-read-known
  "Evaluates body with *read-eval* set to a \"known\" value,
   i.e. substituting true for :unknown if necessary."
  [& body]
  `(binding [*read-eval* (if (= :unknown *read-eval*) true *read-eval*)]
     ~@body))

;; =============================================================================
;; CLJS Specifics

(defprotocol IReplEnvOptions
  (-repl-options [repl-env] "Return default REPL options for a REPL Env"))

(defprotocol IJavaScriptEnv
  (-setup [repl-env opts] "initialize the environment")
  (-evaluate [repl-env filename line js] "evaluate a javascript string")
  (-load [repl-env provides url] "load code at url into the environment")
  (-tear-down [repl-env] "dispose of the environment"))

(extend-type
  Object
  IReplEnvOptions
  (-repl-options [_] nil))

(defprotocol IParseStacktrace
  (-parse-stacktrace [repl-env stacktrace error build-options]
    "Given the original JavaScript stacktrace string, the entire original error
     value and current compiler build options, parse the stacktrace into the
     canonical form:

     [{:file <string>
       :function <string>
       :line <integer>
       :column <integer>}*]

     :file must be a URL path (without protocol) relative to :output-dir."))

(defprotocol IPrintStacktrace
  (-print-stacktrace [repl-env stacktrace error build-options]
    "Implementing REPL evaluation environments are given the opportunity to
     print the mapped stacktrace themselves. This permits further processing."))

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

(defn read-source-map
  "Return the source map for the JavaScript source file."
  [f]
  (let [smf (io/file (str f ".map"))]
    (when (.exists smf)
      (sm/decode (json/read-str (slurp smf) :key-fn keyword)))))

(defn ^File js-src->cljs-src
  "Map a JavaScript output file back to the original ClojureScript source
   file."
  [f]
  (let [f (io/file f)
        dir (.getParentFile f)
        name (.getName f)]
    (io/file dir (string/replace name ".js" ".cljs"))))

(defn ns-info
  "Given a path to a js source file return the ns info for the corresponding
   ClojureScript file if it exists."
  [f]
  (let [f' (js-src->cljs-src f)]
    (when (.exists f')
      (ana/parse-ns f'))))

(defn mapped-line-and-column
  "Given a cljs.source-map source map data structure map a generated line
   and column back to the original line and column."
  [source-map line column]
  (let [default [line column]]
    ;; source maps are 0 indexed for lines
    (if-let [columns (get source-map (dec line))]
      (vec
        (map inc
          (map
            ;; source maps are 0 indexed for columns
            ;; multiple segments may exist at column
            ;; the last segment seems most accurate
            (last
              (if-let [mapping (get columns (dec column))]
                mapping
                (second (first columns))))
            [:line :col])))
      default)))

(defn mapped-stacktrace
  "Given a vector representing the canonicalized JavaScript stacktrace
   return the ClojureScript stacktrace. The canonical stacktrace must be
   in the form:

    [{:file <string>
      :function <string>
      :line <integer>
      :column <integer>}*]

   :file must be a URL path (without protocol) relative to :output-dir. The
   returned mapped stacktrace will also contain :url entries to the original
   sources if it can be determined from the classpath."
  ([stacktrace] (mapped-stacktrace stacktrace nil))
  ([stacktrace opts]
    (let [read-source-map' (memoize read-source-map)
          ns-info' (memoize ns-info)]
      (vec
        (for [{:keys [function file line column] :as frame} stacktrace]
          ;; need to convert file, a relative URL style path, to host-specific file
          (let [rfile (io/file (URL. (.toURL (io/file (util/output-directory opts))) file))
                [sm {:keys [ns source-file] :as ns-info}]
                ((juxt read-source-map' ns-info') rfile)
                [line' column'] (if ns-info
                                  (mapped-line-and-column sm line column)
                                  [line column])
                name' (if (and ns-info function)
                        (symbol (name ns) (cljrepl/demunge function))
                        function)
                file' (string/replace
                        (.getCanonicalFile
                          (if ns-info
                            source-file
                            (io/file rfile)))
                        (str (System/getProperty "user.dir") File/separator) "")
                url   (or (and ns-info (io/resource (util/ns->relpath ns)))
                          (io/resource file))]
            (merge
              {:function name'
               :file     (io/file file')
               :line     line'
               :column   column'}
              (when url
                {:url url}))))))))

(defn print-mapped-stacktrace
  "Given a vector representing the canonicalized JavaScript stacktrace
   print the ClojureScript stacktrace. See mapped-stacktrace."
  ([stacktrace] (print-mapped-stacktrace stacktrace *repl-opts*))
  ([stacktrace opts]
    (doseq [{:keys [function file line column]}
            (mapped-stacktrace stacktrace opts)]
      ((:print opts) "\t" (str function " (" file ":" line ":" column ")")))))

(comment
  (cljsc/build "samples/hello/src"
    {:optimizations :none
     :output-dir "samples/hello/out"
     :output-to "samples/hello/out/hello.js"
     :source-map true})

  (mapped-stacktrace
    [{:file "hello/core.js"
      :function "first"
      :line 2
      :column 1}]
    {:output-dir "samples/hello/out"})

  (print-mapped-stacktrace
    [{:file "hello/core.js"
      :function "first"
      :line 2
      :column 1}]
    {:output-dir "samples/hello/out"})

  ;; URL example

  (cljsc/build "samples/hello/src"
    {:optimizations :none
     :output-dir "out"
     :output-to "out/hello.js"
     :source-map true})

  (mapped-stacktrace
    [{:file "cljs/core.js"
      :function "first"
      :line 2
      :column 1}]
    {:output-dir "out"})

  (print-mapped-stacktrace
    [{:file "cljs/core.js"
      :function "first"
      :line 2
      :column 1}]
    {:output-dir "out"})
  )

(defn- display-error
  ([repl-env ret form opts]
    (display-error repl-env ret form (constantly nil) opts))
  ([repl-env ret form f opts]
    (f)
    ((:print opts) (:value ret))
    (when-let [st (:stacktrace ret)]
      (if (and (true? (:source-map opts))
               (satisfies? IParseStacktrace repl-env))
        (let [cst (-parse-stacktrace repl-env st ret opts)]
          (if (vector? cst)
            (if (satisfies? IPrintStacktrace repl-env)
              (-print-stacktrace repl-env cst ret opts)
              (print-mapped-stacktrace cst opts))
            ((:print opts) st)))
        ((:print opts) st))
      ((:flush opts)))))

(defn evaluate-form
  "Evaluate a ClojureScript form in the JavaScript environment. Returns a
  string which is the ClojureScript return value. This string may or may
  not be readable by the Clojure reader."
  ([repl-env env filename form]
    (evaluate-form repl-env env filename form identity))
  ([repl-env env filename form wrap]
    (evaluate-form repl-env env filename form wrap *repl-opts*))
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
            ((:print opts) js))
          (let [ret (-evaluate repl-env filename (:line (meta form)) wrap-js)]
            (case (:status ret)
              :error (display-error repl-env ret form opts)
              :exception (display-error repl-env ret form
                           (if (:repl-verbose opts)
                             #(prn "Error evaluating:" form :as js)
                             (constantly nil))
                           opts)
              :success (:value ret)))))
      (catch Throwable ex
        (.printStackTrace ex)
        ((:print opts) (str ex))
        ((:flush opts))))))

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
        (let [res (if (= File/separatorChar (first f)) f (io/resource f))]
          (assert res (str "Can't find " f " in classpath"))
          (load-stream repl-env f res))))))

(defn- wrap-fn [form]
  (cond (and (seq? form) (= 'ns (first form))) identity
        ('#{*1 *2 *3 *e} form) (fn [x] `(cljs.core.pr-str ~x))
        :else (fn [x] `(cljs.core.pr-str
                         (try
                           (let [ret# ~x]
                             (set! *3 *2)
                             (set! *2 *1)
                             (set! *1 ret#)
                             ret#)
                           (catch :default e#
                             (set! *e e#)
                             (throw e#)))))))

(defn- eval-cljs
  "Given a REPL evaluation environment, an analysis environment, and a
   form, evaluate the form and return the result. The result is always the value
   represented as a string."
  ([repl-env env form]
    (eval-cljs repl-env env form nil))
  ([repl-env env form opts]
   (evaluate-form repl-env
     (assoc env :ns (ana/get-namespace ana/*cljs-ns*))
     "<cljs repl>"
     form
     (wrap-fn form)
     opts)))

;; Special REPL fns, these provide compatiblity with Clojure functions
;; that are not possible to reproduce given ClojureScript's compilation model
;; All functions should have the following signature
;;
;; (fn self
;;   ([repl-env env form]
;;     (self repl-env env form))
;;   ([repl-env env form opts]
;;     ..))
;;
;; repl-env - IJavaScriptEnv instance
;; env      - a cljs.analyzer environment, *not* cljs.env environment
;; form     - complete form entered at the repl
;; opts     - REPL options, essentially augmented cljs.closure/build options

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
         ;; guard against craziness like '5 which wreaks havoc
         (when-not (and (= quote 'quote) (symbol? ns-name))
           (throw (IllegalArgumentException. "Argument to in-ns must be a symbol.")))
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
         (evaluate-form repl-env env "<cljs repl>"
           (with-meta
             `(~'ns ~ana/*cljs-ns*
                (:require
                  ~@(map
                      (fn [quoted-spec-or-kw]
                        (if (keyword? quoted-spec-or-kw)
                          quoted-spec-or-kw
                          (second quoted-spec-or-kw)))
                      specs)))
             {:merge true :line 1 :column 1})
           identity opts)))
     'require-macros
     (fn self
       ([repl-env env form]
         (self repl-env env form nil))
       ([repl-env env [_ & specs :as form] opts]
         (evaluate-form repl-env env "<cljs repl>"
           (with-meta
             `(~'ns ~ana/*cljs-ns*
                (:require-macros
                  ~@(map
                      (fn [quoted-spec-or-kw]
                        (if (keyword? quoted-spec-or-kw)
                          quoted-spec-or-kw
                          (second quoted-spec-or-kw)))
                      specs)))
             {:merge true :line 1 :column 1})
           identity opts)))
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
  ([src-dir] (analyze-source src-dir nil))
  ([src-dir opts]
    (if-let [src-dir (and (not (empty? src-dir))
                       (File. src-dir))]
      (doseq [file (comp/cljs-files-in src-dir)]
        (ana/analyze-file (str "file://" (.getAbsolutePath file)) opts)))))

(defn repl-prompt []
  (print (str "ClojureScript:" ana/*cljs-ns* "> ")))

(defn repl-caught [e]
  )

(defn repl*
  [repl-env {:keys [init need-prompt prompt flush read eval print caught]
             :or {init        #()
                  need-prompt (if (instance? LineNumberingPushbackReader *in*)
                                #(.atLineStart ^LineNumberingPushbackReader *in*)
                                #(identity true))
                  prompt      repl-prompt
                  flush       flush
                  read        repl-read
                  eval        eval-cljs
                  print       println
                  caught      repl-caught}
             :as opts}]
  (print "To quit, type: " :cljs/quit)
  (let [ups-deps (cljsc/get-upstream-deps)
        {:keys [analyze-path repl-verbose warn-on-undeclared special-fns static-fns] :as opts
         :or   {warn-on-undeclared true}}
        (assoc (merge (-repl-options repl-env) opts)
          :print print
          :flush flush
          :ups-libs (:libs ups-deps)
          :ups-foreign-libs (:foreign-libs ups-deps))]
    (env/with-compiler-env
     (or (::env/compiler repl-env) (env/default-compiler-env opts))
     (binding [ana/*cljs-ns* 'cljs.user
               *cljs-verbose* repl-verbose
               ana/*cljs-warnings* (assoc ana/*cljs-warnings*
                                     :unprovided warn-on-undeclared
                                     :undeclared-var warn-on-undeclared
                                     :undeclared-ns warn-on-undeclared
                                     :undeclared-ns-form warn-on-undeclared)
               ana/*cljs-static-fns* static-fns
               *repl-opts* opts]
       ;; TODO: the follow should become dead code when the REPL is
       ;; sufficiently enhanced to understand :cache-analysis - David
       (let [env {:context :expr :locals {}}
             special-fns (merge default-special-fns special-fns)
             is-special-fn? (set (keys special-fns))
             request-prompt (Object.)
             request-exit (Object.)
             read-error (Object.)
             opts (if-let [merge-opts (:merge-opts (-setup repl-env opts))]
                    (merge opts merge-opts)
                    opts)]
         (comp/with-core-cljs opts
           (fn []
             (when analyze-path
               (analyze-source analyze-path opts))
             (evaluate-form repl-env env "<cljs repl>"
               (with-meta
                 '(ns cljs.user
                    (:require [cljs.repl :refer-macros [doc]]))
                 {:line 1 :column 1})
               identity opts)
             (when-let [src (:watch opts)]
               (future
                 (let [log-file (io/file (util/output-directory opts) "watch.log")]
                   (print "Watch compilation log available at:" (str log-file))
                   (binding [*out* (FileWriter. log-file)]
                     (cljsc/watch src (dissoc opts :watch))))))
             (loop []
               ;; try to let things flush before printing prompt
               (Thread/sleep 10)
               (prompt)
               (flush)
               (let [rdr (readers/source-logging-push-back-reader
                           (PushbackReader. (io/reader *in*))
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
                              (print (.getMessage e))
                              read-error))]
                 ;; TODO: need to catch errors here too - David
                 (cond
                   (identical? form read-error) (recur)
                   (= form :cljs/quit) :quit

                   (and (seq? form) (is-special-fn? (first form)))
                   (do
                     (try
                       ((get special-fns (first form)) repl-env env form opts)
                       (catch Throwable ex
                         (print "Failed to execute special function:" (pr-str (first form)))
                         (trace/print-cause-trace ex 12)))
                     ;; flush output which could include stack traces
                     (flush)
                     (print)
                     (recur))

                   :else
                   (let [value (eval repl-env env form opts)]
                     (print value)
                     (recur)))))))
         (-tear-down repl-env))))))

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
