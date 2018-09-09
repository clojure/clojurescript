;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl
  (:refer-clojure :exclude [load load-file])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.data.json :as json]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [cljs.tagged-literals :as tags]
            [clojure.edn :as edn]
            [cljs.util :as util]
            [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.env :as env]
            [cljs.js-deps :as deps]
            [cljs.closure :as cljsc]
            [cljs.source-map :as sm])
  (:import [java.io File PushbackReader FileWriter PrintWriter]
           [java.net URL]
           [java.util Base64]
           [java.util.concurrent.atomic AtomicLong]
           [clojure.lang IExceptionInfo]
           [java.util.regex Pattern]
           [com.google.common.base Throwables]))

(def ^:dynamic *cljs-verbose* false)
(def ^:dynamic *repl-opts* nil)
(def ^:dynamic *repl-env* nil)

(def known-repl-opts
  "Set of all known REPL options."
  #{:analyze-path :bind-err :caught :compiler-env :def-emits-var :eval :flush
    :init :inits :need-prompt :print :print-no-newline :prompt :quit-prompt :read
    :reader :repl-requires :repl-verbose :source-map-inline :watch :watch-fn
    :wrap})

(defmacro ^:private err-out [& body]
  `(binding [*out* *err*]
     ~@body))

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
  (let [c (readers/read-char s)]
    (case c
      \newline :line-start
      nil :stream-end
      (do (readers/unread s c) :body))))

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
  (loop [c (readers/read-char s)]
    (case c
      \newline :line-start
      nil :stream-end
      \; (do (readers/read-line s) :line-start)
      (if (or (Character/isWhitespace c) (identical? c \,))
        (recur (readers/read-char s))
        (do (readers/unread s c) :body)))))

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
  ([request-prompt request-exit]
   (repl-read request-prompt request-exit *repl-opts*))
  ([request-prompt request-exit opts]
   (let [current-in *in*
         bind-in?   (true? (:source-map-inline opts))]
     (binding [*in* (if bind-in?
                      ((:reader opts))
                      *in*)]
       (or ({:line-start request-prompt :stream-end request-exit}
             (skip-whitespace *in*))
         (let [input (reader/read {:read-cond :allow :features #{:cljs}} *in*)]
           ;; Transfer 1-char buffer to original *in*
           (readers/unread current-in (readers/read-char *in*))
           (skip-if-eol (if bind-in? current-in *in*))
           input))))))

;; =============================================================================
;; CLJS Specifics

(defprotocol IReplEnvOptions
  (-repl-options [repl-env] "Return default REPL options for a REPL Env"))

(defn repl-options [repl-env]
  (-repl-options repl-env))

(defprotocol IJavaScriptEnv
  (-setup [repl-env opts] "initialize the environment")
  (-evaluate [repl-env filename line js] "evaluate a javascript string")
  (-load [repl-env provides url] "load code at url into the environment")
  (-tear-down [repl-env] "dispose of the environment"))

(defn setup [repl-env opts]
  (-setup repl-env opts))

(defn evaluate [repl-env filename line js]
  (-evaluate repl-env filename line js))

(defn load [repl-env provides url]
  (-load repl-env provides url))

(defn tear-down [repl-env]
  (-tear-down repl-env))

(extend-type
  Object
  IReplEnvOptions
  (-repl-options [_] nil))

(defprotocol IParseError
  (-parse-error [repl-env error build-options]
    "Given the original JavaScript error return the error to actually
     use."))

(defprotocol IGetError
  (-get-error [repl-env name env build-options]
    "Given a symbol representing a var holding an error, an analysis
     environment, and the REPL/compiler options return the canonical error
     representation:

     {:value <string>
      :stacktrace <string>}

    :value should be the host environment JavaScript error message string.
    :stacktrace should be the host JavaScript environment stacktrace string."))

(defprotocol IParseStacktrace
  (-parse-stacktrace [repl-env stacktrace error build-options]
    "Given the original JavaScript stacktrace string, the entire original error
     value and current compiler build options, parse the stacktrace into the
     canonical form:

     [{:file <string>
       :function <string>
       :line <integer>
       :column <integer>}*]

     :file must be a URL path (without protocol) relative to :output-dir. If
     no source file can be supplied (such as REPL defs), :file may be a custom
     identifier string surrounded by angle brackets, i.e. \"<cljs repl>\"."))

(defprotocol IPrintStacktrace
  (-print-stacktrace [repl-env stacktrace error build-options]
    "Implementing REPL evaluation environments are given the opportunity to
     print the mapped stacktrace themselves. This permits further processing."))

(defn- env->opts
  "Returns a hash-map containing all of the entries in [repl-env], translating
:working-dir to :output-dir."
  ([repl-env] (env->opts repl-env nil))
  ([repl-env opts]
    ;; some bits in cljs.closure use the options value as an ifn :-/
   (-> (into {} repl-env)
     (assoc :optimizations
            (or (:optimizations opts) (get repl-env :optimizations :none)))
     (assoc :output-dir
            (or (:output-dir opts) (get repl-env :working-dir ".repl"))))))

(defn add-url [ijs]
  (cond-> ijs
    (not (contains? ijs :url))
    (assoc :url (io/resource (:file ijs)))))

(defn ns->input [ns opts]
  (or (some-> (util/ns->source ns) (ana/parse-ns opts))
      (some-> (get-in @env/*compiler* [:js-dependency-index (str ns)]) add-url)
      (some-> (deps/find-classpath-lib ns))
      (throw
        (ex-info (str ns " does not exist")
          {::error :invalid-ns}))))

(defn compilable? [input]
  (contains? input :source-file))

(defn- load-sources
  "Load the compiled `sources` into the REPL."
  [repl-env sources opts]
  (if (:output-dir opts)
    ;; REPLs that read from :output-dir just need to add deps,
    ;; environment will handle actual loading - David
    (let [sb (StringBuffer.)]
      (doseq [source sources]
        (with-open [rdr (io/reader (:url source))]
          (.append sb (cljsc/add-dep-string opts source))))
      (when (:repl-verbose opts)
        (println (.toString sb)))
      (-evaluate repl-env "<cljs repl>" 1 (.toString sb)))
    ;; REPLs that stream must manually load each dep - David
    (doseq [{:keys [url provides]} sources]
      (-load repl-env provides url))))

(defn- load-cljs-loader
  "Compile and load the cljs.loader namespace if it's present in `sources`."
  [repl-env sources opts]
  (when-let [source (first (filter #(= (:ns %) 'cljs.loader) sources))]
    (cljsc/compile-loader sources opts)
    (load-sources repl-env [source] opts)))

(defn load-namespace
  "Load a namespace and all of its dependencies into the evaluation environment.
  The environment is responsible for ensuring that each namespace is
  loaded once and only once. Returns the compiled sources."
  ([repl-env ns] (load-namespace repl-env ns nil))
  ([repl-env ns opts]
   (let [ns      (if (and (seq? ns) (= (first ns) 'quote)) (second ns) ns)
         sources (seq
                   (when-not (ana/node-module-dep? ns)
                     (let [input (ns->input ns opts)]
                       (if (compilable? input)
                         (->> (cljsc/compile-inputs [input]
                                (merge (env->opts repl-env) opts))
                           (remove (comp #{["goog"]} :provides)))
                         (map #(cljsc/source-on-disk opts %)
                              (cljsc/add-js-sources [input] opts))))))]
     (when (:repl-verbose opts)
       (println (str "load-namespace " ns " , compiled:") (map :provides sources)))
     (load-sources repl-env sources opts)
     sources)))

(defn- load-dependencies
  "Compile and load the given `requires` and return the compiled sources."
  ([repl-env requires]
   (load-dependencies repl-env requires nil))
  ([repl-env requires opts]
   (doall (mapcat #(load-namespace repl-env % opts) (distinct requires)))))

(defn ^File js-src->cljs-src
  "Map a JavaScript output file back to the original ClojureScript source
   file (.cljs or .cljc)."
  [f]
  (let [f (io/file f)
        dir (.getParentFile f)
        base-name (string/replace (.getName f) ".js" "")
        cljsf (io/file dir (str base-name ".cljs"))]
    (if (.exists cljsf)
      cljsf
      (let [cljcf (io/file dir (str base-name ".cljc"))]
        (if (.exists cljcf)
          cljcf)))))

(defn read-source-map
  "Return the source map for the JavaScript source file."
  [f]
  (when-let [smf (util/file-or-resource (str f ".map"))]
    (let [ns (if (= f "cljs/core.aot.js")
               'cljs.core
               (some-> (js-src->cljs-src f) ana/parse-ns :ns))]
      (when ns
        (as-> @env/*compiler* compiler-env
          (let [t (util/last-modified smf)]
            (if (or (and (= ns 'cljs.core)
                         (nil? (get-in compiler-env [::source-maps ns])))
                    (and (not= ns 'cljs.core)
                         (> t (get-in compiler-env [::source-maps ns :last-modified] 0))))
              (swap! env/*compiler* assoc-in [::source-maps ns]
                {:last-modified t
                 :source-map (sm/decode (json/read-str (slurp smf) :key-fn keyword))})
              compiler-env))
          (get-in compiler-env [::source-maps ns :source-map]))))))

(defn ns-info
  "Given a path to a js source file return the ns info for the corresponding
   ClojureScript file if it exists."
  [f]
  (let [f' (js-src->cljs-src f)]
    (when (and f' (.exists f'))
      (ana/parse-ns f'))))

(defn- mapped-line-column-call
  "Given a cljs.source-map source map data structure map a generated line
   and column back to the original line, column, and function called."
  [source-map line column]
  (let [default [line column nil]]
    ;; source maps are 0 indexed for lines
    (if-let [columns (get source-map (dec line))]
      (vec
        (map #(%1 %2)
          [inc inc identity]
          (map
            ;; source maps are 0 indexed for columns
            ;; multiple segments may exist at column
            ;; the last segment seems most accurate
            (last
              (or
                (get columns (last (filter #(<= % (dec column)) (sort (keys columns)))))
                (second (first columns))))
            [:line :col :name])))
      default)))

(defn- mapped-frame
  "Given opts and a canonicalized JavaScript stacktrace frame, return the
  ClojureScript frame."
  [{:keys [function file line column]} opts]
  (let [no-source-file? (if-not file
                          true
                          (.startsWith file "<"))
        rfile (when-not no-source-file?
                (io/file (URL. (.toURL (io/file (util/output-directory opts))) file)))
        [sm {:keys [ns source-file] :as ns-info}]
        (when-not no-source-file?
          ((juxt read-source-map ns-info) rfile))
        [line' column' call] (if ns-info
                               (mapped-line-column-call sm line column)
                               [line column])
        name' (when (and ns-info function)
                function)
        file' (if no-source-file?
                file
                (string/replace
                  (.getCanonicalFile
                    (if ns-info
                      source-file
                      (io/file rfile)))
                  (str (System/getProperty "user.dir") File/separator) ""))
        url (or (and ns-info (util/ns->source ns))
                (and file (io/resource file)))]
    (merge
      {:function name'
       :call     call
       :file     (if no-source-file?
                   (str "<NO_SOURCE_FILE>"
                        (when file
                          (str " " file)))
                   (io/file file'))
       :line     line'
       :column   column'}
      (when url
        {:url url}))))

(defn mapped-stacktrace
  "Given a vector representing the canonicalized JavaScript stacktrace
   return the ClojureScript stacktrace. The canonical stacktrace must be
   in the form:

    [{:file <string>
      :function <string>
      :line <integer>
      :column <integer>}*]

   :file must be a URL path (without protocol) relative to :output-dir or a
   identifier delimited by angle brackets. The returned mapped stacktrace will
   also contain :url entries to the original sources if it can be determined
   from the classpath."
  ([stacktrace] (mapped-stacktrace stacktrace nil))
  ([stacktrace opts]
   (vec
     (let [mapped-frames (map (memoize #(mapped-frame % opts)) stacktrace)]
       ;; take each non-nil :call and optionally merge it into :function one-level up
       ;; to avoid replacing with local symbols, we only replace munged name if we can munge call symbol back to it
       (map #(merge-with (fn [munged-fn-name unmunged-call-name]
                           (if (= munged-fn-name (string/replace (cljs.compiler/munge unmunged-call-name) "." "$"))
                             unmunged-call-name
                             munged-fn-name)) %1 %2)
         (map #(dissoc % :call) mapped-frames)
         (concat (rest (map #(if (:call %)
                              (hash-map :function (:call %))
                              {})
                         mapped-frames)) [{}]))))))

(defn file-display
  [file {:keys [output-dir temp-output-dir?]}]
  (if temp-output-dir?
    (let [canonicalize (fn [file] (.getCanonicalPath (io/file file)))
          can-file (canonicalize file)
          can-out (canonicalize output-dir)]
      (if (.startsWith can-file can-out)
        (subs can-file (inc (count can-out)))
        (subs can-file (inc (.lastIndexOf can-file java.io.File/separator)))))
    file))

(defn print-mapped-stacktrace
  "Given a vector representing the canonicalized JavaScript stacktrace
   print the ClojureScript stacktrace. See mapped-stacktrace."
  ([stacktrace] (print-mapped-stacktrace stacktrace *repl-opts*))
  ([stacktrace opts]
   (doseq [{:keys [function file line column]}
           (mapped-stacktrace stacktrace opts)]
     (err-out
       (println "\t"
         (str (when function (str function " "))
           "(" (file-display file opts) (when line (str ":" line)) (when column (str ":" column)) ")"))))))

(comment
  (def st (env/default-compiler-env))

  (cljsc/build "samples/hello/src"
    {:optimizations :none
     :output-dir "samples/hello/out"
     :output-to "samples/hello/out/hello.js"
     :source-map true}
    st)

  (env/with-compiler-env st
    (mapped-stacktrace
      [{:file "hello/core.js"
        :function "first"
        :line 6
        :column 0}]
      {:output-dir "samples/hello/out"}))

  (env/with-compiler-env st
    (print-mapped-stacktrace
      [{:file "hello/core.js"
        :function "first"
        :line 6
        :column 0}]
      {:output-dir "samples/hello/out"}))

  ;; URL example

  (cljsc/build "samples/hello/src"
    {:optimizations :none
     :output-dir "out"
     :output-to "out/hello.js"
     :source-map true}
    st)

  (env/with-compiler-env st
    (mapped-stacktrace
      [{:file "cljs/core.js"
        :function "first"
        :line 2
        :column 1}]
      {:output-dir "out"}))

  (env/with-compiler-env st
    (print-mapped-stacktrace
      [{:file "cljs/core.js"
        :function "first"
        :line 2
        :column 1}]
      {:output-dir "out"}))
  )

(defn- display-error
  ([repl-env ret form opts]
   (display-error repl-env ret form (constantly nil) opts))
  ([repl-env ret form f opts]
   (err-out
     (f)
     (when-let [value (:value ret)]
       (println value))
     (when-let [st (:stacktrace ret)]
       (if (and (true? (:source-map opts))
             (satisfies? IParseStacktrace repl-env))
         (let [cst (try
                     (-parse-stacktrace repl-env st ret opts)
                     (catch Throwable e
                       (when (:repl-verbose opts)
                         (println "Failed to canonicalize stacktrace")
                         (println (Throwables/getStackTraceAsString e)))))]
           (if (vector? cst)
             (if (satisfies? IPrintStacktrace repl-env)
               (-print-stacktrace repl-env cst ret opts)
               (print-mapped-stacktrace cst opts))
             (println st)))
         (println st))))))

(defn- bytes-to-base64-str
  "Convert a byte array into a base-64 encoded string."
  [^bytes bytes]
  (.encodeToString (Base64/getEncoder) bytes))

(defn evaluate-form
  "Evaluate a ClojureScript form in the JavaScript environment. Returns a
  string which is the ClojureScript return value. This string may or may
  not be readable by the Clojure reader."
  ([repl-env env filename form]
    (evaluate-form repl-env env filename form identity))
  ([repl-env env filename form wrap]
    (evaluate-form repl-env env filename form wrap *repl-opts*))
  ([repl-env env filename form wrap opts]
   (binding [ana/*cljs-file* filename]
     (let [env (merge env
                 {:root-source-info {:source-type :fragment
                                     :source-form form}
                  :repl-env repl-env})
           def-emits-var (:def-emits-var opts)
           backup-comp @env/*compiler*
           ->ast (fn [form]
                   (binding [ana/*analyze-deps* false]
                     (ana/analyze (assoc env :def-emits-var def-emits-var)
                       (wrap form) nil opts)))
           ast (->ast form)
           ast (if-not (#{:ns :ns*} (:op ast))
                 ast
                 (let [ijs (ana/parse-ns [form])]
                   (cljsc/handle-js-modules opts
                     (deps/dependency-order
                       (cljsc/add-dependency-sources [ijs] opts))
                     env/*compiler*)
                   (binding [ana/*check-alias-dupes* false]
                     (ana/no-warn (->ast form))))) ;; need new AST after we know what the modules are - David
           wrap-js
           ;; TODO: check opts as well - David
           (if (:source-map repl-env)
             (binding [comp/*source-map-data*
                       (atom {:source-map (sorted-map)
                              :gen-line 0})
                       comp/*source-map-data-gen-col* (AtomicLong.)]
               (let [js (comp/emit-str ast)
                     t (System/currentTimeMillis)]
                 (str js
                   "\n//# sourceURL=repl-" t ".js"
                   "\n//# sourceMappingURL=data:application/json;base64,"
                   (bytes-to-base64-str
                     (.getBytes
                       (sm/encode
                         {(str "repl-" t ".cljs")
                          (:source-map @comp/*source-map-data*)}
                         {:lines (+ (:gen-line @comp/*source-map-data*) 3)
                          :file (str "repl-" t ".js")
                          :sources-content
                          [(or (:source (meta form))
                             ;; handle strings / primitives without metadata
                             (with-out-str (pr form)))]})
                       "UTF-8")))))
             (comp/emit-str ast))]
       ;; NOTE: means macros which expand to ns aren't supported for now
       ;; when eval'ing individual forms at the REPL - David
       (when (#{:ns :ns*} (:op ast))
         (let [ast (try
                     (ana/no-warn (ana/analyze env form nil opts))
                     (catch #?(:clj Exception :cljs js/Error) e
                         (reset! env/*compiler* backup-comp)
                         (throw e)))
               sources (load-dependencies repl-env
                         (into (vals (:requires ast))
                               (distinct (vals (:uses ast))))
                         opts)]
           (load-cljs-loader repl-env sources opts)))
       (when *cljs-verbose*
         (err-out (println wrap-js)))
       (let [ret (-evaluate repl-env filename (:line (meta form)) wrap-js)]
         (case (:status ret)
           :error (throw
                    (ex-info (:value ret)
                      {:type :js-eval-error
                       :error ret
                       :repl-env repl-env
                       :form form}))
           :exception (throw
                        (ex-info (:value ret)
                          {:type :js-eval-exception
                           :error ret
                           :repl-env repl-env
                           :form form
                           :js wrap-js}))
           :success (:value ret)))))))

(defn load-stream [repl-env filename res]
  (let [env (ana/empty-env)]
    (with-open [rdr (io/reader res)]
      (doseq [form (ana/forms-seq* rdr filename)]
        (let [env (assoc env :ns (ana/get-namespace ana/*cljs-ns*))]
          (evaluate-form repl-env env filename form))))))

(defn load-file
  ([repl-env f] (load-file repl-env f *repl-opts*))
  ([repl-env f opts]
    (if (:output-dir opts)
      (let [src (cond
                  (util/url? f) f
                  (.exists (io/file f)) (io/file f)
                  :else (io/resource f))
            compiled (binding [ana/*reload-macros* true]
                       (cljsc/compile src
                         (assoc opts
                           :output-file (cljsc/src-file->target-file src)
                           :force true
                           :mode :interactive)))]
        ;; copy over the original source file if source maps enabled
        (when-let [ns (and (:source-map opts) (first (:provides compiled)))]
          (spit
            (io/file (io/file (util/output-directory opts))
              (util/ns->relpath ns (util/ext (:source-url compiled))))
            (slurp src)))
        ;; need to load dependencies first
        (let [sources (load-dependencies repl-env (:requires compiled) opts)]
          (load-cljs-loader repl-env (conj sources compiled) opts))
        (-evaluate repl-env f 1 (cljsc/add-dep-string opts compiled))
        (-evaluate repl-env f 1
          (cljsc/src-file->goog-require src
            {:wrap true :reload true :macros-ns (:macros-ns compiled)})))
      (binding [ana/*cljs-ns* ana/*cljs-ns*]
        (let [res (if (= File/separatorChar (first f)) f (io/resource f))]
          (assert res (str "Can't find " f " in classpath"))
          (load-stream repl-env f res))))))

(defn- root-resource
  "Returns the root directory path for a lib"
  {:tag String}
  [lib]
  (str \/
       (.. (name lib)
           (replace \- \_)
           (replace \. \/))))

(defn- root-directory
  "Returns the root resource path for a lib"
  [lib]
  (let [d (root-resource lib)]
    (subs d 0 (.lastIndexOf d "/"))))

(defn- load-path->cp-path
  [path]
  (let [src (if (= File/separatorChar (first path))
              path
              (str (root-directory ana/*cljs-ns*) \/ path))
        src (.substring src 1)]
    (or (io/resource (str src ".cljs"))
        (io/resource (str src ".cljc")))))

(defn- wrap-fn [form]
  (cond
    (and (seq? form)
         (#{'ns 'require 'require-macros
            'use 'use-macros 'import 'refer-clojure} (first form)))
    identity

    ('#{*1 *2 *3 *e} form) (fn [x] `(cljs.core.pr-str ~x))
    :else
    (fn [x]
      `(try
         (cljs.core.pr-str
           (let [ret# ~x]
             (set! *3 *2)
             (set! *2 *1)
             (set! *1 ret#)
             ret#))
         (catch :default e#
           (set! *e e#)
           (throw e#))))))

(defn- init-wrap-fn [form]
  (cond
    (and (seq? form)
      (#{'ns 'require 'require-macros
         'use 'use-macros 'import 'refer-clojure} (first form)))
    identity

    ('#{*1 *2 *3 *e} form) (fn [x] `(cljs.core.pr-str ~x))
    :else
    (fn [x]
      `(cljs.core.pr-str ~x))))

(defn eval-cljs
  "Given a REPL evaluation environment, an analysis environment, and a
   form, evaluate the form and return the result. The result is always the value
   represented as a string."
  ([repl-env env form]
    (eval-cljs repl-env env form *repl-opts*))
  ([repl-env env form opts]
   (evaluate-form repl-env
     (assoc env :ns (ana/get-namespace ana/*cljs-ns*))
     "<cljs repl>"
     form
     ;; the pluggability of :wrap is needed for older JS runtimes like Rhino
     ;; where catching the error will swallow the original trace
     ((or (:wrap opts) wrap-fn) form)
     opts)))

(defn decorate-specs [specs]
  (if-let [k (some #{:reload :reload-all} specs)]
    (->> specs (remove #{k}) (map #(vary-meta % assoc :reload k)))
    specs))

(comment
  (ana/canonicalize-specs
    '['foo.bar '[bar.core :as bar]])

  (ana/canonicalize-specs
    '['foo.bar '[bar.core :as bar] :reload])

  (map meta
    (decorate-specs
      (ana/canonicalize-specs
        '['foo.bar '[bar.core :as bar] :reload])))
  )

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

(defn- wrap-self
  "Takes a self-ish fn and returns it wrapped with exception handling.
  Compiler state is restored if self-ish fn fails."
  [f]
  (fn g
    ([a b c]
     (g a b c nil))
    ([a b c d]
     (let [backup-comp @env/*compiler*]
       (try
         (apply f [a b c d])
         (catch #?(:clj Exception :cljs js/Error) e ;;Exception
           (reset! env/*compiler* backup-comp)
           (throw e)))))))

(defn- wrap-special-fns
  [wfn fns]
  "Wrap wfn around all (fn) values in fns hashmap."
  (into {} (for [[k v] fns] [k (wfn v)])))

(def default-special-fns
  (let [load-file-fn
        (fn self
          ([repl-env env form]
            (self repl-env env form nil))
          ([repl-env env [_ file :as form] opts]
            (load-file repl-env file opts)))
        in-ns-fn
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
        load-fn
        (fn self
          ([repl-env env form]
           (self env repl-env form nil))
          ([repl-env env [_ & paths :as form] opts]
           (let [cp-paths (map load-path->cp-path paths)]
             (run! #(load-file repl-env % opts) cp-paths))))]
    (wrap-special-fns wrap-self
     {'in-ns in-ns-fn
      'clojure.core/in-ns in-ns-fn
      'load-file load-file-fn
      'clojure.core/load-file load-file-fn
      'load-namespace
      (fn self
        ([repl-env env form]
         (self env repl-env form nil))
        ([repl-env env [_ ns :as form] opts]
         (load-namespace repl-env ns opts)))
      'load load-fn
      'clojure.core/load load-fn})))

(defn analyze-source
  "Given a source directory, analyzes all .cljs files. Used to populate
  (:cljs.analyzer/namespaces compiler-env) so as to support code reflection."
  ([src-dir] (analyze-source src-dir nil))
  ([src-dir opts]
    (if-let [src-dir (and (not (empty? src-dir))
                       (File. src-dir))]
      (doseq [file (comp/cljs-files-in src-dir)]
        (ana/analyze-file (str "file://" (.getAbsolutePath file)) opts)))))

(defn repl-title []
  (when-not (util/synthetic-version?)
    (println "ClojureScript" (util/clojurescript-version))))

(defn repl-quit-prompt []
  (println "To quit, type:" :cljs/quit))

(defn repl-prompt []
  (print (str ana/*cljs-ns* "=> ")))

(defn repl-caught [e repl-env opts]
  (if (and (instance? IExceptionInfo e)
           (#{:js-eval-error :js-eval-exception} (:type (ex-data e))))
    (let [{:keys [type repl-env error form js]} (ex-data e)]
      (case type
        :js-eval-error
        (display-error repl-env error form opts)

        :js-eval-exception
        (display-error repl-env error form
          (if (:repl-verbose opts)
            #(prn "Error evaluating:" form :as js)
            (constantly nil))
          opts)))
    (.printStackTrace e *err*)))

(defn repl-nil? [x]
  (boolean (#{"" "nil"} x)))

(defn run-inits [renv inits]
  (doseq [{:keys [type] :as init} inits]
    (case type
      :init-forms
      (doseq [form (:forms init)]
        (eval-cljs renv (ana/empty-env) form))
      :eval-forms
      (binding [*repl-opts* (merge *repl-opts* {:def-emits-var true :wrap init-wrap-fn})]
        (doseq [form (:forms init)]
          (let [value (eval-cljs renv (ana/empty-env) form *repl-opts*)]
            (when-not (repl-nil? value)
              (println value)))))
      :init-script
      (let [script (:script init)]
        (load-stream renv (util/get-name script) script)))))

(defn maybe-install-npm-deps [opts]
  (when (:install-deps opts)
    (cljsc/check-npm-deps opts)
    (swap! env/*compiler* update-in [:npm-deps-installed?]
      (fn [installed?]
        (if-not installed?
          (cljsc/maybe-install-node-deps! opts)
          installed?)))))

(defn initial-prompt [quit-prompt prompt]
  (quit-prompt)
  (prompt)
  (flush))

(defn repl*
  [repl-env {:keys [init inits need-prompt quit-prompt prompt flush read eval print caught reader
                    print-no-newline source-map-inline wrap repl-requires ::fast-initial-prompt?
                    compiler-env bind-err]
             :or {need-prompt #(if (readers/indexing-reader? *in*)
                                (== (readers/get-column-number *in*) 1)
                                (identity true))
                  fast-initial-prompt? false
                  quit-prompt repl-title
                  prompt repl-prompt
                  flush flush
                  read repl-read
                  eval eval-cljs
                  print println
                  caught repl-caught
                  reader #(readers/source-logging-push-back-reader
                           *in*
                           1 "<NO_SOURCE_FILE>")
                  print-no-newline print
                  source-map-inline true
                  repl-requires '[[cljs.repl :refer-macros [source doc find-doc apropos dir pst]]
                                  [cljs.pprint :refer [pprint] :refer-macros [pp]]]
                  bind-err true}
             :as opts}]
  (doseq [[unknown-opt suggested-opt] (util/unknown-opts (set (keys opts)) (set/union known-repl-opts cljsc/known-opts))]
    (when suggested-opt
      (println (str "WARNING: Unknown option '" unknown-opt "'. Did you mean '" suggested-opt "'?"))))
  (when fast-initial-prompt?
    (initial-prompt quit-prompt prompt))
  (let [repl-opts (-repl-options repl-env)
        repl-requires (into repl-requires (:repl-requires repl-opts))
        {:keys [analyze-path repl-verbose warn-on-undeclared special-fns
                checked-arrays static-fns fn-invoke-direct]
         :as opts
         :or   {warn-on-undeclared true}}
        (merge
          {:def-emits-var true}
          (cljsc/add-implicit-options
            (merge-with (fn [a b] (if (nil? b) a b))
              repl-opts
              opts
              {:prompt prompt
               :need-prompt need-prompt
               :flush flush
               :read read
               :print print
               :caught caught
               :reader reader
               :print-no-newline print-no-newline
               :source-map-inline source-map-inline})))
        done? (atom false)]
    (env/with-compiler-env (or compiler-env env/*compiler* (env/default-compiler-env opts))
     (when (:source-map opts)
       (.start (Thread. (bound-fn [] (read-source-map "cljs/core.aot.js")))))
     (binding [*repl-env* repl-env
               ana/*unchecked-if* false
               ana/*unchecked-arrays* false
               *err* (if bind-err
                       (cond-> *out*
                         (not (instance? PrintWriter *out*)) (PrintWriter.))
                       *err*)
               ana/*cljs-ns* ana/*cljs-ns*
               *cljs-verbose* repl-verbose
               ana/*cljs-warnings*
               (let [warnings (opts :warnings)]
                 (merge
                   ana/*cljs-warnings*
                   (if (or (true? warnings)
                           (false? warnings))
                     (zipmap (keys ana/*cljs-warnings*) (repeat warnings))
                     warnings)
                   (zipmap
                     [:unprovided :undeclared-var
                      :undeclared-ns :undeclared-ns-form]
                     (repeat (if (false? warnings)
                               false
                               warn-on-undeclared)))
                   {:infer-warning false}))
               ana/*checked-arrays* checked-arrays
               ana/*cljs-static-fns* static-fns
               ana/*fn-invoke-direct* (and static-fns fn-invoke-direct)
               *repl-opts* opts]
       (try
         (let [env {:context :expr :locals {}}
               special-fns (merge default-special-fns special-fns)
               is-special-fn? (set (keys special-fns))
               request-prompt (Object.)
               request-exit (Object.)
               opts (comp/with-core-cljs opts
                      (fn []
                        (if-let [merge-opts (:merge-opts (-setup repl-env opts))]
                          (merge opts merge-opts)
                          opts)))
               init (do
                      (evaluate-form repl-env env "<cljs repl>"
                        `(~'set! ~'cljs.core/*print-namespace-maps* true)
                        identity opts)
                      (or init
                        #(evaluate-form repl-env env "<cljs repl>"
                           (with-meta
                             `(~'ns ~'cljs.user
                                (:require ~@repl-requires))
                             {:line 1 :column 1})
                           identity opts)))
               maybe-load-user-file #(when-let [user-resource (util/ns->source 'user)]
                                       (when (= "file" (.getProtocol ^URL user-resource))
                                         (load-file repl-env (io/file user-resource) opts)))
               read-eval-print
               (fn []
                 (let [input (binding [*ns* (create-ns ana/*cljs-ns*)
                                       reader/resolve-symbol ana/resolve-symbol
                                       reader/*data-readers* tags/*cljs-data-readers*
                                       reader/*alias-map*
                                       (apply merge
                                         ((juxt :requires :require-macros)
                                           (ana/get-namespace ana/*cljs-ns*)))]
                               (read request-prompt request-exit))]
                   (or ({request-exit request-exit
                         :cljs/quit request-exit
                         request-prompt request-prompt} input)
                     (if (and (seq? input) (is-special-fn? (first input)))
                       (do
                         ((get special-fns (first input)) repl-env env input opts)
                         (print nil))
                       (let [value (eval repl-env env input opts)]
                         (print value))))))]
           (maybe-install-npm-deps opts)
           (comp/with-core-cljs opts
             (fn []
               (binding [*repl-opts* opts]
                 (try
                   (when analyze-path
                     (if (vector? analyze-path)
                       (run! #(analyze-source % opts) analyze-path)
                       (analyze-source analyze-path opts)))
                   (init)
                   (run-inits repl-env inits)
                   (maybe-load-user-file)
                   (catch Throwable e
                     (caught e repl-env opts)))
                 (when-let [src (:watch opts)]
                   (.start
                     (Thread.
                       ((ns-resolve 'clojure.core 'binding-conveyor-fn)
                         (fn []
                           (let [log-file (io/file (util/output-directory opts) "watch.log")]
                             (err-out (println "Watch compilation log available at:" (str log-file)))
                             (try
                               (let [log-out (FileWriter. log-file)]
                                 (binding [*err* log-out
                                           *out* log-out]
                                   (cljsc/watch src (dissoc opts :watch)
                                     env/*compiler* done?)))
                               (catch Throwable e
                                 (caught e repl-env opts)))))))))
                 ;; let any setup async messages flush
                 (Thread/sleep 50)
                 (binding [*in* (if (true? (:source-map-inline opts))
                                  *in*
                                  (reader))]
                   (when-not fast-initial-prompt?
                     (initial-prompt quit-prompt prompt))
                   (loop []
                     (when-not
                       (try
                         (identical? (read-eval-print) request-exit)
                         (catch Throwable e
                           (caught e repl-env opts)
                           nil))
                       (when (need-prompt)
                         (prompt)
                         (flush))
                       (recur))))))))
         (finally
           (reset! done? true)
           (-tear-down repl-env)))))))

(defn repl
  "Generic, reusable, read-eval-print loop. By default, reads from *in* using
  a c.t.r.reader-types/source-logging-push-back-reader,
  writes to *out*, and prints exception summaries to *err*. If you use the
  default :read hook, *in* must either be an instance of
  c.t.r.reader-types/PushbackReader or duplicate its behavior of both supporting
  unread and collapsing CR, LF, and CRLF into a single \\newline. Options
  are sequential keyword-value pairs. The first argument is the JavaScript
  evaluation environment, the second argument is an extended version of the
  standard ClojureScript compiler options. In addition to ClojureScript compiler
  build options it also take a set of options similar to clojure.main/repl with
  adjustments for ClojureScript evalution and compilation model:

  Available clojure.main/repl style options and their defaults:

     - :init, function of no arguments, initialization hook called with
       bindings for set!-able vars in place.
       default: #()

     - :need-prompt, function of no arguments, called before each
       read-eval-print except the first, the user will be prompted if it
       returns true.
       default: #(if (c.t.r.readers-types/indexing-reader? *in*)
                   (== (c.t.r.reader-types/get-column-number *in*) 1)
                   (identity true))

     - :prompt, function of no arguments, prompts for more input.
       default: repl-prompt

     - :flush, function of no arguments, flushes output
       default: flush

     - :read, function of two arguments, reads from *in*:
         - returns its first argument to request a fresh prompt
           - depending on need-prompt, this may cause the repl to prompt
             before reading again
         - returns its second argument to request an exit from the repl
         - else returns the next object read from the input stream
       default: repl-read

     - :eval, function of one argument, returns the evaluation of its
       argument. The eval function must take repl-env, the JavaScript evaluation
       environment, env, the ClojureScript analysis environment, the form
       and opts, the standard ClojureScript REPL/compiler options.
       default: eval

     - :print, function of one argument, prints its argument to the output
       default: println

     - :caught, function of three arguments, a throwable, called when
       read, eval, or print throws an exception or error default. The second
       argument is the JavaScript evaluation environment this permits context
       sensitive handling if necessary. The third argument is opts, the standard
       ClojureScript REPL/compiler options. In the case of errors or exception
       in the JavaScript target, these will be thrown as
       clojure.lang.IExceptionInfo instances.
       default: repl-caught

     - :reader, the c.t.r reader to use.
       default: c.t.r.reader-types/source-logging-push-back-reader

     - :print-no-newline, print without a newline.
       default: print

     - :source-map-inline, whether inline source maps should be enabled. Most
       useful in browser context. Implies using a fresh reader for each form.
       default: true"
  [repl-env & opts]
  (assert (even? (count opts))
    "Arguments after repl-env must be interleaved key value pairs")
  (repl* repl-env (apply hash-map opts)))

;; =============================================================================
;; ClojureScript REPL interaction support

(def special-doc-map
  '{. {:forms [(.instanceMethod instance args*)
               (.-instanceField instance)]
       :doc "The instance member form works for methods and fields.
  They all expand into calls to the dot operator at macroexpansion time."}
    ns {:forms [(name docstring? attr-map? references*)]
        :doc "You must currently use the ns form only with the following caveats

    * You must use the :only form of :use
    * :require supports :as, :refer, and :rename
      - all options can be skipped
      - in this case a symbol can be used as a libspec directly
        - that is, (:require lib.foo) and (:require [lib.foo]) are both
          supported and mean the same thing
      - :rename specifies a map from referred var names to different
        symbols (and can be used to prevent clashes)
      - prefix lists are not supported
    * The only options for :refer-clojure are :exclude and :rename
    * :import is available for importing Google Closure classes
      - ClojureScript types and records should be brought in with :use
        or :require :refer, not :import ed
    * Macros must be defined in a different compilation stage than the one
      from where they are consumed. One way to achieve this is to define
      them in one namespace and use them from another. They are referenced
      via the :require-macros / :use-macros options to ns
      - :require-macros and :use-macros support the same forms that
        :require and :use do

  Implicit macro loading: If a namespace is required or used, and that
  namespace itself requires or uses macros from its own namespace, then
  the macros will be implicitly required or used using the same
  specifications. Furthermore, in this case, macro vars may be included
  in a :refer or :only spec. This oftentimes leads to simplified library
  usage, such that the consuming namespace need not be concerned about
  explicitly distinguishing between whether certain vars are functions
  or macros. For example:

  (ns testme.core (:require [cljs.test :as test :refer [test-var deftest]]))

  will result in test/is resolving properly, along with the test-var
  function and the deftest macro being available unqualified.

  Inline macro specification: As a convenience, :require can be given
  either :include-macros true or :refer-macros [syms...]. Both desugar
  into forms which explicitly load the matching Clojure file containing
  macros. (This works independently of whether the namespace being
  required internally requires or uses its own macros.) For example:

  (ns testme.core
  (:require [foo.core :as foo :refer [foo-fn] :include-macros true]
            [woz.core :as woz :refer [woz-fn] :refer-macros [app jx]]))

  is sugar for

  (ns testme.core
  (:require [foo.core :as foo :refer [foo-fn]]
            [woz.core :as woz :refer [woz-fn]])
  (:require-macros [foo.core :as foo]
                   [woz.core :as woz :refer [app jx]]))

  Auto-aliasing clojure namespaces: If a non-existing clojure.* namespace
  is required or used and a matching cljs.* namespace exists, the cljs.*
  namespace will be loaded and an alias will be automatically established
  from the clojure.* namespace to the cljs.* namespace. For example:

  (ns testme.core (:require [clojure.test]))

  will be automatically converted to

  (ns testme.core (:require [cljs.test :as clojure.test]))"}
    def {:forms [(def symbol doc-string? init?)]
         :doc "Creates and interns a global var with the name
  of symbol in the current namespace (*ns*) or locates such a var if
  it already exists.  If init is supplied, it is evaluated, and the
  root binding of the var is set to the resulting value.  If init is
  not supplied, the root binding of the var is unaffected."}
    do {:forms [(do exprs*)]
        :doc "Evaluates the expressions in order and returns the value of
  the last. If no expressions are supplied, returns nil."}
    if {:forms [(if test then else?)]
        :doc "Evaluates test. If not the singular values nil or false,
  evaluates and yields then, otherwise, evaluates and yields else. If
  else is not supplied it defaults to nil."}
    new {:forms [(Constructor. args*) (new Constructor args*)]
         :url "java_interop#new"
         :doc "The args, if any, are evaluated from left to right, and
  passed to the JavaScript constructor. The constructed object is
  returned."}
    quote {:forms [(quote form)]
           :doc "Yields the unevaluated form."}
    recur {:forms [(recur exprs*)]
           :doc "Evaluates the exprs in order, then, in parallel, rebinds
  the bindings of the recursion point to the values of the exprs.
  Execution then jumps back to the recursion point, a loop or fn method."}
    set! {:forms[(set! var-symbol expr)
                 (set! (.- instance-expr instanceFieldName-symbol) expr)]
          :url "vars#set"
          :doc "Used to set vars and JavaScript object fields"}
    throw {:forms [(throw expr)]
           :doc "The expr is evaluated and thrown."}
    try {:forms [(try expr* catch-clause* finally-clause?)]
         :doc "catch-clause => (catch classname name expr*)
  finally-clause => (finally expr*)
  Catches and handles JavaScript exceptions."}
    var {:forms [(var symbol)]
         :doc "The symbol must resolve to a var, and the Var object
itself (not its value) is returned. The reader macro #'x expands to (var x)."}})

(defn- special-doc [name-symbol]
  (assoc (or (special-doc-map name-symbol) (meta (resolve name-symbol)))
    :name name-symbol
    :special-form true))

(def repl-special-doc-map
  '{in-ns {:arglists ([name])
           :doc "Sets *cljs-ns* to the namespace named by the symbol, creating it if needed."}
    load-file {:arglists ([name])
               :doc "Sequentially read and evaluate the set of forms contained in the file."}
    load {:arglists ([& paths])
               :doc "Loads Clojure code from resources in classpath. A path is interpreted as
  classpath-relative if it begins with a slash or relative to the root
  directory for the current namespace otherwise."}})

(defn- repl-special-doc [name-symbol]
  (assoc (repl-special-doc-map name-symbol)
    :name name-symbol
    :repl-special-function true))

(defmacro doc
  "Prints documentation for a var or special form given its name,
  or for a spec if given a keyword"
  [name]
  `(print
     (binding [cljs.core/*print-newline* true]
       (with-out-str
         ~(if-let [special-name ('{& fn catch try finally try} name)]
            `(doc ~special-name)
            (cond
              (special-doc-map name)
              `(cljs.repl/print-doc (quote ~(special-doc name)))

              (repl-special-doc-map name)
              `(cljs.repl/print-doc (quote ~(repl-special-doc name)))

              (keyword? name)
              `(cljs.repl/print-doc {:spec ~name :doc (cljs.spec.alpha/describe ~name)})

              (ana-api/find-ns name)
              `(cljs.repl/print-doc
                 (quote ~(select-keys (ana-api/find-ns name) [:name :doc])))

              (ana-api/resolve &env name)
              `(cljs.repl/print-doc
                 (quote ~(let [var (ana-api/resolve &env name)
                               m (select-keys var
                                   [:ns :name :doc :forms :arglists :macro :url])]
                           (cond-> (update-in m [:name] clojure.core/name)
                             (:protocol-symbol var)
                             (assoc :protocol true
                                    :methods
                                    (->> (get-in var [:protocol-info :methods])
                                      (map (fn [[fname sigs]]
                                             [fname {:doc (:doc
                                                            (ana-api/resolve &env
                                                              (symbol (str (:ns var)) (str fname))))
                                                     :arglists (seq sigs)}]))
                                      (into {})))))))))))))

(defmacro find-doc
  "Prints documentation for any var whose documentation or name
 contains a match for re-string-or-pattern"
  [re-string-or-pattern]
  (let [re (re-pattern re-string-or-pattern)
        ms (concat
             (mapcat
               (fn [ns]
                 (map
                   (fn [m]
                     (update-in (select-keys m [:ns :name :doc :forms :arglists :macro :url])
                       [:name] #(if-not (nil? %) (clojure.core/name %) %)))
                   (sort-by :name (vals (ana-api/ns-interns ns)))))
               (ana-api/all-ns))
             (map #(select-keys (ana-api/find-ns %) [:name :doc]) (ana-api/all-ns))
             (map special-doc (keys special-doc-map)))
        ms (for [m ms
                 :when (and (:doc m)
                            (or (re-find (re-matcher re (:doc m)))
                                (re-find (re-matcher re (str (:name m))))))]
             m)]
    `(doseq [m# (quote ~ms)]
       (cljs.repl/print-doc m#))))

(defn source-fn
  "Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath.  Returns nil if
  it can't find the source.  For most REPL usage, 'source' is more
  convenient.

  Example: (source-fn 'filter)"
  [env x]
  (when-let [v (ana-api/resolve env x)]
    (when-let [filepath (:file v)]
      (let [f (io/file filepath)
            f (if (.exists f)
                f
                (io/resource filepath))]
        (when f
          (with-open [pbr (PushbackReader. (io/reader f))]
            (let [rdr (readers/source-logging-push-back-reader pbr)]
              (dotimes [_ (dec (:line v))] (readers/read-line rdr))
              (binding [reader/*alias-map*    identity
                        reader/*data-readers* tags/*cljs-data-readers*]
                (-> (reader/read {:read-cond :allow :features #{:cljs}} rdr)
                  meta :source)))))))))

(comment
  (def cenv (env/default-compiler-env))
  (def aenv (assoc-in (ana/empty-env) [:ns :name] 'cljs.user))

  (binding [ana/*cljs-ns* 'cljs.user]
    (env/with-compiler-env cenv
      (comp/with-core-cljs {}
        (fn []
          (source-fn aenv 'cljs.core/first)))))
  )

(defmacro source
  "Prints the source code for the given symbol, if it can find it.
  This requires that the symbol resolve to a Var defined in a
  namespace for which the .cljs is in the classpath.

  Example: (source filter)"
  [n]
  `(println ~(or (source-fn &env n) (str "Source not found"))))

(defn- named-publics-vars
  "Gets the public vars in a namespace that are not anonymous."
  [ns]
  (->> (ana-api/ns-publics ns)
       (remove (comp :anonymous val))
       (map key)))

(defmacro apropos
  "Given a regular expression or stringable thing, return a seq of all
public definitions in all currently-loaded namespaces that match the
str-or-pattern."
  [str-or-pattern]
  (let [matches? (if (instance? Pattern str-or-pattern)
                   #(re-find str-or-pattern (str %))
                   #(.contains (str %) (str str-or-pattern)))]
    `(quote
       ~(sort
          (mapcat
            (fn [ns]
              (let [ns-name (str ns)]
                (map #(symbol ns-name (str %))
                  (filter matches? (named-publics-vars ns)))))
            (ana-api/all-ns))))))

(defn- resolve-ns
  "Resolves a namespace symbol to a namespace by first checking to see if it
  is a namespace alias."
  [ns-sym]
  (or (get-in @env/*compiler* [::ana/namespaces ana/*cljs-ns* :requires ns-sym])
      (get-in @env/*compiler* [::ana/namespaces ana/*cljs-ns* :require-macros ns-sym])
      ns-sym))

(defmacro dir
  "Prints a sorted directory of public vars in a namespace"
  [ns]
  `(doseq [sym# (quote ~(sort (named-publics-vars (resolve-ns ns))))]
     (println sym#)))

(defmacro pst
  ([] `(pst *e))
  ([e]
   (let [{:keys [repl-env] :as env} &env]
     (when (and e repl-env)
       (when-let [ret (if (satisfies? IGetError repl-env)
                   (-get-error repl-env e env *repl-opts*)
                   (edn/read-string
                     (evaluate-form repl-env env "<cljs repl>"
                       `(when ~e
                          (pr-str
                            {:value (str ~e)
                             :stacktrace (.-stack ~e)})))))]
         (display-error repl-env
           (if (satisfies? IParseError repl-env)
             (-parse-error repl-env ret *repl-opts*)
             ret)
           nil *repl-opts*))))))
