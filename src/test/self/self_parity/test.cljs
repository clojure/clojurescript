;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns
  ^{:doc "Builds and runs the ClojureScript compiler test suite
  in self-host mode, ensuring parity of bootstrapped ClojureScript
  with JVM based ClojureScript.

  This involves dynamically loading the test suite files at runtime,
  excercising that they can be compiled by the bootstrapped
  ClojureScript compiler, and also running the resulting tests."}
  self-parity.test
  (:require [clojure.string :as string]
            [cljs.compiler :as comp]
            [cljs.nodejs :as nodejs]
            [cljs.js :as cljs]
            [cljs.tools.reader :as reader]
            [cljs.stacktrace :as st]
            [goog.object :as gobj]))

(def out-dir "builds/out-self-parity")

(def src-paths [out-dir
                "src/main/cljs"
                "src/main/clojure"
                "src/test/cljs"])

(defn init-runtime
  "Initializes the runtime so that we can use the cljs.user
  namespace and so that Google Closure is set up to work
  properly with :optimizations :none."
  []
  (set! (.-user js/cljs) #js {})
  ;; monkey-patch isProvided_ to avoid useless warnings
  (js* "goog.isProvided_ = function(x) { return false; };")
  ;; monkey-patch goog.require, skip all the loaded checks
  (set! (.-require js/goog)
    (fn [name]
      (js/CLOSURE_IMPORT_SCRIPT
        (if goog/debugLoader_
          (.getPathFromDeps_ goog/debugLoader_ name)
          (gobj/get (.. js/goog -dependencies_ -nameToPath) name)))))
  ;; setup printing
  (nodejs/enable-util-print!)
  ;; redef goog.require to track loaded libs
  (set! *loaded-libs* #{"cljs.core"})
  (set! (.-require js/goog)
    (fn [name reload]
      (when (or (not (contains? *loaded-libs* name)) reload)
        (set! *loaded-libs* (conj (or *loaded-libs* #{}) name))
        (js/CLOSURE_IMPORT_SCRIPT
          (if goog/debugLoader_
            (.getPathFromDeps_ goog/debugLoader_ name)
            (gobj/get (.. js/goog -dependencies_ -nameToPath) name)))))))

;; Node file reading fns

(def fs (nodejs/require "fs"))

(defn node-read-file
  "Accepts a filename to read and a callback. Upon success, invokes
  callback with the source. Otherwise invokes the callback with nil."
  [filename cb]
  (.readFile fs filename "utf-8"
    (fn [err source]
      (cb (when-not err
            source)))))

(defn node-read-file-sync
  "Accepts a filename to read. Upon success, returns the source.
  Otherwise returns nil."
  [filename]
  (.readFileSync fs filename "utf-8"))

;; Facilities for loading Closure deps

(defn closure-index
  "Builds an index of Closure files. Similar to
  cljs.js-deps/goog-dependencies*"
  []
  (let [paths-to-provides
        (map (fn [[_ path provides]]
               [path (map second
                       (re-seq #"'(.*?)'" provides))])
          (re-seq #"\ngoog\.addDependency\('(.*)', \[(.*?)\].*"
            (node-read-file-sync (str out-dir "/goog/deps.js"))))]
    (into {}
      (for [[path provides] paths-to-provides
            provide provides]
        [(symbol provide) (str out-dir "/goog/" (second (re-find #"(.*)\.js$" path)))]))))

(def closure-index-mem (memoize closure-index))

(defn load-goog
  "Loads a Google Closure implementation source file."
  [name cb]
  (if-let [goog-path (get (closure-index-mem) name)]
    (if-let [source (node-read-file-sync (str goog-path ".js"))]
      (cb {:source source
           :lang   :js})
      (cb nil))
    (cb nil)))

;; Facilities for loading files

(defn- filename->lang
  "Converts a filename to a lang keyword by inspecting the file
  extension."
  [filename]
  (if (string/ends-with? filename ".js")
    :js
    :clj))

(defn replace-extension
  "Replaces the extension on a file."
  [filename new-extension]
  (string/replace filename #".clj[sc]?$" new-extension))

(defn parse-edn
  "Parses edn source to Clojure data."
  [edn-source]
  (reader/read-string edn-source))

(defn- read-some
  "Reads the first filename in a sequence of supplied filenames,
  using a supplied read-file-fn, calling back upon first successful
  read, otherwise calling back with nil. Before calling back, first
  attempts to read AOT artifacts (JavaScript and cache edn)."
  [[filename & more-filenames] read-file-fn cb]
  (if filename
    (read-file-fn
      filename
      (fn [source]
        (if source
          (let [source-cb-value {:lang   (filename->lang filename)
                                 :file   filename
                                 :source source}]
            (if (or (string/ends-with? filename ".cljs")
                    (string/ends-with? filename ".cljc"))
              (read-file-fn
                (replace-extension filename ".js")
                (fn [javascript-source]
                  (if javascript-source
                    (read-file-fn
                      (str filename ".cache.edn")
                      (fn [cache-edn]
                        (if cache-edn
                          (cb {:lang   :js
                               :source javascript-source
                               :cache  (parse-edn cache-edn)})
                          (cb source-cb-value))))
                    (cb source-cb-value))))
              (cb source-cb-value)))
          (read-some more-filenames read-file-fn cb))))
    (cb nil)))

(defn filenames-to-try
  "Produces a sequence of filenames to try reading, in the
  order they should be tried."
  [src-paths macros path]
  (let [extensions (if macros
                     [".clj" ".cljc"]
                     [".cljs" ".cljc" ".js"])]
    (for [extension extensions
          src-path  src-paths]
      (str src-path "/" path extension))))

(defn skip-load?
  "Indicates namespaces that we either don't need to load,
  shouldn't load, or cannot load (owing to unresolved
  technical issues)."
  [name macros]
  ((if macros
     #{'cljs.core}
     #{'goog.object
       'goog.string
       'goog.string.StringBuffer
       'goog.array
       'cljs.core
       'cljs.env
       'cljs.tagged-literals
       'cljs.tools.reader
       'clojure.walk}) name))

;; An atom to keep track of things we've already loaded
(def loaded (atom #{}))

(defn load?
  "Determines whether the given namespace should be loaded."
  [name macros]
  (let [do-not-load (or (@loaded [name macros])
                        (skip-load? name macros))]
    (swap! loaded conj [name macros])
    (not do-not-load)))

(defn make-load-fn
  "Makes a load function that will read from a sequence of src-paths
  using a supplied read-file-fn. It returns a cljs.js-compatible
  *load-fn*.
  Read-file-fn is a 2-arity function (fn [filename source-cb] ...) where
  source-cb is itself a function (fn [source] ...) that needs to be called
  with the source of the library (as string)."
  [src-paths read-file-fn]
  (fn [{:keys [name macros path]} cb]
    (if (load? name macros)
      (if (re-matches #"^goog/.*" path)
        (load-goog name cb)
        (read-some (filenames-to-try src-paths macros path) read-file-fn cb))
      (cb {:source ""
           :lang   :js}))))

;; Facilities for evaluating JavaScript

(def vm (nodejs/require "vm"))

(defn node-eval
  "Evaluates JavaScript in node."
  [{:keys [name source]}]
  (if-not js/COMPILED
    (.runInThisContext vm source (str (munge name) ".js"))
    (js/eval source)))

;; Facilities for driving cljs.js

(def st (cljs/empty-state))

(def load-fn (make-load-fn src-paths node-read-file))

(defn eval-form
  "Evaluates a supplied form in a given namespace,
  calling back with the evaluation result."
  [st ns form cb]
  (cljs/eval st
    form
    {:ns         ns
     :context    :expr
     :load       load-fn
     :eval       node-eval
     :source-map true
     :verbose    false}
    cb))

;; Error handler

(defn- handle-error
  [error sms]
  (loop [error error]
    (let [message (if (instance? ExceptionInfo error)
                    (ex-message error)
                    (.-message error))
          parsed-stacktrace (st/parse-stacktrace {}
                              (.-stack error)
                              {:ua-product :nodejs}
                              {})]
      (println message)
      (print (st/mapped-stacktrace-str parsed-stacktrace sms))
      (when-some [cause (.-cause error)]
        (print "caused by: ")
        (recur cause)))))

;; Test suite runner

(defn run-tests
  "Runs the tests."
  []
  ;; Ideally we'd just load test_runner.cljs, but a few namespace tests
  ;; don't yet run in bootstrapped ClojureScript. These are commented
  ;; out below and can be uncommented as fixed.
  (eval-form st 'cljs.user
    '(ns parity.core
       (:require [cljs.test :refer-macros [run-tests]]
                 [cljs.eval-test]
                 [cljs.primitives-test]
                 [cljs.destructuring-test]
                 [cljs.new-new-test]
                 [cljs.printing-test]
                 [cljs.seqs-test]
                 [cljs.collections-test]
                 [cljs.hashing-test]
                 [cljs.core-test :as core-test]
                 [cljs.reader-test]
                 [cljs.binding-test]
                 #_[cljs.ns-test]
                 [clojure.string-test]
                 [clojure.data-test]
                 [clojure.walk-test]
                 [cljs.macro-test]
                 [cljs.letfn-test]
                 [foo.ns-shadow-test]
                 [cljs.top-level]
                 [cljs.reducers-test]
                 [cljs.keyword-test]
                 [cljs.import-test]
                 [cljs.ns-test.foo]
                 [cljs.pprint]
                 [cljs.pprint-test]
                 [cljs.spec-test]
                 [cljs.spec.test-test]
                 [cljs.clojure-alias-test]
                 [cljs.hash-map-test]
                 [cljs.map-entry-test]
                 [cljs.set-equiv-test]
                 [cljs.syntax-quote-test]
                 [cljs.predicates-test]
                 [cljs.test-test]
                 [static.core-test]
                 [cljs.recur-test]
                 [cljs.array-access-test]
                 [cljs.inference-test]
                 [cljs.walk-test]
                 [cljs.extend-to-native-test]))
    (fn [{:keys [value error]}]
      (if error
        (handle-error error (:source-maps @st))
        (eval-form st 'parity.core
          '(run-tests
             'cljs.eval-test
             'cljs.primitives-test
             'cljs.destructuring-test
             'cljs.new-new-test
             'cljs.printing-test
             'cljs.seqs-test
             'cljs.collections-test
             'cljs.hashing-test
             'cljs.core-test
             'cljs.reader-test
             'clojure.string-test
             'clojure.data-test
             'clojure.walk-test
             'cljs.letfn-test
             'cljs.reducers-test
             'cljs.binding-test
             'cljs.macro-test
             'cljs.top-level
             'cljs.keyword-test
             #_'cljs.ns-test
             'cljs.ns-test.foo
             'foo.ns-shadow-test
             'cljs.import-test
             'cljs.pprint
             'cljs.pprint-test
             'cljs.spec-test
             'cljs.spec.test-test
             'cljs.clojure-alias-test
             'cljs.hash-map-test
             'cljs.map-entry-test
             'cljs.set-equiv-test
             'cljs.syntax-quote-test
             'cljs.predicates-test
             'cljs.test-test
             'static.core-test
             'cljs.recur-test
             'cljs.array-access-test
             'cljs.inference-test
             'cljs.walk-test
             'cljs.extend-to-native-test)
          (fn [{:keys [value error]}]
            (when error
              (handle-error error (:source-maps @st)))))))))

(defn -main [& args]
  (init-runtime)
  (run-tests))

(set! *main-cli-fn* -main)
