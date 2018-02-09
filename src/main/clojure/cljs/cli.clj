;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.cli
  (:require [cljs.util :as util]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.compiler :as comp]
            [cljs.repl :as repl]
            [cljs.build.api :as build])
  (:import [java.io StringReader]))

(def ^:dynamic *cli-opts* nil)

(defn output-dir-opt
  [repl-env output-dir]
  (set! *cli-opts* (merge *cli-opts* {:output-dir output-dir})))

(defn verbose-opt
  [repl-env value]
  (set! *cli-opts* (merge *cli-opts* {:verbose (or (= value "true") false)})))

(defn- eval-opt
  [repl-env form-str]
  (set! *cli-opts*
    (merge *cli-opts* {:eval-forms (ana-api/forms-seq (StringReader. form-str))})))

(defn init-dispatch
  "Returns the handler associated with an init opt"
  [repl-env opt]
  ({"-e"           (partial eval-opt repl-env)
    "--eval"       (partial eval-opt repl-env)
    "-v"           (partial verbose-opt repl-env)
    "--verbose"    (partial verbose-opt repl-env)
    "-o"           (partial output-dir-opt repl-env)
    "--output-dir" (partial output-dir-opt repl-env)} opt))

(defn- initialize
  "Common initialize routine for repl, script, and null opts"
  [repl-env inits]
  (doseq [[opt arg] inits]
    ((init-dispatch repl-env opt) arg)))

(defn repl-opt
  "Start a repl with args and inits. Print greeting if no eval options were
present"
  [repl-env [_ & args] inits]
  ;; TODO: handle eval forms
  (repl/repl* (repl-env) (build/add-implicit-options *cli-opts*)))

(defn main-opt
  "Call the -main function from a namespace with string arguments from
  the command line."
  [repl-env [_ main-ns & args] inits]
  (env/ensure
    (initialize repl-env inits)
    (let [renv (repl-env)]
      (binding [repl/*repl-opts*
                (build/add-implicit-options
                  (merge (repl/-repl-options renv) *cli-opts*))
                ana/*verbose* (:verbose repl/*repl-opts*)]
        (comp/with-core-cljs repl/*repl-opts*
          (fn []
            (repl/-setup renv (merge (repl/-repl-options renv) repl/*repl-opts*))
            (doseq [form (:eval-forms repl/*repl-opts*)]
              (println (repl/evaluate-form renv (ana/empty-env) "<cljs repl>" form)))
            (when main-ns
              (repl/evaluate-form renv (ana/empty-env) "<cljs repl>"
                `(do
                   (set! *command-line-args* (list ~@args))
                   (~(symbol (name main-ns) "-main") ~@args))))
            (repl/-tear-down renv)))))))

(defn- null-opt
  "No repl or script opt present, just bind args and run inits"
  [repl-env args inits]
  (initialize repl-env inits))

(defn main-dispatch
  "Returns the handler associated with a main option"
  [repl-env opt]
  ({"-r"     (partial repl-opt repl-env)
    "--repl" (partial repl-opt repl-env)
    "-m"     (partial main-opt repl-env)
    "--main" (partial main-opt repl-env)
    nil      (partial null-opt repl-env)
    ;"-h" help-opt
    ;"--help" help-opt
    ;"-?" help-opt
    } opt))

(defn adapt-args [args]
  (cond-> args
    (and (some #{"-e" "--eval"} args)
         (not (some #{"-m" "--main"} args)))
    (concat ["-m"])))

(defn main [repl-env & args]
  (binding [*cli-opts* {}]
    (try
      (if args
        (let [args' (adapt-args args)]
          (loop [[opt arg & more :as args] args' inits []]
            (if (init-dispatch repl-env opt)
              (recur more (conj inits [opt arg]))
              ((main-dispatch repl-env opt) args inits))))
        (repl-opt repl-env nil nil))
      (finally
        (flush)))))
