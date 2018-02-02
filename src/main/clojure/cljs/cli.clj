;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.cli
  (:require [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.repl :as repl]
            [cljs.env :as env])
  (:import [java.io StringReader]))

(defn repl-opt
  "Start a repl with args and inits. Print greeting if no eval options were
present"
  [repl-env [_ & args] inits]
  (repl/repl (repl-env)))

(defn- eval-opt
  "Evals expressions in str, prints each non-nil result using prn"
  [repl-env str]
  ;; TODO: use forms-seq instead of read-string
  (env/ensure
    (let [renv  (repl-env)
          forms (ana-api/forms-seq (StringReader. str))]
      (repl/-setup renv (repl/-repl-options renv))
      (doseq [form forms]
        (println
          (repl/evaluate-form renv (ana/empty-env) "<cljs repl>" form)))
      (repl/-tear-down renv))))

(defn main-opt
  "Call the -main function from a namespace with string arguments from
  the command line."
  [repl-env [_ main-ns & args] inits]
  ;; NOT YET IMPLEMENTED
  )

(defn init-dispatch
  "Returns the handler associated with an init opt"
  [repl-env opt]
  ;; NOT YET IMPLEMENTED
  ({"-e"     (partial eval-opt repl-env)
    "--eval" (partial eval-opt repl-env)} opt))

(defn- initialize
  "Common initialize routine for repl, script, and null opts"
  [repl-env args inits]
  (doseq [[opt arg] inits]
    ((init-dispatch repl-env opt) arg)))

(defn- null-opt
  "No repl or script opt present, just bind args and run inits"
  [repl-env args inits]
  (initialize repl-env args inits))

(defn main-dispatch
  "Returns the handler associated with a main option"
  [repl-env opt]
  ({"-r"     (partial repl-opt repl-env)
    "--repl" (partial repl-opt repl-env)
    ;"-m" main-opt
    ;"--main" main-opt
    nil      (partial null-opt repl-env)
    ;"-h" help-opt
    ;"--help" help-opt
    ;"-?" help-opt
    } opt))

(defn main [repl-env & args]
  (try
    (if args
      (loop [[opt arg & more :as args] args inits []]
        (if (init-dispatch repl-env opt)
          (recur more (conj inits [opt arg]))
          ((main-dispatch repl-env opt) args inits)))
      (repl-opt repl-env nil nil))
    (finally
      (flush))))