;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl-tests
  (:require [clojure.java.io :as io]
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.env :as env]
            [cljs.repl :as repl]
            [cljs.repl.rhino :as rhino]
            [cljs.compiler :as comp])
    (:use clojure.test))

(def st (env/default-compiler-env))

(env/with-compiler-env st
  (ana/analyze-file "cljs/core.cljs")
  (ana/load-core))

(deftest test-doc
  (env/with-compiler-env st
    (is (string? (:doc (ana-api/resolve {} '->))))))

#_(deftest file-info
  (let [repl-env (rhino/repl-env)
        compiler-env (env/default-compiler-env)
        repl-env (assoc repl-env ::env/compiler compiler-env)]
    (env/with-compiler-env compiler-env
      (binding [ana/*cljs-ns* 'cljs.user]
        (repl/-setup repl-env)))
    (let [assoc-info (get-in @compiler-env [:cljs.analyzer/namespaces 'cljs.core :defs 'assoc])
          {:keys [file line]} assoc-info]
      
      (is assoc-info)
      (is (number? line))
      (is file)
      (and file
           (is (io/resource file))))))
