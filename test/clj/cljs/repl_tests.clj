(ns cljs.repl-tests
  (:require [clojure.java.io :as io]
            [cljs.analyzer :as ana]
            [cljs.env :as env]
            [cljs.repl :as repl]
            [cljs.repl.rhino :as rhino])
    (:use clojure.test))


(deftest file-info
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

