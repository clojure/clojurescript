(ns cljs.repl-tests
  (:require [clojure.java.io :as io]
            [cljs.analyzer :as ana]
            [cljs.env :as env]
            [cljs.repl :as repl]
            [cljs.repl.rhino :as rhino])
    (:use clojure.test))

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

(deftest test-update-require-spec
  (is (= (repl/update-require-spec
           '()
           '[cljs.reader :as reader])
        '((:require [cljs.reader :as reader]))))
  (is (= (repl/update-require-spec
           '((:refer-clojure :exclude [==]))
           '[cljs.reader :as reader])
        '((:refer-clojure :exclude [==])
          (:require [cljs.reader :as reader]))))
  (is (= (repl/update-require-spec
           '((:refer-clojure :exclude [==])
              (:require [clojure.string :as clojure.string]))
           '[cljs.reader :as reader])
        '((:refer-clojure :exclude [==])
          (:require [clojure.string :as clojure.string]
                    [cljs.reader :as reader])))))
