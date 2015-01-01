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

(deftest test-merge-spec
  (is (= (repl/merge-spec
           '[clojure.string :refer [join]]
           '[clojure.string :refer [trim]])
         '[clojure.string :refer [join trim]]))
  (is (= (repl/merge-spec
           '[cljs.repl :refer [print-doc]]
           '[cljs.repl :refer-macros [doc]])
        '[cljs.repl :refer [print-doc] :refer-macros [doc]]))
  (is (= (repl/merge-spec
           '[cljs.repl :refer [print-doc] :include-macros true]
           '[cljs.repl :refer-macros [doc]])
        '[cljs.repl :refer [print-doc] :refer-macros [doc] :include-macros true])))

#_(deftest test-merge-require
  (is (= (repl/merge-require
           '[[cljs.reader :as r]
             [clojure.string :refer [join]]
             [cljs.repl :refer-macros [doc]]]
           '[clojure.string :refer [trim]])
        '[[cljs.reader :as r]
          [clojure.string :refer [join trim]]
          [cljs.repl :refer-macros [doc]]]))
  (is (= (repl/merge-require
           '[[cljs.reader :as r]
             [clojure.string :refer [join]]
             [cljs.repl :refer-macros [doc]]]
           '[clojure.string :refer [join]])
        '[[cljs.reader :as r]
          [clojure.string :refer [join]]
          [cljs.repl :refer-macros [doc]]]))
  (is (= (repl/merge-require
           '[[cljs.reader :as r]
             [clojure.string :refer [join]]
             [cljs.repl :refer-macros [doc]]]
           '[clojure.string :refer [join trim]])
        '[[cljs.reader :as r]
          [clojure.string :refer [join trim]]
          [cljs.repl :refer-macros [doc]]])))
