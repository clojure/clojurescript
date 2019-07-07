;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.module-processing-tests
  (:require [clojure.java.io :as io]
            [cljs.closure :as closure]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.js-deps :as deps]
            [cljs.util :as util]
            [cljs.test-util :as test])
  (:import [java.io File]))

;; Hard coded JSX transform for the test case
(defn preprocess-jsx [ijs _]
  (assoc ijs :source (clojure.string/replace
                       (:source ijs)
                       (re-pattern (str "\\(\n"
                                        "\\s*<svg width=\"200px\" height=\"200px\" className=\"center\">\n"
                                        "\\s*<circle cx=\"100px\" cy=\"100px\" r=\"100px\" fill=\\{this.props.color\\}>\n"
                                        "\\s*</circle>\n"
                                        "\\s*</svg>\n"
                                        "\\s*\\)"))
                       (str " React.createElement(\"svg\", {width:\"200px\", height:\"200px\", className:\"center\"}, "
                            "React.createElement(\"circle\", {cx:\"100px\", cy:\"100px\", r:\"100px\", fill:this.props.color})"
                            ")"))))

(defn absolute-module-path
  ([relpath]
   (absolute-module-path relpath false))
  ([relpath code?]
   (let [filename (as-> (subs relpath (inc (.lastIndexOf relpath "/"))) $
                    (string/replace $ "_" "-")
                    (subs $ 0 (.lastIndexOf $ ".")))
         dirname (as-> (io/file relpath) $
                   (.getAbsolutePath $)
                   (subs $ 0 (.lastIndexOf $ (str File/separator)))
                   (string/replace $ "/" "$")
                   (string/replace $ \. \-)
                   (cond-> $ code? (string/replace "-" "_"))
                   ;; Windows
                   (string/replace $ "\\" "$")
                   (if code?
                     (string/replace $ ":" "_")
                     (string/replace $ ":" "-")))]
     (str "module" (when-not (.startsWith dirname "$") "$") dirname "$" filename))))

(defmethod closure/js-transforms :jsx [ijs opts]
  (preprocess-jsx ijs opts))

(deftest commonjs-module-processing
  (test/delete-out-files)
  (let [cenv (env/default-compiler-env)]
    ;; Reset load-library cache so that changes to processed files are noticed
    (with-redefs [cljs.js-deps/load-library (memoize cljs.js-deps/load-library*)]
      (is (= {:foreign-libs []
              :ups-foreign-libs []
              :libs [(test/platform-path "out/src/test/cljs/reactJS.js")
                     (test/platform-path "out/src/test/cljs/Circle.js")]
              :closure-warnings {:non-standard-jsdoc :off}}
            (env/with-compiler-env cenv
              (closure/process-js-modules
                {:foreign-libs [{:file        "src/test/cljs/reactJS.js"
                                 :provides    ["React"]
                                 :module-type :commonjs}
                                {:file        "src/test/cljs/Circle.js"
                                 :provides    ["Circle"]
                                 :module-type :commonjs
                                 :preprocess  :jsx}]
                 :closure-warnings {:non-standard-jsdoc :off}})))
        "processed modules are added to :libs"))
    (is (= {"React" {:name (absolute-module-path "src/test/cljs/reactJS.js")
                     :module-type :commonjs}
            "Circle" {:name (absolute-module-path "src/test/cljs/Circle.js")
                      :module-type :commonjs}}
           (:js-module-index @cenv))
        "Processed modules are added to :js-module-index")))

(deftest es6-module-processing
  (test/delete-out-files)
  (let [cenv (env/default-compiler-env)]

    ;; Reset load-library cache so that changes to processed files are noticed in REPL
    (with-redefs [cljs.js-deps/load-library  (memoize cljs.js-deps/load-library*)]

      (is (= {:foreign-libs []
              :ups-foreign-libs []
              :libs [(test/platform-path "out/src/test/cljs/es6_hello.js")]
              :closure-warnings {:non-standard-jsdoc :off}}
             (env/with-compiler-env cenv
               (closure/process-js-modules
                 {:foreign-libs [{:file        "src/test/cljs/es6_hello.js"
                                  :provides    ["es6-hello"]
                                  :module-type :es6}]
                  :closure-warnings {:non-standard-jsdoc :off}})))
          "processed modules are added to :libs")

      (is (= {"es6-hello" {:name (absolute-module-path "src/test/cljs/es6_hello.js")
                           :module-type :es6}}
             (:js-module-index @cenv))
          "Processed modules are added to :js-module-index")

      (is (re-find
            #"goog.provide\(\"module\$[a-zA-Z0-9$_]+?src\$test\$cljs\$es6_hello\"\);"
            (slurp "out/src/test/cljs/es6_hello.js"))))))

(deftest test-module-name-substitution
  (test/delete-out-files)
  (let [cenv (env/default-compiler-env)]
    ;; Make sure load-library is not cached when developing on REPL
    (with-redefs [cljs.js-deps/load-library (memoize cljs.js-deps/load-library*)
                  cljs.js-deps/load-foreign-library (memoize cljs.js-deps/load-foreign-library*)]
      (env/with-compiler-env cenv
        (let [opts (closure/process-js-modules {:foreign-libs [{:file "src/test/cljs/calculator.js"
                                                                :provides ["calculator"]
                                                                :module-type :commonjs}]})
              compile (fn [form]
                        (with-out-str
                          (comp/emit (ana/analyze (ana/empty-env) form))))
              crlf (if util/windows? "\r\n" "\n")
              output (str (absolute-module-path "src/test/cljs/calculator.js" true) "[\"default\"].add((3),(4));" crlf)]
          (swap! cenv
                 #(assoc % :js-dependency-index (deps/js-dependency-index opts)))
          (binding [ana/*cljs-ns* 'cljs.user]
            (is (= (str "goog.provide('my_calculator.core');" crlf
                        "goog.require('cljs.core');" crlf
                        "goog.require('" (absolute-module-path "src/test/cljs/calculator.js" true) "');"
                        crlf)
                   (compile '(ns my-calculator.core (:require [calculator :as calc :refer [subtract add] :rename {subtract sub}])))))
            (is (= output (compile '(calc/add 3 4))))
            (is (= output (compile '(calculator/add 3 4))))
            (is (= output (compile '(add 3 4))))
            (is (= (str (absolute-module-path "src/test/cljs/calculator.js" true)
                        "[\"default\"].subtract((5),(4));" crlf)
                   (compile '(sub 5 4))))))))))

(deftest test-cljs-1822
  (test/delete-out-files)
  (let [cenv (env/default-compiler-env)]
    ;; Make sure load-library is not cached when developing on REPL
    (with-redefs [cljs.js-deps/load-library (memoize cljs.js-deps/load-library*)
                  cljs.js-deps/load-foreign-library (memoize cljs.js-deps/load-foreign-library*)]
      (is (= {:optimizations :simple
              :foreign-libs []
              :ups-foreign-libs []
              :libs [(test/platform-path "out/src/test/cljs/react-min.js")
                     (test/platform-path "out/src/test/cljs/Circle-min.js")]
              :closure-warnings {:non-standard-jsdoc :off}}
            (env/with-compiler-env cenv
              (closure/process-js-modules
                {:optimizations :simple
                 :foreign-libs [{:file        "src/test/cljs/reactJS.js"
                                 :file-min    "src/test/cljs/react-min.js"
                                 :provides    ["React"]
                                 :module-type :commonjs}
                                {:file        "src/test/cljs/Circle.js"
                                 :file-min    "src/test/cljs/Circle-min.js"
                                 :provides    ["Circle"]
                                 :module-type :commonjs
                                 :preprocess  :jsx}]
                 :closure-warnings {:non-standard-jsdoc :off}})))
        "processed modules are added to :libs"))
    (is (= {"React" {:name (absolute-module-path "src/test/cljs/react-min.js")
                     :module-type :commonjs}
            "Circle" {:name (absolute-module-path "src/test/cljs/Circle-min.js")
                      :module-type :commonjs}}
           (:js-module-index @cenv))
        "Processed modules are added to :js-module-index")))

(deftest commonjs-module-processing-preprocess-symbol
  (test/delete-out-files)
  (let [cenv (env/default-compiler-env)]
    ;; Reset load-library cache so that changes to processed files are noticed
    (with-redefs [cljs.js-deps/load-library (memoize cljs.js-deps/load-library*)]
      (is (= {:foreign-libs []
              :ups-foreign-libs []
              :libs [(test/platform-path "out/src/test/cljs/reactJS.js")
                     (test/platform-path "out/src/test/cljs/Circle.js")]
              :closure-warnings {:non-standard-jsdoc :off}}
            (env/with-compiler-env cenv
              (closure/process-js-modules
                {:foreign-libs [{:file        "src/test/cljs/reactJS.js"
                                 :provides    ["React"]
                                 :module-type :commonjs}
                                {:file        "src/test/cljs/Circle.js"
                                 :provides    ["Circle"]
                                 :module-type :commonjs
                                 :preprocess  'cljs.module-processing-tests/preprocess-jsx}]
                 :closure-warnings {:non-standard-jsdoc :off}})))
        "processed modules are added to :libs"))

    (is (= {"React" {:name (absolute-module-path "src/test/cljs/reactJS.js")
                     :module-type :commonjs}
            "Circle" {:name (absolute-module-path "src/test/cljs/Circle.js")
                      :module-type :commonjs}}
           (:js-module-index @cenv))
        "Processed modules are added to :js-module-index")))
