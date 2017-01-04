(ns cljs.module-processing-tests
  (:require [clojure.java.io :as io]
            [cljs.closure :as closure]
            [clojure.test :refer :all]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.js-deps :as deps]
            [cljs.test-util :as test]))

;; Hard coded JSX transform for the test case
(defmethod closure/js-transforms :jsx [ijs _]
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

(deftest commonjs-module-processing
  (test/delete-out-files)
  (let [cenv (env/default-compiler-env)]

    ;; Reset load-library cache so that changes to processed files are noticed
    (alter-var-root #'cljs.js-deps/load-library (constantly (memoize cljs.js-deps/load-library*)))

    (is (= {:foreign-libs []
            :ups-foreign-libs []
            :libs ["out/src/test/cljs/react.js"
                   "out/src/test/cljs/Circle.js"]
            :closure-warnings {:non-standard-jsdoc :off}}
           (env/with-compiler-env cenv
             (closure/process-js-modules
              {:foreign-libs [{:file        "src/test/cljs/react.js"
                               :provides    ["React"]
                               :module-type :commonjs}
                              {:file        "src/test/cljs/Circle.js"
                               :provides    ["Circle"]
                               :module-type :commonjs
                               :preprocess  :jsx}]
               :closure-warnings {:non-standard-jsdoc :off}})))
        "processed modules are added to :libs")

    (is (= {"React" "module$src$test$cljs$react"
            "Circle" "module$src$test$cljs$Circle"}
           (:js-module-index @cenv))
        "Processed modules are added to :js-module-index")))

(deftest test-module-name-substitution
  (test/delete-out-files)
  (let [cenv (env/default-compiler-env)]
    (env/with-compiler-env cenv
      (let [opts (closure/process-js-modules {:foreign-libs [{:file "src/test/cljs/calculator.js"
                                                              :provides ["calculator"]
                                                              :module-type :commonjs}]})
            compile (fn [form] (with-out-str
                                 (comp/emit (ana/analyze (ana/empty-env) form))))
            output "module$src$test$cljs$calculator.add.call(null,(3),(4));\n"]
        (swap! cenv
               #(assoc % :js-dependency-index (deps/js-dependency-index opts)))
        (binding [ana/*cljs-ns* 'cljs.user]
          (is (= (compile '(ns my-calculator.core (:require [calculator :as calc :refer [subtract add] :rename {subtract sub}])))
                 "goog.provide('my_calculator.core');\ngoog.require('cljs.core');\ngoog.require('module$src$test$cljs$calculator');\n"))
          (is (= (compile '(calc/add 3 4)) output))
          (is (= (compile '(calculator/add 3 4)) output))
          (is (= (compile '(add 3 4)) output))
          (is (= (compile '(sub 5 4))
                 "module$src$test$cljs$calculator.subtract.call(null,(5),(4));\n")))))))
