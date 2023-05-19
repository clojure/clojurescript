(ns cljs.foreign.node-test
  (:require [cljs.foreign.node :as node]
            [cljs.util :as util]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.test :as test :refer [deftest is testing]]))

(defn cleanup
  ([] (cleanup #()))
  ([f]
   (doseq [f (-> "node_modules/.bin" io/file file-seq reverse)]
     (io/delete-file f))
   (doseq [f (concat
               (-> "node_modules" io/file file-seq reverse)
               (map io/file ["package.json" "package-lock.json"
                             "yarn.lock" "yarn-error.log"]))]
     (when (.exists f)
       (io/delete-file f)))
   (f)))

(defn install
  ([lib version]
   (install :npm lib version))
  ([cmd lib version]
   (let [action ({:npm "install" :yarn "add"} cmd)]
     (sh/sh (name cmd) action (str lib "@" version)))))

(test/use-fixtures :once cleanup)

;; =============================================================================
;; Tests

(defn indexed-lib-specs []
  (as-> (-> (util/module-file-seq {})
          (node/node-file-seq->libs-spec* {}))
    xs (zipmap (map :file xs) xs)))

(defn path->lib-spec [index path]
  (get index (.getAbsolutePath (io/file path))))

(deftest test-basic
  (install "left-pad" "1.3.0")
  (testing "Install left-pad, verify that it is indexed and has a sensible lib-spec"
   (let [index (indexed-lib-specs)]
     (let [left-pad (path->lib-spec index "node_modules/left-pad/index.js")]
       (is (some? (:file left-pad)))
       (is (some? (:module-type left-pad)))
       (is (= #{"left-pad/index.js" "left-pad/index" "left-pad"}
             (into #{} (:provides left-pad)))))
     (testing "\tleft-pad has a dep on bablyon, which uses main with different path, check"
       (let [babylon (path->lib-spec (indexed-lib-specs) "node_modules/babylon/lib/index.js")]
         (is (some? (:file babylon)))
         (is (some? (:module-type babylon)))
         (is (= #{"babylon/lib/index.js" "babylon/lib/index" "babylon" "babylon/lib"}
                (into #{} (:provides babylon)))))))))

(deftest test-exports-basic
  (install :yarn "react-select" "5.7.2"))

(comment

  (test/run-tests)
  (cleanup)
  (install :yarn "react-select" "5.7.2")

  )
