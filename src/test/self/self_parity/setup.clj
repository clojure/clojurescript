;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns self-parity.setup
  ^{:doc "Sets up the filesystem, priming the output directory
  with needed source files so that the self-hosted compiler
  being executed within Node has various dependency sources
  available (without the benefit of being able to load resources
  from a classpath)."}
  (:require
    [clojure.java.io :as io]))

(def out-path (io/file "builds" "out-self-parity"))

(defn copy-source
  [source-resource-name]
  (let [target-file (io/file out-path source-resource-name)]
    (io/make-parents target-file)
    (io/copy (io/input-stream (io/resource source-resource-name)) target-file)))

(def test-check-source-resource-names
  ["clojure/test/check.cljc"
   "clojure/test/check/random.clj"
   "clojure/test/check/random.cljs"
   "clojure/test/check/rose_tree.cljc"
   "clojure/test/check/clojure_test.cljc"
   "clojure/test/check/clojure_test/assertions.cljc"
   "clojure/test/check/clojure_test/assertions/cljs.cljc"
   "clojure/test/check/results.cljc"
   "clojure/test/check/impl.cljc"
   "clojure/test/check/properties.cljc"
   "clojure/test/check/random/longs.cljs"
   "clojure/test/check/random/doubles.cljs"
   "clojure/test/check/random/longs/bit_count_impl.cljs"
   "clojure/test/check/generators.cljc"])

(def source-resource-names
  (into ["clojure/template.clj"]
    test-check-source-resource-names))

(defn -main []
  (run! copy-source source-resource-names))
