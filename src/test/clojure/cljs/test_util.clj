;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.test-util
  (:require [clojure.java.io :as io]))

(defn clean-outputs
  "Remove the :output-to artifacts of a ClojureScript build."
  [{:keys [output-to modules] :as opts}]
  (some-> output-to io/file .delete)
  (doseq [{:keys [output-to]} (vals modules)]
    (.delete (io/file output-to))))

(defn project-with-modules
  "Returns the build config for a project that uses Google Closure modules."
  [output-dir]
  {:inputs "src/test/cljs"
   :opts
   {:main "module-test.main"
    :output-dir output-dir
    :optimizations :advanced
    :verbose true
    :modules
    {:cljs-base
     {:output-to (str output-dir "/module-main.js")}
     :module-a
     {:output-to (str output-dir "/module-a.js")
      :entries #{'module-test.modules.a}}
     :module-b
     {:output-to (str output-dir "/module-b.js")
      :entries #{'module-test.modules.b}}}}})
