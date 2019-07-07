;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.profile
  (:require [clojure.java.io :as io]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]))

(comment

  ;; ~900ms
  (dotimes [_ 20]
    (time
      (ana/analyze-file (io/resource "cljs/core.cljs"))))

  ;; ~2700ms
  ;; after change ~2500
  (dotimes [_ 20]
    (time
      (env/with-compiler-env (env/default-compiler-env)
        (comp/compile-file (.getPath (io/resource "cljs/core.cljs")))
        (.delete (io/file "src/main/cljs/cljs/core.js")))))

  )