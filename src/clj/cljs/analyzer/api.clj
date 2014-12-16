;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.api
  (:refer-clojure :exclude [all-ns ns-interns ns-resolve resolve find-ns])
  (:require [cljs.env :as env]
            [cljs.analyzer :as ana]))

(defn resolve [env sym]
  (ana/resolve-var env sym))

(defn all-ns []
  (keys (get @env/*compiler* ::ana/namespaces)))

(defn find-ns [sym]
  (get-in @env/*compiler* [::ana/namespaces sym]))

(defn ns-interns [ns]
  (get-in @env/*compiler* [::ana/namespaces ns :defs]))

(defn ns-resolve [ns sym]
  (get-in @env/*compiler* [::ana/namespaces ns :defs sym]))
