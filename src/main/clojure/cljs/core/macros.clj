;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core.macros
  (:refer-clojure :exclude [alias])
  (:require [clojure.java.io :as io]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.repl :refer [source]])
  (:import [java.io PushbackReader]))

(defn source-fn
  [x]
  (when-let [m (-> x resolve meta)]
    (when-let [filepath (:file m)]
      (let [f (io/file filepath)
            f (if (.exists f)
                f
                (io/resource filepath))]
        (when f
          (with-open [pbr (PushbackReader. (io/reader f))]
            (let [rdr (readers/source-logging-push-back-reader pbr)]
              (dotimes [_ (dec (:line m))] (readers/read-line rdr))
              (reader/read {:read-cond :allow :features #{:clj}} rdr))))))))

(defmacro import-macros [ns [& vars]]
  `(do
     ~@(binding [*ns* (find-ns ns)]
         (doall (map source-fn vars)))))

(defmacro alias [[_ ns] [_ alias]]
  (swap! env/*compiler* assoc-in [::namespaces (.getName *ns*) :requires]
    alias ns)
  nil)