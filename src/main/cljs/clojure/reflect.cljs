;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.reflect
  ^{:doc "DEPRECATED. Do not use, superceded by REPL enhancements."}
  (:refer-clojure :exclude [meta macroexpand])
  (:require [clojure.browser.net :as net]
            [clojure.browser.event :as event]))

(defn- evaluate-javascript [block]
  (let [result (try (js* "eval(~{block})")
                    (catch :default e
                      (.log js/console e)))]
    result))

(defn- query-reflection
  "Issues a GET to /reflect with a single query-parameter string.
  Calls cb with the result."
  [query-param cb]
  (let [conn (net/xhr-connection)
        url  (str "/reflect?" query-param)]
    (event/listen conn :success (fn [e]
                                  (let [resp (.getResponseText (.-currentTarget e) ())]
                                    (cb resp))))
    (event/listen conn :error #(println "Reflection query failed."))
    (net/transmit conn url)))

(defn meta
  "Queries the reflection api with a fully qualified symbol, then calls
  callback fn cb with the evaluated cljs map containing that symbol's
  meta information."
  [sym cb]
  (query-reflection (str "var=" (js/encodeURIComponent (str sym)))
                    #(cb (evaluate-javascript %))))

(defn macroexpand
  "Queries the reflection api with a quoted macro form, then calls the
  callback function with the macroexpanded form, as a string."
  [form]
  (query-reflection (str "macroform=" (js/encodeURIComponent (str form))) println))

(defn print-doc [{:keys [name method-params doc]}]
  (when-not (empty? name)
    (println name)
    (println method-params)
    (println doc)))

(defn doc
  "Queries the reflection api with a fully qualified symbol, then prints
  documentation information at the repl."
  [sym]
  (meta sym print-doc))
