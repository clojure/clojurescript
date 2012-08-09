(ns clojure.reflect
  (:refer-clojure :exclude [meta])
  (:require [clojure.browser.net :as net]
            [clojure.browser.event :as event]))

(defn- evaluate-javascript [block]
  (let [result (try (js* "eval(~{block})")
                    (catch js/Error e
                      (.log js/console e)))]
    result))

(defn- query-reflection
  "Issues a GET to /reflect with a single query-parameter string.
  Calls cb with the result."
  [query-param cb]
  (let [conn (net/xhr-connection)
        url  (str "/reflect?" query-param)]
    (event/listen conn :success (fn [e]
                                  (let [resp (.getResponseText e/currentTarget ())]
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
