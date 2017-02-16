;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.pprint
  (:refer-clojure :exclude [deftype #?(:cljs macroexpand)])
  (:require [clojure.walk :as walk]
            #?(:cljs [cljs.analyzer :as ana])))


;; required the following changes:
;;  replace .ppflush with -ppflush to switch from Interface to Protocol

(defmacro with-pretty-writer [base-writer & body]
  `(let [base-writer# ~base-writer
         new-writer# (not (pretty-writer? base-writer#))]
     (cljs.core/binding [cljs.core/*out* (if new-writer#
                         (make-pretty-writer base-writer# *print-right-margin* *print-miser-width*)
                         base-writer#)]
       ~@body
       (-ppflush cljs.core/*out*))))


(defmacro getf
  "Get the value of the field a named by the argument (which should be a keyword)."
  [sym]
  `(~sym @@~'this))

;; change alter to swap!

(defmacro setf
  "Set the value of the field SYM to NEW-VAL"
  [sym new-val]
  `(swap! @~'this assoc ~sym ~new-val))

(defmacro deftype
  [type-name & fields]
  (let [name-str (name type-name)
        fields (map (comp symbol name) fields)]
    `(do
       (defrecord ~type-name [~'type-tag ~@fields])
       (defn- ~(symbol (str "make-" name-str))
         ~(vec fields)
         (~(symbol (str type-name ".")) ~(keyword name-str) ~@fields))
       (defn- ~(symbol (str name-str "?")) [x#] (= (:type-tag x#) ~(keyword name-str))))))

(defn- parse-lb-options [opts body]
  (loop [body body
         acc []]
    (if (opts (first body))
      (recur (drop 2 body) (concat acc (take 2 body)))
      [(apply hash-map acc) body])))

(defmacro pprint-logical-block
  "Execute the body as a pretty printing logical block with output to *out* which
  must be a pretty printing writer. When used from pprint or cl-format, this can be
  assumed.

  This function is intended for use when writing custom dispatch functions.

  Before the body, the caller can optionally specify options: :prefix, :per-line-prefix
  and :suffix."
  [& args]
  (let [[options body] (parse-lb-options #{:prefix :per-line-prefix :suffix} args)]
    `(do (if (cljs.pprint/level-exceeded)
           (~'-write cljs.core/*out* "#")
           (do
             (cljs.core/binding [cljs.pprint/*current-level* (inc cljs.pprint/*current-level*)
                       cljs.pprint/*current-length* 0]
               (cljs.pprint/start-block cljs.core/*out*
                                        ~(:prefix options)
                                        ~(:per-line-prefix options)
                                        ~(:suffix options))
               ~@body
               (cljs.pprint/end-block cljs.core/*out*))))
         nil)))

#?(:cljs
   (defn macroexpand [env form]
     (loop [form form
            form' (ana/macroexpand-1 env form)]
       (if-not (identical? form form')
         (recur form' (ana/macroexpand-1 env form'))
         form'))))

(defn- pll-mod-body [env var-sym body]
  (letfn [(inner [form]
                 (if (seq? form)
                   (let [form #?(:clj  (macroexpand form)
                                 :cljs (macroexpand env form))]
                     (condp = (first form)
                       'loop* form
                       'recur (concat `(recur (inc ~var-sym)) (rest form))
                       (walk/walk inner identity form)))
                   form))]
    (walk/walk inner identity body)))

(defmacro print-length-loop
  "A version of loop that iterates at most *print-length* times. This is designed
  for use in pretty-printer dispatch functions."
  [bindings & body]
  (let [count-var (gensym "length-count")
        mod-body (pll-mod-body &env count-var body)]
    `(loop ~(apply vector count-var 0 bindings)
       (if (or (not cljs.core/*print-length*) (< ~count-var cljs.core/*print-length*))
         (do ~@mod-body)
         (~'-write cljs.core/*out* "...")))))

(defn- process-directive-table-element [[char params flags bracket-info & generator-fn]]
  [char,
   {:directive char,
    :params `(array-map ~@params),
    :flags flags,
    :bracket-info bracket-info,
    :generator-fn (concat '(fn [params offset]) generator-fn)}])

(defmacro ^{:private true}
  defdirectives
  [& directives]
  `(def ^{:private true}
        ~'directive-table (hash-map ~@(mapcat process-directive-table-element directives))))

(defmacro formatter
  "Makes a function which can directly run format-in. The function is
fn [stream & args] ... and returns nil unless the stream is nil (meaning
output to a string) in which case it returns the resulting string.

format-in can be either a control string or a previously compiled format."
  [format-in]
  `(let [format-in# ~format-in
         my-c-c# cljs.pprint/cached-compile
         my-e-f# cljs.pprint/execute-format
         my-i-n# cljs.pprint/init-navigator
         cf# (if (string? format-in#) (my-c-c# format-in#) format-in#)]
     (fn [stream# & args#]
       (let [navigator# (my-i-n# args#)]
         (my-e-f# stream# cf# navigator#)))))

(defmacro formatter-out
  "Makes a function which can directly run format-in. The function is
fn [& args] ... and returns nil. This version of the formatter macro is
designed to be used with *out* set to an appropriate Writer. In particular,
this is meant to be used as part of a pretty printer dispatch method.

format-in can be either a control string or a previously compiled format."
  [format-in]
  `(let [format-in# ~format-in
         cf# (if (string? format-in#) (cljs.pprint/cached-compile format-in#) format-in#)]
     (fn [& args#]
       (let [navigator# (cljs.pprint/init-navigator args#)]
         (cljs.pprint/execute-format cf# navigator#)))))

(defmacro with-pprint-dispatch
  "Execute body with the pretty print dispatch function bound to function."
  [function & body]
  `(cljs.core/binding [cljs.pprint/*print-pprint-dispatch* ~function]
     ~@body))

(defmacro pp
  "A convenience macro that pretty prints the last thing output. This is
exactly equivalent to (pprint *1)."
  {:added "1.2"}
  [] `(cljs.pprint/pprint *1))
