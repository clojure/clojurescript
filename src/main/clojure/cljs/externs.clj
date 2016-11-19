;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.externs
  (:require [clojure.string :as string]
            [cljs.util :as util])
  (:import [java.util.logging Level]
           [com.google.javascript.jscomp
            CompilerOptions SourceFile JsAst CommandLineRunner]
           [com.google.javascript.rhino Node Token]))

;; ------------------------------------------------------------------------------
;; Externs Parsing

(defmulti parse-extern-node (fn [^Node node] (.getType node)))

(defmethod parse-extern-node Token/VAR [node]
  (when (> (.getChildCount node) 0)
    (parse-extern-node (.getFirstChild node))))

(defmethod parse-extern-node Token/EXPR_RESULT [node]
  (when (> (.getChildCount node) 0)
    (parse-extern-node (.getFirstChild node))))

(defmethod parse-extern-node Token/ASSIGN [node]
  (when (> (.getChildCount node) 0)
    (let [lhs (first (parse-extern-node (.getFirstChild node)))]
      (if (> (.getChildCount node) 1)
        (let [externs (parse-extern-node (.getChildAtIndex node 1))]
          (conj (map (fn [ext] (concat lhs ext)) externs)
            lhs))
        [lhs]))))

(defmethod parse-extern-node Token/NAME [node]
  (let [lhs (map symbol (string/split (.getQualifiedName node) #"\."))]
    (if (> (.getChildCount node) 0)
      (let [externs (parse-extern-node (.getFirstChild node))]
        (conj (map (fn [ext] (concat lhs ext)) externs)
          lhs))
      [lhs])))

(defmethod parse-extern-node Token/GETPROP [node]
  [(map symbol (string/split (.getQualifiedName node) #"\."))])

(defmethod parse-extern-node Token/OBJECTLIT [node]
  (when (> (.getChildCount node) 0)
    (loop [nodes (.children node)
           externs []]
      (if (empty? nodes)
        externs
        (recur (rest nodes)
          (concat externs (parse-extern-node (first nodes))))))))

(defmethod parse-extern-node Token/STRING_KEY [node]
  (let [lhs (map symbol (string/split (.getString node) #"\."))]
    (if (> (.getChildCount node) 0)
      (let [externs (parse-extern-node (.getFirstChild node))]
        (conj (map (fn [ext] (concat lhs ext)) externs)
          lhs))
      [lhs])))

(defmethod parse-extern-node :default [node])

(defn parse-externs [^SourceFile source-file]
  (let [^CompilerOptions compiler-options (CompilerOptions.)
        closure-compiler
        (doto
          (let [compiler (com.google.javascript.jscomp.Compiler.)]
            (com.google.javascript.jscomp.Compiler/setLoggingLevel Level/WARNING)
            compiler)
          (.init (list source-file) '() compiler-options))
        js-ast (JsAst. source-file)
        ^Node root (.getAstRoot js-ast closure-compiler)]
    (loop [nodes (.children root)
           externs []]
      (if (empty? nodes)
        externs
        (let [node (first nodes)
              new-extern (parse-extern-node node)]
          (recur (rest nodes) (concat externs new-extern)))))))

(defn index-externs [externs]
  (reduce
    (fn [m xs]
      (cond-> m
        (seq xs) (update-in xs merge {})))
    {} externs))

(defn default-externs []
  (let [xs (CommandLineRunner/getDefaultExterns)]
    (reduce
      (fn [externs externs-file]
        (util/map-merge
          externs (index-externs (parse-externs externs-file))))
      {} xs)))

(comment
  (default-externs)

  ;; webkit_dom.js defines Console and Window.prototype.console
  (filter
    (fn [s]
      (let [m (-> s parse-externs index-externs)]
        (get-in m '[Window prototype console])))
    (CommandLineRunner/getDefaultExterns))

  (->
    (filter
      (fn [s]
        (= "externs.zip//webkit_dom.js" (.getName s)))
      (CommandLineRunner/getDefaultExterns))
    first parse-externs index-externs)

  )