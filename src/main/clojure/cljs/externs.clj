;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.externs
  (:require [cljs.util :as util]
            [cljs.js-deps :as js-deps]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.google.javascript.jscomp
            CompilerOptions CompilerOptions$Environment SourceFile CompilerInput CommandLineRunner]
           [com.google.javascript.jscomp.parsing Config$JsDocParsing]
           [com.google.javascript.rhino
            Node Token JSTypeExpression JSDocInfo JSDocInfo$Visibility]
           [java.util.logging Level]
           [java.net URL]))

(def ^:dynamic *ignore-var* false)
(def ^:dynamic *source-file* nil)
(def ^:dynamic *goog-ns* nil)

(defn default-externs []
  (CommandLineRunner/getBuiltinExterns CompilerOptions$Environment/BROWSER))

;; ------------------------------------------------------------------------------
;; Externs Parsing

(defn annotate
  "Given a sequential list of properties [foo core baz] representing segments
  of the namespace, annotate the last symbol with the type information."
  [props ty]
  (when (seq props)
    (conj
      (into [] (butlast props))
      (with-meta (last props) ty))))

(def token->kw
  {Token/BANG      :bang
   Token/BLOCK     :block
   Token/PIPE      :pipe
   Token/STRINGLIT :string-lit
   Token/QMARK     :qmark
   Token/STAR      :star})

(defn parse-texpr [^Node root]
  (when-let [token (get token->kw (.getToken root))]
    (let [children (.children root)]
      (merge
        {:type token}
        (when-not (empty? children)
          {:children (vec (map parse-texpr (.children root)))})
        (when (= :string-lit token)
          {:value (.getString root)})))))

(defn undefined?
  [{:keys [type value] :as texpr}]
  (and (= type :string-lit)
       (= "undefined" value)))

(defn add-prefix
  "Externs inference uses :prefix meta to both resolve externs as well as generate
  missing externs information. Google Closure Compiler default externs includes
  nested types like webCrypto.Crypto. Add prefix information to the returned symbol to
  simplify resolution later."
  [type-str]
  (with-meta (symbol type-str)
    {:prefix (->> (string/split (name type-str) #"\.")
               (map symbol) vec)}))

(defn simplify-texpr
  [texpr]
  (case (:type texpr)
    :string-lit    (-> texpr :value add-prefix)
    :star          'any
    ;; TODO: qmark should probably be #{nil T}
    (:qmark :bang) (simplify-texpr (-> texpr :children first))
    :pipe          (let [[x y] (:children texpr)]
                     (if (undefined? y)
                       (simplify-texpr x)
                       'any))
    'any))

(defn get-tag [^JSTypeExpression texpr]
  (some-> (.getRoot texpr) parse-texpr simplify-texpr))

(defn params->method-params [xs]
  (let [not-opt? (complement :optional?)
        required (into [] (map :name (take-while not-opt? xs)))
        opts     (map :name (drop-while not-opt? xs))]
    (loop [ret [required] opts opts]
      (if-let [opt (first opts)]
        (recur (conj ret (conj (last ret) opt)) (drop 1 opts))
        (seq ret)))))

(defn generic? [t]
  (let [s (name t)]
    (boolean (re-matches #"[A-Z]" s))))

(defn gtype->cljs-type [t]
  (when t
    (cond
      (generic? t) 'any
      (= t 'Array) 'array
      :else t)))

(defn get-params
  "Return param information in JSDoc appearance order. GCL is relatively
  civilized, so this isn't really a problem."
  [^JSDocInfo info]
  (map
    (fn [n]
      (let [t (.getParameterType info n)]
        {:name      (symbol n)
         :optional? (.isOptionalArg t)
         :var-args? (.isVarArgs t)}))
    (.getParameterNames info)))

(defn get-var-info [^Node node]
  (when node
    (let [info (.getJSDocInfo node)]
      (when info
        (merge
          (if-let [^JSTypeExpression ty (.getType info)]
            {:tag (get-tag ty)}
            (if (or (.isConstructor info) (.isInterface info))
              (let [qname (symbol (.. node getFirstChild getQualifiedName))]
                (cond-> {:tag 'Function}
                  (.isConstructor info) (merge {:ctor qname})
                  (.isInterface info)   (merge {:iface qname})
                  (.hasBaseType info)   (merge {:super (get-tag (.getBaseType info))})))
              (if (or (.hasReturnType info)
                      (as-> (.getParameterCount info) c
                        (and c (pos? c))))
                (let [arglist  (get-params info)
                      arglists (params->method-params arglist)]
                  {:tag             'Function
                   :js-fn-var       true
                   :ret-tag         (or (some-> (.getReturnType info)
                                          get-tag gtype->cljs-type)
                                        'clj-nil)
                   :variadic?       (boolean (some :var-args? arglist))
                   :max-fixed-arity (count (take-while (complement :var-args?) arglist))
                   :method-params   arglists
                   :arglists        arglists}))))
          {:file *source-file*
           :line (.getLineno node)}
          (when-let [doc (.getOriginalCommentString info)]
            {:doc doc})
          (when (= JSDocInfo$Visibility/PRIVATE (.getVisibility info))
            {:private true}))))))

(defmulti parse-extern-node
  (fn [^Node node]
    (.getToken node)))

;; handle named function case (i.e. goog.modules)
;; function foo {}, the entire function is the node
(defmethod parse-extern-node Token/FUNCTION [^Node node]
  (when (> (.getChildCount node) 0)
    (let [ty (get-var-info node)]
      (doto
        (cond-> (parse-extern-node (.getFirstChild node))
          ty (-> first (annotate ty) vector))))))

(defmethod parse-extern-node Token/VAR [^Node node]
  (when (> (.getChildCount node) 0)
    (let [ty (get-var-info node)]
      (cond-> (parse-extern-node (.getFirstChild node))
        ty (-> first (annotate ty) vector)))))

(defmethod parse-extern-node Token/EXPR_RESULT [^Node node]
  (when (> (.getChildCount node) 0)
    (parse-extern-node (.getFirstChild node))))

(defmethod parse-extern-node Token/ASSIGN [^Node node]
  (when (> (.getChildCount node) 0)
    (let [ty  (get-var-info node)
          lhs (cond-> (first (parse-extern-node (.getFirstChild node)))
                ty (annotate ty))]
      (if (> (.getChildCount node) 1)
        (let [externs
              (binding [*ignore-var* true]
                (parse-extern-node (.getChildAtIndex node 1)))]
          (conj (map (fn [ext] (concat lhs ext)) externs)
            lhs))
        [lhs]))))

;; JavaScript name
;; function foo {}, in this case the `foo` name node
;; {"foo": bar}, in this case the `bar` name node
(defmethod parse-extern-node Token/NAME [^Node node]
  (if (= Token/STRING_KEY (-> node .getParent .getToken))
    ;; if we are inside an object literal we are done
    []
    ;; also check .getString - goog.module defs won't have qualified names
    (let [name (or (.getQualifiedName node) (.getString node))
          lhs  (when-not (string/blank? name)
                 (map symbol (string/split name #"\.")))]
      (if (seq lhs)
        (if (> (.getChildCount node) 0)
          (let [externs (parse-extern-node (.getFirstChild node))]
            (conj (map (fn [ext] (concat lhs ext)) externs)
              lhs))
          [lhs])
        []))))

(defmethod parse-extern-node Token/GETPROP [^Node node]
  (when-not *ignore-var*
    (let [props (map symbol (string/split (.getQualifiedName node) #"\."))]
      [(if-let [ty (get-var-info node)]
         (annotate props ty)
         props)])))

;; JavaScript Object literal
;; { ... }
(defmethod parse-extern-node Token/OBJECTLIT [^Node node]
  (when (> (.getChildCount node) 0)
    (loop [nodes (.children node)
           externs []]
      (if (empty? nodes)
        externs
        (recur (rest nodes)
          (concat externs (parse-extern-node (first nodes))))))))

;; Object literal string key node
;; {"foo": bar} - the key and value together
(defmethod parse-extern-node Token/STRING_KEY [^Node node]
  (let [lhs [(-> node .getString symbol)]]
    (if (> (.getChildCount node) 0)
      (let [externs (parse-extern-node (.getFirstChild node))]
        (conj (map (fn [ext] (concat lhs ext)) externs)
          lhs))
      [lhs])))

(defmethod parse-extern-node :default [node])

(defn parse-externs
  "Returns a sequential collection of the form:

    [[foo core first]
     [foo core next]
     [foo core baz last] ...]

  Where the last symbol is annotated with var info via metadata. This simple
  structure captures the nested form of Closure namespaces and aids
  direct indexing."
  [^SourceFile source-file]
  (binding [*source-file* (.getName source-file)]
    (let [^CompilerOptions compiler-options
          (doto (CompilerOptions.)
            (.setParseJsDocDocumentation
              Config$JsDocParsing/INCLUDE_DESCRIPTIONS_WITH_WHITESPACE))
          closure-compiler
          (doto
            (let [compiler (com.google.javascript.jscomp.Compiler.)]
              (com.google.javascript.jscomp.Compiler/setLoggingLevel Level/WARNING)
              compiler)
            (.init (list source-file) '() compiler-options))
          js-ast (CompilerInput. source-file)
          ^Node root (.getAstRoot js-ast closure-compiler)
          ;; TODO: switch to getFirstChild + getNext in the loop
          nodes (.children root)]
      (loop [nodes (cond-> nodes
                     ;; handle goog.modules which won't have top-levels
                     ;; need to look at internal children
                     (= Token/MODULE_BODY (some-> nodes ^Node (first) .getToken))
                     (-> ^Node (first) .children))
             externs []]
        (if (empty? nodes)
          externs
          (let [node (first nodes)
                new-extern (parse-extern-node node)]
            (recur (rest nodes) (concat externs new-extern))))))))

(defn index-externs [externs]
  (reduce
    (fn [m xs]
      (cond-> m
        (seq xs) (update-in xs merge {})))
    {} externs))

(defn externs-map*
  ([]
 (externs-map* (default-externs)))
  ([sources]
   (externs-map* sources
     '{eval {}
       global {}
       goog {nodeGlobalRequire {}}
       COMPILED {}
       TypeError {}
       Error {prototype {number {} columnNumber {}}}
       ReferenceError {}}))
  ([sources defaults]
   (let [sources (if-not (empty? sources)
                   sources
                   (default-externs))]
     (reduce
       (fn [externs externs-file]
         (util/map-merge
           externs (index-externs (parse-externs externs-file))))
       defaults sources))))

(def externs-map (memoize externs-map*))

(defn ns-match? [ns-segs var-segs]
  (or
    ;; exact match (i.e. ctors)
    (= ns-segs var-segs)
    (and
      (= (inc (count ns-segs)) (count var-segs))
      (= ns-segs (take (count ns-segs) var-segs)))))

(defmulti parsed->defs (fn [_ module-type] module-type))

(defmethod parsed->defs :goog
  ([externs _]
   (let [grouped (group-by #(= 'exports (first %)) externs)
         exports (->> (get grouped true)
                   (map (comp vec rest))
                   (remove empty?)
                   set)
         exported (filter exports (get grouped false))]
     (reduce
       (fn [m xs]
         (let [sym (last xs)]
           (cond-> m
             (seq xs) (assoc sym (merge (meta sym) {:ns *goog-ns* :name sym})))))
       {} exported))))

(defmethod parsed->defs :default
  ([externs _]
   (let [ns-segs (into [] (map symbol (string/split (str *goog-ns*) #"\.")))]
     (reduce
       (fn [m xs]
         ;; ignore definitions from other provided namespaces not under consideration
         (if (ns-match? ns-segs xs)
           (let [sym (last xs)]
             (cond-> m
               (seq xs) (assoc sym (merge (meta sym) {:ns *goog-ns* :name sym}))))
           m))
       {} externs))))

(defn resource->source-file
  [^URL resource]
  (-> (SourceFile/builder)
    (.withPath (.getPath resource))
    (.withContent (io/input-stream resource))
    (.build)))

(defn analyze-goog-file
  ([f]
   (analyze-goog-file f nil))
  ([f ns]
   (let [rsrc (io/resource f)
         desc (js-deps/parse-js-ns (line-seq (io/reader rsrc)))
         ns   (or ns (-> (:provides desc) first symbol))]
     (binding [*goog-ns* ns]
       {:name ns
        :defs (parsed->defs
                (parse-externs (resource->source-file rsrc))
                (:module desc))}))))

(defn info
  "Helper for grabbing var info from an externs map.
  Example:
    (info externs '[Number isNaN])
  See `externs-map`"
  [externs props]
  (-> externs
    (get-in (butlast props))
    (find (last props))
    first meta))

(defn filtered-externs [f]
  (->>
    (filter
      #(= f (.getName %))
      (default-externs))
    first parse-externs index-externs))

(comment
  (require '[clojure.java.io :as io]
           '[cljs.closure :as closure]
           '[clojure.pprint :refer [pprint]]
           '[cljs.js-deps :as js-deps])

  (resource->source-file (io/resource "goog/dom/dom.js"))

  (pprint
    (get-in (analyze-goog-file "goog/dom/dom.js")
      [:defs 'setTextContent]))

  (pprint (analyze-goog-file "goog/string/string.js"))

  (get (js-deps/js-dependency-index {}) "goog.string")

  ;; {:tag Function :ret-tag boolean}
  (->
    (nth
      (parse-externs
        (closure/js-source-file "goog/string/string.js"
          (io/input-stream (io/resource "goog/string/string.js"))))
      2)
    last meta)

  (parse-externs
    (closure/js-source-file "goog/string/string.js"
      (io/input-stream (io/resource "goog/string/string.js"))))

  (-> (externs-map
        [(closure/js-source-file "goog/string/string.js"
           (io/input-stream (io/resource "goog/string/string.js")))]
        {})
    (get-in '[goog string])
    (find 'numberAwareCompare_)
    first meta)

  (-> (externs-map
        [(closure/js-source-file "goog/date/date.js"
           (io/input-stream (io/resource "goog/date/date.js")))]
        {})
    (get-in '[goog date month])
    )

  (pprint (analyze-goog-file "goog/date/date.js" 'goog.date.month))

  (externs-map)

  (-> (externs-map)
    (find 'console) first meta)

  (get (externs-map) 'Function)

  (get (externs-map) 'Error)

  ;; values are not on the prototype
  (get (externs-map) 'Symbol)
  (get (externs-map) 'Number)

  (-> (get-in (externs-map) '[Window prototype])
    (find 'performance) first meta)

  ;; webkit_dom.js defines Console and Window.prototype.console
  (filter
    (fn [s]
      (let [m (-> s parse-externs index-externs)]
        (get-in m '[Window prototype console])))
    (default-externs))

  (->
    (filter
      (fn [s]
        (= "externs.zip//webkit_dom.js" (.getName s)))
      (default-externs))
    first parse-externs index-externs
    (find 'console) first meta)

  (->
    (filter
      (fn [s]
        (= "externs.zip//webkit_dom.js" (.getName s)))
      (default-externs))
    first parse-externs index-externs
    (get-in '[Console prototype])
    (find 'log) first meta)

  (require '[clojure.java.io :as io]
           '[cljs.closure :as cc])

  (-> (cc/js-source-file nil (io/file "react.ext.js"))
    parse-externs index-externs
    (get 'React)
    (find 'Component) first meta)
  )
