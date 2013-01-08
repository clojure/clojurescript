(ns cljs.compiler-macros)

(defmacro emit-wrap [env & body]
  `(let [env# ~env]
     (when (= :return (:context env#)) (cljs.compiler/emits "return "))
     ~@body
     (when-not (= :expr (:context env#)) (cljs.compiler/emitln ";"))))

(defmacro with-core-cljs
  "Ensure that core.cljs has been loaded."
  [& body]
  `(do (when-not (:defs (get @cljs.analyzer/namespaces 'cljs.core))
         (println "// analyzing cljs/core.cljs")
         (cljs.analyzer/analyze-file "cljs/core.cljs"))
       ~@body))

