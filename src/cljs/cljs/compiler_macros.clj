(ns cljs.compiler-macros)

(defmacro emit-wrap [env & body]
  `(let [env# ~env]
     (when (= :return (:context env#)) (cljs.compiler/emits "return "))
     ~@body
     (when-not (= :expr (:context env#)) (cljs.compiler/emitln ";"))))

