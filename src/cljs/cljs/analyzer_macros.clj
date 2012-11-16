(ns cljs.analyzer-macros)

(defmacro disallowing-recur [& body]
  `(binding [cljs.analyzer/*recur-frames* (cons nil cljs.analyzer/*recur-frames*)] ~@body))
