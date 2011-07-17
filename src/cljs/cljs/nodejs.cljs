(ns cljs.nodejs)

; Define namespaced references to Node's externed globals:
(def require (js* "require"))
(def process (js* "process"))

; Have ClojureScript print using Node's sys.print function
(set! cljs.core/string-print (.print (require "sys")))
