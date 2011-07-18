; Projects compiled with :target :nodejs can 'require' this namespace
; to get the nodejs globals loaded into cljs.nodejs and get
; ClojureScript's 'print' set up correctly.
(ns cljs.nodejs)

; Define namespaced references to Node's externed globals:
(def require (js* "require"))
(def process (js* "process"))

; Have ClojureScript print using Node's sys.print function
(set! cljs.core/string-print (.print (require "sys")))
