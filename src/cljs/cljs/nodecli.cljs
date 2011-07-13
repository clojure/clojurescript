(ns cljs.nodecli)

; yuck.  should use something better to "extern" these Node things:
(def require (js* "require"))
(def process (js* "process"))
(def argv (aget process "argv"))

; Now, grap Node's 'sys' module
(def sys (require "sys"))

; Have ClojureScript print using Node's sys.print function
(set! cljs.core/string-print (.print sys))

; Call the user's main function
(apply cljs.core/*main-cli-fn* argv)
