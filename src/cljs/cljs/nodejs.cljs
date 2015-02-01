;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Projects compiled with :target :nodejs can 'require' this namespace
; to get the nodejs globals loaded into cljs.nodejs and get
; ClojureScript's 'print' set up correctly.
(ns cljs.nodejs)

; Define namespaced references to Node's externed globals:
(def require (js* "require"))
(def process (js* "process"))

; Have ClojureScript print using Node's sys.print function
(defn enable-util-print! []
  (set! *print-fn* (.-print (require "util"))))

; Export NodeJS globals.
(.exportSymbol js/goog "setTimeout" js/setTimeout)
(.exportSymbol js/goog "clearTimeout" js/clearTimeout)
(.exportSymbol js/goog "setInterval" js/setInterval)
(.exportSymbol js/goog "clearInterval" js/clearInterval)
