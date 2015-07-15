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

(defn enable-util-print! []
  (set! *print-newline* false)
  (set! *print-fn*
    (fn [& args]
      (.apply (.-log js/console) js/console (into-array args))))
  (set! *print-err-fn*
    (fn [& args]
      (.apply (.-error js/console) js/console (into-array args))))
  nil)
