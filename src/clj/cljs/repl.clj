;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.repl
  (:require [cljs.compiler :as comp]))

;; All of this is a work in progress.

(defprotocol IEvaluator
  (setup [this])
  (evaluate [this form])
  (tear-down [this]))

(defn repl
  "This is just a stub REPL which does no compilation. It is currently
  being used for testing."
  [repl-env]
  (prn "Type: " :cljs/quit " to quit")
  (setup repl-env)
  (loop []
    (print (str "ClojureScript:> "))
    (flush)
    (let [form (read)]
      (cond (= form :cljs/quit) :quit
            :else (let [ret (evaluate repl-env (str form))]
                    (prn ret)
                    (recur)))))
  (tear-down repl-env))

