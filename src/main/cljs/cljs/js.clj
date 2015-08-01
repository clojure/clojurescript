;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.js
  (:require [cljs.env :as env]
            [cljs.env.macros :as menv]
            [cljs.analyzer :as ana]
            [clojure.java.io :as io]))

(defmacro with-state
  [state & body]
  `(menv/with-compiler-env ~state
     ~@body))

(defmacro dump-core []
  (let [state @env/*compiler*]
    (if-not (false? (get-in state [:options :dump-core]))
      `(quote ~(get-in state [::ana/namespaces 'cljs.core]))
      `(hash-map))))
