;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software

(ns cljs.loader
  (:refer-clojure :exclude [load])
  (:require [cljs.module-graph :as mg]
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]))

(defn load-expr
  ([env module-name]
   (load-expr env module-name nil))
  ([env module-name cb]
   (let [loader (mg/module-for (-> env :ns :name)
                  (:modules (ana-api/get-options)))]
     `(cljs.loader/load* ~module-name ~loader ~cb))))

(defmacro load
  "Load a module. module-name should be a keyword matching a :modules module
   definition."
  ([module-name]
   (load-expr &env module-name))
  ([module-name cb]
   (load-expr &env module-name cb)))