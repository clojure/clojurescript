;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.server.node
  (:require [cljs.env :as env]
            [cljs.repl :as repl]
            [cljs.repl.node :as node]
            [cljs.core.server :as server])
  (:import [java.net Socket]))

(defonce envs (atom {}))

(defn env-opts->key [{:keys [host port]}]
  [host port])

(defn stale? [{:keys [socket] :as repl-env}]
  (if-let [sock (:socket @socket)]
    (.isClosed ^Socket sock)
    false))

(defn get-envs [env-opts]
  (let [env-opts (merge {:host "localhost" :port 49001} env-opts)
        k (env-opts->key env-opts)]
    (swap! envs
      #(cond-> %
         (or (not (contains? % k))
             (stale? (get-in % [k 0])))
         (assoc k
           [(node/repl-env* env-opts)
            (env/default-compiler-env)])))
    (get @envs k)))

(defn repl
  ([]
   (repl nil))
  ([{:keys [opts env-opts]}]
   (let [[env cenv] (get-envs env-opts)]
     (env/with-compiler-env cenv
       (repl/repl* env opts)))))

(defn prepl
  ([]
   (prepl nil))
  ([{:keys [opts env-opts]}]
   (let [[env cenv] (get-envs env-opts)]
     (env/with-compiler-env cenv
       (apply server/io-prepl
         (mapcat identity
           {:repl-env env :opts opts}))))))
