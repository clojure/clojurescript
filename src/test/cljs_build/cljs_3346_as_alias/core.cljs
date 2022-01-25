(ns cljs-3346-as-alias.core
  (:require [clojure.set :as-alias set]
            [made.up.lib :as-alias lib]))

(println ::set/foo ::lib/bar)
