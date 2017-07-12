(ns emit-node-requires-test.native-modules
  (:require [path :refer [isAbsolute]]))

(enable-console-print!)

(println (isAbsolute (path/resolve js/__filename)))
