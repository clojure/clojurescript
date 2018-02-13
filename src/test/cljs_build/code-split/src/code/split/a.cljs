(ns code.split.a
  (:require [cljs.loader :as loader]
            [clojure.pprint :refer [pprint]]
            [goog.dom :as gdom]
            [goog.events :as events])
  (:import [goog.debug Console]
           [goog.events EventType]))

(def loader
  "The module loader."
  (.getLoader loader/*module-manager*))

;; Enable logging, to see debug messages.
(.setCapturing (Console.) true)

(defn print-modules []
  (println "Module Infos:")
  (pprint loader/module-infos)
  (println "Module URIs:")
  (pprint loader/module-uris))

(events/listen (gdom/getElement "button-b") EventType.CLICK
               (fn [e] (loader/load :b #((resolve 'code.split.b/hello)))))

(events/listen (gdom/getElement "button-c") EventType.CLICK
               (fn [e] (loader/load :c #((resolve 'code.split.c/hello)))))

(enable-console-print!)
(print-modules)

(loader/set-loaded! :a)
