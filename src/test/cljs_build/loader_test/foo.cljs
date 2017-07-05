(ns loader-test.foo
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [cljs.loader :as loader])
  (:import [goog.events EventType]))

(enable-console-print!)

(println "Hello from foo!")

(events/listen (gdom/getElement "button") EventType.CLICK
  (fn [e]
    (loader/load :bar
      (fn []
        ((resolve 'loader-test.bar/woz))))))
