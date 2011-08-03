(ns dom.test
  (:require [clojure.browser.event :as event]
            [clojure.browser.dom   :as dom]))

(defn log [& args]
  (.log js/console (apply pr-str args)))

(defn log-obj [obj]
  (.log js/console obj))

(defn log-listener-count []
  (log "listener count: " (event/total-listener-count)))

(def source      (dom/get-element "source"))
(def destination (dom/get-element "destination"))

(dom/append source
            (dom/element "Testing me out"))

(def success-count (atom 0))

(log-listener-count)

(event/listen source
              :click
              (fn [e]
                (let [i (swap! success-count inc)
                      e (dom/element :li
                                     {:id "testing"
                                      :class "test me out please"}
                                     "It worked!")]
                  (log-obj e)
                  (log i)
                  (dom/append destination
                              e))))

(log-obj (dom/element "Text node"))
(log-obj (dom/element :li))
(log-obj (dom/element :li {:class "foo"}))
(log-obj (dom/element :li {:class "bar"} "text node"))
(log-obj (dom/element [:ul [:li :li :li]]))
(log-obj (dom/element :ul [:li :li :li]))
(log-obj (dom/element :li {} [:ul {} [:li :li :li]]))
(log-obj (dom/element [:li {:class "baz"} [:li {:class "quux"}]]))

(log-obj source)
(log-listener-count)