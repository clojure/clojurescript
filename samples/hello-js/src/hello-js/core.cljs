(ns hello-js.core)

(defn ^:export popup-msg
  [msg]
  (js/send_alert msg))

(popup-msg "ClojureScript calling a global function defined in an external JavaScript library")

(popup-msg (str "ClojureScript: the time is now " (js/Date.)))
