(ns hello.extern-example)

(defn ^:export foo [] 42)

(external.lib/send_alert "ClojureScript calling a function defined in an externed JavaScript library")
