(ns repl.endpoint
  (:use compojure.core)
  (:require [compojure.route :as route]))

(defroutes endpoint
  (GET  "/javascript" [] "1+1;")
  (POST "/javascript" {{body :body} :params} body)
  (route/resources "/")
  (route/not-found "<h1>Page not found</h1>"))
