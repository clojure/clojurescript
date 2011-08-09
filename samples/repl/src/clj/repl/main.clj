(ns repl.main
  (:require [compojure.core     :as compojure]
            [cljs.compiler      :as compiler]
            [compojure.route    :as route]
            [ring.util.response :as response]))

(def client-count (atom 0))
(def requests     (atom []))
(def responses    (atom {}))

(defn integer
  [i]
  (try
    (java.lang.Integer/parseInt i)
    (catch Throwable t
      -1)))

(defn compile-form
  [form]
  (compiler/emits
   (compiler/analyze {:context :statement
                      :locals {}
                      :ns {:name 'cljs.user}}
                     (read-string form))))

(compojure/defroutes root
  (compojure/GET "/" []
                 (response/file-response "resources/public/index.html"))
  (route/resources "/static" {:root "public/javascripts"})
  (route/not-found "<h1>Page not found</h1>"))

(compojure/defroutes clients
  (compojure/POST "/clients" [] (str (swap! client-count inc))))

(compojure/defroutes javascript
  (compojure/GET "/javascript/:id" [id]
                 (when-let [form (get @requests (integer id))]
                   (compile-form form))))

(compojure/defroutes clojurescript
  (compojure/POST "/clojurescript/in" {body :body}
                  (let [form (slurp body)]
                    (swap! requests conj form)
                    form))

  (compojure/POST "/clojurescript/out/:id" [id :as {body :body}]
                  (let [id (integer id)
                        rs (get @responses id [])]
                    (if (or (empty? rs)
                            (= body (first rs)))
                      (swap! responses assoc id (conj rs body))
                      {:status  500
                       :headers {}
                       :body    "This javascript evaluation was different from the other clients"})))

  (compojure/GET "/clojurescript/out/:id" [id]
                 (when-let [rs (get @responses id)]
                   (when (= (count rs) @client-count)
                     (first rs)))))

(def handler (compojure/routes clients
                               javascript
                               clojurescript
                               root))