;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl.browser
  (:refer-clojure :exclude [loaded-libs])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [cljs.compiler :as comp]
            [cljs.closure :as cljsc]
            [cljs.repl :as repl]
            [cljs.repl.server :as server])
  (:import cljs.repl.IJavaScriptEnv
           [java.util.regex Pattern]))

(defonce browser-state (atom {:return-value-fn nil
                              :client-js nil}))

(def loaded-libs (atom #{}))
(def preloaded-libs (atom #{}))

(defn- set-return-value-fn
  "Save the return value function which will be called when the next
  return value is received."
  [f]
  (swap! browser-state (fn [old] (assoc old :return-value-fn f))))

(defn send-for-eval
  "Given a form and a return value function, send the form to the
  browser for evaluation. The return value function will be called
  when the return value is received."
  ([form return-value-fn]
     (send-for-eval @(server/connection) form return-value-fn))
  ([conn form return-value-fn]
     (do (set-return-value-fn return-value-fn)
         (server/send-and-close conn 200 form "text/javascript"))))

(defn- return-value
  "Called by the server when a return value is received."
  [val]
  (when-let [f (:return-value-fn @browser-state)]
    (f val)))

(defn repl-client-js []
  (slurp @(:client-js @browser-state)))

(defn send-repl-client-page
  [request conn opts]
  (server/send-and-close conn 200
    (str "<html><head><meta charset=\"UTF-8\"></head><body>
          <script type=\"text/javascript\">"
         (repl-client-js)
         "</script>"
         "<script type=\"text/javascript\">
          clojure.browser.repl.client.start(\"http://" (-> request :headers :host) "\");
          </script>"
         "</body></html>")
    "text/html"))

(defn send-static [{path :path :as request} conn opts]
  (if (and (:static-dir opts)
           (not= "/favicon.ico" path))
    (let [path   (if (= "/" path) "/index.html" path)
          st-dir (:static-dir opts)
          local-path (cond->
                       (seq (for [x (if (string? st-dir) [st-dir] st-dir)
                                  :when (.exists (io/file (str x path)))]
                              (str x path)))
                       (complement nil?) first)
          local-path (if (nil? local-path)
                       (cond
                         (re-find #".jar" path)
                         (io/resource (second (string/split path #".jar!/")))
                         (re-find (Pattern/compile (System/getProperty "user.dir")) path)
                         (io/file (string/replace path (str (System/getProperty "user.dir") "/") ""))
                         :else nil)
                       local-path)]
      (if local-path
        (server/send-and-close conn 200 (slurp local-path)
          (condp #(.endsWith %2 %1) path
            ".html" "text/html"
            ".css" "text/css"
            ".html" "text/html"
            ".jpg" "image/jpeg"
            ".js" "text/javascript"
            ".cljs" "text/x-clojure"
            ".map" "application/json"
            ".png" "image/png"
            "text/plain"))
        (server/send-404 conn path)))
    (server/send-404 conn path)))

(server/dispatch-on :get
                    (fn [{:keys [path]} _ _] (.startsWith path "/repl"))
                    send-repl-client-page)

(server/dispatch-on :get
                    (fn [{:keys [path]} _ _] (or (= path "/")
                                                (.endsWith path ".js")
                                                (.endsWith path ".cljs")
                                                (.endsWith path ".map")
                                                (.endsWith path ".html")))
                    send-static)

(defmulti handle-post (fn [m _ _ ] (:type m)))

(server/dispatch-on :post (constantly true) handle-post)

(def ordering (agent {:expecting nil :fns {}}))

(defmethod handle-post :ready [_ conn _]
  (do (reset! loaded-libs @preloaded-libs)
      (send ordering (fn [_] {:expecting nil :fns {}}))
      (send-for-eval conn
                     (cljsc/-compile
                      '[(ns cljs.user)
                        (set! *print-fn* clojure.browser.repl/repl-print)] {})
                     identity)))

(defn add-in-order [{:keys [expecting fns]} order f]
  {:expecting (or expecting order) :fns (assoc fns order f)})

(defn run-in-order [{:keys [expecting fns]}]
  (loop [order expecting
         fns fns]
    (if-let [f (get fns order)]
      (do (f)
          (recur (inc order) (dissoc fns order)))
      {:expecting order :fns fns})))

(defn constrain-order
  "Elements to be printed in the REPL will arrive out of order. Ensure
  that they are printed in the correct order."
  [order f]
  (send-off ordering add-in-order order f)
  (send-off ordering run-in-order))

(defmethod handle-post :print [{:keys [content order]} conn _ ]
  (do (constrain-order order (fn [] (do (print (read-string content))
                                       (.flush *out*))))
      (server/send-and-close conn 200 "ignore__")))

(defmethod handle-post :result [{:keys [content order]} conn _ ]
  (constrain-order order (fn [] (do (return-value content)
                                   (server/set-connection conn)))))

(defn browser-eval
  "Given a string of JavaScript, evaluate it in the browser and return a map representing the
   result of the evaluation. The map will contain the keys :type and :value. :type can be
   :success, :exception, or :error. :success means that the JavaScript was evaluated without
   exception and :value will contain the return value of the evaluation. :exception means that
   there was an exception in the browser while evaluating the JavaScript and :value will
   contain the error message. :error means that some other error has occured."
  [form]
  (let [return-value (promise)]
    (send-for-eval form
                   (fn [val] (deliver return-value val)))
    (let [ret @return-value]
      (try (read-string ret)
           (catch Exception e
             {:status :error
              :value (str "Could not read return value: " ret)})))))

(defn load-javascript
  "Accepts a REPL environment, a list of namespaces, and a URL for a
  JavaScript file which contains the implementation for the list of
  namespaces. Will load the JavaScript file into the REPL environment
  if any of the namespaces have not already been loaded from the
  ClojureScript REPL."
  [repl-env ns-list url]
  (let [missing (remove #(contains? @loaded-libs %) ns-list)]
    (when (seq missing)
      (browser-eval (slurp url))
      (swap! loaded-libs (partial apply conj) missing))))

(defrecord BrowserEnv []
  repl/IJavaScriptEnv
  (-setup [this]
    (do (require 'cljs.repl.reflect)
        (repl/analyze-source (:src this))
        (comp/with-core-cljs (server/start this))))
  (-evaluate [_ _ _ js] (browser-eval js))
  (-load [this ns url] (load-javascript this ns url))
  (-tear-down [_]
    (do (server/stop)
        (reset! server/state {})
        (reset! browser-state {}))))

(defn compile-client-js [opts]
  (cljsc/build '[(ns clojure.browser.repl.client
                   (:require [goog.events :as event]
                             [clojure.browser.repl :as repl]))
                 (defn start [url]
                   (event/listen js/window
                                 "load"
                                 (fn []
                                   (repl/start-evaluator url))))]
               {:optimizations (:optimizations opts)
                :output-dir (:working-dir opts)}))

(defn create-client-js-file [opts file-path]
  (let [file (io/file file-path)]
    (when (not (.exists file))
      (spit file (compile-client-js opts)))
    file))

(defn- provides-and-requires
  "Return a flat list of all provided and required namespaces from a
  sequence of IJavaScripts."
  [deps]
  (flatten (mapcat (juxt :provides :requires) deps)))

(defn- always-preload
  "Return a list of all namespaces which are always loaded into the browser
  when using a browser-connected REPL."
  []
  (let [cljs (provides-and-requires (cljsc/cljs-dependencies {} ["clojure.browser.repl"]))
        goog (provides-and-requires (cljsc/js-dependencies {} cljs))]
    (disj (set (concat cljs goog)) nil)))

(defn repl-env
  "Create a browser-connected REPL environment.

  Options:

  port:           The port on which the REPL server will run. Defaults to 9000.
  working-dir:    The directory where the compiled REPL client JavaScript will
                  be stored. Defaults to \".repl\".
  serve-static:   Should the REPL server attempt to serve static content?
                  Defaults to true.
  static-dir:     List of directories to search for static content. Defaults to
                  [\".\" \"out/\"].
  preloaded-libs: List of namespaces that should not be sent from the REPL server
                  to the browser. This may be required if the browser is already
                  loading code and reloading it would cause a problem.
  optimizations:  The level of optimization to use when compiling the client
                  end of the REPL. Defaults to :simple.
  src:            The source directory containing user-defined cljs files. Used to
                  support reflection. Defaults to \"src/\".
  "
  [& {:as opts}]
  (let [compiler-env (cljs.env/default-compiler-env opts)
        opts (merge (BrowserEnv.)
                    {:port          9000
                     :optimizations :simple
                     :working-dir   ".repl"
                     :serve-static  true
                     :static-dir    ["." "out/"]
                     :preloaded-libs   []
                     :src           "src/"
                     :cljs.env/compiler compiler-env
                     :source-map    true}
                    opts)]
    (cljs.env/with-compiler-env compiler-env
      (reset! preloaded-libs (set (concat (always-preload) (map str (:preloaded-libs opts)))))
        (reset! loaded-libs @preloaded-libs)
        (swap! browser-state
               (fn [old] (assoc old :client-js
                               (future (create-client-js-file
                                        opts
                                        (io/file (:working-dir opts) "client.js"))))))
        opts)))

(comment

  (require '[cljs.repl :as repl])
  (require '[cljs.repl.browser :as browser])
  (def env (browser/repl-env))
  (repl/repl env)
  ;; simulate the browser with curl
  ;; curl -v -d "ready" http://127.0.0.1:9000
  ClojureScript:> (+ 1 1)
  ;; curl -v -d "2" http://127.0.0.1:9000

  )
