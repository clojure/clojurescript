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
            [clojure.java.browse :as browse]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [cljs.vendor.clojure.data.json :as json]
            [cljs.util :as util]
            [cljs.closure :as cljsc]
            [cljs.repl :as repl]
            [cljs.cli :as cli]
            [cljs.repl.server :as server]
            [cljs.stacktrace :as st]
            [cljs.analyzer :as ana]
            [cljs.build.api :as build]
            [clojure.string :as str])
  (:import [java.util.concurrent Executors ConcurrentHashMap]))

(def ^:dynamic browser-state nil)
(def ^:dynamic ordering nil)
(def ^:dynamic es nil)
(def outs (ConcurrentHashMap.))

(defn thread-name []
  (let [name (.getName (Thread/currentThread))]
    (if (string/starts-with? name "nREPL") "main" name)))

(def ext->mime-type
  {".html" "text/html"
   ".css" "text/css"

   ".ttf" "font/ttf"
   ".otf" "font/otf"

   ".pdf" "application/pdf"

   ".jpg" "image/jpeg"
   ".png" "image/png"
   ".gif" "image/gif"
   ".svg" "image/svg+xml"

   ".mp4" "video/mp4"
   ".m4a" "audio/m4a"
   ".m4v" "video/mp4"
   ".mp3" "audio/mpeg"
   ".mpeg" "video/mpeg"
   ".wav" "audio/wav"

   ".js" "text/javascript"
   ".json" "application/json"
   ".clj" "text/x-clojure"
   ".cljs" "text/x-clojure"
   ".cljc" "text/x-clojure"
   ".edn" "text/x-clojure"
   ".map" "application/json"})

(def mime-type->encoding
  {"text/html" "UTF-8"
   "text/css" "UTF-8"

   "font/ttf" "ISO-8859-1"
   "font/otf" "ISO-8859-1"

   "application/pdf" "ISO-8859-1"

   "image/jpeg" "ISO-8859-1"
   "image/png" "ISO-8859-1"
   "image/gif" "ISO-8859-1"
   "image/svg+xml" "UTF-8"

   "video/mp4" "ISO-8859-1"
   "audio/m4a" "ISO-8859-1"
   "audio/mpeg" "ISO-8859-1"
   "video/mpeg" "ISO-8859-1"
   "audio/wav" "ISO-8859-1"

   "text/javascript" "UTF-8"
   "text/x-clojure" "UTF-8"
   "application/json" "UTF-8"})

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
    (set-return-value-fn return-value-fn)
    (server/send-and-close conn 200
      (json/write-str
        {"repl" (thread-name)
         "form" form})
      "application/json")))

(defn- return-value
  "Called by the server when a return value is received."
  [val]
  (when-let [f (:return-value-fn @browser-state)]
    (f val)))

(defn repl-client-js []
  (slurp (:client-js @browser-state)))

(defn send-repl-client-page
  [{:keys [path] :as request} conn opts]
  (if-not browser-state
    (server/send-404 conn path)
    (server/send-and-close conn 200
      (str
        "<html><head><meta charset=\"UTF-8\"></head><body>"
        "<script type=\"text/javascript\">"
        (repl-client-js)
        "</script>"
        "<script type=\"text/javascript\">
        clojure.browser.repl.client.start(\"http://" (-> request :headers :host) "\");
          </script>"
        "</body></html>")
     "text/html")))

(defn default-index [output-to]
  (str
    "<!DOCTYPE html><html>"
    "<head>"
    "<meta charset=\"UTF-8\">"
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" >"
    "<link rel=\"shortcut icon\" type=\"image/x-icon\" href=\"cljs-logo-icon-32.png\"/>"
    "</head>"
    "<body>"
    "<div id=\"app\">"
    "<link href=\"https://fonts.googleapis.com/css?family=Open+Sans\" rel=\"stylesheet\">"
    "<style>"
    "body { padding: 40px; margin: auto; max-width: 38em; "
    "font-family: \"Open Sans\", sans-serif; }"
    "code { color: #4165a2; font-size: 17px; }"
    "pre  { color: #4165a2; font-size: 15px; white-space: pre-wrap; }"
    "</style>"
    "<center><img src=\"cljs-logo.svg\" style=\"width: 200px; height: 200px; margin: 15px;\"/></center>"
    "<p>Welcome to the ClojureScript browser REPL.</p>"
    "<p>This page hosts your REPL and application evaluation environment. "
    "Validate the connection by typing <code>(js/alert&nbsp;\"Hello&nbsp;CLJS!\")</code> in the REPL.</p>"
    "<p>To provide your own custom page, place an <code>index.html</code> file in "
    "the REPL launch directory, starting with this template:</p>"
    "<pre>"
    "&lt;!DOCTYPE html&gt;\n"
    "&lt;html&gt;\n"
    "  &lt;head&gt;\n"
    "    &lt;meta charset=\"UTF-8\"&gt;\n"
    "  &lt;/head&gt;\n"
    "  &lt;body&gt;\n"
    "    &lt;script src=\"" output-to "\" type=\"text/javascript\"&gt;&lt;/script&gt;\n"
    "  &lt;/body&gt;\n"
    "&lt;/html&gt;\n"
    "</pre>"
    "</div></div>"
    "<script src=\"" output-to "\"></script>"
    "</body></html>"))

(defn- path->mime-type [ext->mime-type path default]
  (let [lc-path (str/lower-case path)
        last-dot (.lastIndexOf path ".")]
    (if (pos? last-dot)
      (-> lc-path
          (subs last-dot)
          (ext->mime-type default))
      default)))

(defn send-static
  [{path :path :as request} conn
   {:keys [static-dir output-dir host port gzip?] :or {output-dir "out"} :as opts}]
  (let [output-dir (when-not (.isAbsolute (io/file output-dir)) output-dir)]
    (if (and static-dir (not= "/favicon.ico" path))
      (let [path (if (= "/" path) "/index.html" path)
            local-path
            (cond->
              (seq (for [x (if (string? static-dir) [static-dir] static-dir)
                         :when (.exists (io/file (str x path)))]
                     (str x path)))
              (complement nil?) first)
            local-path
            (if (nil? local-path)
              (cond
                (re-find #".jar" path)
                (io/resource (second (string/split path #".jar!/")))
                (string/includes? path (System/getProperty "user.dir"))
                (io/file (string/replace path (str (System/getProperty "user.dir") "/") ""))
                (#{"/cljs-logo-icon-32.png" "/cljs-logo.svg"} path)
                (io/resource (subs path 1))
                :else nil)
              local-path)]
        (cond
          local-path
          (let [mime-type (path->mime-type ext->mime-type path "text/plain")
                encoding (mime-type->encoding mime-type "UTF-8")]
            (server/send-and-close conn 200 (slurp local-path :encoding encoding)
                                   mime-type encoding (and gzip? (= "text/javascript" mime-type))))

          ;; "/index.html" doesn't exist, provide our own
          (= path "/index.html")
          (server/send-and-close conn 200
            (default-index (str output-dir "/main.js"))
            "text/html" "UTF-8")

          ;; "/main.js" doesn't exist, provide our own
          (= path (cond->> "/main.js" output-dir (str "/" output-dir )))
          (let [closure-defines (-> `{"goog.json.USE_NATIVE_JSON" true
                                      clojure.browser.repl/HOST ~host
                                      clojure.browser.repl/PORT ~port}
                                  (merge (:closure-defines @browser-state))
                                  cljsc/normalize-closure-defines
                                  json/write-str)]
            (server/send-and-close conn 200
              (str "var CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
                   "var CLOSURE_NO_DEPS = true;\n"
                   "document.write('<script src=\"" output-dir "/goog/base.js\"></script>');\n"
                   "document.write('<script src=\"" output-dir "/goog/deps.js\"></script>');\n"
                   (when (.exists (io/file output-dir "cljs_deps.js"))
                     (str "document.write('<script src=\"" output-dir "/cljs_deps.js\"></script>');\n"))
                   "document.write('<script src=\"" output-dir "/brepl_deps.js\"></script>');\n"
                   "document.write('<script>goog.require(\"clojure.browser.repl.preload\");</script>');\n")
              "text/javascript" "UTF-8"))

          :else (server/send-404 conn path)))
      (server/send-404 conn path))))

(server/dispatch-on :get
  (fn [{:keys [path]} _ _]
    (.startsWith path "/repl"))
  send-repl-client-page)

(server/dispatch-on :get
  (fn [{:keys [path]} _ _]
    (or (= path "/") (path->mime-type ext->mime-type path nil)))
  send-static)

(defmulti handle-post (fn [m _ _ ] (:type m)))

(server/dispatch-on :post (constantly true) handle-post)

(defmethod handle-post :ready [_ conn _]
  (send-via es ordering (fn [_] {:expecting nil :fns {}}))
  ;; browser refresh, reset connq
  (locking server/lock
    (.clear server/connq))
  (send-for-eval conn
    (binding [ana/*cljs-warnings*
              (assoc ana/*cljs-warnings*
                :undeclared-var false)]
      (cljsc/-compile
       '[(set! *print-fn* clojure.browser.repl/repl-print)
         (set! *print-err-fn* clojure.browser.repl/repl-print)
         (set! *print-newline* true)
         (when (pos? (count clojure.browser.repl/print-queue))
           (clojure.browser.repl/flush-print-queue!
             @clojure.browser.repl/xpc-connection))] {}))
    identity))

(defn add-in-order [{:keys [expecting fns]} order f]
  {:expecting (or expecting order)
   :fns (assoc fns order f)})

(defn run-in-order [{:keys [expecting fns]}]
  (loop [order expecting fns fns]
    (if-let [f (get fns order)]
      (do
        (f)
        (recur (inc order) (dissoc fns order)))
      {:expecting order :fns fns})))

(defn constrain-order
  "Elements to be printed in the REPL will arrive out of order. Ensure
  that they are printed in the correct order."
  [order f]
  (send-via es ordering add-in-order order f)
  (send-via es ordering run-in-order))

(defmethod handle-post :print [{:keys [repl content order]} conn _]
  (constrain-order order
    (fn []
      (binding [*out* (or (and repl (.get outs repl)) *out*)]
        (print (read-string content))
        (.flush *out*))))
  (server/send-and-close conn 200 "ignore__"))

(defmethod handle-post :result [{:keys [content order]} conn _]
  (constrain-order order
    (fn []
      (return-value content)
      (server/set-connection conn))))

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
      (try
        (read-string ret)
        (catch Exception e
          {:status :error
           :value (str "Could not read return value: " ret)})))))

(defn load-javascript
  "Accepts a REPL environment, a list of namespaces, and a URL for a
  JavaScript file which contains the implementation for the list of
  namespaces. Will load the JavaScript file into the REPL environment
  if any of the namespaces have not already been loaded from the
  ClojureScript REPL."
  [repl-env provides url]
  (browser-eval (slurp url)))

(defn serve [{:keys [host port output-dir] :as opts}]
  (println "Serving HTTP on" host "port" port)
  (binding [ordering (agent {:expecting nil :fns {}})
            es (Executors/newFixedThreadPool 16)
            server/state (atom {:socket nil})]
    (server/start
      (merge opts
        {:static-dir (cond-> ["." "out/"] output-dir (conj output-dir))
         :gzip? true}))))

;; =============================================================================
;; BrowserEnv

(def lock (Object.))

(defn- waiting-to-connect-message [url]
  (print-str "Waiting for browser to connect to" url "..."))

(defn- maybe-browse-url [base-url]
  (try
    (browse/browse-url (str base-url "?rel=" (System/currentTimeMillis)))
    (catch Throwable t
      (if-some [error-message (not-empty (.getMessage t))]
        (println "Failed to launch a browser:\n" error-message "\n")
        (println "Could not launch a browser.\n"))
      (println "You can instead launch a non-browser REPL (Node or Nashorn).\n")
      (println "You can disable automatic browser launch with this REPL option")
      (println "  :launch-browser false")
      (println "and you can specify the listen IP address with this REPL option")
      (println "  :host \"127.0.0.1\"\n")
      (println (waiting-to-connect-message base-url)))))

(defn setup [{:keys [working-dir launch-browser server-state] :as repl-env} {:keys [output-dir] :as opts}]
  (locking lock
    (when-not (:socket @server-state)
      (binding [browser-state (:browser-state repl-env)
                ordering (:ordering repl-env)
                es (:es repl-env)
                server/state (:server-state repl-env)]
        (swap! browser-state
          (fn [old]
            (assoc old :client-js
              (cljsc/create-client-js-file
                {:optimizations :simple
                 :output-dir working-dir}
                (io/file working-dir "brepl_client.js"))
              :closure-defines (:closure-defines opts))))
        ;; TODO: this could be cleaner if compiling forms resulted in a
        ;; :output-to file with the result of compiling those forms - David
        (when (and output-dir (not (.exists (io/file output-dir "clojure" "browser" "repl" "preload.js"))))
          (let [target (io/file output-dir "brepl_deps.js")]
            (util/mkdirs target)
            (spit target
              (build/build
                '[(require '[clojure.browser.repl.preload])]
                (merge (dissoc (select-keys opts cljsc/known-opts) :modules)
                  {:opts-cache "brepl_opts.edn"})))))
        (server/start repl-env)
        (let [base-url (str "http://" (:host repl-env) ":" (:port repl-env))]
          (if launch-browser
            (maybe-browse-url base-url)
            (println (waiting-to-connect-message base-url)))))))
  (.put outs (thread-name) *out*)
  (swap! server-state update :listeners inc))

(defrecord BrowserEnv []
  repl/IJavaScriptEnv
  (-setup [this opts]
    (setup this opts))
  (-evaluate [this _ _ js]
    (binding [browser-state (:browser-state this)
              ordering (:ordering this)
              es (:es this)
              server/state (:server-state this)]
      (browser-eval js)))
  (-load [this provides url]
    (load-javascript this provides url))
  (-tear-down [this]
    (.remove outs (thread-name))
    (let [server-state (:server-state this)]
      (when (zero? (:listeners (swap! server-state update :listeners dec)))
        (binding [server/state server-state] (server/stop))
        (when-not (.isShutdown (:es this))
          (.shutdownNow (:es this))))))
  repl/IReplEnvOptions
  (-repl-options [this]
    {:browser-repl true
     :repl-requires
     '[[clojure.browser.repl] [clojure.browser.repl.preload]]
     ::repl/fast-initial-prompt? :after-setup
     :cljs.cli/commands
     {:groups {::repl {:desc "browser REPL options"}}
      :init
      {["-H" "--host"]
       {:group ::repl
        :fn #(-> %1
               (assoc-in [:repl-env-options :host] %2)
               (assoc-in [:options :closure-defines 'clojure.browser.repl/HOST] %2))
        :arg "address"
        :doc "Address to bind"}
       ["-p" "--port"]
       {:group ::repl
        :fn #(let [port (Integer/parseInt %2)]
               (-> %1
                 (assoc-in [:repl-env-options :port] port)
                 (assoc-in [:options :closure-defines 'clojure.browser.repl/PORT] port)))
        :arg "number"
        :doc "Port to bind"}}}})
  repl/IParseStacktrace
  (-parse-stacktrace [this st err opts]
    (st/parse-stacktrace this st err opts))
  repl/IGetError
  (-get-error [this e env opts]
    (edn/read-string
      (repl/evaluate-form this env "<cljs repl>"
        `(when ~e
           (pr-str
             {:ua-product (clojure.browser.repl/get-ua-product)
              :value (str ~e)
              :stacktrace (.-stack ~e)}))))))

(defn repl-env*
  [{:keys [output-dir host port] :or {host "localhost" port 9000} :as opts}]
  (merge (BrowserEnv.)
    {:host host
     :port port
     :launch-browser true
     :working-dir (->> [".repl" (util/clojurescript-version)]
                       (remove empty?) (string/join "-"))
     :static-dir (cond-> ["." "out/"] output-dir (conj output-dir))
     :preloaded-libs []
     :src "src/"
     :browser-state (atom {:return-value-fn nil
                           :client-js nil})
     :ordering (agent {:expecting nil :fns {}})
     :es (Executors/newFixedThreadPool 16)
     :server-state
     (atom
       {:socket nil
        :listeners 0})}
    opts))

(defn repl-env
  "Create a browser-connected REPL environment.

  Options:

  port:           The port on which the REPL server will run. Defaults to 9000.
  launch-browser: A Boolean indicating whether a browser should be automatically
                  launched connecting back to the terminal REPL. Defaults to true.
  working-dir:    The directory where the compiled REPL client JavaScript will
                  be stored. Defaults to \".repl\" with a ClojureScript version
                  suffix, eg. \".repl-0.0-2138\".
  static-dir:     List of directories to search for static content. Defaults to
                  [\".\" \"out/\"].
  src:            The source directory containing user-defined cljs files. Used to
                  support reflection. Defaults to \"src/\".
  "
  [& {:as opts}]
  (repl-env* opts))

(defn -main [& args]
  (apply cli/main repl-env args))

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
