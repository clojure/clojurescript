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
            [clojure.edn :as edn]
            [cljs.compiler :as comp]
            [cljs.util :as util]
            [cljs.env :as env]
            [cljs.closure :as cljsc]
            [cljs.repl :as repl]
            [cljs.repl.server :as server])
  (:import [java.util.regex Pattern]
           [java.util.concurrent Executors]))

(def ^:dynamic browser-state nil)
(def ^:dynamic ordering nil)
(def ^:dynamic es nil)

(def ext->mime-type
  {".html" "text/html"
   ".css" "text/css"

   ".jpg" "image/jpeg"
   ".png" "image/png"
   ".gif" "image/gif"

   ".js" "text/javascript"
   ".cljs" "text/x-clojure"
   ".cljc" "text/x-clojure"
   ".map" "application/json"})

(def mime-type->encoding
  {"text/html" "UTF-8"
   "text/css" "UTF-8"
   "image/jpeg" "ISO-8859-1"
   "image/png" "ISO-8859-1"
   "image/gif" "ISO-8859-1"
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
    (server/send-and-close conn 200 form "text/javascript")))

(defn- return-value
  "Called by the server when a return value is received."
  [val]
  (when-let [f (:return-value-fn @browser-state)]
    (f val)))

(defn repl-client-js []
  (slurp (:client-js @browser-state)))

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
          local-path
          (cond->
            (seq (for [x (if (string? st-dir) [st-dir] st-dir)
                       :when (.exists (io/file (str x path)))]
                   (str x path)))
            (complement nil?) first)
          local-path
          (if (nil? local-path)
            (cond
              (re-find #".jar" path)
              (io/resource (second (string/split path #".jar!/")))
              (re-find (Pattern/compile (System/getProperty "user.dir")) path)
              (io/file (string/replace path (str (System/getProperty "user.dir") "/") ""))
              :else nil)
            local-path)]
      (if local-path
        (if-let [ext (some #(if (.endsWith path %) %) (keys ext->mime-type))]
          (let [mime-type (ext->mime-type ext "text/plain")
                encoding (mime-type->encoding mime-type "UTF-8")]
            (server/send-and-close
              conn
              200
              (slurp local-path :encoding encoding)
              mime-type
              encoding))
          (server/send-and-close conn 200 (slurp local-path) "text/plain"))
        (server/send-404 conn path)))
    (server/send-404 conn path)))

(server/dispatch-on :get
  (fn [{:keys [path]} _ _]
    (.startsWith path "/repl"))
  send-repl-client-page)

(server/dispatch-on :get
  (fn [{:keys [path]} _ _]
    (or (= path "/") (some #(.endsWith path %) (keys ext->mime-type))))
  send-static)

(defmulti handle-post (fn [m _ _ ] (:type m)))

(server/dispatch-on :post (constantly true) handle-post)

(defmethod handle-post :ready [_ conn _]
  (send-via es ordering (fn [_] {:expecting nil :fns {}}))
  (send-for-eval conn
    (cljsc/-compile
      '[(set! *print-fn* clojure.browser.repl/repl-print)
        (set! *print-newline* true)] {})
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

(defmethod handle-post :print [{:keys [content order]} conn _ ]
  (constrain-order order
    (fn []
      (print (read-string content))
      (.flush *out*)))
  (server/send-and-close conn 200 "ignore__"))

(defmethod handle-post :result [{:keys [content order]} conn _ ]
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

;; =============================================================================
;; Stracktrace parsing

(defmulti parse-stacktrace (fn [repl-env st err opts] (:ua-product err)))

(defmethod parse-stacktrace :default
  [repl-env st err opts] st)

(defn parse-file-line-column [flc]
  (let [xs (string/split flc #":")
        [pre [line column]]
        (reduce
          (fn [[pre post] [x i]]
            (if (<= i 2)
              [pre (conj post x)]
              [(conj pre x) post]))
          [[] []] (map vector xs (range (count xs) 0 -1)))
        file (string/join ":" pre)]
    [(cond-> file
       (.startsWith file "(") (string/replace "(" ""))
     (Long/parseLong
       (cond-> line
         (.endsWith line ")") (string/replace ")" "")))
     (Long/parseLong
       (cond-> column
         (.endsWith column ")") (string/replace ")" "")))]))

(defn parse-file [{:keys [host port] :as repl-env} file {:keys [asset-path] :as opts}]
  (let [base-url-pattern (Pattern/compile (str "http://" host ":" port "/"))]
    (if (re-find base-url-pattern file)
      (-> file
        (string/replace base-url-pattern "")
        (string/replace
          (Pattern/compile
            (str "^" (or asset-path (util/output-directory opts)) "/")) ""))
      (if-let [asset-root (:asset-root opts)]
        (string/replace file asset-root "")
        (throw
          (ex-info (str "Could not relativize URL " file)
            {:type :parse-stacktrace
             :reason :relativize-url}))))))

;; -----------------------------------------------------------------------------
;; Chrome Stacktrace

(defn chrome-st-el->frame
  [repl-env st-el opts]
  (let [xs (-> st-el
             (string/replace #"\s+at\s+" "")
             (string/split #"\s+"))
        [function flc] (if (== (count xs) 1)
                         [nil (first xs)]
                         [(first xs) (last xs)])
        [file line column] (parse-file-line-column flc)]
    (if (and file function line column)
      {:file (parse-file repl-env file opts)
       :function (string/replace function #"Object\." "")
       :line line
       :column column}
      (when-not (string/blank? function)
        {:file nil
         :function (string/replace function #"Object\." "")
         :line nil
         :column nil}))))

(comment
  (chrome-st-el->frame {:host "localhost" :port 9000}
    "\tat cljs$core$ffirst (http://localhost:9000/out/cljs/core.js:5356:34)" {})
  )

(defmethod parse-stacktrace :chrome
  [repl-env st err opts]
  (->> st
    string/split-lines
    (drop-while #(.startsWith % "Error"))
    (take-while #(not (.startsWith % "    at eval")))
    (map #(chrome-st-el->frame repl-env % opts))
    (remove nil?)
    vec))

(comment
  (parse-stacktrace {:host "localhost" :port 9000}
    "Error: 1 is not ISeqable
    at Object.cljs$core$seq [as seq] (http://localhost:9000/out/cljs/core.js:4258:8)
    at Object.cljs$core$first [as first] (http://localhost:9000/out/cljs/core.js:4288:19)
    at cljs$core$ffirst (http://localhost:9000/out/cljs/core.js:5356:34)
    at http://localhost:9000/out/cljs/core.js:16971:89
    at cljs.core.map.cljs$core$map__2 (http://localhost:9000/out/cljs/core.js:16972:3)
    at http://localhost:9000/out/cljs/core.js:10981:129
    at cljs.core.LazySeq.sval (http://localhost:9000/out/cljs/core.js:10982:3)
    at cljs.core.LazySeq.cljs$core$ISeqable$_seq$arity$1 (http://localhost:9000/out/cljs/core.js:11073:10)
    at Object.cljs$core$seq [as seq] (http://localhost:9000/out/cljs/core.js:4239:13)
    at Object.cljs$core$pr_sequential_writer [as pr_sequential_writer] (http://localhost:9000/out/cljs/core.js:28706:14)"
    {:ua-product :chrome}
    nil)

  (parse-stacktrace {:host "localhost" :port 9000}
    "Error: 1 is not ISeqable
    at Object.cljs$core$seq [as seq] (http://localhost:9000/out/cljs/core.js:4259:8)
    at Object.cljs$core$first [as first] (http://localhost:9000/out/cljs/core.js:4289:19)
    at cljs$core$ffirst (http://localhost:9000/out/cljs/core.js:5357:18)
    at eval (eval at <anonymous> (http://localhost:9000/out/clojure/browser/repl.js:23:272), <anonymous>:1:106)
    at eval (eval at <anonymous> (http://localhost:9000/out/clojure/browser/repl.js:23:272), <anonymous>:9:3)
    at eval (eval at <anonymous> (http://localhost:9000/out/clojure/browser/repl.js:23:272), <anonymous>:14:4)
    at http://localhost:9000/out/clojure/browser/repl.js:23:267
    at clojure$browser$repl$evaluate_javascript (http://localhost:9000/out/clojure/browser/repl.js:26:4)
    at Object.callback (http://localhost:9000/out/clojure/browser/repl.js:121:169)
    at goog.messaging.AbstractChannel.deliver (http://localhost:9000/out/goog/messaging/abstractchannel.js:142:13)"
    {:ua-product :chrome}
    nil)
  )

;; -----------------------------------------------------------------------------
;; Safari Stacktrace

(defn safari-st-el->frame
  [repl-env st-el opts]
  (let [[function flc] (if (re-find #"@" st-el)
                         (string/split st-el #"@")
                         [nil st-el])
        [file line column] (parse-file-line-column flc)]
    (if (and file function line column)
      {:file (parse-file repl-env file opts)
       :function function
       :line line
       :column column}
      (when-not (string/blank? function)
        {:file nil
         :function (string/trim function)
         :line nil
         :column nil}))))

(comment
  (safari-st-el->frame {:host "localhost" :port 9000}
    "cljs$core$seq@http://localhost:9000/out/cljs/core.js:4259:17" {})

  (safari-st-el->frame {:host "localhost" :port 9000}
    "cljs$core$seq@http://localhost:9000/js/cljs/core.js:4259:17" {:asset-path "js"})
  )

(defmethod parse-stacktrace :safari
  [repl-env st err opts]
  (->> st
    string/split-lines
    (drop-while #(.startsWith % "Error"))
    (take-while #(not (.startsWith % "eval code")))
    (remove string/blank?)
    (map #(safari-st-el->frame repl-env % opts))
    (remove nil?)
    vec))

(comment
  (parse-stacktrace nil
    "cljs$core$seq@http://localhost:9000/out/cljs/core.js:4259:17
cljs$core$first@http://localhost:9000/out/cljs/core.js:4289:22
cljs$core$ffirst@http://localhost:9000/out/cljs/core.js:5357:39
http://localhost:9000/out/cljs/core.js:16972:92
http://localhost:9000/out/cljs/core.js:16973:3
http://localhost:9000/out/cljs/core.js:10982:133
sval@http://localhost:9000/out/cljs/core.js:10983:3
cljs$core$ISeqable$_seq$arity$1@http://localhost:9000/out/cljs/core.js:11074:14
cljs$core$seq@http://localhost:9000/out/cljs/core.js:4240:44
cljs$core$pr_sequential_writer@http://localhost:9000/out/cljs/core.js:28707:17
cljs$core$IPrintWithWriter$_pr_writer$arity$3@http://localhost:9000/out/cljs/core.js:29386:38
cljs$core$pr_writer_impl@http://localhost:9000/out/cljs/core.js:28912:57
cljs$core$pr_writer@http://localhost:9000/out/cljs/core.js:29011:32
cljs$core$pr_seq_writer@http://localhost:9000/out/cljs/core.js:29015:20
cljs$core$pr_sb_with_opts@http://localhost:9000/out/cljs/core.js:29078:24
cljs$core$pr_str_with_opts@http://localhost:9000/out/cljs/core.js:29092:48
cljs$core$pr_str__delegate@http://localhost:9000/out/cljs/core.js:29130:34
cljs$core$pr_str@http://localhost:9000/out/cljs/core.js:29139:39
eval code
eval@[native code]
http://localhost:9000/out/clojure/browser/repl.js:23:271
clojure$browser$repl$evaluate_javascript@http://localhost:9000/out/clojure/browser/repl.js:26:4
http://localhost:9000/out/clojure/browser/repl.js:121:173
deliver@http://localhost:9000/out/goog/messaging/abstractchannel.js:142:21
xpcDeliver@http://localhost:9000/out/goog/net/xpc/crosspagechannel.js:733:19
messageReceived_@http://localhost:9000/out/goog/net/xpc/nativemessagingtransport.js:321:23
fireListener@http://localhost:9000/out/goog/events/events.js:741:25
handleBrowserEvent_@http://localhost:9000/out/goog/events/events.js:862:34
http://localhost:9000/out/goog/events/events.js:276:42"
    {:ua-product :safari}
    nil)
  )

;; -----------------------------------------------------------------------------
;; Firefox Stacktrace

(defn firefox-clean-function [f]
  (as-> f f
    (cond
      (string/blank? f) nil
      (not= (.indexOf f "</") -1)
      (let [idx (.indexOf f "</")]
        (.substring f (+ idx 2)))
      :else f)
    (-> f
      (string/replace #"<" "")
      (string/replace #"\/" ""))))

(defn firefox-st-el->frame
  [repl-env st-el opts]
  (let [[function flc] (if (re-find #"@" st-el)
                         (string/split st-el #"@")
                         [nil st-el])
        [file line column] (parse-file-line-column flc)]
    (if (and file function line column)
      {:file (parse-file repl-env file opts)
       :function (firefox-clean-function function)
       :line line
       :column column}
      (when-not (string/blank? function)
        {:file nil
         :function (firefox-clean-function function)
         :line nil
         :column nil}))))

(comment
  (firefox-st-el->frame {:host "localhost" :port 9000}
    "cljs$core$seq@http://localhost:9000/out/cljs/core.js:4258:8" {})

  (firefox-st-el->frame {:host "localhost" :port 9000}
    "cljs.core.map</cljs$core$map__2/</<@http://localhost:9000/out/cljs/core.js:16971:87" {})

  (firefox-st-el->frame {:host "localhost" :port 9000}
    "cljs.core.map</cljs$core$map__2/</<@http://localhost:9000/out/cljs/core.js:16971:87" {})

  (firefox-st-el->frame {:host "localhost" :port 9000}
    "cljs.core.pr_str</cljs$core$pr_str@http://localhost:9000/out/cljs/core.js:29138:8" {})

  (firefox-st-el->frame {:host "localhost" :port 9000}
    "cljs.core.pr_str</cljs$core$pr_str__delegate@http://localhost:9000/out/cljs/core.js:29129:8" {})
  )

(defmethod parse-stacktrace :firefox
  [repl-env st err opts]
  (->> st
    string/split-lines
    (drop-while #(.startsWith % "Error"))
    (take-while #(= (.indexOf % "> eval") -1))
    (remove string/blank?)
    (map #(firefox-st-el->frame repl-env % opts))
    (remove nil?)
    vec))

(comment
  (parse-stacktrace nil
    "cljs$core$seq@http://localhost:9000/out/cljs/core.js:4258:8
cljs$core$first@http://localhost:9000/out/cljs/core.js:4288:9
cljs$core$ffirst@http://localhost:9000/out/cljs/core.js:5356:24
cljs.core.map</cljs$core$map__2/</<@http://localhost:9000/out/cljs/core.js:16971:87
cljs.core.map</cljs$core$map__2/<@http://localhost:9000/out/cljs/core.js:16970:1
cljs.core.LazySeq.prototype.sval/self__.s<@http://localhost:9000/out/cljs/core.js:10981:119
cljs.core.LazySeq.prototype.sval@http://localhost:9000/out/cljs/core.js:10981:13
cljs.core.LazySeq.prototype.cljs$core$ISeqable$_seq$arity$1@http://localhost:9000/out/cljs/core.js:11073:1
cljs$core$seq@http://localhost:9000/out/cljs/core.js:4239:8
cljs$core$pr_sequential_writer@http://localhost:9000/out/cljs/core.js:28706:4
cljs.core.LazySeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3@http://localhost:9000/out/cljs/core.js:29385:8
cljs$core$pr_writer_impl@http://localhost:9000/out/cljs/core.js:28911:8
cljs$core$pr_writer@http://localhost:9000/out/cljs/core.js:29010:8
cljs$core$pr_seq_writer@http://localhost:9000/out/cljs/core.js:29014:1
cljs$core$pr_sb_with_opts@http://localhost:9000/out/cljs/core.js:29077:1
cljs$core$pr_str_with_opts@http://localhost:9000/out/cljs/core.js:29091:23
cljs.core.pr_str</cljs$core$pr_str__delegate@http://localhost:9000/out/cljs/core.js:29129:8
cljs.core.pr_str</cljs$core$pr_str@http://localhost:9000/out/cljs/core.js:29138:8
@http://localhost:9000/out/clojure/browser/repl.js line 23 > eval:1:25
@http://localhost:9000/out/clojure/browser/repl.js line 23 > eval:1:2
clojure$browser$repl$evaluate_javascript/result<@http://localhost:9000/out/clojure/browser/repl.js:23:267
clojure$browser$repl$evaluate_javascript@http://localhost:9000/out/clojure/browser/repl.js:23:15
clojure$browser$repl$connect/</<@http://localhost:9000/out/clojure/browser/repl.js:121:128
goog.messaging.AbstractChannel.prototype.deliver@http://localhost:9000/out/goog/messaging/abstractchannel.js:142:5
goog.net.xpc.CrossPageChannel.prototype.xpcDeliver@http://localhost:9000/out/goog/net/xpc/crosspagechannel.js:733:7
goog.net.xpc.NativeMessagingTransport.messageReceived_@http://localhost:9000/out/goog/net/xpc/nativemessagingtransport.js:321:1
goog.events.fireListener@http://localhost:9000/out/goog/events/events.js:741:10
goog.events.handleBrowserEvent_@http://localhost:9000/out/goog/events/events.js:862:1
goog.events.getProxy/f<@http://localhost:9000/out/goog/events/events.js:276:16"
    {:ua-product :firefox}
    nil)
  )

;; =============================================================================
;; BrowserEnv

(defn compile-client-js [opts]
  (cljsc/build
    '[(ns clojure.browser.repl.client
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

(defn setup [{:keys [working-dir] :as repl-env} opts]
  (binding [browser-state (:browser-state repl-env)
            ordering (:ordering repl-env)
            es (:es repl-env)
            server/state (:server-state repl-env)]
    (repl/err-out (println "Compiling client js ..."))
    (swap! browser-state
      (fn [old]
        (assoc old :client-js
          (create-client-js-file
            repl-env (io/file working-dir "client.js")))))
    (repl/err-out (println "Waiting for browser to connect ..."))
    opts
    (server/start repl-env)))

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
    (binding [server/state (:server-state this)]
      (server/stop))
    (.shutdown (:es this)))
  repl/IReplEnvOptions
  (-repl-options [this]
    {:repl-requires
     '[[clojure.browser.repl]]})
  repl/IParseStacktrace
  (-parse-stacktrace [this st err opts]
    (parse-stacktrace this st err opts))
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
  [{:keys [output-dir] :as opts}]
  (merge (BrowserEnv.)
    {:host "localhost"
     :port 9000
     :working-dir (->> [".repl" (util/clojurescript-version)]
                       (remove empty?) (string/join "-"))
     :serve-static true
     :static-dir (cond-> ["." "out/"] output-dir (conj output-dir))
     :preloaded-libs []
     :optimizations :simple
     :src "src/"
     :browser-state (atom {:return-value-fn nil
                          :client-js nil})
     :ordering (agent {:expecting nil :fns {}})
     :es (Executors/newFixedThreadPool 16)
     :server-state
     (atom
       {:socket nil
        :connection nil
        :promised-conn nil})}
    opts))

(defn repl-env
  "Create a browser-connected REPL environment.

  Options:

  port:           The port on which the REPL server will run. Defaults to 9000.
  working-dir:    The directory where the compiled REPL client JavaScript will
                  be stored. Defaults to \".repl\" with a ClojureScript version
                  suffix, eg. \".repl-0.0-2138\".
  serve-static:   Should the REPL server attempt to serve static content?
                  Defaults to true.
  static-dir:     List of directories to search for static content. Defaults to
                  [\".\" \"out/\"].
  optimizations:  The level of optimization to use when compiling the client
                  end of the REPL. Defaults to :simple.
  src:            The source directory containing user-defined cljs files. Used to
                  support reflection. Defaults to \"src/\".
  "
  [& {:as opts}]
  (repl-env* opts))

(defn -main []
  (repl/repl (repl-env)))

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
