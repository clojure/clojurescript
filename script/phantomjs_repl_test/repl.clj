(ns phantomjs-repl-test.repl
  (:require [cljs.repl :as repl]
            [cljs.repl.browser :as browser]
            [cljs.repl.server :as server])
  (:import java.io.Reader
           clojure.lang.LineNumberingPushbackReader
           java.util.concurrent.LinkedBlockingQueue))
   
(defn make-reader-queue
  "Returns a LineNumberingPushbackReader suitable for binding to *in*.
  The input-queue part can be used to provide new *in* content. This
  function is heavily inspired from
  clojure.tools.nrepl.middleware.session/-session-in."
  []
  (let [input-queue (LinkedBlockingQueue.)
        do-read (fn [buf off len]
                  (locking input-queue
                    (loop [i off]
                      (cond
                        (>= i (+ off len))
                          (+ off len)
                        (.peek input-queue)
                          (do (aset-char buf i (char (.take input-queue)))
                            (recur (inc i)))
                        :else
                          i))))
        reader (LineNumberingPushbackReader.
                (proxy [Reader] []
                  (close [] (.clear input-queue))
                  (read
                    ([]
                      (let [^Reader this this] (proxy-super read)))
                    ([x]
                      (let [^Reader this this]
                        (if (instance? java.nio.CharBuffer x)
                          (proxy-super read ^java.nio.CharBuffer x)
                          (proxy-super read ^chars x))))
                    ([^chars buf off len]
                      (if (or (zero? len) (<= (.size input-queue) 0))
                        -1
                        (- (do-read buf off len) off))))))]
    {:input-queue input-queue
    :stdin-reader reader}))

(defn enqueue-string
  [q string]
  (if (empty? string)
      (.put q -1)
      (locking q
        (doseq [c string] (.put q c)))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn start
  "Starts a repl in a seperate thread that can be fed content to evaluate via
  in"
  [env in]
  (let [out (new java.io.StringWriter)]
      (binding [*in* in
                *out* out]
      {:out out :in in :repl (future (repl/repl env))})))

(defn stop
  []
  (server/stop)
  (reset! server/state {})
  (reset! browser/browser-state {}))
