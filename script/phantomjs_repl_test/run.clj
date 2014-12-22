(ns phantomjs-repl-test.run
  (:gen-class)
  (:require [phantomjs-repl-test.repl :as repl]
            [phantomjs-repl-test.phantomjs :as phantomjs]
            [cljs.repl.browser :as browser]))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; test specs

(def tests
  [{:expected #"phantomjs load"
    :timeout 10000}
   {:cljs '(.log js/console "cljs says hello!")
    :expected #"cljs says hello"
    :timeout 10000}
   {:cljs '(do (enable-console-print!) (println "cljs prints"))
    :expected #"cljs prints"
    :timeout 10000}
  ;; load file + require not yet working -- ns issue?
  {:cljs '(do
            (clojure.core/load-file "clojure/string.cljs")
            (println (clojure.string/reverse "Hello")))
    :expected #"olleH"
    :timeout 10000}
  ; {:cljs '(do
  ;           (ns test.crypt (:require [goog.crypt :as c]))
  ;           (println (c/stringToByteArray "ClojureScript")))
  ;   :expected #"cljs says hello"
  ;   :timeout 1000}
  ])

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn test-timeout?
  [test duration]
  (> duration (:timeout test)))

(defn test-successful?
  [test phantomjs-output]
  (not (nil? (re-find (:expected test) phantomjs-output))))

(defn start-test!
  [test phantomjs-input]
  (some->> test
           (:cljs)
           (str)
           (repl/enqueue-string (:input-queue phantomjs-input))))

(defn -main
  "Starts a phantomjs process and a clojurescript repl. Polls for the phantomjs
  output and matches it with the test specs."

  [& [phantom-script-file]]
  (let [env (browser/repl-env)
        repl-in (repl/make-reader-queue)
        repl-proc (repl/start env (:stdin-reader repl-in))
        phantom-out (phantomjs/run-script phantom-script-file)
        start (System/currentTimeMillis)]

      (future
        (loop [tests tests
               test (first tests)
               start-time (System/currentTimeMillis)]
          (Thread/sleep 200)
          (cond
            (nil? test) (System/exit 0)
            (test-timeout? test (- (System/currentTimeMillis) start-time))
              (do
                (println "test" test "timed out")
                (System/exit 1))
            (test-successful? test (doall @phantom-out))
              (do
                (if (:cljs test)
                  (println "Successfully tested for" (:cljs test)))
                (reset! phantom-out "")
                (start-test! (second tests) repl-in)
                (recur (rest tests) (second tests) (System/currentTimeMillis)))
            :else (recur tests test start-time))))))
