(ns noderepl
  (:require [cljs.core]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.reader :as reader]))

(def prompt "cljs.user=> ")

(defn repl-print [text cls]
  (doseq [line (.split (str text) #"\n")]
    (when (= "err" cls)
      (print "ERR: "))
    (println line)))

(defn postexpr [text]
  (println prompt text))

(defn ep [text]
  (try
   (let [res (comp/emit-str (ana/analyze js/env (reader/read-string text)))]
     (repl-print (pr-str (js/eval res)) "rtn"))
   (catch js/Error e
    (repl-print e "err")
    #_(set! *e e))))

(defn pep [text]
  (postexpr text)
  (ep text))

(defn -main [& args]
  (set! js/env (assoc js/env :context :expr))
  (println ";; ClojureScript")
  (println ";;   - http://github.com/kanaka/clojurescript")
  (println ";;   - A port of the ClojureScript to ClojureScript")
  (println ";;   - No macros (yet)")
  (pep "(+ 1 2)")
  (pep "(def sqr (fn* [x] (* x x)))")
  (pep "(sqr 8)")
  (let [readline (js/require "readline")
        rl (.createInterface readline js/process.stdin js/process.stdout)]
    (.setPrompt rl prompt)
    (.prompt rl)
    (.on rl "line" (fn [line]
                     (ep line)
                     (.prompt rl)))
    (.on rl "close" (fn [] (.exit js/process 0)))))

(set! *main-cli-fn* -main)

