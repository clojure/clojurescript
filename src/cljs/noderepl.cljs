(ns noderepl
  (:require [cljs.core]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.reader :as reader]))

(def ^:dynamic *debug* false)
(def ^:dynamic *e nil)

(defn prompt [] (str ana/*cljs-ns* "=> "))

(defn repl-print [text cls]
  (doseq [line (.split (str text) #"\n")]
    (when (= "err" cls)
      (print "ERR: "))
    (println line)))

(defn- read-next-form [text]
  (binding [*ns-sym* ana/*cljs-ns*]
    (reader/read-string text)))

(defn postexpr [text]
  (println (str (prompt) text)))

(defn ep [text]
  (try
   (let [env (assoc (ana/empty-env) :context :expr)
         form (read-next-form text)
         _ (when *debug* (println "READ:" (pr-str form)))
         body (ana/analyze env form)
         _ (when *debug* (println "ANALYZED:" (pr-str (:form body))))
         res (comp/emit-str body)
         _ (when *debug* (println "EMITTED:" (pr-str res)))]
     (repl-print (pr-str (js/eval res)) "rtn"))
   (catch js/Error e
     (repl-print (.-stack e) "err")
     (set! *e e))))

(defn pep [text]
  (postexpr text)
  (ep text))

(defn -main [& args]
  (println ";; ClojureScript")
  (println ";;   - http://github.com/kanaka/clojurescript")
  (println ";;   - A port of the ClojureScript compiler to ClojureScript")
  (pep "(+ 1 2)")
  (pep "(defn sqr [x] (* x x))")
  (pep "(sqr 8)")
  (pep "(defmacro unless [pred a b] `(if (not ~pred) ~a ~b))")
  (pep "(unless false :yep :nope)")
  (let [readline (js/require "readline")
        rl (.createInterface readline js/process.stdin js/process.stdout)]
    (.setPrompt rl (prompt))
    (.prompt rl)
    (.on rl "line" (fn [line]
                     (when (seq (filter #(not= " " %) line))
                       (ep line))
                     (.setPrompt rl (prompt))
                     (.prompt rl)))
    (.on rl "close" (fn [] (.exit js/process 0)))))

(set! *main-cli-fn* -main)

