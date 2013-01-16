(ns webrepl
  (:require [cljs.core]
            [clojure.string :as str]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.reader :as reader]))

(def ^:dynamic *debug* false)
(def ^:dynamic *e nil)

(defn prompt [] (str ana/*cljs-ns* "=> "))

(defn- read-next-form [text]
  (binding [*ns-sym* ana/*cljs-ns*]
    (reader/read-string text)))

(defn ep [text]
  (try
    (let [env (assoc (ana/empty-env) :context :expr)
          form (read-next-form text)
          _ (when *debug* (println "READ:" (pr-str form)))
          body (ana/analyze env form)
          _ (when *debug* (println "ANALYZED:" (pr-str (:form body))))
          res (comp/emit-str body)
          _ (when *debug* (println "EMITTED:" (pr-str res)))
          value (js/eval res)]
      (set! *3 *2)
      (set! *2 *1)
      (set! *1 value)
      {:value value :js res})
    (catch js/Error e
      (set! *e e)
      {:error (.-stack e)})))

(defn pep [text]
 (.promptText js/jconsole text)
 (.commandTrigger js/jconsole))


(defn- map->js [m]
  (let [out (js-obj)]
    (doseq [[k v] m]
      (aset out (name k) v))
    out))

(defn- on-validate [input]
  (not (empty? input)))

(defn- build-msg
  [title msg klass]
  (array
   (map->js {:msg (str title msg)
             :className klass})))

(defn- starts-with? [o s]
  (= (.slice (clojure.string/trim s)
             0
             (.-length o))
     o))

(def ^:private is-comment? #(starts-with? ";" %))

(defn- on-handle [line report]
    (build-msg "" "" "jquery-console-message-value")
    (let [input (.trim js/jQuery line)
          compiled (ep input)]
      (if-let [err (and compiled (:error compiled))]
        (build-msg "Compilation error: " err "jquery-console-message-error")

        (try
          (.promptLabel js/jconsole (prompt))
          (build-msg "" (pr-str (:value compiled)) "jquery-console-message-value")
          (catch js/Error e
            (build-msg "Compilation error: " e "jquery-console-message-error"))))))

(.ready (js/jQuery js/document)
  (fn []
    ;; Bootstrap an empty version of the cljs.user namespace
    (swap! cljs.compiler/*emitted-provides* conj (symbol "cljs.user"))
    (.provide js/goog "cljs.user")
    (set! cljs.core/*ns-sym* (symbol "cljs.user"))

    ;; setup the REPL console
    (set! js/controller (js/jQuery "#console"))
    (set! js/jconsole (.console js/controller
                  (map->js {:welcomeMessage "ClojureScript-in-ClojureScript Web REPL"
                            :promptLabel "cljs.user> "
                            :commandValidate on-validate
                            :commandHandle on-handle
                            :autofocus true
                            :animateScroll true
                            :promptHistory true})))
    (set! *print-fn* #(.message js/jconsole (clojure.string/trim %)))

    ;; print,evaluate,print some example forms
    (pep "(+ 1 2)")
    (pep "(let [sqr #(* % %)] (sqr 8))")
    (pep "(defmacro unless [pred a b] `(if (not ~pred) ~a ~b))")
    (pep "(unless false :yep :nope)")))


