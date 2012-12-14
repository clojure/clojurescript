(ns webrepl
  (:require [cljs.core]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.reader :as reader]))

(def ^:dynamic *debug* false)

(defn prompt [] (str ana/*cljs-ns* "=> "))

(def append-dom)

(defn dom [o]
  (if (coll? o)
    (let [[tag attrs & body] o]
      (if (keyword? tag)
        (let [elem (.createElement js/document (name tag))]
          (when (map? attrs)
            (doseq [[k v] attrs]
              (when v (.setAttribute elem (name k) v))))
          [(append-dom elem (if (map? attrs) body (cons attrs body)))])
        (mapcat dom o)))
    (when o
      [(.createTextNode js/document (str o))])))

(defn append-dom [parent v]
  (doseq [i (dom v)]
    (.appendChild parent i))
  parent)

(def *print-class* nil)
(def *e nil)

(defn repl-print [log text cls]
  (doseq [line (.split (str text) #"\n")]
    (append-dom log
      [:div {:class (str "cg "
                         (when cls
                           (str " " cls)))}
       line]))
  (set! (.-scrollTop log) (.-scrollHeight log)))

(defn postexpr [log text]
  (append-dom log
    [:table
     [:tbody
      [:tr
       [:td {:class "cg"} (prompt)]
       [:td (.replace text #"\n$" "")]]]]))

#_(defmacro print-with-class [c m]
  `(binding [*print-class* ~c]
     (println ~m)))

;;(set! *print-length* 103)

#_(defmacro let-elem-ids [ids & body]
  `(let ~(vec (mapcat #(list % (list '.getElementById 'document (str %))) ids))
     ~@body))

(defn pep [log text]
 (postexpr log text)
 (try
   (let [res (comp/emit-str (ana/analyze js/env (reader/read-string text)))]
     (when *debug* (println "emit:" res))
     (repl-print log (pr-str (js/eval res)) "rtn"))
   (catch js/Error e
    (repl-print log (.-stack e) "err")
    #_(set! *e e))))

(set! (.-onload js/window) (fn []
  (let [log (.getElementById js/document "log")
        input (.getElementById js/document "input")
        status (.getElementById js/document "status")]
    (set! *print-fn* #(repl-print log % nil))

    (set! (.-onkeypress input)
          (fn [ev]
            (when (== (.-keyCode (or ev event)) 13)
              (try
                (let [form (reader/read-string (.-value input))]
                  (do
                    (pep log (.-value input))
                    (js/setTimeout #(set! (.-value input) "") 0)
                    (set! (.-src status) "blank.gif")))
                (catch js/Error e
                  (if (= (.-message e) "EOF while reading")
                    (set! (.-src status) "dots.png")
                    (repl-print log e "err")))))))

    (println ";; ClojureScript")
    (append-dom log [:div {:class "cg"}
      ";;   - "
      [:a {:href "http://github.com/kanaka/clojurescript"}
       "http://github.com/kanaka/clojurescript"]])
    (println ";;   - A port of the ClojureScript compiler to ClojureScript")
    (println ";;   - No predefined macros (yet), but defmacro works.")
    (pep log "(+ 1 2)")
    (pep log "(def sqr (fn* [x] (* x x)))")
    (pep log "(sqr 8)")
    (pep log "(defmacro unless [pred a b] `(if (not ~pred) ~a ~b))")
    (pep log "(unless false :yep :nope)")

    (.focus input))))
