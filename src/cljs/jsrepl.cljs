(ns jsrepl
  (:require [cljs.core]
            [bs :as bs]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.reader :as reader]))

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

(defn postexpr [log input]
  (append-dom log
    [:table
     [:tbody
      [:tr
       [:td {:class "cg"} "user=> "]
       [:td (.replace (.-value input) #"\n$" "")]]]]))

#_(defmacro print-with-class [c m]
  `(binding [*print-class* ~c]
     (println ~m)))

;;(set! *print-length* 103)

#_(defmacro let-elem-ids [ids & body]
  `(let ~(vec (mapcat #(list % (list '.getElementById 'document (str %))) ids))
     ~@body))

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
                    (postexpr log input)
                    (try (repl-print log (pr-str (js/eval (comp/emit-str (ana/analyze js/env form)))) "rtn")
                      (catch js/Error e
                        (repl-print log e "err")
                        #_(set! *e e)))
                    (js/setTimeout #(set! (.-value input) "") 0)
                    (set! (.-src status) "blank.gif")))
                (catch js/Error e
                  (if (= (.-message e) "EOF while reading")
                    (set! (.-src status) "dots.png")
                    (repl-print log e "err")))))))

    (println "ClojureScript")

    (.focus input))))
