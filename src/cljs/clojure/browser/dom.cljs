(ns clojure.browser.dom
  (:require [goog.dom :as dom]))

(defn append [parent & children]
  (do (doseq [child children]
        (dom/appendChild parent child))
      parent))

(defprotocol DOMBuilder
  (-element [this] [this attrs-or-children] [this attrs children]))

(defn text-node
  [s]
  (dom/createTextNode (str s)))

(defn log [& args]
  (.log js/console (apply pr-str args)))

(defn log-obj [obj]
  (.log js/console obj))

(declare element)
(extend-protocol DOMBuilder

  string
  (-element
    ([this]
       (log "string (-element " this ")")
       (cond (keyword? this) (dom/createElement (name this))
             :else           (text-node this)))

    ([this attrs-or-children]
       (log "string (-element " this " " attrs-or-children ")")
       (let [attrs (first attrs-or-children)]
         (if (map? attrs)
           (-element this attrs (rest attrs-or-children))
           (-element this nil attrs-or-children))))

    ([this attrs children]
       (log "string (-element " this " " attrs " " children ")")
       (let [str-attrs (if (and (map? attrs) (seq attrs))
                         (.strobj (reduce (fn [m [k v]]
                                            (log "m = " m)
                                            (log "k = " k)
                                            (log "v = " v)
                                            (when (or (keyword? k)
                                                      (string? k))
                                              (assoc m (name k) v)))
                                          {}
                                          attrs))
                         nil)]
         (log-obj str-attrs)
         (if (seq children)
           (apply dom/createDom
                  (name this)
                  str-attrs
                  (map -element children))
           (dom/createDom (name this)
                          str-attrs)))))

  Vector
  (-element
    [this]
    (log "Vector (-element " this ")")
    (let [tag      (first this)
          attrs    (second this)
          children (drop 2 this)]
      (if (map? attrs)
        (-element tag attrs children)
        (-element tag nil (rest this)))))

  js/Element
  (-element [this]
    (log "js/Element (-element " this ")")
    this))

(defn element
  ([tag-or-text]
     (log "(element " tag-or-text ")")
     (-element tag-or-text))
  ([tag & children]
     (log "(element " tag " " children ")")
     (let [attrs (first children)]
       (if (map? attrs)
         (-element tag attrs (rest children))
         (-element tag nil children)))))

(defn remove-children
  "Remove all children from the element with the passed id."
  [id]
  (let [parent (dom/getElement (name id))]
    (do (dom/removeChildren parent))))

(defn get-element [id]
  (dom/getElement (name id)))

(defn html [s]
  (dom/htmlToDocumentFragment s))

(defn element-arg? [x]
  (or (keyword? x)
      (map? x)
      (string? x)))

(defn insert-at [parent child index]
  (dom/insertChildAt parent child index))
