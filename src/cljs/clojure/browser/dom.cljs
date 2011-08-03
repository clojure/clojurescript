(ns clojure.browser.dom
  (:require [goog.dom :as dom]))

(defn append [parent & children]
  (do (doseq [child children]
        (dom/appendChild parent child))
      parent))

(defn)

(defn element
  "Create a dom element using a keyword for the element name and a map
  for the attributes."
  [tag attrs & children]
  (dom/createDom (name tag)
                 (.strobj (reduce (fn [m [k v]]
                                    (assoc m k v))
                                  {}
                                  (map #(vector (name %1) %2)
                                       (keys attrs)
                                       (vals attrs))))
                 children)
  #_(let [parent (dom/createDom (name tag)
                              (.strobj (reduce (fn [m [k v]]
                                                 (assoc m k v))
                                               {}
                                               (map #(vector (name %1) %2)
                                                    (keys attrs)
                                                    (vals attrs)))))
        [parent children] (if (string? (first children))
                            [(doto (element tag attrs) (dom/setTextContent (first children)))
                             (rest children)]
                            [parent children])]
    (apply append parent children)))

(defn remove-children
  "Remove all children from the element with the passed id."
  [id]
  (let [parent (dom/getElement (name id))]
    (do (dom/removeChildren parent))))

(defn get-element [id]
  (dom/getElement (name id)))

(defn text-node
  [s]
  (dom/createTextNode (str s)))

(defn html [s]
  (dom/htmlToDocumentFragment s))

(defn element-arg? [x]
  (or (keyword? x)
      (map? x)
      (string? x)))

(defn build
  "Build up a dom element from nested vectors."
  [x]
  (if (vector? x)
    (let [[parent children] (if (keyword? (first x))
                              [(apply element (take-while element-arg? x))
                               (drop-while element-arg? x)]
                              [(first x) (rest x)])
          children (map build children)]
      (apply append parent children))
    x))

(defn insert-at [parent child index]
  (dom/insertChildAt parent child index))
