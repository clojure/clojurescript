;;  Copyright (c) Rich Hickey. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;  which can be found in the file epl-v10.html at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.

(ns clojure.browser.dom
  (:require [goog.dom :as gdom]
            [goog.object :as gobject]))

(defn append [parent & children]
  (apply gdom/append parent children)
  parent)

(defprotocol DOMBuilder
  (-element [this] [this attrs-or-children] [this attrs children]))

(defn log [& args]
  (.log js/console (apply pr-str args)))

(defn log-obj [obj]
  (.log js/console obj))

(extend-protocol DOMBuilder

  string
  (-element
    ([this]
       (log "string (-element " this ")")
       (cond (keyword? this) (gdom/createElement  (name this))
             :else           (gdom/createTextNode (name this))))

    ([this attrs-or-children]
       (log "string (-element " this " " attrs-or-children ")")
       (let [attrs (first attrs-or-children)]
         (if (map? attrs)
           (-element this attrs (rest attrs-or-children))
           (-element this nil attrs-or-children))))

    ([this attrs children]
       (log "string (-element " this " " attrs " " children ")")
       (let [str-attrs (if (and (map? attrs) (seq attrs))
                         (reduce (fn [o [k v]]
                                   (log "m = " m)
                                   (log "k = " k)
                                   (log "v = " v)
                                   (when (or (keyword? k)
                                             (string? k))
                                     (aset o (name k) v)))
                                 (js-obj)
                                 attrs)
                         nil)]
         (log-obj str-attrs)
         (if (seq children)
           (apply gdom/createDom
                  (name this)
                  str-attrs
                  (map -element children))
           (gdom/createDom (name this)
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
  (let [parent (gdom/getElement (name id))]
    (do (gdom/removeChildren parent))))

(defn get-element [id]
  (gdom/getElement (name id)))

(defn html->dom [s]
  (gdom/htmlToDocumentFragment s))

(defn insert-at [parent child index]
  (gdom/insertChildAt parent child index))

(defn ensure-element
  "Coerce the argument to a dom element if possible."
  [e]
  (cond (keyword? e) (get-element e)
        (string? e) (html->dom e)
        :else e))

(defn replace-node
  "Replace old-node with new-node. old-node can be an element or a
   keyword which is the id of the node to replace.  new-node can be an
   element or an html string."
  [old-node new-node]
  (let [old-node (ensure-element old-node)
        new-node (ensure-element new-node)]
    (gdom/replaceNode new-node old-node)
    new-node))

(defn set-text
  "Set the text content for the passed element returning the
  element. If a keyword is passed in the place of e, the element with
  that id will be used and returned."
  [e s]
  (gdom/setTextContent (ensure-element e) s))

(defn get-value
  "Get the value of an element."
  [e]
  (.-value (ensure-element e)))

(defn set-properties
  "Set properties on an element"
  [e m]
  (gdom/setProperties (ensure-element e)
                      (apply gobject/create (interleave (keys m) (vals m)))))

(defn set-value
  "Set the value property for an element."
  [e v]
  (set-properties e {"value" v}))

(defn click-element
  [e]
  (.click (ensure-element e) ()))

;; TODO CSS class manipulation
;; TODO Query syntax
