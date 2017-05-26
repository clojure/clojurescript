;;  Copyright (c) Dariusz Luksza. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;  which can be found in the file epl-v10.html at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.

(ns clojure.browser.dom.forms
  (:require [goog.dom.forms :as gforms]
            [clojure.browser.dom :as dom]))

(defn get-form-data
  "Returns form data as a map of name to value arrays. This doesn't support file inputs."
  [form-id]
  (let [form (dom/get-element form-id)
        form (gforms/getFormDataMap form)
        form-data (map #(vector (keyword %) (first (.get form %))) (.getKeys form))]
    (into {} form-data)))

(defn has-file-input?
  "Whether the form has a file input."
  [form-id]
  (gforms/hasFileInput (dom/get-element form-id)))

(defn disable
  "Enables or disables either all elements in a form or a single form element."
  [id disabled]
  (do
    (gforms/setDisabled (dom/get-element id) disabled)
    nil))

(defn focus-and-select
  "Focuses, and optionally selects the content of, a form element."
  [id]
  (do
    (gforms/focusAndSelect (dom/get-element id))
    nil))

(defn has-value?
  "Whether a form element has a value."
  [id]
  (gforms/hasValue (dom/get-element id)))

(defn value
  "Gets the current value of any element with a type."
  [id]
  (gforms/getValue (dom/get-element id)))

(defn value!
  "Sets the current value of any element with a type."
  [id value]
  (do
    (gforms/setValue (dom/get-element id) value)
    nil))
