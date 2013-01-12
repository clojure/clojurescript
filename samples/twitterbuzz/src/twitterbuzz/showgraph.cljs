;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns twitterbuzz.showgraph
  (:require [twitterbuzz.core :as buzz]
            [twitterbuzz.layout :as layout]
            [twitterbuzz.dom-helpers :as dom]
            [twitterbuzz.timeline :as timeline]
            [goog.events :as events]
            [goog.style :as style]
            [goog.math.Coordinate :as Coordinate]
            [goog.ui.HoverCard :as HoverCard]
            [goog.graphics.Font :as Font]
            [goog.graphics.Stroke :as Stroke]
            [goog.graphics.SolidFill :as SolidFill]
            [goog.graphics :as graphics]))

; Drawing configuration
(def avatar-size 32) ; used for both x and y dimensions of avatars
; fail whale
;(def default-avatar "http://farm3.static.flickr.com/2562/4140195522_e207b97280_s.jpg")
; google+ silhouette
(def default-avatar "http://ssl.gstatic.com/s2/profiles/images/silhouette48.png")
(defn debug [_])
;(defn debug [a] (str "t: " (:t a) " score: " (:best-score a)))

; BAD HACK: don't change globals like this -- find a better way:
;(set! anim/TIMEOUT 500)

(def edge-stroke (graphics/Stroke. 1 "#009"))

(def g
  (doto (graphics/createGraphics "100%" "100%")
    (.render (dom/get-element :network))))

(def font (graphics/Font. 12 "Arial"))
(def fill (graphics/SolidFill. "#f00"))

(defn unit-to-pixel [unit-arg canvas-size]
  (+ (* unit-arg (- canvas-size avatar-size)) (/ avatar-size 2)))

(defn log [& args]
  (.log js/console (apply pr-str args)))

(def avatar-hover
  (doto
    (goog.ui/HoverCard. (js-obj)) ; svg IMAGE tags don't work here
    (.setElement (dom/get-element :avatar-hover))))

(defn hide-tooltip [event]
  (.setVisible avatar-hover false))

(defn attach-tooltip [img canvas-offset px py tweet]
  (events/listen img events/EventType.MOUSEOUT hide-tooltip)
  (events/listen
    img events/EventType.MOUSEOVER
    (fn [event]
        (hide-tooltip event)
        (.setPosition avatar-hover
                    (goog.ui/Tooltip.CursorTooltipPosition.
                        (Coordinate/sum (goog.math/Coordinate. px py)
                                        canvas-offset)))
        (dom/remove-children :avatar-hover-body)
        (dom/append (dom/get-element :avatar-hover-body)
                    (timeline/timeline-element tweet))
        (.triggerForElement avatar-hover img))))

(defn draw-graph [{:keys [locs mentions]} text]
  (let [canvas-size (. g (getPixelSize))
        canvas-offset (style/getPageOffset (dom/get-element :network))]
    (. g (clear))

    ; Draw mention edges
    (doseq [[username {ux1 :x, uy1 :y}] locs
            :let [x1 (unit-to-pixel ux1 (.-width canvas-size))
                  y1 (unit-to-pixel uy1 (.-height canvas-size))]
            [mention-name mention-count] (:mentions (get mentions username))]
      (when-let [{ux2 :x, uy2 :y} (get locs mention-name)]
        (let [x2 (unit-to-pixel ux2 (.-width canvas-size))
              y2 (unit-to-pixel uy2 (.-height canvas-size))]
          (.drawPath g
                    (-> (. g (createPath)) (.moveTo x1 y1) (.lineTo x2 y2))
                    edge-stroke nil))))

    ; Draw avatar nodes
    (doseq [[username {:keys [x y] :as foo}] locs]
      ;(log (pr-str foo))
      (let [px (- (unit-to-pixel x (.-width canvas-size))  (/ avatar-size 2))
            py (- (unit-to-pixel y (.-height canvas-size)) (/ avatar-size 2))
            user (get mentions username)
            image-url (get user :image-url default-avatar)
            img (.drawImage g px py avatar-size avatar-size image-url)]
        (attach-tooltip img canvas-offset px py
                        {:profile_image_url image-url
                         :text (:last-tweet user)
                         :from_user (:username user)})))

    (let [text (if (empty? locs)
                 "No locations to graph"
                 text)]
      (when text
        (.drawTextOnLine g text 5 20 (.-width canvas-size) 20
                        "left" font nil fill)))))

(def graph-data (atom nil))

;; Register event listeners.

(buzz/register :graph-update
  (fn [data]
    (reset! graph-data data)
    (draw-graph (layout/radial data) nil)))

(events/listen (dom/get-element :network) events/EventType.CLICK
               #(draw-graph (layout/radial @graph-data) nil))
(buzz/register :track-clicked #(. g (clear)))
