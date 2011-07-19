(ns twitterbuzz.showgraph
  (:require [twitterbuzz.core :as buzz]
            [twitterbuzz.anneal :as ann]
            [twitterbuzz.layout :as layout]
            [goog.dom :as dom]
            [goog.events :as events]
            [goog.fx.Animation :as anim]
            [goog.graphics.Font :as Font]
            [goog.graphics.Stroke :as Stroke]
            [goog.graphics.SolidFill :as SolidFill]
            [goog.graphics :as graphics]))

; Drawing configuration
(def avatar-size 48) ; used for both x and y dimensions of avatars
(def edge-widths [0 1 2 3 4]) ; More mentions == thicker edges
(def anneal-skipping 100)
(def cooling 1000)
; fail whale
;(def default-avatar "http://farm3.static.flickr.com/2562/4140195522_e207b97280_s.jpg")
; google+ silhouette
(def default-avatar "http://ssl.gstatic.com/s2/profiles/images/silhouette48.png")
(defn debug [_])
;(defn debug [a] (str "t: " (:t a) " score: " (:best-score a)))

; BAD HACK: don't change globals like this -- find a better way:
;(set! anim/TIMEOUT 500)

(def edge-strokes
  (vec (map #(graphics/Stroke. % "#009") edge-widths)))

(def max-stroke (peek edge-strokes))

(def g
  (doto (graphics/createGraphics "100%" "100%")
    (.render (dom/getElement "network"))))

(def font (graphics/Font. 12 "Arial"))
(def fill (graphics/SolidFill. "#f00"))

(defn unit-to-pixel [unit-arg canvas-size]
  (+ (* unit-arg (- canvas-size avatar-size)) (/ avatar-size 2)))

(defn draw-graph [{:keys [locs mentions]} text]
  (let [canvas-size (. g (getPixelSize))]
    (. g (clear))

    ; Draw mention edges
    (doseq [[username {ux1 :x, uy1 :y}] locs
            :let [x1 (unit-to-pixel ux1 (.width canvas-size))
                  y1 (unit-to-pixel uy1 (.height canvas-size))]
            [mention-name mention-count] (:mentions (get mentions username))]
      (when-let [{ux2 :x, uy2 :y} (get locs mention-name)]
        (let [x2 (unit-to-pixel ux2 (.width canvas-size))
              y2 (unit-to-pixel uy2 (.height canvas-size))]
          (.drawPath g
                    (-> (. g (createPath)) (.moveTo x1 y1) (.lineTo x2 y2))
                    (get edge-strokes mention-count max-stroke) nil))))

    ; Draw avatar nodes
    (doseq [[username {:keys [x y]}] locs]
      (.drawImage g
                  (- (unit-to-pixel x (.width canvas-size))  (/ avatar-size 2))
                  (- (unit-to-pixel y (.height canvas-size)) (/ avatar-size 2))
                  avatar-size avatar-size
                  (get (get mentions username) :image-url default-avatar)))

    (let [text (if (empty? locs)
                 "No locations to graph"
                 text)]
      (when text
        (.drawTextOnLine g text 5 20 (.width canvas-size) 20
                        "left" font nil fill)))))

(def users (atom nil))
(buzz/register :graph-update #(reset! users %))

(def animation (atom nil))

(set! (.cycle animation)
  (fn [t]
    (let [a (first @animation)]
      (draw-graph (:best a) (debug a))
      (swap! animation #(drop anneal-skipping %))
      (when (= (:best a) (:best (first @animation)))
        ; no better graph in the last 'anneal-skipping' steps, so quit trying.
        (anim/unregisterAnimation animation)))))

(defn start-anneal []
  (reset! animation
    (ann/anneal
      layout/score
      (ann/linear-cooling cooling)
      layout/permute-move
      ann/standard-prob
      (layout/init-state @users)))
  (anim/registerAnimation animation))

(events/listen
  (dom/getElement "network") (array events/EventType.CLICK) start-anneal)
(buzz/register :track-clicked start-anneal)
(buzz/register :refresh-clicked start-anneal)
