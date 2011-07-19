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
(def stroke (graphics/Stroke. 1 "#f00"))
(def fill (graphics/SolidFill. "#f00"))

(defn unit-to-pixel [unit-arg canvas-size]
  (+ (* unit-arg (- canvas-size avatar-size)) (/ avatar-size 2)))

(defn draw-graph [{:keys [locs mentions]} text]
  (. g (clear))

  (let [canvas-size (. g (getPixelSize))]

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

    (when text (.drawTextOnLine g text 0 20 0 (.height canvas-size)
                                "left" font stroke fill))))

; This is temporary.  The graph data should flow somehow from the
; tweets.  For now, just hardcode some:
(def test-users {"djspiewak" {:image-url "http://a0.twimg.com/profile_images/746976711/angular-final_normal.jpg", :last-tweet "Does Clojure have a Sinatra clone?", :mentions {}}, "tobsn" {:image-url "http://a2.twimg.com/profile_images/1364411587/yr40_normal.png", :last-tweet "Creating a Query DSL using Clojure and MongoDB http://tob.sn/qgCxkm #mongodb", :mentions {}}, "CzarneckiD" {:image-url "http://a3.twimg.com/profile_images/1156755747/head_trees_normal.jpg", :last-tweet "@greymouser I need to start writing some Clojure I guess :)", :mentions {"greymouser" 2}}, "sbtourist" {:image-url "http://a0.twimg.com/profile_images/72494229/Cryer_Black_normal.jpg", :last-tweet "Clooj, a lightweight Clojure IDE: http://t.co/OSCjr9X", :mentions {"djspiewak" 2}}, "jboner" {:image-url "http://a2.twimg.com/profile_images/1395654712/jonas_bw_small_normal.JPG", :last-tweet "RT @sbtourist: Clooj, a lightweight Clojure IDE: http://t.co/OSCjr9X", :mentions {"sbtourist" 2, "djspiewak" 2, "romanroe" 2}}})

(def animation (atom nil))

(set! (.cycle animation)
  (fn [t]
    (let [a (first @animation)]
      (draw-graph (:best a) (debug a))
      (swap! animation #(drop anneal-skipping %))
      (when (= (:best a) (:best (first @animation)))
        ; no better graph in the last 'anneal-skipping' steps, so quit trying.
        (anim/unregisterAnimation animation)))))

(events/listen
  (dom/getElement "network")
  (array events/EventType.CLICK)
  (fn [_]
    (reset! animation
      (ann/anneal
        layout/score
        (ann/linear-cooling cooling)
        layout/permute-move
        ann/standard-prob
        (layout/init-state test-users)))
    (anim/registerAnimation animation)))

