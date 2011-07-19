(ns twitterbuzz.showgraph
  (:require [twitterbuzz.core :as buzz]
            [twitterbuzz.anneal :as ann]
            [twitterbuzz.layout :as layout]
            [goog.dom :as dom]
            [goog.events :as events]
            [goog.fx.Animation :as anim]
            [goog.graphics :as graphics]))

; Drawing configuration
(def avatar-size 0.07) ; used for both x and y dimensions of avatars
(def edge-widths [0 0.001 0.005 0.010 0.020]) ; More mentions == thicker edges

; BAD HACK: don't change globals like this -- find a better way:
(set! anim/TIMEOUT 500)

(def edge-strokes
  (vec (map #(graphics/Stroke. % "#009") edge-widths)))

(def max-stroke (peek edge-strokes))

(def g
  (doto (graphics/createGraphics "100%" "100%" 1.0 1.0)
    (.render (dom/getElement "network"))))

(defn draw-graph [{:keys [locs mentions]}]
  (. g (clear))

  ; Draw mention edges
  (doseq [[username {x1 :x, y1 :y}] locs
          [mention-name mention-count] (:mentions (get mentions username))]
    (when-let [{x2 :x, y2 :y} (get locs mention-name)]
      (.drawPath g
                 (-> (. g (createPath)) (.moveTo x1 y1) (.lineTo x2 y2))
                 (get edge-strokes mention-count max-stroke) nil)))

  ; Draw avatar nodes
  (let [offset (/ avatar-size 2)]
    (doseq [[username {:keys [x y]}] locs]
      (.drawImage g (- x offset) (- y offset) avatar-size avatar-size
                  (:image-url (get mentions username))))))

; This is temporary.  The graph data should flow somehow from the
; tweets.  For now, just hardcode some:
(def test-users {"djspiewak" {:image-url "http://a0.twimg.com/profile_images/746976711/angular-final_normal.jpg", :last-tweet "Does Clojure have a Sinatra clone?", :mentions {}}, "tobsn" {:image-url "http://a2.twimg.com/profile_images/1364411587/yr40_normal.png", :last-tweet "Creating a Query DSL using Clojure and MongoDB http://tob.sn/qgCxkm #mongodb", :mentions {}}, "CzarneckiD" {:image-url "http://a3.twimg.com/profile_images/1156755747/head_trees_normal.jpg", :last-tweet "@greymouser I need to start writing some Clojure I guess :)", :mentions {"greymouser" 2}}, "sbtourist" {:image-url "http://a0.twimg.com/profile_images/72494229/Cryer_Black_normal.jpg", :last-tweet "Clooj, a lightweight Clojure IDE: http://t.co/OSCjr9X", :mentions {"djspiewak" 2}}, "jboner" {:image-url "http://a2.twimg.com/profile_images/1395654712/jonas_bw_small_normal.JPG", :last-tweet "RT @sbtourist: Clooj, a lightweight Clojure IDE: http://t.co/OSCjr9X", :mentions {"sbtourist" 2, "djspiewak" 2, "romanroe" 2}}})

(def test-graph (atom (list
  {"jboner" {:x 0.2 :y 0.8} "djspiewak" {:x 0.3 :y 0.2}}
  {"jboner" {:x 0.4 :y 0.7} "djspiewak" {:x 0.4 :y 0.3}}
  {"jboner" {:x 0.3 :y 0.8} "sbtourist" {:x 0.3 :y 0.2} "djspiewak" {:x 0.5 :y 0.5}}
  {"jboner" {:x 0.2 :y 0.7} "sbtourist" {:x 0.4 :y 0.1} "djspiewak" {:x 0.7 :y 0.4}})))

(def animation (atom nil))

;(set! cljs.core/string-print (fn [x] (js* "console.log(~{x})")))

(set! (.cycle animation)
  (fn [t]
    (draw-graph (:best (first @animation)))
    (swap! animation rest)))

(events/listen
  (dom/getElement "network")
  (array events/EventType.CLICK)
  (fn [_]
    (reset! animation
      (ann/anneal
        layout/score
        (ann/linear-cooling 1000)
        layout/permute-move
        ann/standard-prob
        (layout/init-state test-users)))
    (anim/registerAnimation animation)))

