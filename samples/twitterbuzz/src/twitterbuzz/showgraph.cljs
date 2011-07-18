(ns twitterbuzz.showgraph
  (:require [twitterbuzz.core :as buzz]
            [goog.dom :as dom]
            [goog.graphics :as graphics]))

; Drawing configuration
(def avatar-size 0.15) ; used for both x and y dimensions of avatars
(def edge-widths [0 0.001 0.005 0.010 0.020]) ; More mentions == thicker edges

(def edge-strokes
  (vec (map #(graphics/Stroke. % "#009") edge-widths)))

(def max-stroke (peek edge-strokes))

(def g
  (doto (graphics/createGraphics "100%" "100%" 1.0 1.0)
    (.render (dom/getElement "network"))))

(defn draw-graph [users nodes]
  ; Draw mention edges
  (doseq [[username {:keys [mentions]}] users
          :when (get nodes username)
          [mention-name mention-count] mentions]
    (when-let [{x2 :x, y2 :y} (get nodes mention-name)]
      (let [{x1 :x, y1 :y} (get nodes username)]
        (.drawPath g
                   (-> (. g (createPath)) (.moveTo x1 y1) (.lineTo x2 y2))
                   (get edge-strokes mention-count max-stroke) nil))))

  ; Draw avatar nodes
  (let [offset (/ avatar-size 2)]
    (doseq [[username {:keys [x y]}] nodes]
      (.drawImage g (- x offset) (- y offset) avatar-size avatar-size
                  (-> users (get username) :image-url)))))

; This is temporary.  The graph data should flow somehow from the
; tweets.  For now, just hardcode some:
(defn update-graph [tweets]
  (draw-graph
    {"bob"
    {:image-url "http://a1.twimg.com/profile_images/1324487726/dr_bunsen_honeydew_normal.jpg"
     :mentions {"susan" 3 "joe" 2}
     :last-tweet "Clojure on JavaScript. Wow!"}
  "susan"
    {:image-url "http://a1.twimg.com/profile_images/1324487726/dr_bunsen_honeydew_normal.jpg"
     :mentions {"joe" 5}
     :last-tweet "ClojureScript doesn't have TCO! I'll never use it."}}
    {"bob" {:x 0.2 :y 0.7} "susan" {:x 0.4 :y 0.1}}))

(buzz/register update-graph)
