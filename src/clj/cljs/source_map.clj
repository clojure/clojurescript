(ns cljs.source-map
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [clojure.pprint :as pp]
            [cljs.source-map.base64-vlq :as base64-vlq]))

(defn source-compare [sources]
  (let [sources (->> sources
                  (map-indexed (fn [a b] [a b]))
                  (reduce (fn [m [i v]] (assoc m v i)) {}))]
    (fn [a b] (compare (sources a) (sources b)))))

(defn seg->map [seg source-map]
  (let [[gcol source line col name] seg]
   {:gcol   gcol
    :source (nth (:sources source-map) source)
    :line   line
    :col    col
    :name   (if-let [name (-> seg meta :name)]
              (nth (:names source-map) name))}))

(defn seg-combine [seg relseg]
  (let [[gcol source line col name] seg
        [rgcol rsource rline rcol rname] relseg
        nseg [(+ gcol rgcol)
              (+ (or source 0) rsource)
              (+ (or line 0) rline)
              (+ (or col 0) rcol)
              (+ (or name 0) rname)]]
    (if name
      (with-meta nseg {:name (+ name rname)})
      nseg)))

(defn update-result [result segmap gline]
  (let [{:keys [gcol source line col name]} segmap
        d {:gline gline
           :gcol gcol}
        d (if name (assoc d :name name) d)]
    (update-in result [source]
      (fnil (fn [m]
              (update-in m [line]
                (fnil (fn [m]
                        (assoc m col d))
                      (sorted-map))))
            (sorted-map)))))

(defn decode
  ([source-map]
     (decode (:mappings source-map) source-map))
  ([mappings source-map]
     (let [{:keys [sources]} source-map
           relseg-init [0 0 0 0 0]
           lines (seq (string/split mappings #";"))]
       (loop [gline 0
              lines lines
              relseg relseg-init
              result (sorted-map-by (source-compare sources))]
         (if lines
           (let [line (first lines)
                 [result relseg]
                 (let [segs (seq (string/split line #","))]
                   (loop [segs segs relseg relseg result result]
                     (if segs
                       (let [seg (first segs)
                             nrelseg (seg-combine (base64-vlq/decode seg) relseg)]
                         (recur (next segs) nrelseg
                           (update-result result (seg->map nrelseg source-map) gline)))
                       [result relseg])))]
             (recur (inc gline) (next lines) (assoc relseg 0 0) result))
           result)))))

(defn encode [m]
  )

(defn gen-merged-map [cljs-map closure-map]
  )

(comment
  ;; INSTRUCTIONS:
  
  ;; switch into samples/repl
  ;; run repl to start clojure
  ;; build with
  (require '[cljs.closure :as cljsc])
  (cljsc/build "src" {:optimizations :simple :output-to "repl_sample.js" :source-map "repl_sample_map.json"})

  ;; load source map
  (def raw-source-map
    (json/read-json (slurp (io/file "samples/repl/repl_sample_map.json"))))

  ;; test it out
  (first (decode raw-source-map))

  ;; decoded source map preserves file order
  (= (keys (decode raw-source-map)) (:sources raw-source-map))
  )
