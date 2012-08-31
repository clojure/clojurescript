(ns cljs.sourcemap
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [clojure.pprint :as pp]
            [cljs.sourcemap.base64-vlq :as base64-vlq]))

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
  (update-in result [(:source segmap)]
    (fnil (fn [m]
            (update-in m [(:line segmap)]
              (fnil (fn [m]
                      (assoc m (:col segmap)
                        (let [d {:gline gline
                                 :gcol (:gcol segmap)}]
                          (if-let [name (:name segmap)]
                            (assoc d :name name)
                            d))))
                    (sorted-map))))
          (sorted-map))))

(defn decode
  ([source-map]
     (decode (:mappings source-map) source-map))
  ([mappings source-map]
     (let [relseg-init [0 0 0 0 0]
           lines (seq (string/split mappings #";"))]
       (loop [i 0 lines lines relseg relseg-init result {}]
         (if lines
           (let [line (first lines)
                 [result relseg]
                 (let [segs (seq (string/split line #","))]
                   (loop [segs segs relseg relseg result result]
                     (if segs
                       (let [seg (first segs)
                             nrelseg (seg-combine (base64-vlq/decode seg) relseg)]
                         (recur (next segs) nrelseg
                           (update-result result (seg->map nrelseg source-map) i)))
                       [result relseg])))]
             (recur (inc i) (next lines) (assoc relseg 0 0) result))
           result)))))

(defn encode [xs]
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
  )
