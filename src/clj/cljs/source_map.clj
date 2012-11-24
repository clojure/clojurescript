(ns cljs.source-map
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [clojure.pprint :as pp]
            [cljs.source-map.base64-vlq :as base64-vlq]))

(defn indexed-sources [sources]
  (->> sources
    (map-indexed (fn [a b] [a b]))
    (reduce (fn [m [i v]] (assoc m v i)) {})))

(defn source-compare [sources]
  (let [sources (indexed-sources sources)]
    (fn [a b] (compare (sources a) (sources b)))))

(defn seg->map [seg source-map]
  (let [[gcol source line col name] seg]
   {:gcol   gcol
    :source (nth (:sources source-map) source)
    :line   line
    :col    col
    :name   (when-let [name (-> seg meta :name)]
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
                        (update-in m [col]
                          (fnil (fn [v] (conj v d))
                            [])))
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

(defn encode [m opts]
  (let [source-map {"version" 3
                    "file" (:file opts)
                    "lineCount" (:lines opts)
                    "mappings" nil
                    "names" nil}]
    (json/write-str source-map)))

(defn merge-source-maps
  ([cljs-map closure-map] (merge-source-maps cljs-map closure-map 0))
  ([cljs-map closure-map line-offset]
     (loop [line-map-seq (seq cljs-map) new-lines (sorted-map)]
       (if line-map-seq
         (let [[line col-map] (first line-map-seq)
               new-cols
               (loop [col-map-seq (seq col-map) new-cols (sorted-map)]
                 (if col-map-seq
                   (let [[col {:keys [gline gcol]}] (first col-map-seq)]
                     (recur (rest col-map-seq)
                       (assoc new-cols col
                         (get-in closure-map [(+ gline line-offset) gcol]))))
                   new-cols))]
           (recur (rest line-map-seq)
             (assoc new-lines line
               (new-cols))))
         new-lines))))

(comment
  ;; INSTRUCTIONS:
  
  ;; switch into samples/hello
  ;; run repl to start clojure
  ;; build with
  
  (require '[cljs.closure :as cljsc])
  (cljsc/build "src" {:optimizations :simple :output-to "hello.js" :source-map "hello.js.map"})

  ;; load source map
  (def raw-source-map
    (json/read-str (slurp (io/file "hello.js.map")) :key-fn keyword))

  ;; test it out
  (first (decode raw-source-map))

  ;; decoded source map preserves file order
  (= (keys (decode raw-source-map)) (:sources raw-source-map))
  )
