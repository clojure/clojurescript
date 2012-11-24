(ns cljs.source-map
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [clojure.set :as set]
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

;; TODO: the lastseg needs to be passed into encode-cols - everything
;; but :gcol is relative

(defn encode-cols [vs source-idx line col names->idx name-idx]
  (loop [vs (seq vs) lastseg nil cols-segs []]
    (if vs
      (let [v (first vs)
            seg [(:gcol v) source-idx line col]
            seg (if-let [name (:name v)]
                  (let [idx (if-let [idx (get @names->idx name)]
                              idx
                              (let [cidx @name-idx]
                                (swap! names->idx assoc name cidx)
                                (swap! name-idx inc)
                                cidx))]
                    (conj seg idx))
                  seg)
            relseg (if lastseg
                      (into [] (map - seg lastseg))
                      seg)]
        (recur (next vs)
          (if (and (= (count seg) 4)
                   (= (count lastseg) 5))
            (conj seg (peek lastseg))
            seg)
          (conj cols-segs (base64-vlq/encode relseg))))
      cols-segs)))

(defn encode-source [lines segs source-idx names->idx name-idx]
  (loop [lines (seq lines) segs segs]
    (if lines
      (let [[line cols] (first lines)
            segs
            (loop [cols (seq cols) segs segs]
              (if cols
                (let [[col vs] (first cols)
                      col-segs (encode-cols vs source-idx line col names->idx name-idx)]
                  (recur (next cols) (conj segs col-segs)))
                segs))]
        (recur (next lines) segs))
      segs)))

(defn encode [m opts]
  (let [names->idx (atom {})
        name-idx   (atom 0)
        segs (loop [sources (seq m) source-idx 0 segs []]
               (if sources
                 (let [[source lines] (first sources)
                       segs (encode-source lines segs source-idx names->idx name-idx)]
                   (recur (next sources) (inc source-idx) segs))
                 segs))]
    (with-out-str
      (json/pprint 
       {"version" 3
        "file" (:file opts)
        "sources" (into [] (keys m))
        "lineCount" (:lines opts)
        "mappings" (->> segs
                        (map #(string/join "," %))
                        (string/join ";"))
        "names" (into [] (map (set/map-invert @names->idx) (range (count @names->idx))))}))))

(defn merge-source-maps
  ([cljs-map closure-map] (merge-source-maps cljs-map closure-map 0))
  ([cljs-map closure-map line-offset]
     (loop [line-map-seq (seq cljs-map) new-lines (sorted-map)]
       (if line-map-seq
         (let [[line col-map] (first line-map-seq)
               new-cols
               (loop [col-map-seq (seq col-map) new-cols (sorted-map)]
                 (if col-map-seq
                   (let [[col infos] (first col-map-seq)]
                     (recur (next col-map-seq)
                       (assoc new-cols col
                         (reduce (fn [v {:keys [gline gcol]}]
                                   (into v (get-in closure-map [(+ gline line-offset) gcol])))
                                 [] infos))))
                   new-cols))]
           (recur (next line-map-seq)
             (assoc new-lines line new-cols)))
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
