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
    :name   (when name
              (nth (:names source-map) name))}))

(defn seg-combine [seg relseg]
  (let [[gcol source line col name] seg
        [rgcol rsource rline rcol rname] relseg]
    [(+ gcol rgcol)
     (+ (or source 0) rsource)
     (+ (or line 0) rline)
     (+ (or col 0) rcol)
     (+ (or name 0) rname)]))

(defn decode
  ([source-map]
     (decode (:mappings source-map) source-map))
  ([mappings source-map]
     (let [relseg-init [0 0 0 0 0]
           update-result (fn [result seg gline]
                           (let [segmap (seg->map seg source-map)]
                             (update-in result [(:source segmap)]
                               (fnil (fn [m]
                                       (update-in m [(:line segmap)]
                                         (fnil (fn [m]
                                                 (assoc m (:col segmap)
                                                   {:gline gline
                                                    :gcol (:gcol segmap)
                                                    :name (:name segmap)}))
                                               (sorted-map))))
                                     (sorted-map)))))
           lines (seq (string/split mappings #";"))]
       (loop [i 0 lines lines relseg relseg-init result {}]
         (if lines
           (let [line (first lines)
                 [result relseg] (let [segs (seq (string/split line #","))]
                                   (loop [segs segs relseg relseg result result]
                                     (if segs
                                       (let [seg (first segs)
                                             nrelseg (seg-combine (base64-vlq/decode seg) relseg)]
                                         (recur (next segs) nrelseg (update-result result nrelseg i)))
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

  ;; :version :file :lineCount :mappings :sources :names
  (keys raw-source-map)
  (count (string/split (:mappings raw-source-map) #";"))

  (-> raw-source-map
      :mappings
      (string/split #";")
      first
      (string/split #",")
      count)

  (first (decode raw-source-map))
  
  (-> raw-source-map
      :mappings
      (string/split #";")
      first
      (string/split #",")
      (nth 30)
      base64-vlq/decode
      (seg->map raw-source-map))
  
  (first (string/split (:mappings raw-source-map) #";"))

  (def amapping
    {:generated 12
     :original 0
     :source "foo.cljs"
     :name "bar"})
  
  (json/json-str amapping)

  ;; source map v3
  {:version 3,
   :file "out.js",
   :sourceRoot "",
   :sources ["foo.js" "bar.js"],
   :names ["src" "maps" "are" "fun"],
   :mappings "AAgBC,SAAQ,CAAEA"}

  ;; each vector is a line, each vector is
  ;; [gencol, file, oline, ocol, [name]]
  (def files ["foo.cljs"])
  (def names ["baz" "bar"])
  (def dummy-mappings
    [[[0 0 12 16 0],
      [0 0 13 12 1]]])

  (map base64-vlq/encode [0 0 12 16 0])

  ;; we know the file list, atom?
  ;; we know the list of names, atom?
  (defn generate [f])

  (defn segs->str [segs]
    (->> segs
         (map (fn [seg]
                (apply str (map base64-vlq/encode seg))))
         (interpose ",")
         (apply str)))

  (defn from-mappings [mappings]
    (->> mappings
         (map #(segs->str %))
         (interpose ";")
         (apply str)))

  (from-mappings dummy-mappings)
  (defn to-mappings [source-map])
  
  )
