;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.source-map
  (:require [goog.object :as gobj]
            [clojure.string :as string]
            [clojure.set :as set]
            [cljs.source-map.base64-vlq :as base64-vlq]))

;; =============================================================================
;; Source map code in the file assumes the following in memory
;; representation of source map data.
;;
;; { gline[Integer]
;;   { gcol[Integer]
;;    [{ :line ..., :col ..., :name ..., :source ... }] } }
;;
;; Reverse source map code in the file assumes the following in memory
;; representation of source map data.
;;
;; { file-name[String]
;;   { line[Integer]
;;     { col[Integer]
;;       [{ :gline ..., :gcol ..., :name ... }] } }

;; -----------------------------------------------------------------------------
;; Utilities

(defn indexed-sources
  "Take a seq of source file names and return a map from
   file number to integer index. For reverse source maps."
  [sources]
  (->> sources
    (map-indexed (fn [a b] [a b]))
    (reduce (fn [m [i v]] (assoc m v i)) {})))

(defn source-compare
  "Take a seq of source file names and return a comparator
   that can be used to construct a sorted map. For reverse
   source maps."
  [sources]
  (let [sources (indexed-sources sources)]
    (fn [a b] (compare (sources a) (sources b)))))

;; -----------------------------------------------------------------------------
;; Decoding

(defn seg->map
  "Take a source map segment represented as a vector
   and return a map."
  [seg source-map]
  (let [[gcol source line col name] seg]
   {:gcol   gcol
    :source (aget (gobj/get source-map "sources") source)
    :line   line
    :col    col
    :name   (when-let [name (-> seg meta :name)]
              (aget (gobj/get source-map "names") name))}))

(defn seg-combine
  "Combine a source map segment vector and a relative
   source map segment vector and combine them to get
   an absolute segment posititon information as a vector."
  [seg relseg]
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

(defn update-reverse-result
  "Helper for decode-reverse. Take a reverse source map and
  update it with a segment map."
  [result segmap gline]
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

(defn decode-reverse
  "Convert a v3 source map JSON object into a reverse source map
  mapping original ClojureScript source locations to the generated
  JavaScript."
  ([source-map]
   (decode-reverse
     (gobj/get source-map "mappings") source-map))
  ([mappings source-map]
   (let [sources     (gobj/get source-map "sources")
         relseg-init [0 0 0 0 0]
         lines       (seq (string/split mappings #";"))]
     (loop [gline  0
            lines  lines
            relseg relseg-init
            result (sorted-map-by (source-compare sources))]
       (if lines
         (let [line (first lines)
               [result relseg]
               (if (string/blank? line)
                 [result relseg]
                 (let [segs (seq (string/split line #","))]
                   (loop [segs segs relseg relseg result result]
                     (if segs
                       (let [seg (first segs)
                             nrelseg (seg-combine (base64-vlq/decode seg) relseg)]
                         (recur (next segs) nrelseg
                           (update-reverse-result result (seg->map nrelseg source-map) gline)))
                       [result relseg]))))]
           (recur (inc gline) (next lines) (assoc relseg 0 0) result))
         result)))))

(defn update-result
  "Helper for decode. Take a source map and update it based on a
  segment map."
  [result segmap gline]
  (let [{:keys [gcol source line col name]} segmap
        d {:line   line
           :col    col
           :source source}
        d (if name (assoc d :name name) d)]
    (update-in result [gline]
      (fnil (fn [m]
              (update-in m [gcol]
                (fnil #(conj % d) [])))
            (sorted-map)))))

(defn decode
  "Convert a v3 source map JSON object into a source map mapping
  generated JavaScript source locations to the original
  ClojureScript."
  ([source-map]
     (decode (gobj/get source-map "mappings") source-map))
  ([mappings source-map]
     (let [sources     (gobj/get source-map "sources")
           relseg-init [0 0 0 0 0]
           lines       (seq (string/split mappings #";"))]
       (loop [gline 0 lines lines relseg relseg-init result {}]
         (if lines
           (let [line (first lines)
                 [result relseg]
                 (if (string/blank? line)
                   [result relseg]
                   (let [segs (seq (string/split line #","))]
                     (loop [segs segs relseg relseg result result]
                       (if segs
                         (let [seg     (first segs)
                               nrelseg (seg-combine (base64-vlq/decode seg) relseg)]
                           (recur (next segs) nrelseg
                             (update-result result (seg->map nrelseg source-map) gline)))
                         [result relseg]))))]
             (recur (inc gline) (next lines) (assoc relseg 0 0) result))
           result)))))

;; -----------------------------------------------------------------------------
;; Encoding

(defn lines->segs
  "Take a nested sorted map encoding line and column information
   for a file and return a vector of vectors of encoded segments.
   Each vector represents a line, and the internal vectors are segments
   representing the contents of the line."
  [lines]
  (let [relseg (atom [0 0 0 0 0])]
    (reduce
      (fn [segs cols]
        (swap! relseg
          (fn [[_ source line col name]]
            [0 source line col name]))
        (conj segs
          (reduce
            (fn [cols [gcol sidx line col name :as seg]]
              (let [offset (map - seg @relseg)]
                (swap! relseg
                  (fn [[_ _ _ _ lname]]
                    [gcol sidx line col (or name lname)]))
                (conj cols (base64-vlq/encode offset))))
            [] cols)))
      [] lines)))

(defn encode
  "Take an internal source map representation represented as nested
   sorted maps of file, line, column and return a source map v3 JSON
   string."
  [m opts]
  (let [lines          (atom [[]])
        names->idx     (atom {})
        name-idx       (atom 0)
        preamble-lines (take (or (:preamble-line-count opts) 0) (repeat []))
        info->segv     (fn [info source-idx line col]
                         (let [segv [(:gcol info) source-idx line col]]
                           (if-let [name (:name info)]
                             (let [idx (if-let [idx (get @names->idx name)]
                                         idx
                                         (let [cidx @name-idx]
                                           (swap! names->idx assoc name cidx)
                                           (swap! name-idx inc)
                                           cidx))]
                               (conj segv idx))
                             segv)))
        encode-cols    (fn [infos source-idx line col]
                         (doseq [info infos]
                           (let [segv  (info->segv info source-idx line col)
                                 gline (:gline info)
                                 lc    (count @lines)]
                             (if (> gline (dec lc))
                               (swap! lines
                                 (fn [lines]
                                   (conj (into lines (repeat (dec (- gline (dec lc))) [])) [segv])))
                               (swap! lines
                                 (fn [lines]
                                   (update-in lines [gline] conj segv)))))))]
    (doseq [[source-idx [_ lines]] (map-indexed (fn [i v] [i v]) m)]
      (doseq [[line cols] lines]
        (doseq [[col infos] cols]
          (encode-cols infos source-idx line col))))
    (let [source-map-file-contents
          (cond-> #js {"version"   3
                       "file"      (:file opts)
                       "sources"   (let [paths (keys m)
                                         f     (comp
                                                 (if (true? (:source-map-timestamp opts))
                                                   #(str % "?rel=" (.valueOf (js/Date.)))
                                                   identity)
                                                 #(last (string/split % #"/")))]
                                     (->> paths (map f) (into-array)))
                       "lineCount" (:lines opts)
                       "mappings"  (->> (lines->segs (concat preamble-lines @lines))
                                     (map #(string/join "," %))
                                     (string/join ";"))
                       "names"     (into-array
                                     (map (set/map-invert @names->idx)
                                       (range (count @names->idx))))}
            (:sources-content opts)
            (doto (gobj/set "sourcesContent" (into-array (:sources-content opts)))))]
      (.stringify js/JSON source-map-file-contents))))

;; -----------------------------------------------------------------------------
;; Merging

(defn merge-source-maps
  "Merge an internal source map representation of a single
   ClojureScript file mapping original to generated with a
   second source map mapping original JS to generated JS.
   The is to support source maps that work through multiple
   compilation steps like Google Closure optimization passes."
  [cljs-map js-map]
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
                                (into v (get-in js-map [gline gcol])))
                        [] infos))))
                new-cols))]
        (recur (next line-map-seq)
          (assoc new-lines line new-cols)))
      new-lines)))

;; -----------------------------------------------------------------------------
;; Reverse Source Map Inversion

(defn invert-reverse-map
  "Given a ClojureScript to JavaScript source map, invert it. Useful when
   mapping JavaScript stack traces when environment support is unavailable."
  [reverse-map]
  (let [inverted (atom (sorted-map))]
    (doseq [[line columns] reverse-map]
      (doseq [[column column-info] columns]
        (doseq [{:keys [gline gcol name]} column-info]
          (swap! inverted update-in [gline]
            (fnil (fn [columns]
                    (update-in columns [gcol] (fnil conj [])
                      {:line line :col column :name name}))
              (sorted-map))))))
    @inverted))

(comment
  (invert-reverse-map
    {1
     {1 [{:gcol 0, :gline 0, :name "cljs.core/map"}],
      5 [{:gcol 24, :gline 0, :name "cljs.core/inc"}]}})
  )