;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.source-map
  (:require [clojure.string :as string]
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
    :source (aget (.split (aget source-map "sources" source) "?") 0)
    :line   line
    :col    col
    :name   (when-let [name (-> seg meta :name)]
              (aget source-map "names" name))}))

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
     (decode-reverse (aget source-map "mappings") source-map))
  ([mappings source-map]
     (let [sources (aget source-map "sources")
           relseg-init [0 0 0 0 0]
           lines (seq (string/split mappings #";"))]
       (loop [gline 0
              lines lines
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
        d {:line line
           :col col
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
     (decode (aget source-map "mappings") source-map))
  ([mappings source-map]
     (let [sources (aget source-map "sources")
           relseg-init [0 0 0 0 0]
           lines (seq (string/split mappings #";"))]
       (loop [gline 0 lines lines relseg relseg-init result {}]
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
                             (update-result result (seg->map nrelseg source-map) gline)))
                         [result relseg]))))]
             (recur (inc gline) (next lines) (assoc relseg 0 0) result))
           result)))))

;; -----------------------------------------------------------------------------
;; Merging

(defn merge-source-maps
  "Merge an internal source map representation of a single
   ClojureScript file mapping original to generated with a
   second source map mapping original JS to generated JS.
   The is to support source maps that work through multiple
   compilation steps like Google Closure optimization passes."
  [cljs-map closure-map]
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
                                (into v (get-in closure-map [gline gcol])))
                        [] infos))))
                new-cols))]
        (recur (next line-map-seq)
          (assoc new-lines line new-cols)))
      new-lines)))
