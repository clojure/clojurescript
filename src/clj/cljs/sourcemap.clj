(ns cljs.sourcemap
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [cljs.sourcemap.base64-vlq :as base64-vlq]))

(defn decode [seg mapping]
  (let [[gcol source line col name] seg]
   {:gcol gcol
    :source (nth (:sources mapping) source)
    :line line
    :col col
    :name (when name
            (nth (:names mapping) name))}))

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
      second
      base64-vlq/decode
      (decode raw-source-map))
  
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
