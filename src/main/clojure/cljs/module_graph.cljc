;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.module-graph
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [cljs.compiler :as comp]))

(defn find-sources-for-module-entry
  "Given an entry as a symbol, find all matching inputs in sources. If the
  symbol ends in a *, then the symbol will be treated as a wildcard. This
  function returns a set and is not order preserving. If there are no matches
  returns nil."
  [entry sources]
  (let [m  (name (comp/munge entry))
        xs (string/split m #"\.")]
    (if (= "_STAR_" (last xs))
      (let [matcher (str (string/join "." (butlast xs)) ".")
            matches (into #{}
                      (filter
                        (fn [source]
                          (when (some #(.startsWith ^String % matcher)
                                  (map (comp str munge) (:provides source)))
                            source)))
                      sources)]
        (when-not (empty? matches)
          matches))
      (when-let [input (some
                         (fn [source]
                           (let [matcher
                                 (into #{}
                                   [(name entry) (name (comp/munge entry))])]
                             (when (some matcher (map (comp str munge) (:provides source)))
                               source)))
                         sources)]
        #{input}))))

;; Passes for constructing complete module information

(defn normalize
  "Normalize compiler :modules. All symbols in a module :entries will be
  converted into munged strings."
  [modules]
  (reduce-kv
    (fn [ret module-name module]
      (assoc ret module-name
                 (update module :entries
                   (fn [es] (into #{} (map (comp str munge)) es)))))
    {} modules))

(defn add-cljs-base
  "Adds :cljs-base module to compiler :modules if not already present."
  [modules]
  (cond-> modules
    (not (contains? modules :cljs-base))
    (assoc :cljs-base {})))

(defn add-cljs-base-dep
  "Adds :cljs-base to any module in compiler :modules with an empty :depends-on."
  [modules]
  (reduce-kv
    (fn [ret k {:keys [depends-on] :as module-info}]
      (assoc ret k
                 (cond-> module-info
                   (and (not= :cljs-base k) (empty? depends-on))
                   (assoc :depends-on [:cljs-base]))))
    {} modules))

(defn depth-of
  "Compute the depth of module-name based on dependency information in
   compiler :modules."
  [module-name modules]
  (if (= module-name :cljs-base)
    0
    (let [mns (get-in modules [module-name :depends-on])]
      (if (empty? mns)
        1
        (apply max
          (map (fn [mn] (+ 1 (depth-of mn modules))) mns))))))

(defn annotate-depths
  "Annotate all modules in compiler :modules with depth information."
  [modules]
  (reduce-kv
    (fn [ret module-name module]
      (let [module' (assoc module :depth (depth-of module-name modules))]
        (assoc ret module-name module')))
    {} modules))

(defn index-inputs
  "Index compiler inputs by :provides. If an input has multiple entries
  in :provides will result in multiple entries in the map. The keys will be munged
  strings not symbols."
  [inputs]
  (reduce
    (fn [ret {:keys [provides] :as input}]
      (into ret (map #(vector (-> % munge str) input)) provides))
    {} inputs))

(defn deps-for
  "Return all dependencies for x in a graph using deps-key."
  [x graph deps-key]
  (let [requires (get-in graph [x deps-key])]
    (-> (mapcat #(deps-for % graph deps-key) requires)
      (concat requires) distinct vec)))

(defn deps-for-entry
  "Return all dependencies for an entry using a compiler inputs index."
  [entry indexed-inputs]
  (map #(-> % munge str) (deps-for entry indexed-inputs :requires)))

(defn deps-for-module
  "Return all dependencies of a module using compiler :modules."
  [module modules]
  (deps-for module modules :depends-on))

(defn deepest-common-parent
  "Given a set of modules and a compiler :modules graph, compute the deepest
  common parent module."
  [modules all-modules]
  (let [common-parents (reduce set/intersection
                         (map #(set (deps-for-module % all-modules)) modules))]
    (apply max-key
      (fn [p] (get-in all-modules [p :depth]))
      common-parents)))

(defn canonical-name
  "Given an entry use indexed-inputs to return the canonical name. Throws if
   entry cannot be found."
  [entry indexed-inputs]
  (if-let [entry (get indexed-inputs (-> entry comp/munge str))]
    (-> (:provides entry) first comp/munge str)
    (throw (Exception. (str "No input matching \"" entry "\"")))))

(defn validate-modules
  "Check that a compiler :modules map does not contain user supplied duplicates.
   Throws if modules fails validation."
  [modules indexed-inputs]
  (let [seen (atom {})]
    (doseq [[module-name {:keys [entries] :as module}] modules]
      (let [entries (into #{} (map #(canonical-name % indexed-inputs)) entries)]
        (doseq [entry entries]
          (let [seen' @seen]
            (if-some [module-name' (get seen' entry)]
              (throw
                (Exception.
                  (str "duplicate entry \"" entry "\", occurs in " module-name
                       " and " module-name' ". entry :provides is "
                       (get-in indexed-inputs [entry :provides]))))
              (swap! seen assoc entry module-name))))))))

(defn inputs->assigned-modules
  "Given compiler inputs assign each to a single module. This is done by first
  starting with :entries. Dependencies for every entry in a module are also added
  to that module. Inputs may of course be assigned to several modules initially
  but we must eventually choose one. User supplied module :entries are respected
  but all other input assignments are computed automatically via
  deepest-common-parent. This function returns a map assigning all inputs (indexed
  by munged name) to a single module. Any orphan inputs will be assigned to
  :cljs-base."
  [inputs modules]
  (let [index    (index-inputs inputs)
        _        (validate-modules modules index)
        deps     #(deps-for-entry % index)
        assign1  (fn [[entry maybe-assigned]]
                   [entry
                    (if (= 1 (count maybe-assigned))
                      (first maybe-assigned)
                      (deepest-common-parent maybe-assigned modules))])
        canon    (fn [xs] (into #{} (map #(canonical-name % index)) xs))
        assigns  (fn [f]
                   (reduce-kv
                     (fn [ret module-name {:keys [entries] :as module}]
                       (let [entries' (canon entries)]
                         (reduce
                           (fn [ret entry]
                             (update ret entry (fnil conj #{}) module-name))
                           ret (canon (f entries')))))
                     {} modules))
        e->ms    (assigns identity)
        d->ms    (assigns #(distinct (mapcat deps %)))
        assigned (merge
                   (into {} (map assign1) d->ms)
                   (into {} (map assign1) e->ms))
        orphans  (zipmap
                   (map (comp str munge first :provides)
                     (-> (reduce-kv (fn [m k _] (dissoc m k)) index assigned)
                       vals set))
                   (repeat :cljs-base))]
    (merge assigned orphans)))

(defn expand-modules
  "Given compiler :modules and a dependency sorted list of compiler inputs return
   a complete :modules map where all depended upon inputs are assigned."
  [modules inputs]
  (let [order    (first
                   (reduce
                     (fn [[ret n] {:keys [provides]}]
                       [(merge ret
                          (zipmap (map (comp str munge) provides) (repeat n)))
                        (inc n)])
                     [{} 0] inputs))
        modules' (-> modules normalize add-cljs-base add-cljs-base-dep)
        assigns  (inputs->assigned-modules inputs
                   (annotate-depths modules'))
        um       (reduce-kv
                   (fn [ret entry module-name]
                     (update-in ret [module-name :entries]
                       (fnil conj #{}) entry))
                   modules' assigns)]
    (reduce-kv
      (fn [ret module-name {:keys [entries]}]
        (update-in ret [module-name :entries] #(vec (sort-by order %))))
      um um)))

(comment
  (inputs->assigned-modules inputs
    (-> modules add-cljs-base add-cljs-base-dep annotate-depths))

  (pprint
    (expand-modules modules inputs))
  )

(defn topo-sort
  "Topologically sort a graph using the given edges-key."
  [graph edges-key]
  (letfn [(no-incoming-edges [graph edges-key]
            (->> graph
              (filter
                (fn [[k v]]
                  (every? #(not (contains? graph %)) (edges-key v))))
              (map first)))]
    (when-not (empty? graph)
      (let [nodes  (no-incoming-edges graph edges-key)
            graph' (reduce #(dissoc %1 %2) graph nodes)]
        (concat nodes (topo-sort graph' edges-key))))))

(defn sort-modules [modules-with-base]
  (into [] (map (fn [module] [module (module modules-with-base)]))
    (topo-sort modules-with-base :depends-on)))

(comment
  (def ms
    (sort-modules
      (->
        {:cljs-base
         {:output-to "out/module/base.js"}
         :core
         {:output-to "out/modules/core.js"
          :entries '#{cljs.core}}
         :landing
         {:output-to "out/modules/reader.js"
          :entries '#{cljs.reader}
          :depends-on #{:core}}}
        add-cljs-base add-cljs-base-dep)))
  )

(defn modules->module-uris
  "Given a :modules map, a dependency sorted list of compiler inputs, and
   compiler options return a Closure module uris map. This map will include
   all inputs by leveraging expand-modules."
  [modules inputs {:keys [optimizations asset-path output-dir] :as opts}]
  (assert optimizations "Must supply :optimizations in opts map")
  (assert (#{:advanced :simple :none} optimizations) "Must supply valid :optimizations in opts map")
  (assert output-dir "Must supply :output-dir in opts map")
  (letfn [(get-uri [rel-path]
            (cond->> rel-path
              asset-path (str asset-path)))
          (get-rel-path* [output-dir file]
            (string/replace (.. (io/file file) getAbsoluteFile getPath) output-dir ""))]
    (let [get-rel-path (partial get-rel-path*
                         (.. (io/file output-dir)
                           getAbsoluteFile getPath))]
      (case optimizations
        :none
        (into {}
          (map
            (fn [[module-name {:keys [entries] :as module}]]
              [module-name
               (into []
                 (comp
                   (mapcat #(find-sources-for-module-entry % inputs))
                   (map
                     (comp get-uri get-rel-path
                       (fn [{:keys [out-file] :as ijs}]
                         (if-not out-file
                           (throw (Exception. (str "No :out-file for IJavaScript " (pr-str ijs))))
                           out-file))))
                   (distinct))
                 entries)]))
          (expand-modules modules inputs))
        (:advanced :simple)
        (reduce-kv
          (fn [ret k {:keys [output-to]}]
            (assoc ret k [(-> output-to get-rel-path get-uri)]))
          {:cljs-base [(-> (or (get-in modules [:cljs-base :output-to])
                               (io/file output-dir "cljs_base.js"))
                         get-rel-path get-uri)]}
          modules)))))

(defn modules->module-infos
  "Given a :modules map return a Closure module info map which maps modules
   to depended upon modules."
  [modules]
  (let [modules (-> modules add-cljs-base add-cljs-base-dep)]
    (reduce-kv
      (fn [ret module-name {:keys [depends-on] :or {depends-on []} :as module}]
        (assoc ret module-name depends-on))
      {} modules)))
