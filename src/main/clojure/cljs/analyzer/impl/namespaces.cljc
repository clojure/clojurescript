;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.impl.namespaces)

(defn check-and-remove-as-alias
  "Given a libspec return a map of :as-alias alias, if was present. Return the
   libspec with :as-alias elided. If the libspec was *only* :as-alias do not
   return it."
  [libspec]
  ;; ignore simple requires (symbols) and
  ;; REPL stuff (keywords, i.e. :reload)
  (if (or (symbol? libspec)
          (keyword? libspec))
    {:libspec libspec}
    (let [[lib & spec :as libspec] libspec
          [pre-spec [_ alias & post-spec :as post]] (split-with (complement #{:as-alias}) spec)]
      (if (seq post)
        (let [libspec' (into [lib] (concat pre-spec post-spec))]
          (assert (symbol? alias)
            (str ":as-alias must be followed by a symbol, got: " alias))
          (cond-> {:as-alias {alias lib}}
            (> (count libspec') 1) (assoc :libspec libspec')))
        {:libspec libspec}))))

(defn check-as-alias-duplicates
  [as-aliases new-as-aliases]
  (doseq [[alias _] new-as-aliases]
    (assert (not (contains? as-aliases alias))
      (str "Duplicate :as-alias " alias ", already in use for lib "
        (get as-aliases alias)))))

(defn elide-aliases-from-libspecs
  "Given libspecs, elide all :as-alias. Return a map of :libspecs (filtered)
   and :as-aliases."
  ([libspecs]
   (elide-aliases-from-libspecs libspecs {}))
  ([libspecs as-aliases]
   (let [ret {:as-aliases as-aliases
              :libspecs   []}]
     (reduce
       (fn [ret libspec]
         (let [{:keys [as-alias libspec]} (check-and-remove-as-alias libspec)]
           (check-as-alias-duplicates (:as-aliases ret) as-alias)
           (cond-> ret
             libspec  (update :libspecs conj libspec)
             as-alias (update :as-aliases merge as-alias))))
       ret libspecs))))

(defn elide-aliases-from-ns-specs [ns-specs]
  "Given ns specs, elide all :as-alias. Return a map of :libspecs (filtered)
   and :as-aliases."
  (let [ret {:as-aliases {}
             :libspecs   []}]
    (reduce
      (fn [{:keys [as-aliases] :as ret} [spec-key & libspecs]]
        (if-not (= :refer-clojure spec-key)
          (let [{:keys [as-aliases libspecs]} (elide-aliases-from-libspecs libspecs as-aliases)]
            (cond-> ret
              (not (empty? as-aliases)) (update :as-aliases merge as-aliases)
              (not (empty? libspecs))   (update :libspecs conj (list* spec-key libspecs))))
          (update ret :libspecs conj (list* spec-key libspecs))))
      ret ns-specs)))
