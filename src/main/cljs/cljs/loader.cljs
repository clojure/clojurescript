;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software

(ns cljs.loader
  (:require [goog.object :as gobj])
  (:import [goog.module ModuleLoader]
           [goog.module ModuleManager]))

(def module-infos MODULE_INFOS) ;; set by compiler
(def module-uris
  (if (exists? js/COMPILED_MODULE_URIS)
    js/COMPILED_MODULE_URIS
    MODULE_URIS)) ;; set by compiler

(defn deps-for [x graph]
  (let [depends-on (get graph x)]
    (-> (mapcat #(deps-for % graph) depends-on)
      (concat depends-on) distinct vec)))

(defn munge-kw [x]
  (cond-> x
    (keyword? x) (-> name munge)))

(defn to-js [m]
  (reduce-kv
    (fn [ret k xs]
      (let [arr (into-array (map munge-kw xs))]
        (doto ret (gobj/set (-> k name munge) arr))))
    #js {} m))

(defn create-module-manager []
  (let [mm (ModuleManager.)
        ml (ModuleLoader.)]
    (.setLoader mm ml)
    mm))

(defonce ^:dynamic *module-manager* (create-module-manager))

(.setAllModuleInfo *module-manager* (to-js module-infos))
(.setModuleUris *module-manager*
  (cond-> module-uris (map? module-uris) to-js))

(defn loaded?
  "Return true if modules is loaded. module-name should be a keyword matching
   a :modules module definition."
  [module-name]
  (assert (contains? module-infos module-name)
    (str "Module " module-name " does not exist"))
  (let [mname (-> module-name name munge)
        module (.getModuleInfo *module-manager* mname)]
    (when (some? module)
      (.isLoaded module))))

(defn load
  "Load a module. module-name should be a keyword matching a :modules module
   definition."
  ([module-name]
    (load module-name nil))
  ([module-name cb]
   (assert (contains? module-infos module-name)
     (str "Module " module-name " does not exist"))
   (let [mname (-> module-name name munge)]
     (if-not (nil? cb)
       (.execOnLoad *module-manager* mname cb)
       (.load *module-manager* mname)))))

(defn set-loaded!
  "Set a module as being loaded. module-name should be a keyword matching a
  :modules module definition. Will mark all parent modules as also being
  loaded."
  [module-name]
  (assert (contains? module-infos module-name)
    (str "Module " module-name " does not exist"))
  (let [xs (deps-for module-name module-infos)]
    (doseq [x xs]
      (.setLoaded *module-manager* (munge-kw x)))
    (.setLoaded *module-manager* (munge-kw module-name))))

(defn prefetch
  "Prefetch a module. module-name should be a keyword matching a :modules
  module definition. Will download the module but not evaluate it. To
  complete module load, one must also call cljs.loader/load after prefetching
  the module. Does nothing if the module is loading or has been loaded."
  [module-name]
  (assert (contains? module-infos module-name)
          (str "Module " module-name " does not exist"))
  (when-not (loaded? module-name)
    (let [mname (-> module-name name munge)]
      (when-not (.isModuleLoading *module-manager* mname)
        (.prefetchModule *module-manager* mname)))))
