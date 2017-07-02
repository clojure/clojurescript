(ns cljs.loader
  (:require [goog.object :as gobj])
  (:import [goog.module ModuleLoader]
           [goog.module ModuleManager]))

(def module-infos MODULE_INFOS) ;; set by compiler
(def module-uris MODULE_URIS) ;; set by compiler

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
(.setModuleUris *module-manager* (to-js module-uris))

(defn load
  "Load a module. module-name should be a keyword matching a :modules module
   definition."
  ([module-name]
    (load module-name nil))
  ([module-name cb]
   (let [mname (-> module-name name munge)]
     (if-not (nil? cb)
       (.execOnLoad *module-manager* mname cb)
       (.load *module-manager* mname)))))

(defn set-loaded!
  "Set a module as being loaded. module-name should be a keyword matching a
  :modules module definition. Will mark all parent modules as also being
  loaded."
  [module-name]
  (let [xs (deps-for module-name module-infos)]
    (doseq [x xs]
      (.setLoaded *module-manager* (munge-kw x)))
    (.setLoaded *module-manager* (munge-kw module-name))))