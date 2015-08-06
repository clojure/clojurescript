(ns self-host.test
  (:require [cljs.test :as test
             :refer-macros [run-tests deftest testing is async]]
            [cljs.js :as cljs]))

(defn latch [m f]
  (let [r (atom 0)]
    (add-watch r :latch
      (fn [_ _ o n]
        (when (== n m) (f))))
    r))

(defn inc! [r]
  (swap! r inc))

(def st (cljs/empty-state))

(deftest test-compile-str
  (async done
    (let [l (latch 3 done)]
      (cljs/compile-str st "(+ 1 1)"
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= value "(1 + 1);\n"))
          (inc! l)))
      (cljs/compile-str st "(fn [])" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= value "(function (){\nreturn null;\n})"))
          (inc! l)))
      (cljs/compile-str st "(if cljs.core.first 1 2)" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= value "(cljs.core.truth_(cljs.core.first)?1:2)"))
          (inc! l))))))