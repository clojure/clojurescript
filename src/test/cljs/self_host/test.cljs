(ns self-host.test
  (:require [cljs.test :as test
             :refer-macros [run-tests deftest testing is async]]
            [cljs.js :as cljs]
            [cljs.nodejs :as nodejs]))

(nodejs/enable-util-print!)

(defn latch [m f]
  (let [r (atom 0)]
    (add-watch r :latch
      (fn [_ _ o n]
        (when (== n m) (f))))
    r))

(defn inc! [r]
  (swap! r inc))

(def vm (js/require "vm"))
(def fs (js/require "fs"))
(def st (cljs/empty-state))

(defn node-eval [{:keys [name source]}]
  (.runInThisContext vm source (str (munge name) ".js")))

(def libs
  {'bootstrap-test.core :cljs
   'bootstrap-test.macros :clj
   'bootstrap-test.helper :clj})

(defn node-load [{:keys [name macros]} cb]
  (if (contains? libs name)
    (let [path (str "src/test/cljs/" (cljs/ns->relpath name)
                 "." (cljs.core/name (get libs name)))]
      (.readFile fs path "utf-8"
        (fn [err src]
          (cb (if-not err
                {:lang :clj :source src}
                (.error js/console err))))))
    (cb nil)))

(defn elide-env [env ast opts]
  (dissoc ast :env))

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

(deftest test-eval-str
  (async done
    (let [l (latch 3 done)]
      (cljs/eval-str st "(+ 1 1)" nil
        {:eval node-eval}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== value 2))
          (inc! l)))
      #_(cljs/eval-str st "(def x 1)" nil
        {:eval node-eval
         :context :expr
         :def-emits-var true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (var? value))
          (inc! l)))
      (cljs/eval-str st "(fn [])" nil
        {:eval node-eval
         :context :expr
         :def-emits-var true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (fn? value))
          (inc! l))))))

(defn -main [& args]
  (run-tests))

(set! *main-cli-fn* -main)
