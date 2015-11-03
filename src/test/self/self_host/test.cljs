(ns self-host.test
  (:require [cljs.test :as test
             :refer-macros [run-tests deftest testing is async]]
            [cljs.js :as cljs]
            [clojure.string :as string]
            [cljs.nodejs :as nodejs]))

(set! (.-user js/cljs) #js {})

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
  (if-not js/COMPILED
    (.runInThisContext vm source (str (munge name) ".js"))
    (js/eval source)))

(def libs
  {'bootstrap-test.core :cljs
   'bootstrap-test.macros :clj
   'bootstrap-test.helper :clj})

(defn node-load [{:keys [name macros]} cb]
  (if (contains? libs name)
    (let [path (str "src/test/self/" (cljs/ns->relpath name)
                    "." (cljs.core/name (get libs name)))]
      (.readFile fs path "utf-8"
        (fn [err src]
          (cb (if-not err
                {:lang :clj :source src}
                (.error js/console err))))))
    (cb nil)))

(defn elide-env [env ast opts]
  (dissoc ast :env))

;; NOTE: can't set passes because callbacks happen _inside_ binding
;; do so will effect other tests

(deftest test-analyze-str
  (async done
    (let [l (latch 2 done)]
      (cljs/analyze-str st "(+ 1 1)" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= :js (:op value)))
          (inc! l)))
      (cljs/analyze-str st "(defprotocol IFoo)" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (inc! l))))))

(deftest test-compile-str
  (async done
    (let [l (latch 6 done)]
      (cljs/compile-str st "(+ 1 1)"
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= "(1 + 1);\n" value))
          (inc! l)))
      (cljs/compile-str st "(fn [])" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= "(function (){\nreturn null;\n})" value))
          (inc! l)))
      (cljs/compile-str st "(if cljs.core.first 1 2)" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= "(cljs.core.truth_(cljs.core.first)?1:2)" value))
          (inc! l)))
      (cljs/compile-str st "(.toString \"a\")" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (= "\"a\".toString()" value))
          (inc! l)))
      (cljs/compile-str st "(do (defn foo [a b] (+ a b)) (foo 1 2))" nil
        {:context :expr}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (string/index-of value "cljs.user.foo.call(null,1,2)"))
          (inc! l)))
      (cljs/compile-str st "(do (defn foo [a b] (+ a b)) (foo 1 2))" nil
        {:context :expr
         :static-fns true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (string/index-of value "cljs.user.foo(1,2)"))
          (inc! l))))))

(deftest test-eval-str
  (async done
    (let [l (latch 8 done)]
      (cljs/eval-str st "(+ 1 1)" nil
        {:eval node-eval}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== 2 value))
          (inc! l)))
      (cljs/eval-str st "(def x 1)" nil
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
          (inc! l)))
      (cljs/eval-str st "((fn [a b] (+ a b)) 1 2)" nil
        {:eval node-eval
         :context :expr
         :def-emits-var true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== 3 value))
          (inc! l)))
      (cljs/eval-str st "(ns foo.bar)" nil
          {:eval node-eval
           :context :expr
           :def-emits-var true}
          (fn [{:keys [error value]}]
            (is (nil? error))
            (is (not (nil? js/foo.bar)))
            (inc! l)))
      (cljs/eval-str st "(defn foo [a b] (+ a b))" nil
        {:eval node-eval
         :context :expr
         :def-emits-var true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== 3 (js/cljs.user.foo 1 2)))
          (inc! l)))
      (cljs/eval-str st "(do (defn foo [a b] (+ a b)) (foo 1 2))" nil
        {:eval node-eval
         :context :expr
         :def-emits-var true
         :static-fns true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== 3 value))
          (inc! l)))
      (cljs/eval-str st "(def foo (let [x 1] (let [x (inc x)] x)))" nil
        {:eval node-eval
         :context :statement
         :def-emits-var true}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (inc! l)))
      (cljs/eval-str st "(with-out-str (doseq [x [1 2]] (println x)))" nil
        {:eval node-eval
         :context :expr}
        (fn [{:keys [error value]}]
          (is (= "1\n2\n" value))
          (inc! l))))))

(deftest test-eval-str-with-require-macros
  (async done
    (let [l (latch 2 done)]
      (cljs/eval-str st
        "(ns cljs.user (:require-macros [cljs.user.macros]))"
        nil
        {:eval node-eval
         :load (fn [_ cb] (cb {:lang :clj :source "(ns cljs.user.macros)"}))}
        (fn [{:keys [value error]}]
          (is (nil? error))
          (inc! l)))
      (cljs/eval-str st
        "(ns cljs.user (:require-macros [cljs.user.macros :as cljs-user-macros]))"
        nil
        {:eval node-eval
         :load (fn [_ cb] (cb {:lang :clj :source "(ns cljs.user.macros)"}))}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (inc! l))))))

#_(deftest test-eval-str-with-require
  (async done
    (let [l (latch 3 done)]
      (cljs/eval-str st
        "(ns foo.bar (:require [bootstrap-test.core]))\n(bootstrap-test.core/foo 3 4)"
        nil
        {:eval node-eval
         :load node-load}
        (fn [{:keys [value error]}]
          (is (nil? error))
          (is (== 7 value))
          (inc! l)))
      (cljs/eval-str st
        "(ns foo.bar (:require-macros [bootstrap-test.macros :refer [foo]]))\n(foo 4 4)"
        nil
        {:eval node-eval
         :load node-load}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== 16 value))
          (inc! l)))
      (cljs/eval-str st
        "(ns foo.bar)\n(first [1 2 3])"
        nil
        {:eval node-eval
         :load node-load}
        (fn [{:keys [error value]}]
          (is (nil? error))
          (is (== 1 value))
          (inc! l))))))

(defn -main [& args]
  (run-tests))

(set! *main-cli-fn* -main)

(comment
  )
