(ns cljs.async-await-test
  (:refer-global :only [Date Promise])
  (:require [clojure.test :refer [deftest is] :as t]
            [cljs.core :as cc :refer [await] :rename {await aw}]
            [goog.object :as gobj])
  (:require-macros [cljs.macro-test.macros :refer [await!] :as macros]))

(defn ^:async foo [n]
  (let [x (await (js/Promise.resolve 10))
        y (let [y (await (js/Promise.resolve 20))]
            (inc y))
        ;; not async
        f (fn [] 20)]
    (+ n x y (f))))

(deftest ^:async defn-test
  (let [v (await (foo 10))]
    (is (= 61 v)))
  (let [v (await (apply foo [10]))]
    (is (= 61 v))))

(defn ^:async variadic-foo [n & ns]
  (let [x (await (js/Promise.resolve n))
        y (let [y (await (js/Promise.resolve (apply + ns)))]
            (inc y))
        ;; not async
        f (fn [] 20)]
    (+ n x y (f))))

(deftest ^:async variadic-defn-test
  (let [v (await (variadic-foo 10))]
    (is (= 41 v)))
  (let [v (await (variadic-foo 10 1 2 3))]
    (is (= 47 v)))
  (let [v (await (apply variadic-foo [10 1 2 3]))]
    (is (= 47 v))))

(defn ^:async multi-arity-foo
  ([n] (await n))
  ([n x] (+ (await n) x)))

(deftest ^:async multi-arity-defn-test
  (let [v (await (multi-arity-foo 10))]
    (is (= 10 v)))
  (let [v (await (multi-arity-foo 10 20))]
    (is (= 30 v)))
  (let [v (await (apply multi-arity-foo [10]))]
    (is (= 10 v)))
  (let [v (await (apply multi-arity-foo [10 20]))]
    (is (= 30 v))))

(defn ^:async multi-arity-variadic-foo
  ([n] (await n))
  ([n & xs] (apply + (await n) xs)))

(deftest ^:async multi-arity-variadic-test
  (let [v (await (multi-arity-variadic-foo 10))]
    (is (= 10 v)))
  (let [v (await (multi-arity-variadic-foo 10 20))]
    (is (= 30 v)))
  (let [v (await (apply multi-arity-variadic-foo [10]))]
    (is (= 10 v)))
  (let [v (await (apply multi-arity-variadic-foo [10 20]))]
    (is (= 30 v))))

(deftest ^:async fn-test
  (let [f (^:async fn [x] (+ x (await (js/Promise.resolve 20))))
        v (await (f 10))
        v2 (await (apply f [10]))]
    (is (= 30 v v2))))

(deftest ^:async varargs-fn-test
  (let [f (^:async fn [x & xs] (apply + x (await (js/Promise.resolve 20)) xs))
        v (await (f 10))
        v2 (await (apply f [10]))
        v3 (await (f 5 5))
        v4 (await (apply f [5 5]))]
    (is (= 30 v v2 v3 v4))))

(deftest ^:async variadic-fn-test
  (let [f (^:async fn
            ([x] (await (js/Promise.resolve x)))
            ([x y] (cons (await (js/Promise.resolve x)) [y])))]
    (is (= [1 1 [1 2] [1 2]]
           [(await (f 1))
            (await (apply f [1]))
            (await (f 1 2))
            (await (apply f [1 2]))]))))

(deftest ^:async variadic-varargs-fn-test
  (let [f (^:async fn
            ([x] (await (js/Promise.resolve x)))
            ([x & xs] (cons (await (js/Promise.resolve x)) xs)))]
    (is (= [1 1 [1 2 3] [1 2 3]]
           [(await (f 1))
            (await (apply f [1]))
            (await (f 1 2 3))
            (await (apply f [1 2 3]))]))))

(deftest ^:async await-in-throw-test
  (let [f (^:async fn [x] (inc (if (odd? x) (throw (await (js/Promise.resolve "dude"))) x)))]
    (let [x (await (f 2))]
      (is (= 3 x)))
    (let [x (try (await (f 1))
                 (catch :default e e))]
      (is (= "dude" x)))))

(deftest ^:async await-in-do-test
  (let [a (atom 0)
        f (^:async fn [] (let [_ (do (swap! a inc)
                                     (swap! a + (await (js/Promise.resolve 2))))]
                           @a))
        v (await (f))]
    (is (= 3 v))))

(deftest ^:async await-let-fn-test
  (let [f (^:async fn [] (let [v
                               ;; force letfn in expr position
                               (letfn [(^:async f [] (inc (await (js/Promise.resolve 10))))]
                                 (inc (await (f))))]
                           (identity v)))
        v (await (f))]
    (is (= 12 v))))

(deftest ^:async await-in-loop-test
  (let [f (^:async fn [] (let [x
                               ;; force loop in expr position
                               (loop [xs (map #(js/Promise.resolve %) [1 2 3])
                                      ys []]
                                 (if (seq xs)
                                   (let [x (first xs)
                                         v (await x)]
                                     (recur (rest xs) (conj ys v)))
                                   ys))]
                           (identity x)))
        v (await (f))]
    (is (= [1 2 3] v))))

(deftest ^:async await-in-nested
  (let [f (^:async fn []
            (let [b1 1
                  b2 (let [x 2]
                       (+ x
                          ;; outer let doesn't have awaits
                          ;; but inner let does, so outer let should become async
                          (let [x (await (js/Promise.resolve 1))] x)))
                  b3 (case :foo :foo (case :foo :foo (await (js/Promise.resolve 1))))
                  b4 (int ;; wrapped in int to avoid false positive warning:
                          ;; all arguments must be numbers, got [number
                          ;; ignore] instead
                      (try (throw (throw (await (js/Promise.resolve 1)))) (catch :default _ 1 )))
                  a (atom 0)
                  b5 (do (swap! a inc) (swap! a inc)
                         ;; do with single expr, wrapped in identity to avoid merging with upper do
                         (identity (do (swap! a (await (js/Promise.resolve inc)))))
                         ;; do with multiple exprs, wrapped identity to avoid merging with upper do
                         (identity (do (swap! a inc) (swap! a (await (js/Promise.resolve inc)))))
                         @a)
                  b6 (try (identity (try 1 (finally (await nil))))
                          (finally nil))
                  b7 (letfn [(f [x] x)]
                       (f (letfn [(f [x] x)]
                            (f (await 1)))))]
              (await (+ b1 b2 b3 b4 b5 b6 b7))))]
    (is (= 13 (await (f))))))

(deftest ^:async await-with-aliases-or-renamed-and-via-macros-test
  (let [a (await! (js/Promise.resolve 1))
        b (macros/await! (js/Promise.resolve 1))
        c (cc/await (js/Promise.resolve 1))
        d (aw (js/Promise.resolve 1))
        e (cljs.core/await (js/Promise.resolve 1))
        f (clojure.core/await (js/Promise.resolve 1))]
    (is (= 1 a))
    (is (= 1 b))
    (is (= 1 c))
    (is (= 1 d))
    (is (= 1 e))
    (is (= 1 f))))

(deftest ^:async await-with-ctor
  (let [f (^:async fn [] (Date. (await (Promise/resolve 0))))]
    (is (= (Date. 0) (await (f))))))

(deftest ^:async await-with-literals
  (let [objf (^:async fn [] #js {:foo (await (Promise/resolve "bar"))})]
    (is (gobj/equals #js {:foo "bar"} (await (objf)))))
  (let [arrayf (^:async fn [] #js [0 (await (Promise/resolve 1 )) 2])]
    (is (= [0 1 2] (seq (await (arrayf))))))
  (let [listf (^:async fn [] (list 0 1 2))]
    (is (= '(0 1 2) (await (listf)))))
  (let [vectorf (^:async fn [] [0 (await (Promise/resolve 1 )) 2])]
    (is (= [0 1 2] (await (vectorf)))))
  (let [mapf (^:async fn [] {:foo (await (Promise/resolve :bar))})]
    (is (= {:foo :bar} (await (mapf)))))
  (let [setf (^:async fn [] #{:foo (await (Promise/resolve :bar)) :baz})]
    (is (= #{:foo :bar :baz} (await (setf))))))

(deftest ^:async await-with-if-test
  (let [f (^:async fn [] (if (await (Promise/resolve true)) :success :fail))]
    (is (= :success (await (f))))))

(defn ^:async async-destructure
  [{:keys [foo bar]
    :or   {bar (await (Promise/resolve "hello!"))}}]
  [foo bar])

(deftest ^:async await-in-async-destructure
  (let [res (await (async-destructure {:foo 1}))]
    (is (= [1 "hello!"] res))))

(def throw-in-async-error-test (volatile! false))

(deftest ^:async simulated-async-error-test
  (when @throw-in-async-error-test
    (throw (js/Error. "simulated")))
  (is (not @throw-in-async-error-test)))

(def async-error-reports (atom []))

(defmethod t/report [::error-capture :error] [m]
  (swap! async-error-reports conj m))

(deftest ^:async async-test-error-reporting-test
  (reset! async-error-reports [])
  (let [outer-env (t/get-current-env)
        reports (try
                  (t/set-env! (t/empty-env ::error-capture))
                  (vreset! throw-in-async-error-test true)
                  (await (js/Promise.
                          (fn [resolve _]
                            (t/run-block
                             (concat (t/test-var-block (var simulated-async-error-test))
                                     [(fn [] (resolve @async-error-reports))])))))
                  (finally
                    (vreset! throw-in-async-error-test false)
                    (t/set-env! outer-env)))]
    (is (= 1 (count reports)))
    (let [{:keys [message actual]} (first reports)]
      (is (= "Uncaught exception, not in assertion." message))
      (is (= "simulated" (ex-message actual))))))

(comment
  (clojure.test/run-tests)
  )
