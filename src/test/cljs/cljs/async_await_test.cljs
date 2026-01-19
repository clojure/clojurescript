(ns cljs.async-await-test
  (:require [clojure.test :refer [deftest is async]]))

(defn ^:async foo [n]
  (let [x (await (js/Promise.resolve 10))
        y (let [y (await (js/Promise.resolve 20))]
            (inc y))
        ;; not async
        f (fn [] 20)]
    (+ n x y (f))))

(deftest defn-test
  (async done
    (try
      (let [v (await (foo 10))]
        (is (= 61 v)))
      (let [v (await (apply foo [10]))]
        (is (= 61 v)))
      (catch :default e (prn :should-not-reach-here e))
      (finally (done)))))

(defn ^:async variadic-foo [n & ns]
  (let [x (await (js/Promise.resolve n))
        y (let [y (await (js/Promise.resolve (apply + ns)))]
            (inc y))
        ;; not async
        f (fn [] 20)]
    (+ n x y (f))))

(deftest variadic-defn-test
  (async done
    (try
      (let [v (await (variadic-foo 10))]
        (is (= 41 v)))
      (let [v (await (variadic-foo 10 1 2 3))]
        (is (= 47 v)))
      (let [v (await (apply variadic-foo [10 1 2 3]))]
        (is (= 47 v)))
      (catch :default e (prn :should-not-reach-here e))
      (finally (done)))))

(defn ^:async multi-arity-foo
  ([n] (await n))
  ([n x] (+ (await n) x)))

(deftest multi-arity-defn-test
  (async done
    (try
      (let [v (await (multi-arity-foo 10))]
        (is (= 10 v)))
      (let [v (await (multi-arity-foo 10 20))]
        (is (= 30 v)))
      (let [v (await (apply multi-arity-foo [10]))]
        (is (= 10 v)))
      (let [v (await (apply multi-arity-foo [10 20]))]
        (is (= 30 v)))
      (catch :default e (prn :should-not-reach-here e))
      (finally (done)))))

(defn ^:async multi-arity-variadic-foo
  ([n] (await n))
  ([n & xs] (apply + (await n) xs)))

(deftest multi-arity-variadic-test
  (async done
    (try
      (let [v (await (multi-arity-variadic-foo 10))]
        (is (= 10 v)))
      (let [v (await (multi-arity-variadic-foo 10 20))]
        (is (= 30 v)))
      (let [v (await (apply multi-arity-variadic-foo [10]))]
        (is (= 10 v)))
      (let [v (await (apply multi-arity-variadic-foo [10 20]))]
        (is (= 30 v)))
      (catch :default e (prn :should-not-reach-here e))
      (finally (done)))))

(deftest fn-test
  (async done
    (try
      (let [f (^:async fn [x] (+ x (await (js/Promise.resolve 20))))
            v (await (f 10))
            v2 (await (apply f [10]))]
        (is (= 30 v v2)))
      (catch :default e (prn :should-not-reach-here e))
      (finally (done)))))

(deftest varargs-fn-test
  (async done
    (try
      (let [f (^:async fn [x & xs] (apply + x (await (js/Promise.resolve 20)) xs))
            v (await (f 10))
            v2 (await (apply f [10]))
            v3 (await (f 5 5))
            v4 (await (apply f [5 5]))]
        (is (= 30 v v2 v3 v4)))
      (catch :default e (prn :should-not-reach-here e))
      (finally (done)))))

(deftest variadic-fn-test
  (async done
    (try (let [f (^:async fn
                  ([x] (await (js/Promise.resolve x)))
                  ([x y] (cons (await (js/Promise.resolve x)) [y])))]
           (is (= [1 1 [1 2] [1 2]]
                  [(await (f 1))
                   (await (apply f [1]))
                   (await (f 1 2))
                   (await (apply f [1 2]))])))
         (catch :default e (prn :should-not-reach-here e))
         (finally (done)))))

(deftest variadic-varargs-fn-test
  (async done
    (try (let [f (^:async fn
                  ([x] (await (js/Promise.resolve x)))
                  ([x & xs] (cons (await (js/Promise.resolve x)) xs)))]
           (is (= [1 1 [1 2 3] [1 2 3]]
                  [(await (f 1))
                   (await (apply f [1]))
                   (await (f 1 2 3))
                   (await (apply f [1 2 3]))])))
         (catch :default e (prn :should-not-reach-here e))
         (finally (done)))))

(deftest await-in-throw-test
  (async done
    (let [f (^:async fn [x] (inc (if (odd? x) (throw (await (js/Promise.resolve "dude"))) x)))]
      (try
        (let [x (await (f 2))]
          (is (= 3 x)))
        (let [x (try (await (f 1))
                     (catch :default e e))]
          (is (= "dude" x)))
        (catch :default e (prn :should-not-reach-here e))
        (finally (done))))))

(deftest await-in-do-test
  (async done
    (try
      (let [a (atom 0)
            f (^:async fn [] (let [_ (do (swap! a inc)
                                         (swap! a + (await (js/Promise.resolve 2))))]
                               @a))
            v (await (f))]
        (is (= 3 v)))
      (catch :default e (prn :should-not-reach-here e))
      (finally (done)))))

(deftest await-let-fn-test
  (async done
    (try
      (let [f (^:async fn [] (let [v
                                   ;; force letfn in expr position
                                   (letfn [(^:async f [] (inc (await (js/Promise.resolve 10))))]
                                     (inc (await (f))))]
                               (identity v)))
            v (await (f))]
        (is (= 12 v)))
      (catch :default e (prn :should-not-reach-here e))
      (finally (done)))))

(deftest await-in-loop-test
  (async done
    (try
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
        (is (= [1 2 3] v)))
      (catch :default e (prn :should-not-reach-here e))
      (finally (done)))))
