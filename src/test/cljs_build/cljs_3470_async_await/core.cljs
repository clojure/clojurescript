(ns cljs-3470-async-await.core)

(defn ^:async my-async-fn []
  (let [;; no awaits counted in binding, moreover, they can be optimized in advanced mode
        b1a (let [x 2]
              (+ x
                 (let [x 1] x)))
        ;; two awaits counted in binding, one literally and one inserted to await IIFE
        b1b (let [x 2]
              (+ x
                 (let [x 1] (await x))))
        b2a (case :foo :foo (case :foo :foo 1))
        b2b (case :foo :foo (case :foo :foo (await (js/Promise.resolve 1))))
        b3a (int ;; wrapped in int to avoid false positive warning:
            ;; all arguments must be numbers, got [number
            ;; ignore] instead
            (try (throw (throw 1)) (catch :default _ 1 )))
        b3b (int ;; wrapped in int to avoid false positive warning:
             ;; all arguments must be numbers, got [number
             ;; ignore] instead
             (try (throw (throw (await (js/Promise.resolve 1)))) (catch :default _ 1 )))
        a (atom 0)
        b4a (do (swap! a inc) (swap! a inc)
               ;; do with single expr, wrapped in identity to avoid merging with upper do
               (identity (do (swap! a inc)))
               ;; do with multiple exprs, wrapped identity to avoid merging with upper do
               (identity (do (swap! a inc) (swap! a inc)))
               @a)
        ;; one implicit await for the whole do
        b4b (do (swap! a inc) (swap! a inc)
                ;; 2 awaits: one implicit, one explicit
                (identity (do (swap! a (await (js/Promise.resolve inc)))))
                ;; 2 awaits: one implicit, one explicit
                (identity (do (swap! a inc) (swap! a (await (js/Promise.resolve inc)))))
                @a)
        b5a (try (identity (try 1 (finally nil)))
                (finally nil))
        b5b (try (identity (try 1 (finally (await nil))))
                 (finally nil))
        b6a (letfn [(f [x] x)]
             (f (letfn [(f [x] x)]
                  (f 1))))
        b6b (letfn [(f [x] x)]
              (f (letfn [(f [x] x)]
                   (f (await 1)))))]
    (+ b1a b1b b2a b2b b3a b3b b4a b4b b5a b5b b6a b6b)))
