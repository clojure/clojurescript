(ns cljs.top-level)

(let [foo 1]
  (defn bar []
    foo))

(let [foo 2]
  (defn baz []
    foo))

(defn test []
  (assert (= (bar) 1))
  (assert (= (baz) 2)))