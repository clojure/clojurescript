(ns foo.ns-shadow-test
  (:require baz))

(defn bar [] 1)

(defn quux [foo]
  (+ (foo.ns-shadow-test/bar) foo))

(defn id [x] x)

(defn foo [] (id 42))

(defn baz
  ([] (baz 2))
  ([x] (quux 2)))

(defn test-shadow []
  (assert (= (quux 2) 3))
  (assert (= (foo) 42))
  (assert (= (baz) 3)))
