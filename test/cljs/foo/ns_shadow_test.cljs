(ns foo.ns-shadow-test)

(defn bar [] 1)

(defn quux [foo]
  (+ (foo.ns-shadow-test/bar) foo))

(defn id [x] x)

(defn foo [] (id 42))

(defn test-shadow []
  (assert (= (quux 2) 3))
  (assert (= (foo) 42)))
