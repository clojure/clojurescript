(ns foo.ns-shadow-test)

(defn bar [] 1)

(defn quux [foo]
  (+ (foo.ns-shadow-test/bar) foo))

(defn test-shadow []
  (assert (= (quux 2) 3)))