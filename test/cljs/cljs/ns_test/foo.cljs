(ns cljs.ns-test.foo)

(defn baz [] 123)

(def kw ::foo)

(assert (= (str kw) ":cljs.ns-test.foo/foo"))
