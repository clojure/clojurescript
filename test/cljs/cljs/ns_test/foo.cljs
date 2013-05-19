(ns cljs.ns-test.foo)

(defn baz [] 123)

(def kw ::foo)
(def qkw '::foo)

(assert (= (str kw) ":cljs.ns-test.foo/foo"))
(assert (= (str qkw) ":cljs.ns-test.foo/foo"))
