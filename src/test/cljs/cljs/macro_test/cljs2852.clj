(ns cljs.macro-test.cljs2852)

(defmacro alpha
  ([x]))

(defmacro beta []
  `'~(:arglists (meta #'alpha)))

(defmacro gamma
  ([x])
  ([x y]))

(defmacro delta []
  `'~(:arglists (meta #'gamma)))

(defmacro epsilon
  ([x])
  ([x & xs]))

(defmacro zeta []
  `'~(:arglists (meta #'epsilon)))
