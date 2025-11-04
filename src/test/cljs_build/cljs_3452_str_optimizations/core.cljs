(ns cljs-3452-str-optimizations.core)

(defn my-str-fn [x y]
  (str x y nil ::foobar "my

multiline

string with `backticks`"
       true false 3.14))
