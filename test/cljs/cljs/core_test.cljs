(ns cljs.core-test
  (:require [clojure.string :as s]))

(defn test-stuff []
  ;; js primitives
  (let [keys #(vec (js-keys %))]
    (assert (= [] (keys (js-obj)) (keys (apply js-obj []))))
    (assert (= ["x"] (keys (js-obj "x" "y")) (keys (apply js-obj ["x" "y"])))))

  ;; -equiv
  (assert (= 1))
  (assert (= 1 1))
  (assert (= 1 1 1))
  (assert (= 1 1 1 1))
  (assert (not (= 1 2)))
  (assert (not (= 1 2 1)))
  (assert (not (= 1 1 2)))
  (assert (not (= 1 1 2 1)))
  (assert (not (= 1 1 1 2)))

  ;; arithmetic
  (assert (= (+) 0))
  (assert (= (apply + []) 0))
  (assert (= (+ 1) 1))
  (assert (= (apply + [1]) 1))
  (assert (= (+ 1 1) 2))
  (assert (= (apply + [1 1]) 2))
  (assert (= (+ 1 2 3) 6))
  (assert (= (apply + [1 2 3]) 6))

  (assert (= (- 1) -1))
  (assert (= (apply - [1]) -1))
  (assert (= (- 1 1) 0))
  (assert (= (apply - [1 1]) 0))
  (assert (= (- 3 2 1) 0))
  (assert (= (apply - [3 2 1]) 0))

  (assert (= (*) 1))
  (assert (= (apply * []) 1))
  (assert (= (* 2) 2))
  (assert (= (apply * [2]) 2))
  (assert (= (* 2 3) 6))
  (assert (= (apply * [2 3]) 6))

  (assert (= (/ 2) 0.5))
  (assert (= (apply / [2]) 0.5))
  (assert (= (/ 6 2) 3))
  (assert (= (apply / [6 2]) 3))
  (assert (= (/ 6 3 2) 1))
  (assert (= (apply / [6 3 2]) 1))

  (assert (= (< 1) true))
  (assert (= (apply < [1]) true))
  (assert (= (< 1 2) true))
  (assert (= (apply < [1 2]) true))
  (assert (= (< 1 1) false))
  (assert (= (apply < [1 1]) false))
  (assert (= (< 2 1) false))
  (assert (= (apply < [2 1]) false))
  (assert (= (< 1 2 3) true))
  (assert (= (apply < [1 2 3]) true))
  (assert (= (< 1 1 3) false))
  (assert (= (apply < [1 1 3]) false))
  (assert (= (< 3 1 1) false))
  (assert (= (apply < [3 1 1]) false))

  (assert (= (<= 1) true))
  (assert (= (apply <= [1]) true))
  (assert (= (<= 1 1) true))
  (assert (= (apply <= [1 1]) true))
  (assert (= (<= 1 2) true))
  (assert (= (apply <= [1 2]) true))
  (assert (= (<= 2 1) false))
  (assert (= (apply <= [2 1]) false))
  (assert (= (<= 1 2 3) true))
  (assert (= (apply <= [1 2 3]) true))
  (assert (= (<= 1 1 3) true))
  (assert (= (apply <= [1 1 3]) true))
  (assert (= (<= 3 1 1) false))
  (assert (= (apply <= [3 1 1]) false))

  (assert (= (> 1) true))
  (assert (= (apply > [1]) true))
  (assert (= (> 2 1) true))
  (assert (= (apply > [2 1]) true))
  (assert (= (> 1 1) false))
  (assert (= (apply > [1 1]) false))
  (assert (= (> 1 2) false))
  (assert (= (apply > [1 2]) false))
  (assert (= (> 3 2 1) true))
  (assert (= (apply > [3 2 1]) true))
  (assert (= (> 3 1 1) false))
  (assert (= (apply > [3 1 1]) false))
  (assert (= (> 1 1 3) false))
  (assert (= (apply > [1 1 3]) false))

  (assert (= (>= 1) true))
  (assert (= (apply >= [1]) true))
  (assert (= (>= 2 1) true))
  (assert (= (apply >= [2 1]) true))
  (assert (= (>= 1 1) true))
  (assert (= (apply >= [1 1]) true))
  (assert (= (>= 1 2) false))
  (assert (= (apply >= [1 2]) false))
  (assert (= (>= 3 2 1) true))
  (assert (= (apply >= [3 2 1]) true))
  (assert (= (>= 3 1 1) true))
  (assert (= (apply >= [3 1 1]) true))
  (assert (= (>= 3 1 2) false))
  (assert (= (apply >= [3 1 2]) false))
  (assert (= (>= 1 1 3) false))
  (assert (= (apply >= [1 1 3]) false))

  (assert (= (dec 1) 0))
  (assert (= (apply dec [1]) 0))
  (assert (= (inc 0) 1))
  (assert (= (apply inc [0]) 1))

  (assert (= (zero? 0) true))
  (assert (= (apply zero? [0]) true))
  (assert (= (zero? 1) false))
  (assert (= (apply zero? [1]) false))
  (assert (= (zero? -11) false))
  (assert (= (apply zero? [-11]) false))
  (assert (= (pos? 0) false))
  (assert (= (apply pos? [0]) false))
  (assert (= (pos? 1) true))
  (assert (= (apply pos? [1]) true))
  (assert (= (pos? -1) false))
  (assert (= (apply pos? [-1]) false))
  (assert (= (neg? -1) true))
  (assert (= (apply neg? [-1]) true))

  (assert (= (max 1) 1))
  (assert (= (apply max [1]) 1))
  (assert (= (max 1 2) 2))
  (assert (= (apply max [1 2]) 2))
  (assert (= (max 2 1) 2))
  (assert (= (apply max [2 1]) 2))
  (assert (= (max 1 2 3) 3))
  (assert (= (apply max [1 2 3]) 3))
  (assert (= (max 1 3 2) 3))
  (assert (= (apply max [1 3 2]) 3))

  (assert (= (min 1) 1))
  (assert (= (apply min [1]) 1))
  (assert (= (min 1 2) 1))
  (assert (= (apply min [1 2]) 1))
  (assert (= (min 2 1) 1))
  (assert (= (apply min [2 1]) 1))
  (assert (= (min 1 2 3) 1))
  (assert (= (apply min [1 2 3]) 1))
  (assert (= (min 2 1 3) 1))
  (assert (= (apply min [3 1 3]) 1))

  (assert (= (mod 4 2) 0))
  (assert (= (apply mod [4 2]) 0))
  (assert (= (mod 3 2) 1))
  (assert (= (apply mod [3 2]) 1))
  (assert (= (mod -2 5) 3))

  (assert (= [4 3 2 1 0] (loop [i 0 j ()]
                 (if (< i 5)
                   (recur (inc i) (conj j (fn [] i)))
                   (map #(%) j)))))

  (assert (= [[1 1] [1 2] [1 3] [2 1] [2 2] [2 3]]
             (map #(%) (for [i [1 2] j [1 2 3]] (fn [] [i j])))))

  (assert (integer? 0))
  (assert (integer? 42))
  (assert (integer? -42))
  (assert (not (integer? "")))
  (assert (not (integer? 1e308)))
  (assert (not (integer? js/Infinity)))
  (assert (not (integer? (- js/Infinity))))
  (assert (not (integer? js/NaN)))

  (assert (= 42 (int 42.5)))
  (assert (integer? (int 42.5)))

  (assert (= 42 (long 42.5)))
  (assert (integer? (long 42.5)))

  (assert (= -1 (int -1.5)))
  (assert (= -9 (long -9.8)))

  (assert (= 2 (:b {:a 1 :b 2})))
  (assert (= 2 ('b '{:a 1 b 2})))
  (assert (= 2 ({:a 1 :b 2} :b)))
  (assert (= 2 ({1 1 2 2} 2)))
  (assert (= 2 (:a {:b 1} 2)))
  (assert (= 2 (:a {} 2)))
  (assert (= 2 ({:b 1} :a 2)))
  (assert (= 2 ({} :a 2)))
  (assert (= nil (:a {})))
  (assert (= nil (:a "")))
  (assert (= 2 (:a "" 2)))
  (assert (= 2 (#{1 2 3} 2)))
  (assert (zero? (hash (aget (js-obj) "foo"))))

  (assert (= 1 (apply :a '[{:a 1 a 2}])))
  (assert (= 1 (apply 'a '[{a 1 :b 2}])))
  (assert (= 1 (apply {:a 1} [:a])))
  (assert (= 2 (apply {:a 1} [:b 2])))

  ; See
  ; https://github.com/clojure/tools.reader#differences-from-lispreaderjava
  ; about why these tests won't pass. Not clear if we should change the reader
  ; or the test
  ; (assert (= "baz" (name 'foo/bar/baz)))
  ; (assert (= "foo/bar" (namespace 'foo/bar/baz)))
  ; (assert (= "baz" (name :foo/bar/baz)))
  ;(assert (= "foo/bar" (namespace :foo/bar/baz)))
  (assert (nil? (namespace '/)))
  (assert (= "/" (name '/)))
  (assert (= "keyword" (name :keyword)))
  ;;TODO: These next two tests need Clojure 1.5
  ;(assert (= "foo" (namespace 'foo//)))
  ;(assert (= "/" (name 'foo//)))

  ; str
  (assert (= ":hello" (str :hello)))
  (assert (= "hello" (str 'hello)))
  (assert (= "hello:world" (str "hello" :world)))
  (assert (= ":helloworld" (str :hello 'world)))

  ; symbol
  (assert (= 'a (symbol 'a)))

  ; keyword
  (assert (= :a (keyword "a")))
  (assert (= :a (keyword 'a)))
  (assert (= :a/b (keyword 'a 'b)))
  (assert (= :a (keyword :a)))

  (assert (= {:a :b} (get {[1 2 3] {:a :b}, 4 5} [1 2 3])))
  (assert (= :a (nth [:a :b :c :d] 0)))
  (assert (= :a (nth [:a :b :c :d] 0.1)) )
  (assert (not (= {:a :b :c nil} {:a :b :d nil})))
  (assert (= (list 3 2 1) [3 2 1]))
  (assert (= [3 2 1] (seq (array 3 2 1))))
  (assert (= 9 (reduce + (next (seq (array 1 2 3 4))))))
  (assert (= () (rest nil)))
  (assert (= nil (seq (array))))
  (assert (= nil (seq "")))
  (assert (= nil (seq [])))
  (assert (= nil (seq {})))
  (assert (= () (rest ())))
  (assert (= () (rest [1])))
  (assert (= () (rest (array 1))))
  (assert (= {"x" "y"} (meta ^{"x" "y"} [])))
  (assert (= {:a :b} (dissoc {:a :b :c :d} :c)))
  (assert (= (hash-map :foo 5)
             (assoc (cljs.core.ObjMap. nil (array) (js-obj)) :foo 5)))

  (assert (= "\"asdf\" \"asdf\"" (pr-str "asdf" "asdf")))
  (assert (= "[1 true {:a 2, :b #\"x\\\"y\"} #js [3 4]]"
             (pr-str [1 true {:a 2 :b #"x\"y"} (array 3 4)])))

  (assert (= "\"asdf\"\n" (prn-str "asdf")))
  (assert (= "[1 true {:a 2, :b 42} #js [3 4]]\n"
             (prn-str [1 true {:a 2 :b 42} (array 3 4)])))

  (assert (= "asdf" (print-str "asdf")))
  (assert (= "asdf\n" (println-str "asdf")))

  (assert (= "" (pr-str)))
  (assert (= "\n" (prn-str)))
  (assert  (= "12" (with-out-str (print 1) (print 2))))
  (assert  (= "12" (with-out-str (*print-fn* 1) (*print-fn* 2))))

  ;;this fails in v8 - why?
  ;(assert (= "symbol\"'string" (pr-str (str 'symbol \" \' "string"))))

  (assert (not (= "one" "two")))
  (assert (= 3 (count "abc")))
  (assert (= 4 (count (array 1 2 3 4))))
  (assert (= "c" (nth "abc" 2)))
  (assert (= "quux" (nth "abc" 3 "quux")))
  (assert (= 1 (nth (array 1 2 3 4) 0)))
  (assert (= "val" (nth (array 1 2 3 4) 4 "val")))
  (assert (= "b" (get "abc" 1)))
  (assert (= "harriet" (get "abcd" 4 "harriet")))
  (assert (= 4 (get (array 1 2 3 4) 3)))
  (assert (= "zot" (get (array 1 2 3 4) 4 "zot")))
  (assert (= 10 (reduce + (array 1 2 3 4))))
  (assert (= 20 (reduce + 10 (array 1 2 3 4))))
  (assert (= "cabd" (let [jumble (fn [a b] (str (apply str (reverse (str a))) b))]
                      (reduce jumble "abcd"))))
  (assert (= "cafrogbd" (let [jumble (fn [a b] (str (apply str (reverse (str a))) b))]
                          (reduce jumble "frog" "abcd"))))
  (assert (= [0 0 1 0 1]
               [(bit-and 1 0)
                (bit-and 0 0)
                (bit-and 1 1)
                (bit-and 42 1)
                (bit-and 41 1)]))
  (assert (= [1 0 1 43 41]
               [(bit-or 1 0)
                (bit-or 0 0)
                (bit-or 1 1)
                (bit-or 42 1)
                (bit-or 41 1)]))
  (assert (= [1 0 0 42 40]
               [(bit-and-not 1 0)
                (bit-and-not 0 0)
                (bit-and-not 1 1)
                (bit-and-not 42 1)
                (bit-and-not 41 1)]))
  (assert (= [0 2 968 16649 0]
               [(bit-clear 1 0)
                (bit-clear 2 0)
                (bit-clear 1000 5)
                (bit-clear 16713 6)
                (bit-clear 1024 10)]))
  (assert (= [0 0 992 18761 0]
               [(bit-flip 1 0)
                (bit-flip 2 1)
                (bit-flip 1000 3)
                (bit-flip 16713 11)
                (bit-flip 1024 10)]))
  (assert (= [-2 -3 999 -16714 -1025]
               [(bit-not 1)
                (bit-not 2)
                (bit-not -1000)
                (bit-not 16713)
                (bit-not 1024)]))
  (assert (= [1 2 1000 18761 1024]
               [(bit-set 1 0)
                (bit-set 2 1)
                (bit-set 1000 3)
                (bit-set 16713 11)
                (bit-set 1024 10)]))
  (assert (= [true true true false true]
               [(bit-test 1 0)
                (bit-test 2 1)
                (bit-test 1000 3)
                (bit-test 16713 11)
                (bit-test 1024 10)]))
  (assert (= [true false true false false false]
             [(true? true)
              (true? false)
              (false? false)
              (false? true)
              (true? js/undefined)
              (false? js/undefined)]))
  ;; apply
  (assert (= 0 (apply + nil)))
  (assert (= 0 (apply + (list))))
  (assert (= 1 (apply + (list 1))))
  (assert (= 3 (apply + 1 (list 2))))
  (assert (= 7 (apply + 1 2 (list 4))))
  (assert (= 15 (apply + 1 2 4 (list 8))))
  (assert (= 31 (apply + 1 2 4 8 (list 16))))
  (assert (= 63 (apply + 1 2 4 8 16 (list 32))))
  (assert (= 127 (apply + 1 2 4 8 16 (list 32 64))))
  (assert (= 4950 (apply + (take 100 (iterate inc 0)))))
  (assert (= () (apply list [])))
  (assert (= [1 2 3] (apply list [1 2 3])))
  (assert (= 6 (apply apply [+ [1 2 3]])))
  ;; apply with infinite sequence
  (assert (= 3 (apply (fn [& args]
                        (+ (nth args 0)
                           (nth args 1)
                           (nth args 2)))
                      (iterate inc 0))))
  (assert (= [0 1 2 3 4] (take 5 (apply (fn [& m] m) (iterate inc 0)))))
  (assert (= [1 2 3 4 5] (take 5 (apply (fn [x & m] m) (iterate inc 0)))))
  (assert (= [2 3 4 5 6] (take 5 (apply (fn [x y & m] m) (iterate inc 0)))))
  (assert (= [3 4 5 6 7] (take 5 (apply (fn [x y z & m] m) (iterate inc 0)))))
  (assert (= [4 5 6 7 8] (take 5 (apply (fn [x y z a & m] m) (iterate inc 0)))))
  (assert (= [5 6 7 8 9] (take 5 (apply (fn [x y z a b & m] m) (iterate inc 0)))))
  ;; apply arity tests
  (let [single-arity-non-variadic (fn [x y z] [z y x])
        multiple-arity-non-variadic (fn ([x] x) ([x y] [y x]) ([x y z] [z y x]))
        single-arity-variadic-fixedargs (fn [x y & more] [more y x])
        single-arity-variadic-nofixedargs (fn [& more] more)
        multiple-arity-variadic (fn ([x] x) ([x y] [y x]) ([x y & more] [more y x]))]
    (assert (= [3 2 1] (apply single-arity-non-variadic [1 2 3])))
    (assert (= [3 2 1] (apply single-arity-non-variadic 1 [2 3])))
    (assert (= [3 2 1] (apply single-arity-non-variadic 1 2 [3])))
    (assert (= 42 (apply multiple-arity-non-variadic [42])))
    (assert (= [2 1] (apply multiple-arity-non-variadic [1 2])))
    (assert (= [2 1] (apply multiple-arity-non-variadic 1 [2])))
    (assert (= [3 2 1] (apply multiple-arity-non-variadic [1 2 3])))
    (assert (= [3 2 1] (apply multiple-arity-non-variadic 1 [2 3])))
    (assert (= [3 2 1] (apply multiple-arity-non-variadic 1 2 [3])))
    (assert (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs [1 2 3 4 5])))
    (assert (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs 1 [2 3 4 5])))
    (assert (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs 1 2 [3 4 5])))
    (assert (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs 1 2 3 [4 5])))
    (assert (= [[3 4 5] 2 1] (apply single-arity-variadic-fixedargs 1 2 3 4 [5])))
    (assert (= [3 4 5] (take 3 (first (apply single-arity-variadic-fixedargs (iterate inc 1))))))
    (assert (= [2 1] (rest (apply single-arity-variadic-fixedargs (iterate inc 1)))))
    (assert (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs [1 2 3 4 5])))
    (assert (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs 1 [2 3 4 5])))
    (assert (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs 1 2 [3 4 5])))
    (assert (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs 1 2 3 [4 5])))
    (assert (= [1 2 3 4 5] (apply single-arity-variadic-nofixedargs 1 2 3 4 [5])))
    (assert (= [1 2 3 4 5] (take 5 (apply single-arity-variadic-nofixedargs (iterate inc 1)))))
    (assert (= 42 (apply multiple-arity-variadic [42])))
    (assert (= [2 1] (apply multiple-arity-variadic [1 2])))
    (assert (= [2 1] (apply multiple-arity-variadic 1 [2])))
    (assert (= [[3 4 5] 2 1] (apply multiple-arity-variadic [1 2 3 4 5])))
    (assert (= [[3 4 5] 2 1] (apply multiple-arity-variadic 1 [2 3 4 5])))
    (assert (= [[3 4 5] 2 1] (apply multiple-arity-variadic 1 2 [3 4 5])))
    (assert (= [[3 4 5] 2 1] (apply multiple-arity-variadic 1 2 3 [4 5])))
    (assert (= [[3 4 5] 2 1] (apply multiple-arity-variadic 1 2 3 4 [5])))
    (assert (= [3 4 5] (take 3 (first (apply multiple-arity-variadic (iterate inc 1))))))
    (assert (= [2 1] (rest (apply multiple-arity-variadic (iterate inc 1))))))

  ;; CLJS-383
  (let [f1 (fn f1 ([] 0) ([a] 1) ([a b] 2) ([a b c & more] 3))
        f2 (fn f2 ([x] :foo) ([x y & more] (apply f1 y more)))]
    (assert (= 1 (f2 1 2))))
  (let [f (fn ([]) ([a & more] more))]
    (assert (nil? (f :foo))))
  (assert (nil? (array-seq (array 1) 1)))

  ;; Functions with metadata
  (let [f (fn [x] (* x 2))
        m {:foo "bar"}
        mf (with-meta f m)]
    (assert (nil? (meta f)))
    (assert (fn? mf))
    (assert (= 4 (mf 2)))
    (assert (= 4 (apply mf [2])))
    (assert (= (meta mf) m)))

  (let [a (atom 0)]
    (assert (= 0 (deref a)))
    (assert (= 1 (swap! a inc)))
    (assert (= false (compare-and-set! a 0 42)))
    (assert (= true (compare-and-set! a 1 7)))
    (assert (nil? (meta a)))
    (assert (nil? (get-validator a))))
  (let [a (atom 0)]
    (assert (= 1 (swap! a + 1)))
    (assert (= 4 (swap! a + 1 2)))
    (assert (= 10 (swap! a + 1 2 3)))
    (assert (= 20 (swap! a + 1 2 3 4))))
  (let [a (atom [1] :validator coll? :meta {:a 1})]
    (assert (= coll? (get-validator a)))
    (assert (= {:a 1} (meta a)))
    (alter-meta! a assoc :b 2)
    (assert (= {:a 1 :b 2} (meta a))))
  (assert (nil? (empty nil)))
  (let [e-lazy-seq (empty (with-meta (lazy-seq (cons :a nil)) {:b :c}))]
    (assert (seq? e-lazy-seq))
    (assert (empty? e-lazy-seq))
    (assert (= {:b :c} (meta e-lazy-seq))))
  (let [e-list (empty '^{:b :c} (1 2 3))]
    (assert (seq? e-list))
    (assert (empty? e-list)))
  (let [e-elist (empty '^{:b :c} ())]
    (assert (seq? e-elist))
    (assert (empty? e-elist))
    (assert (= :c (get (meta e-elist) :b))))
  (let [e-cons (empty (with-meta (cons :a nil) {:b :c}))]
    (assert (seq? e-cons))
    (assert (empty? e-cons))
    (assert (= {:b :c} (meta e-cons))))
  (let [e-vec (empty ^{:b :c} [:a :d :g])]
    (assert (vector? e-vec))
    (assert (empty? e-vec))
    (assert (= {:b :c} (meta e-vec))))
  (let [e-omap (empty ^{:b :c} {:a :d :g :h})]
    (assert (map? e-omap))
    (assert (empty? e-omap))
    (assert (= {:b :c} (meta e-omap))))
  (let [e-hmap (empty ^{:b :c} {[1 2] :d :g :h})]
    (assert (map? e-hmap))
    (assert (empty? e-hmap))
    (assert (= {:b :c} (meta e-hmap))))

  (let [a (atom nil)]
    (assert (= 1 (try 1)))
    (assert (= 2 (try 1 (throw (js/Error.)) (catch js/Error e 2))))
    (assert (= 2 (try 1 (throw (js/Error.)) (catch js/Error e 1 2))))
    (assert (= 2 (try 1 (throw (js/Error.)) (catch js/Error e 2) (catch :default e 3))))
    (assert (= 3 (try 1 (throw true) (catch js/Error e 2) (catch :default e 3))))
    (assert (= 2 (try 1 (throw 2) (catch js/Error e 3) (catch :default e e))))
    (assert (= 1 (try 1 (finally (reset! a 42)))))
    (assert (= 42 (deref a))))

  (assert (= [3] (nthnext [1 2 3] 2)))
  (let [v [1 2 3]]
    (assert (= v (for [e v] e)))
    (assert (= [[1 1] [2 4] [3 9]] (for [e v :let [m (* e e)]] [e m])))
    (assert (= [1 2] (for [e v :while (< e 3)] e)))
    (assert (= [3] (for [e v :when (> e 2)] e)))
    (assert (= [[1 1] [2 4]] (for [e v :while (< e 3) :let [m (* e e)]] [e m]))))
  (assert (not= 1 2))
  (assert (not (not= 1 1)))
  (assert (not (not-empty [])))
  (assert (boolean (not-empty [1 2 3])))
  (assert (= "joel" (min-key count "joel" "tom servo" "crooooooooow")))
  (assert (= "crooooooooow" (max-key count "joel" "tom servo" "crooooooooow")))
  (assert (= (partition-all 4 [1 2 3 4 5 6 7 8 9])
             [[1 2 3 4] [5 6 7 8] [9]]))
  (assert (= (partition-all 4 2 [1 2 3 4 5 6 7 8 9])
             [[1 2 3 4] [3 4 5 6] [5 6 7 8] [7 8 9] [9]]))
  (assert (= [true true] (take-while true? [true true 2 3 4])))
  (assert (= [[true true] [false false false] [true true]]
             (partition-by true? [true true false false false true true])))
  (assert (= [0 2 4 6 8 10] (take-nth 2 [0 1 2 3 4 5 6 7 8 9 10])))
  (let [a10 (partial + 10)
        a20 (partial + 10 10)
        a21 (partial + 10 10 1)
        a22 (partial + 10 5  4 3)
        a23 (partial + 10 5  4 3 1)]
    (assert (= 110 (a10 100)))
    (assert (= 120 (a20 100)))
    (assert (= 121 (a21 100)))
    (assert (= 122 (a22 100)))
    (assert (= 123 (a23 100))))
  (let [n2 (comp first rest)
        n3 (comp first rest rest)
        n4 (comp first rest rest rest)
        n5 (comp first rest rest rest rest)
        n6 (comp first rest rest rest rest rest)]
    (assert (= 2 (n2 [1 2 3 4 5 6 7])))
    (assert (= 3 (n3 [1 2 3 4 5 6 7])))
    (assert (= 4 (n4 [1 2 3 4 5 6 7])))
    (assert (= 5 (n5 [1 2 3 4 5 6 7])))
    (assert (= 6 (n6 [1 2 3 4 5 6 7]))))
  (let [sf (some-fn number? keyword? symbol?)]
    (assert (sf :foo 1))
    (assert (sf :foo))
    (assert (sf 'bar 1))
    (assert (not (sf [] ()))))
  (let [ep (every-pred number? zero?)]
    (assert (ep 0 0 0))
    (assert (not (ep 1 2 3 0))))
  (assert ((complement number?) :foo))
  (assert (= [1 [2 3] [1 2 3]] ((juxt first rest seq) [1 2 3])))
  (assert (= 5 (max 1 2 3 4 5)))
  (assert (= 5 (max 5 4 3 2 1)))
  (assert (= 5.5 (max 1 2 3 4 5 5.5)))
  (assert (= 1 (min 5 4 3 2 1)))
  (assert (= 1 (min 1 2 3 4 5)))
  (assert (= 0.5 (min 5 4 3 0.5 2 1)))
  (let [x (array 1 2 3)]
    (set! (.-foo x) :hello)
    (assert (= (.-foo x) :hello)))

  (assert (set []))
  (assert (= #{} (set [])))
  (assert (= #{} (hash-set)))
  (assert (identical? cljs.core.PersistentHashSet (type (hash-set))))

  (assert (= #{"foo"} (set ["foo"])))
  (assert (= #{"foo"} (hash-set "foo")))
  (assert (= #{1 2 3} #{1 3 2}))
  (assert (= #{#{1 2 3} [4 5 6] {7 8} 9 10}
             #{10 9 [4 5 6] {7 8} #{1 2 3}}))
  (assert (not (= #{nil [] {} 0 #{}} #{})))
  (assert (= (count #{nil [] {} 0 #{}}) 5))
  (assert (= (conj #{1} 1) #{1}))
  (assert (= (conj #{1} 2) #{2 1}))
  (assert (= #{} (-empty #{1 2 3 4})))
  (assert (= (reduce + #{1 2 3 4 5}) 15))
  (assert (= 4 (get #{1 2 3 4} 4)))
  (assert (contains? #{1 2 3 4} 4))
  (assert (contains? #{[] nil 0 {} #{}} {}))
  (assert (contains? #{[1 2 3]} [1 2 3]))
  (assert (not (contains? (-disjoin #{1 2 3} 3) 3)))
  (assert (neg? -1))
  (assert (not (neg? 1)))
  (assert (neg? -1.765))
  (assert (not (neg? 0)))
  (assert (= [true false true false true false true false]
             (map integer?
                  [1 1.00001 0x7e7 [] (- 88 1001991881) :foo 0 "0"])))
  (assert (= [true false true false true false]
             (map odd? [1 2 3 4 -1 0])))
  (assert (= [true false true false true true]
             (map even? [2 3 4 5 -2 0])))
  (assert (contains? {:a 1 :b 2} :a))
  (assert (not (contains? {:a 1 :b 2} :z)))
  (assert (contains? [5 6 7] 1))
  (assert (contains? [5 6 7] 2))
  (assert (not (contains? [5 6 7] 3)))
  (assert (contains? (to-array [5 6 7]) 1))
  (assert (contains? (to-array [5 6 7]) 2))
  (assert (not (contains? (to-array [5 6 7]) 3)))
  (assert (not (contains? nil 42)))
  (assert (contains? "f" 0))
  (assert (not (contains? "f" 55)))
  (assert (distinct? 1 2 3))
  (assert (not (distinct? 1 2 3 1)))

  ;; distinct
  (assert (= (distinct ()) ()))
  (assert (= (distinct '(1)) '(1)))
  (assert (= (distinct '(1 2 3 1 1 1)) '(1 2 3)))
  (assert (= (distinct [1 1 1 2]) '(1 2)))
  (assert (= (distinct [1 2 1 2]) '(1 2)))
  (assert (= (distinct "a") ["a"]))
  (assert (= (distinct "abcabab") ["a" "b" "c"]))
  (assert (= (distinct ["abc" "abc"]) ["abc"]))
  (assert (= (distinct [nil nil]) [nil]))
  (assert (= (distinct [0.0 0.0]) [0.0]))
  (assert (= (distinct ['sym 'sym]) '[sym]))
  (assert (= (distinct [:kw :kw]) [:kw]))
  (assert (= (distinct [42 42]) [42]))
  (assert (= (distinct [[] []]) [[]]))
  (assert (= (distinct ['(1 2) '(1 2)]) '[(1 2)]))
  (assert (= (distinct [() ()]) [()]))
  (assert (= (distinct [[1 2] [1 2]]) [[1 2]]))
  (assert (= (distinct [{:a 1 :b 2} {:a 1 :b 2}]) [{:a 1 :b 2}]))
  (assert (= (distinct [{} {}]) [{}]))
  (assert (= (distinct [#{1 2} #{1 2}]) [#{1 2}]))
  (assert (= (distinct [#{} #{}]) [#{}]))

  ;;regexps
  (assert (= (str (re-pattern "f(.)o")) (str (js* "/f(.)o/"))))
  (assert (= (re-find (re-pattern "foo") "foo bar foo baz foo zot") "foo"))
  (assert (= (re-find (re-pattern "f(.)o") "foo bar foo baz foo zot") ["foo" "o"]))
  (assert (= (re-matches (re-pattern "foo") "foo") "foo"))
  (assert (= (re-matches (re-pattern "foo") "foo bar foo baz foo zot") nil))
  (assert (= (re-matches (re-pattern "foo.*") "foo bar foo baz foo zot") "foo bar foo baz foo zot"))
  (assert (= (re-seq (re-pattern "foo") "foo bar foo baz foo zot") (list "foo" "foo" "foo")))
  (assert (= (re-seq (re-pattern "f(.)o") "foo bar foo baz foo zot") (list ["foo" "o"] ["foo" "o"] ["foo" "o"])))
  (assert (= (re-matches (re-pattern "(?i)foo") "Foo") "Foo"))
  ; new RegExp("").source => "(?:)" on webkit-family envs, "" elsewhere
  (assert (#{"#\"\"" "#\"(?:)\""} (pr-str #"")))

  ;; destructuring
  (assert (= [2 1] (let [[a b] [1 2]] [b a])))
  (assert (= #{1 2} (let [[a b] [1 2]] #{a b})))
  (assert (= [1 2] (let [{a :a b :b} {:a 1 :b 2}] [a b])))
  (assert (= [1 2] (let [{:keys [a b]} {:a 1 :b 2}] [a b])))
  (assert (= [1 2 [1 2]] (let [[a b :as v] [1 2]] [a b v])))
  (assert (= [1 42] (let [{:keys [a b] :or {b 42}} {:a 1}] [a b])))
  (assert (= [1 nil] (let [{:keys [a b] :or {c 42}} {:a 1}] [a b])))
  (assert (= [2 1] (let [[a b] '(1 2)] [b a])))
  (assert (= {1 2} (let [[a b] [1 2]] {a b})))
  (assert (= [2 1] (let [[a b] (seq [1 2])] [b a])))

  ;; update-in
  (assert (= {:foo {:bar {:baz 1}}}
             (update-in {:foo {:bar {:baz 0}}} [:foo :bar :baz] inc)))
  (assert (= {:foo 1 :bar 2 :baz 10}
             (update-in {:foo 1 :bar 2 :baz 3} [:baz] + 7)))
  (assert (= [{:foo 1, :bar 2} {:foo 1, :bar 3}]
               (update-in [{:foo 1 :bar 2}, {:foo 1 :bar 2}] [1 :bar] inc)))
  (assert (= [{:foo {:bar 2}} {:foo {:bar 3}}]
               (update-in [{:foo {:bar 2}}, {:foo {:bar 2}}] [1 :foo :bar] inc)))

  ;; assoc-in
  (assert (= {:foo {:bar {:baz 100}}}
             (assoc-in {:foo {:bar {:baz 0}}} [:foo :bar :baz] 100)))
  (assert (= {:foo 1 :bar 2 :baz 100}
             (assoc-in {:foo 1 :bar 2 :baz 3} [:baz] 100)))
  (assert (= [{:foo [{:bar 2} {:baz 3}]} {:foo [{:bar 2} {:baz 100}]}]
             (assoc-in [{:foo [{:bar 2} {:baz 3}]}, {:foo [{:bar 2} {:baz 3}]}]
                       [1 :foo 1 :baz] 100)))
  (assert (= [{:foo 1, :bar 2} {:foo 1, :bar 100}]
             (assoc-in [{:foo 1 :bar 2}, {:foo 1 :bar 2}] [1 :bar] 100)))

  ;; get-in
  (assert (= 1 (get-in {:foo 1 :bar 2} [:foo])))
  (assert (= 2 (get-in {:foo {:bar 2}} [:foo :bar])))
  (assert (= 1 (get-in [{:foo 1}, {:foo 2}] [0 :foo])))
  (assert (= 4 (get-in [{:foo 1 :bar [{:baz 1}, {:buzz 2}]}, {:foo 3 :bar [{:baz 3}, {:buzz 4}]}]
                       [1 :bar 1 :buzz])))

  ;; arrays
  (let [a (to-array [1 2 3])]
    (assert (= [10 20 30] (seq (amap a i ret (* 10 (aget a i))))))
    (assert (= 6 (areduce a i ret 0 (+ ret (aget a i)))))
    (assert (= (seq a) (seq (to-array [1 2 3]))))
    (assert (= 42 (aset a 0 42)))
    (assert (not= (seq a) (seq (to-array [1 2 3]))))
    (assert (not= a (aclone a))))

  (let [a (array (array 1 2 3) (array 4 5 6))]
    (assert (= (aget a 0 1) 2))
    (assert (= (apply aget a [0 1]) 2))
    (assert (= (aget a 1 1) 5))
    (assert (= (apply aget a [1 1]) 5))
    (aset a 0 0 "foo")
    (assert (= (aget a 0 0) "foo"))
    (apply aset a [0 0 "bar"])
    (assert (= (aget a 0 0) "bar")))

  ;; sort
  (assert (= [1 2 3 4 5] (sort [5 3 1 4 2])))
  (assert (= [1 2 3 4 5] (sort < [5 3 1 4 2])))
  (assert (= [5 4 3 2 1] (sort > [5 3 1 4 2])))

  ;; sort-by
  (assert (= ["a" [ 1 2] "foo"] (sort-by count ["foo" "a" [1 2]])))
  (assert (= ["foo" [1 2] "a"] (sort-by count > ["foo" "a" [1 2]])))

  ;; shuffle
  (let [coll [1 2 3 4 5 6 7 8 9 10]
        ; while it is technically possible for this test to fail with a false negative,
        ; it's _extraordinarily_ unlikely.
        shuffles (filter #(not= coll %) (take 100 (iterate shuffle coll)))]
    (assert (not (empty? shuffles))))

  ;; js->clj
  (assert (= {"a" 1, "b" 2} (js->clj (js* "{\"a\":1,\"b\":2}"))))
  (assert (= {"a" nil} (js->clj (js* "{\"a\":null}"))))
  (assert (= {} (js->clj (js* "{}"))))
  (assert (= {"a" true, "b" false} (js->clj (js* "{\"a\":true,\"b\":false}"))))
  (assert (= {:a 1, :b 2} (js->clj (js* "{\"a\":1,\"b\":2}") :keywordize-keys true)))
  (assert (= [[{:a 1, :b 2} {:a 1, :b 2}]]
               (js->clj (js* "[[{\"a\":1,\"b\":2}, {\"a\":1,\"b\":2}]]") :keywordize-keys true)))
  (assert (= [[{:a 1, :b 2} {:a 1, :b 2}]]
               (js->clj [[{:a 1, :b 2} {:a 1, :b 2}]])))
  (assert (= (js->clj nil) nil))

  ;; clj->js
  (assert (= (clj->js 'a) "a"))
  (assert (= (clj->js :a) "a"))
  (assert (= (clj->js "a") "a"))
  (assert (= (clj->js 1) 1))
  (assert (= (clj->js nil) (js* "null")))
  (assert (= (clj->js true) (js* "true")))
  (assert (goog/isArray (clj->js [])))
  (assert (goog/isArray (clj->js #{})))
  (assert (goog/isArray (clj->js '())))
  (assert (goog/isObject (clj->js {})))
  (assert (= (aget (clj->js {:a 1}) "a") 1))
  (assert (= (-> (clj->js {:a {:b {{:k :ey} :d}}})
                 (aget "a")
                 (aget "b")
                 (aget "{:k :ey}"))
             "d"))

  ;; last
  (assert (= nil (last nil)))
  (assert (= 3 (last [1 2 3])))

  ;; dotimes
  (let [s (atom [])]
    (dotimes [n 5]
      (swap! s conj n))
    (assert (= [0 1 2 3 4] @s)))

  ;; doseq
  (let [v [1 2 3 4 5]
        s (atom ())]
    (doseq [n v] (swap! s conj n))
    (assert (= @s (reverse v))))

  ;; delay
  (let [a (atom 0)
        d (delay (swap! a inc))]
    (assert (false? (realized? d)))
    (assert (zero? @a)) ;; delay hasn't triggered yet
    (assert (= 1 @d)) ;; trigger it
    (assert (= 1 @a)) ;; make sure side effect has happened
    (assert (true? (realized? d)))
    (assert (= 1 @d)) ;; body doesn't happen again
    (assert (= 1 @a)) ;; atom hasn't changed either
    (assert (= (force d) @d))
    (assert (= 1 (force 1)))) ;; you can safely force non-delays

  ;; assoc
  (assert (= {1 2 3 4} (assoc {} 1 2 3 4)))
  (assert (= {1 2} (assoc {} 1 2)))
  (assert (= [42 2] (assoc [1 2] 0 42)))

  ;; dissoc
  (assert (= {} (dissoc {1 2 3 4} 1 3)))
  (assert (= {1 2} (dissoc {1 2 3 4} 3)))
  (assert (nil? (dissoc nil :foo)))

  ;; disj
  (assert (= #{1 2 3} (disj #{1 2 3})))
  (assert (= #{1 2} (disj #{1 2 3} 3)))
  (assert (= #{1} (disj #{1 2 3} 2 3)))
  (assert (nil? (disj nil :foo)))

  ;; memoize
  (let [f (memoize (fn [] (rand)))]
    (f)
    (assert (= (f) (f))))

  ;; find
  (assert (= (find {} :a) nil))
  (assert (= (find {:a 1} :a) [:a 1]))
  (assert (= (find {:a 1} :b) nil))
  (assert (= (find {:a 1 :b 2} :a) [:a 1]))
  (assert (= (find {:a 1 :b 2} :b) [:b 2]))
  (assert (= (find {:a 1 :b 2} :c) nil))
  (assert (= (find {} nil) nil))
  (assert (= (find {:a 1} nil) nil))
  (assert (= (find {:a 1 :b 2} nil) nil))
  (assert (= (find [1 2 3] 0) [0 1]))

  ;; mod,quot,rem
  (assert (= (quot 4 2) 2))
  (assert (= (quot 3 2) 1))
  (assert (= (quot 6 4) 1))
  (assert (= (quot 0 5) 0))
  (assert (= (quot 42 5) 8))
  (assert (= (quot 42 -5) -8))
  (assert (= (quot -42 -5) 8))
  (assert (= (quot 9 3) 3))
  (assert (= (quot 9 -3) -3))
  (assert (= (quot -9 3) -3))
  (assert (= (quot 2 -5) 0))
  (assert (= (quot -2 5) 0))
  (assert (= (quot 0 3) 0))
  (assert (= (quot 0 -3) 0))

  (assert (= (mod 4 2) 0))
  (assert (= (mod 3 2) 1))
  (assert (= (mod 6 4) 2))
  (assert (= (mod 0 5) 0))
  (assert (= (mod 4.5 2.0) 0.5))
  (assert (= (mod 42 5) 2))
  (assert (= (mod 9 3) 0))
  (assert (= (mod 9 -3) 0))
  (assert (= (mod -9 3) 0))
  (assert (= (mod -9 -3) 0))
  (assert (= (mod 0 3) 0))
  (assert (= (mod 3216478362187432 432143214) 120355456))

  (assert (= (rem 4 2) 0))
  (assert (= (rem 0 5) 0))
  (assert (= (rem 4.5 2.0) 0.5))
  (assert (= (rem 42 5) 2))
  (assert (= (rem 2 5) 2))
  (assert (= (rem 2 -5) 2))
  (assert (= (rem 0 3) 0))

  ;; range
  (assert (= (range 10) (list 0 1 2 3 4 5 6 7 8 9)))
  (assert (= (range 10 20) (list 10 11 12 13 14 15 16 17 18 19)))
  (assert (= (range 10 20 2) (list 10 12 14 16 18)))
  (assert (= (take 20 (range)) (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)))

  ;; group-by
  (let [d (group-by second {:a 1 :b 2 :c 1 :d 4 :e 1 :f 2})]
    (assert (= 3 (count (get d 1))))
    (assert (= 2 (count (get d 2))))
    (assert (= 1 (count (get d 4)))))

  (assert (= {1 2 3 4 5 6} (merge {1 2} {3 4} {5 6})))
  (assert (= {1 2 3 4} (merge {1 2} {3 4} nil)))

  ;; frequencies
  (assert (= {:a 3 :b 2} (frequencies [:a :b :a :b :a])))

  ;; reductions
  (assert (= [1 3 6 10 15] (reductions + [1 2 3 4 5])))

  ;; keep
  (assert (= [1 3 5 7 9] (keep #(if (odd? %) %) [1 2 3 4 5 6 7 8 9 10])))
  (assert (= [2 4 6 8 10] (keep #(if (even? %) %) [1 2 3 4 5 6 7 8 9 10])))

  ;; keep-indexed
  (assert (= [1 3 5 7 9] (keep-indexed #(if (odd? %1) %2) [0 1 2 3 4 5 6 7 8 9 10])))
  (assert (= [2 4 5] (keep-indexed #(if (pos? %2) %1) [-9 0 29 -7 45 3 -8])))

  ;; map-indexed
  (assert (= [[0 :a] [1 :b] [2 :c]] (map-indexed #(vector % %2) [:a :b :c])))

  ;; merge-with
  (assert (= '{"Foo" ("foo" "FOO" "fOo"), "Bar" ("bar" "BAR" "BAr"), "Baz" ["baz"], "Qux" ["qux" "quux"]}
             (merge-with concat
                  {"Foo" ["foo" "FOO"]
                   "Bar" ["bar" "BAR"]
                   "Baz" ["baz"]}
                  {"Foo" ["fOo"]
                   "Bar" ["BAr"]
                   "Qux" ["qux" "quux"]})))
  (assert (= {:a 111, :b 102, :c 13}
             (merge-with +
                         {:a 1 :b 2 :c 3}
                         {:a 10 :c 10}
                         {:a 100 :b 100})))

  (assert (= {:a 3, :b 102, :c 13}
             (apply merge-with [+
                                {:a 1 :b 100}
                                {:a 1 :b 2 :c 3}
                                {:a 1 :c 10}])))

  (assert (= '[a c e] (replace '[a b c d e] [0 2 4])))
  (assert (= [:one :zero :two :zero]
             (replace {0 :zero 1 :one 2 :two} '(1 0 2 0))))

  ;; split-at
  (assert (= [[1 2] [3 4 5]] (split-at 2 [1 2 3 4 5])))

  ;; split-with
  (assert (= [[1 2 3] [4 5]] (split-with (partial >= 3) [1 2 3 4 5])))

  ;; trampoline
  (assert (= 10000 (trampoline (fn f [n] (if (>= n 10000) n #(f (inc n)))) 0)))

  ;; vary-meta
  (assert (= {:a 1} (meta (vary-meta [] assoc :a 1))))
  (assert (= {:a 1 :b 2} (meta (vary-meta (with-meta [] {:b 2}) assoc :a 1))))

  ;; hierarchy tests
  (derive ::rect ::shape)
  (derive ::square ::rect)

  (assert (= #{:cljs.core-test/shape} (parents ::rect)))
  (assert (= #{:cljs.core-test/rect :cljs.core-test/shape} (ancestors ::square)))
  (assert (= #{:cljs.core-test/rect :cljs.core-test/square} (descendants ::shape)))
  (assert (true? (isa? 42 42)))
  (assert (true? (isa? ::square ::shape)))

  (derive cljs.core.ObjMap ::collection)
  (derive cljs.core.PersistentHashSet ::collection)
  (assert (true? (isa? cljs.core.ObjMap ::collection)))
  (assert (true? (isa? cljs.core.PersistentHashSet ::collection)))
  (assert (false? (isa? cljs.core.IndexedSeq ::collection)))
  ;; ?? (isa? String Object)
  (assert (true? (isa? [::square ::rect] [::shape ::shape])))
  ;; ?? (ancestors java.util.ArrayList)

  ;; ?? isa? based dispatch tests

  ;; prefer-method test
  (defmulti bar (fn [x y] [x y]))
  (defmethod bar [::rect ::shape] [x y] :rect-shape)
  (defmethod bar [::shape ::rect] [x y] :shape-rect)

  ;;(bar ::rect ::rect)
  ;; -> java.lang.IllegalArgumentException:
  ;;  Multiple methods match dispatch value:
  ;;  [:cljs.core-test/rect :cljs.core-test/rect] -> [:cljs.core-test/rect :cljs.core-test/shape]
  ;;  and [:cljs.core-test/shape :cljs.core-test/rect],
  ;;  and neither is preferred

  (assert (zero? (count (prefers bar))))
  (prefer-method bar [::rect ::shape] [::shape ::rect])
  (assert (= 1 (count (prefers bar))))
  (assert (= :rect-shape (bar ::rect ::rect)))
  (assert (= :rect-shape (apply (-get-method bar [::rect ::shape]) [::rect ::shape])))

  ;; nested data structures tests
  (defmulti nested-dispatch (fn [m] (-> m :a :b)))
  (defmethod nested-dispatch :c [m] :nested-a)
  (defmethod nested-dispatch :default [m] :nested-default)
  (assert (= :nested-a (nested-dispatch {:a {:b :c}})))

  (defmulti nested-dispatch2 ffirst)
  (defmethod nested-dispatch2 :a [m] :nested-a)
  (defmethod nested-dispatch2 :default [m] :nested-default)
  (assert (= :nested-a (nested-dispatch2 [[:a :b]])))

  ;; general tests
  (defmulti foo1 (fn [& args] (first args)))
  (defmethod foo1 :a [& args] :a-return)
  (defmethod foo1 :default [& args] :default-return)
  (assert (= :a-return (foo1 :a)))
  (assert (= :default-return (foo1 1)))

  (defmulti area :Shape)
  (defn rect [wd ht] {:Shape :Rect :wd wd :ht ht})
  (defn circle [radius] {:Shape :Circle :radius radius})
  (defmethod area :Rect [r]
    (* (:wd r) (:ht r)))
  (defmethod area :Circle [c]
    (*  Math/PI (* (:radius c) (:radius c))))
  (defmethod area :default [x] :oops)
  (def r (rect 4 13))
  (def c (circle 12))
  (assert (= 52 (area r)))
  (assert (= :oops (area {})))

  ;; remove method tests
  (assert (= 2 (count (methods bar))))
  (remove-method bar [::rect ::shape])
  (assert (= 1 (count (methods bar))))
  (remove-all-methods bar)
  (assert (zero? (count (methods bar))))

  ;; test apply
  (defmulti apply-multi-test (fn ([_] 0) ([_ _] 0) ([_ _ _] 0)))
  (defmethod apply-multi-test 0
    ([x] :one)
    ([x y] :two)
    ([x y & r] [:three r]))
  (assert (= [:three '(2)] (apply apply-multi-test [0 1 2])))

  ;; custom hierarchy tests
  (def my-map-hierarchy (atom (-> (make-hierarchy)
                                  (derive (type (obj-map)) ::map)
                                  (derive (type (array-map)) ::map)
                                  (derive (type (hash-map)) ::map)
                                  (derive (type (sorted-map)) ::map))))
  (defmulti my-map? type :hierarchy my-map-hierarchy)
  (defmethod my-map? ::map [_] true)
  (defmethod my-map? :default [_] false)
  (doseq [m [(obj-map) (array-map) (hash-map) (sorted-map)]]
    (assert (my-map? m)))
  (doseq [not-m [[] 1 "asdf" :foo]]
    (assert (not (my-map? not-m))))

  ;; Range
  (assert (= (range 0 10 3) (list 0 3 6 9)))
  (assert (= (count (range 0 10 3)) 4))
  (assert (= (range 0 -10 -3) (list 0 -3 -6 -9)))
  (assert (= (count (range 0 -10 -3)) 4))
  (assert (= (range -10 10 3) (list -10 -7 -4 -1 2 5 8)))
  (assert (= (count (range -10 10 3)) 7))
  (assert (= (range 0 1 1) (list 0)))
  (assert (= (range 0 -3 -1) (list 0 -1 -2)))
  (assert (= (range 3 0 -1) (list 3 2 1)))
  (assert (= (range 0 10 -1) (list)))
  (assert (= (range 0 1 0) (list)))
  (assert (= (range 10 0 1) (list)))
  (assert (= (range 0 0 0) (list)))
  (assert (= (count (range 0 10 -1)) 0))
  (assert (= (count (range 0 1 0)) 0))
  (assert (= (count (range 10 0 1)) 0))
  (assert (= (count (range 0 0 0)) 0))
  (assert (= (take 3 (range 1 0 0)) (list 1 1 1)))
  (assert (= (take 3 (range 3 1 0)) (list 3 3 3)))

  ;; PersistentVector
  (let [pv (vec (range 97))]
    (assert (= (nth pv 96) 96))
    (assert (= (nth pv 97 nil) nil))
    (assert (= (pv 96) 96))
    (assert (nil? (rseq [])))
    (assert (= (reverse pv) (rseq pv))))


  (let [pv (vec (range 33))]
    (assert (= pv
               (-> pv
                   pop
                   pop
                   (conj 31)
                   (conj 32)))))

  (let [stack1 (pop (vec (range 97)))
        stack2 (pop stack1)]
    (assert (= 95 (peek stack1)))
    (assert (= 94 (peek stack2))))

  ;; CLJS-513
  (let [sentinel (js-obj)]
    (assert (identical? sentinel (try ([] 0) (catch js/Error _ sentinel)))))

  ;; subvec
  (let [v1 (vec (range 10))
        v2 (vec (range 5))
        s (subvec v1 2 8)]
    (assert (= s
               (-> v1
                   (subvec 2)
                   (subvec 0 6))
               (->> v1
                    (drop 2)
                    (take 6))))
    (assert (= 6 (count s)))
    (assert (= [2 3 4 5 6] (pop s)))
    (assert (= 7 (peek s)))
    (assert (= [2 3 4 5 6 7 1]
               (assoc s 6 1)
               (conj s 1)))
    (assert (= 27 (reduce + s)))
    (assert (= s (vec s))) ; pour into plain vector
    (let [m {:x 1}] (assert (= m (meta (with-meta s m)))))
    ;; go outside ranges
    (assert (= :fail (try (subvec v2 0 6) (catch js/Error e :fail))))
    (assert (= :fail (try (subvec v2 6 10) (catch js/Error e :fail))))
    (assert (= :fail (try (subvec v2 6 10) (catch js/Error e :fail))))
    (assert (= :fail (try (subvec v2 3 6) (catch js/Error e :fail))))
    ;; no layered subvecs
    (assert (identical? v1 (.-v (subvec s 1 4))))
    ;; CLJS-513
    (let [sentinel (js-obj)
          s (subvec [0 1 2 3] 1 2)]
      (assert (identical? sentinel (try (s -1) (catch js/Error _ sentinel))))
      (assert (identical? sentinel (try (s 1) (catch js/Error _ sentinel)))))  
    ;; CLJS-765
    (let [sv1 (subvec [0 1 2 3] 1 2)
          sv2 (subvec [0 1 2 3] 1 1)]
      (assert (= (rseq sv1) '(1)))
      (assert (nil? (rseq sv2)))))

  ;; TransientVector
  (let [v1 (vec (range 15 48))
        v2 (vec (range 40 57))
        v1 (persistent! (assoc! (conj! (pop! (transient v1)) :foo) 0 :quux))
        v2 (persistent! (assoc! (conj! (transient v2) :bar) 0 :quux))
        v  (into v1 v2)]
    (assert (= v (vec (concat [:quux] (range 16 47) [:foo]
                              [:quux] (range 41 57) [:bar])))))
  (loop [v  (transient [])
         xs (range 100)]
    (if-let [x (first xs)]
      (recur
       (condp #(%1 (mod %2 3)) x
         #{0 2} (conj! v x)
         #{1}   (assoc! v (count v) x))
       (next xs))
      (assert (= (vec (range 100)) (persistent! v)))))

  ;; PersistentHashMap & TransientHashMap
  (loop [m1 cljs.core.PersistentHashMap.EMPTY
         m2 (transient cljs.core.PersistentHashMap.EMPTY)
         i 0]
    (if (< i 100)
      (recur (assoc m1 i i) (assoc! m2 i i) (inc i))
      (let [m2 (persistent! m2)]
        (assert (= (count m1) 100))
        (assert (= (count m2) 100))
        (assert (= m1 m2))
        (loop [i 0]
          (if (< i 100)
            (do (assert (= (m1 i) i))
                (assert (= (m2 i) i))
                (assert (= (get m1 i) i))
                (assert (= (get m2 i) i))
                (assert (contains? m1 i))
                (assert (contains? m2 i))
                (recur (inc i)))))
        (assert (= (map vector (range 100) (range 100)) (sort-by first (seq m1))))
        (assert (= (map vector (range 100) (range 100)) (sort-by first (seq m2))))
        (assert (not (contains? (dissoc m1 3) 3))))))
  (let [m (-> (->> (interleave (range 10) (range 10))
                   (apply assoc cljs.core.PersistentHashMap.EMPTY))
              (dissoc 3 5 7))]
    (assert (= (count m) 7))
    (assert (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9})))
  (let [m (-> (->> (interleave (range 10) (range 10))
                   (apply assoc cljs.core.PersistentHashMap.EMPTY))
              (conj [:foo 1]))]
    (assert (= (count m) 11))
    (assert (= m {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :foo 1})))
  (let [m (-> (->> (interleave (range 10) (range 10))
                   (apply assoc cljs.core.PersistentHashMap.EMPTY)
                   transient)
              (conj! [:foo 1])
              persistent!)]
    (assert (= (count m) 11))
    (assert (= m {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :foo 1})))
  (let [tm (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentHashMap.EMPTY)
                transient)]
    (loop [tm tm ks [3 5 7]]
      (if-let [k (first ks)]
        (recur (dissoc! tm k) (next ks))
        (let [m (persistent! tm)]
          (assert (= (count m) 7))
          (assert (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))))
  (let [tm (-> (->> (interleave (range 10) (range 10))
                    (apply assoc cljs.core.PersistentHashMap.EMPTY))
               (dissoc 3 5 7)
               transient)]
    (doseq [k [0 1 2 4 6 8 9]]
      (assert (= k (get tm k))))
    (let [m (persistent! tm)]
      (assert (= 2 (try (dissoc! tm 1) 1 (catch js/Error e 2))))
      (assert (= 2 (try (assoc! tm 10 10) 1 (catch js/Error e 2))))
      (assert (= 2 (try (persistent! tm) 1 (catch js/Error e 2))))
      (assert (= 2 (try (count tm) 1 (catch js/Error e 2))))
      (assert (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))
  (deftype FixedHash [h v]
    IHash
    (-hash [this] h)
    IEquiv
    (-equiv [this other]
      (and (instance? FixedHash other) (= v (.-v other)))))
  (def fixed-hash-foo (FixedHash. 0 :foo))
  (def fixed-hash-bar (FixedHash. 0 :bar))
  (let [m (assoc cljs.core.PersistentHashMap.EMPTY
            fixed-hash-foo 1
            fixed-hash-bar 2)]
    (assert (= (get m fixed-hash-foo) 1))
    (assert (= (get m fixed-hash-bar) 2))
    (assert (= (count m) 2))
    (let [m (dissoc m fixed-hash-foo)]
      (assert (= (get m fixed-hash-bar) 2))
      (assert (not (contains? m fixed-hash-foo)))
      (assert (= (count m) 1))))
  (let [m (into cljs.core.PersistentHashMap.EMPTY ; make sure we're testing
                (zipmap (range 100) (range 100))) ; the correct map type
        m (assoc m fixed-hash-foo 1 fixed-hash-bar 2)]
    (assert (= (count m) 102))
    (assert (= (get m fixed-hash-foo) 1))
    (assert (= (get m fixed-hash-bar) 2))
    (let [m (dissoc m 3 5 7 fixed-hash-foo)]
      (assert (= (get m fixed-hash-bar) 2))
      (assert (not (contains? m fixed-hash-foo)))
      (assert (= (count m) 98))))
  (let [m (into cljs.core.PersistentHashMap.EMPTY ; make sure we're testing
                (zipmap (range 100) (range 100))) ; the correct map type
        m (transient m)
        m (assoc! m fixed-hash-foo 1)
        m (assoc! m fixed-hash-bar 2)
        m (persistent! m)]
    (assert (= (count m) 102))
    (assert (= (get m fixed-hash-foo) 1))
    (assert (= (get m fixed-hash-bar) 2))
    (let [m (dissoc m 3 5 7 fixed-hash-foo)]
      (assert (= (get m fixed-hash-bar) 2))
      (assert (not (contains? m fixed-hash-foo)))
      (assert (= (count m) 98))))

  ;; PersistentArrayMap & TransientArrayMap
  (def array-map-conversion-threshold
    cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD)
  (loop [m1 cljs.core.PersistentArrayMap.EMPTY
         m2 (transient cljs.core.PersistentArrayMap.EMPTY)
         i 0]
    (if (< i array-map-conversion-threshold)
      (recur (assoc m1 i i) (assoc! m2 i i) (inc i))
      (let [m2 (persistent! m2)]
        (assert (= (count m1) array-map-conversion-threshold))
        (assert (= (count m2) array-map-conversion-threshold))
        (assert (= m1 m2))
        (loop [i 0]
          (if (< i array-map-conversion-threshold)
            (do (assert (= (m1 i) i))
                (assert (= (m2 i) i))
                (assert (= (get m1 i) i))
                (assert (= (get m2 i) i))
                (assert (contains? m1 i))
                (assert (contains? m2 i))
                (recur (inc i)))))
        (assert (= (map vector
                        (range array-map-conversion-threshold)
                        (range array-map-conversion-threshold))
                   (sort-by first (seq m1))))
        (assert (= (map vector
                        (range array-map-conversion-threshold)
                        (range array-map-conversion-threshold))
                   (sort-by first (seq m2))))
        (assert (not (contains? (dissoc m1 3) 3))))))
  (let [m (-> (->> (interleave (range 10) (range 10))
                   (apply assoc cljs.core.PersistentArrayMap.EMPTY))
              (dissoc 3 5 7))]
    (assert (= (count m) 7))
    (assert (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9})))
  (let [m (-> (->> (interleave (range 10) (range 10))
                   (apply assoc cljs.core.PersistentArrayMap.EMPTY))
              (conj [:foo 1]))]
    (assert (= (count m) 11))
    (assert (= m {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :foo 1})))
  (let [m (-> (->> (interleave (range 10) (range 10))
                   (apply assoc cljs.core.PersistentArrayMap.EMPTY)
                   transient)
              (conj! [:foo 1])
              persistent!)]
    (assert (= (count m) 11))
    (assert (= m {0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :foo 1})))
  (let [tm (->> (interleave (range 10) (range 10))
                (apply assoc cljs.core.PersistentArrayMap.EMPTY)
                transient)]
    (loop [tm tm ks [3 5 7]]
      (if-let [k (first ks)]
        (recur (dissoc! tm k) (next ks))
        (let [m (persistent! tm)]
          (assert (= (count m) 7))
          (assert (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))))
  (let [tm (-> (->> (interleave (range 10) (range 10))
                    (apply assoc cljs.core.PersistentArrayMap.EMPTY))
               (dissoc 3 5 7)
               transient)]
    (doseq [k [0 1 2 4 6 8 9]]
      (assert (= k (get tm k))))
    (let [m (persistent! tm)]
      (assert (= 2 (try (dissoc! tm 1) 1 (catch js/Error e 2))))
      (assert (= 2 (try (assoc! tm 10 10) 1 (catch js/Error e 2))))
      (assert (= 2 (try (persistent! tm) 1 (catch js/Error e 2))))
      (assert (= 2 (try (count tm) 1 (catch js/Error e 2))))
      (assert (= m {0 0 1 1 2 2 4 4 6 6 8 8 9 9}))))
  (let [m (apply assoc cljs.core.PersistentArrayMap.EMPTY
                 (interleave (range (* 2 array-map-conversion-threshold))
                             (range (* 2 array-map-conversion-threshold))))]
    (assert (= (count m) (* 2 array-map-conversion-threshold)))
    (assert (= (m array-map-conversion-threshold) array-map-conversion-threshold))
    (assert (= m (into cljs.core.PersistentHashMap.EMPTY
                       (map #(vector % %)
                            (range (* 2 array-map-conversion-threshold)))))))

  ;; literal maps
  (loop [m1 {} m2 {} i 0]
    (if (< i 100)
      (recur (assoc m1 i i) (assoc m2 (str "foo" i) i) (inc i))
      (do (assert (= m1 (into cljs.core.PersistentHashMap.EMPTY
                              (map vector (range 100) (range 100)))))
          (assert (= m2 (into cljs.core.PersistentHashMap.EMPTY
                              (map vector
                                   (map (partial str "foo") (range 100))
                                   (range 100)))))
          (assert (= (count m1) 100))
          (assert (= (count m2) 100)))))

  ;; CLJS-461: automatic map conversions
  (loop [i 0 m (with-meta {} {:foo :bar}) result []]
    (if (<= i (+ cljs.core.ObjMap.HASHMAP_THRESHOLD 2))
      (recur (inc i) (assoc m (str i) i) (conj result (meta m)))
      (let [n (inc (+ cljs.core.ObjMap.HASHMAP_THRESHOLD 2))
            expected (repeat n {:foo :bar})]
        (assert (= result expected)))))
  (loop [i 0 m (with-meta {-1 :quux} {:foo :bar}) result []]
    (if (<= i (+ cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD 2))
      (recur (inc i) (assoc m i i) (conj result (meta m)))
      (let [n (inc (+ cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD 2))
            expected (repeat n {:foo :bar})]
        (assert (= result expected)))))

  ;; TransientHashSet
  (loop [s (transient #{})
         i 0]
    (if (< i 100)
      (recur (conj! s i) (inc i))
      (loop [s s i 0]
        (if (< i 100)
          (if (zero? (mod i 3))
            (recur (disj! s i) (inc i))
            (recur s (inc i)))
          (let [s (persistent! s)]
            (assert (= s (loop [s #{} xs (remove #(zero? (mod % 3)) (range 100))]
                           (if-let [x (first xs)]
                             (recur (conj s x) (next xs))
                             s))))
            (assert (= s (set (remove #(zero? (mod % 3)) (range 100))))))))))

  ;; PersistentTreeMap
  (let [m1 (sorted-map)
        c2 (comp - compare)
        m2 (sorted-map-by c2)]
    (assert (identical? cljs.core.PersistentTreeMap (type m1)))
    (assert (identical? cljs.core.PersistentTreeMap (type m2)))
    (assert (identical? compare (.-comp m1)))
    (assert (zero? (count m1)))
    (assert (zero? (count m2)))
    (assert (nil? (rseq m1)))
    (let [m1 (assoc m1 :foo 1 :bar 2 :quux 3)
          m2 (assoc m2 :foo 1 :bar 2 :quux 3)]
      (assert (= (count m1) 3))
      (assert (= (count m2) 3))
      (assert (= (seq m1) (list [:bar 2] [:foo 1] [:quux 3])))
      (assert (= (seq m2) (list [:quux 3] [:foo 1] [:bar 2])))
      (assert (= (seq m1) (rseq m2)))
      (assert (= (seq m2) (rseq m1)))
      (assert (= (conj m1 [:wibble 4]) {:foo 1 :bar 2 :quux 3 :wibble 4}))
      (assert (= (count (conj m1 [:wibble 4])) 4))
      (assert (= (conj m2 [:wibble 4]) {:foo 1 :bar 2 :quux 3 :wibble 4}))
      (assert (= (count (conj m2 [:wibble 4])) 4))
      (assert (= (map key (assoc m1 nil 4)) (list nil :bar :foo :quux)))
      (assert (= (map key (assoc m2 nil 4)) (list :quux :foo :bar nil)))))
  (let [m (->> [[0 10] [20 30] [10 20] [50 60] [30 40] [40 50]]
               (mapcat (partial apply range))
               (mapcat #(list % %))
               (apply sorted-map))
        s1 (map #(vector % %) (range 60))
        s2 (map #(vector % %) (range 59 -1 -1))]
    (assert (= (count m) 60))
    (assert (= (seq m) s1))
    (assert (= (rseq m) s2)))
  (let [m (sorted-map :foo 1 :bar 2 :quux 3)]
    (assert (= (dissoc m :foo) (hash-map :bar 2 :quux 3)))
    (assert (= (count (dissoc m :foo)) 2))
    (assert (= (hash m) (hash (hash-map :foo 1 :bar 2 :quux 3))))
    (assert (= (subseq m < :foo)  (list [:bar 2])))
    (assert (= (subseq m <= :foo) (list [:bar 2] [:foo 1])))
    (assert (= (subseq m > :foo)  (list [:quux 3])))
    (assert (= (subseq m >= :foo) (list [:foo 1] [:quux 3])))
    (assert (= (map #(reduce (fn [_ x] x) %) m) (list 2 1 3)))
    (assert (= (map #(reduce (fn [x _] x) 7 %) m) (list 7 7 7))))

  ;; PersistentTreeSet
  (let [s1 (sorted-set)
        c2 (comp - compare)
        s2 (sorted-set-by c2)
        c3 #(compare (quot %1 2) (quot %2 2))
        s3 (sorted-set-by c3)
        s4 (sorted-set-by <)]
    (assert (identical? cljs.core.PersistentTreeSet (type s1)))
    (assert (identical? cljs.core.PersistentTreeSet (type s2)))
    (assert (identical? compare (-comparator s1)))
    (assert (zero? (count s1)))
    (assert (zero? (count s2)))
    (assert (nil? (rseq s1)))
    (let [s1 (conj s1 1 2 3)
          s2 (conj s2 1 2 3)
          s3 (conj s3 1 2 3 7 8 9)
          s4 (conj s4 1 2 3)]
      (assert (= (hash s1) (hash s2)))
      (assert (= (hash s1) (hash #{1 2 3})))
      (assert (= (seq s1)  (list 1 2 3)))
      (assert (= (rseq s1) (list 3 2 1)))
      (assert (= (seq s2)  (list 3 2 1)))
      (assert (= (rseq s2) (list 1 2 3)))
      (assert (= (count s1) 3))
      (assert (= (count s2) 3))
      (assert (= (count s3) 4))
      (assert (= (get s3 0) 1))
      (assert (= (subseq s3 > 5) (list 7 8)))
      (assert (= (subseq s3 > 6) (list 8)))
      (assert (= (subseq s3 >= 6) (list 7 8)))
      (assert (= (subseq s3 >= 12) nil))
      (assert (= (subseq s3 < 0) (list)))
      (assert (= (subseq s3 < 5) (list 1 2)))
      (assert (= (subseq s3 < 6) (list 1 2)))
      (assert (= (subseq s3 <= 6) (list 1 2 7)))
      (assert (= (subseq s3 >= 2 <= 6) (list 2 7)))
      (assert (= (subseq s4 >= 2 < 3) (list 2)))
      (let [s1 (disj s1 2)
            s2 (disj s2 2)]
        (assert (= (seq s1)  (list 1 3)))
        (assert (= (rseq s1) (list 3 1)))
        (assert (= (seq s2)  (list 3 1)))
        (assert (= (rseq s2) (list 1 3)))
        (assert (= (count s1) 2))
        (assert (= (count s2) 2)))))

  ;; defrecord
  (defrecord Person [firstname lastname])
  (def fred (Person. "Fred" "Mertz"))
  (assert (= (:firstname fred) "Fred"))
  (def fred-too (Person. "Fred" "Mertz"))
  (assert (= fred fred-too))
  (assert (false? (= fred nil)))
  (assert (false? (= nil fred)))

  ;; invalid tests, cannot set meta and extmap directly - David
  (def ethel (with-meta (assoc (Person. "Ethel" "Mertz") :husband :fred)
               {:married true}))
  (assert (= (meta ethel) {:married true}))
  (def ethel-too (with-meta (assoc (Person. "Ethel" "Mertz")  :husband :fred)
                   {:married true}))
  (assert (= ethel ethel-too))

  (assert (= (map->Person {:firstname "Fred" :lastname "Mertz"}) fred))
  (assert (= (->Person "Fred" "Mertz") fred))

  (assert (= (count fred) 2))
  (assert (= (count ethel) 3))

  (defrecord A [])
  (assert (= {:foo 'bar} (meta (with-meta (A.) {:foo 'bar}))))
  (assert (= 'bar (:foo (assoc (A.) :foo 'bar))))

  (defrecord C [a b c])
  (def letters (C. "a" "b" "c"))
  (assert (= (set (keys letters)) #{:a :b :c}))
  (def more-letters (assoc letters :d "d" :e "e" :f "f"))
  (assert (= (set (keys more-letters)) #{:a :b :c :d :e :f}))
  (assert (= (set (keys (dissoc more-letters :d))) #{:a :b :c :e :f}))
  (assert (= (set (keys (dissoc more-letters :d :e))) #{:a :b :c :f}))
  (assert (= (set (keys (dissoc more-letters :d :e :f))) #{:a :b :c}))

  ;; ObjMap
  (let [ks (map (partial str "foo") (range 500))
        m  (apply obj-map (interleave ks (range 500)))]
    (assert (instance? cljs.core.ObjMap m))
    (assert (= 500 (count m)))
    (assert (= 123 (m "foo123"))))

  ;; comparator
  (assert (= [1 1 2 2 3 5] (seq (.sort (to-array [2 3 1 5 2 1]) (comparator <)))))
  (assert (= [5 3 2 2 1 1] (seq (.sort (to-array [2 3 1 5 2 1]) (comparator >)))))
  
  ;; dot
  (let [s "abc"]
    (assert (= 3 (.-length s)))
    (assert (= 3 (. s -length)))
    (assert (= 3 (. (str 138) -length)))
    (assert (= 3 (. "abc" -length)))
    (assert (= "bc" (.substring s 1)))
    (assert (= "bc" (.substring "abc" 1)))
    (assert (= "bc" ((memfn substring start) s 1)))
    (assert (= "bc" (. s substring 1)))
    (assert (= "bc" (. s (substring 1))))
    (assert (= "bc" (. s (substring 1 3))))
    (assert (= "bc" (.substring s 1 3)))
    (assert (= "ABC" (. s (toUpperCase))))
    (assert (= "ABC" (. "abc" (toUpperCase))))
    (assert (= "ABC" ((memfn toUpperCase) s)))
    (assert (= "BC" (. (. s (toUpperCase)) substring 1)))
    (assert (= 2 (.-length (. (. s (toUpperCase)) substring 1)))))

  (assert (= (conj fred {:wife :ethel :friend :ricky})
             (map->Person {:firstname "Fred" :lastname "Mertz" :wife :ethel :friend :ricky})))
  (assert (= (conj fred {:lastname "Flintstone"})
             (map->Person {:firstname "Fred" :lastname "Flintstone"})))
  (assert (= (assoc fred :lastname "Flintstone")
             (map->Person {:firstname "Fred" :lastname "Flintstone"})))
  (assert (= (assoc fred :wife :ethel)
             (map->Person {:firstname "Fred" :lastname "Mertz" :wife :ethel})))
  (assert (= (dissoc ethel :husband)
             (map->Person {:firstname "Ethel" :lastname "Mertz"})))

  (defrecord A [x])
  (defrecord B [x])
  (assert (not= (A. nil) (B. nil)))

  (defprotocol IFoo (foo [this]))
  (assert (= (meta (with-meta (reify IFoo (foo [this] :foo)) {:foo :bar}))
             {:foo :bar}))

  (defmulti foo2 identity)
  (defmethod foo2 0 [x] x)
  (assert (= foo2 (ffirst {foo2 1})))

  (defprotocol IMutate
    (mutate [this]))

  (deftype Mutate [^:mutable a]
    IMutate
    (mutate [_]
      (set! a 'foo)))

  ;; IFn
  (deftype FnLike []
    IFn
    (-invoke [_] :a)
    (-invoke [_ a] :b)
    (-invoke [_ a b] :c))

  (assert (= :a ((FnLike.))))
  (assert (= :b ((FnLike.) 1)))
  (assert (= :c ((FnLike.) 1 2)))

  (assert (= [:b :b :b] (map (FnLike.) [0 0 0])))

  (deftype FnLikeB [a]
    IFn
    (-invoke [_] a))

  (assert (= 1 ((FnLikeB. 1))))

  ;; hashing bug in many JS runtimes CLJ-118
  (let [g #{(conj #{:2} :alt)}
        h #{#{:2 :alt}}]
    (assert (= g h)))
  (assert (= (hash {:a 1 :b 2})
             (hash {:b 2 :a 1})))
  (assert (= (hash (hash-map :a 1 :b 2))
             (hash (hash-map :b 2 :a 1))))
  (assert (= (hash {:start 133 :end 134})
             (hash (apply hash-map [:start 133 :end 134]))))

  (defprotocol IHasFirst
    (-get-first [this]))

  (defprotocol IFindsFirst
    (-find-first [this other]))

  (deftype First [xs]
    ISeqable
    (-seq [this] (seq xs))
    IIndexed
    (-nth [this i] (nth xs i))
    (-nth [this i not-found] (nth xs i not-found))
    IFn
    (-invoke [[x]] x)
    (-invoke [this x] this)
    Object
    (toString [[x]] (str x))
    IHasFirst
    (-get-first [[x]] x)
    IFindsFirst
    (-find-first [_ [x]] x))

  (let [fv (First. [1 2 3])
        fs (First. "asdf")]
    (assert (= (fv) 1))
    (assert (= (fs) \a))
    (assert (= (str fs) \a))
    (assert (= (-get-first fv) 1))
    (assert (= (-get-first fs) \a))
    (assert (= (-find-first fv [1]) 1))
    (assert (identical? (fv 1) fv)))

  (deftype DestructuringWithLocals [a]
    IFindsFirst
    (-find-first [_ [x y]]
      [x y a]))

  (let [t (DestructuringWithLocals. 1)]
    (assert (= [2 3 1] (-find-first t [2 3]))))

  (let [x 1]
    (assert (= (case x 1 :one) :one)))
  (let [x 1]
    (assert (= (case x 2 :two :default) :default)))
  (let [x 1]
    (assert (= (try
                 (case x 3 :three)
                 (catch js/Error e
                     :fail))
               :fail)))
  (let [x 1]
    (assert (= (case x
                 (1 2 3) :ok
                 :fail)
               :ok)))

  (let [x [:a :b]]
    (assert (= (case x
                 [:a :b] :ok)
               :ok)))

  (let [a 'a]
    (assert (= (case a
                 nil nil
                 & :amp
                 :none)
               :none)))

  (let [a '&]
    (assert (= (case a
                 nil nil
                 & :amp
                 :none)
               :amp)))

  (let [foo 'a]
    (assert (= (case foo
                 (a b c) :sym
                 :none)
               :sym))
    (assert (= (case foo
                 (b c d) :sym
                 :none)
               :none)))

  ;; IComparable
  (assert (=  0 (compare false false)))
  (assert (= -1 (compare false true)))
  (assert (=  1 (compare true  false)))

  (assert (= -1 (compare  0  1)))
  (assert (= -1 (compare -1  1)))
  (assert (=  0 (compare  1  1)))
  (assert (=  1 (compare  1  0)))
  (assert (=  1 (compare  1 -1)))

  (assert (=  0 (compare "cljs" "cljs")))
  (assert (=  0 (compare :cljs :cljs)))
  (assert (=  0 (compare 'cljs 'cljs)))
  (assert (= -1 (compare "a" "b")))
  (assert (= -1 (compare :a :b)))
  (assert (= -1 (compare 'a 'b)))
  ;; cases involving ns
  (assert (= -1 (compare :b/a :c/a)))
  (assert (= -1 (compare :c :a/b)))
  (assert (=  1 (compare :a/b :c)))
  (assert (= -1 (compare 'b/a 'c/a)))
  (assert (= -1 (compare 'c 'a/b)))
  (assert (=  1 (compare 'a/b 'c)))

  ;; This is different from clj. clj gives -2 next 3 tests
  (assert (= -1 (compare "a" "c")))
  (assert (= -1 (compare :a :c)))
  (assert (= -1 (compare 'a 'c)))

  (assert (= -1 (compare [1 2] [1 1 1])))
  (assert (= -1 (compare [1 2] [1 2 1])))
  (assert (= -1 (compare [1 1] [1 2])))
  (assert (=  0 (compare [1 2] [1 2])))
  (assert (=  1 (compare [1 2] [1 1])))
  (assert (=  1 (compare [1 1 1] [1 2])))
  (assert (=  1 (compare [1 1 2] [1 1 1])))

  (assert (= -1 (compare (subvec [1 2 3] 1) (subvec [1 2 4] 1))))
  (assert (=  0 (compare (subvec [1 2 3] 1) (subvec [1 2 3] 1))))
  (assert (=  1 (compare (subvec [1 2 4] 1) (subvec [1 2 3] 1))))

  ;; RSeq

  (assert (= '(3 2 1) (reverse (seq (array 1 2 3)))))
  (assert (= '(3 2 1) (reverse [1 2 3])))
  (assert (= '(4 3 2 1) (cons 4 (reverse [1 2 3]))))
  (assert (= 6 (reduce + (reverse [1 2 3]))))
  (assert (= '(4 3 2) (map inc (reverse [1 2 3]))))
  (assert (= '(4 2) (filter even? (reverse [1 2 3 4]))))

  ;; Chunked Sequences

  (let [r (range 64)
        v (into [] r)]
    (assert (= (hash (seq v)) (hash (seq v))))
    (assert (= 6 (reduce + (array-chunk (array 1 2 3)))))
    (assert (instance? ChunkedSeq (seq v)))
    (assert (= r (seq v)))
    (assert (= (map inc r) (map inc v)))
    (assert (= (filter even? r) (filter even? v)))
    (assert (= (filter odd? r) (filter odd? v)))
    (assert (= (concat r r r) (concat v v v)))
    (assert (satisfies? IReduce (seq v)))
    (assert (== 2010 (reduce + (nnext (nnext (seq v))))))
    (assert (== 2020 (reduce + 10 (nnext (nnext (seq v)))))))

  ;; INext

  (assert (= nil (next nil)))
  (assert (= nil (next (seq (array 1)))))
  (assert (= '(2 3) (next (seq (array 1 2 3)))))
  (assert (= nil (next (reverse (seq (array 1))))))
  (assert (= '(2 1) (next (reverse (seq (array 1 2 3))))))
  (assert (= nil (next (cons 1 nil))))
  (assert (= '(2 3) (next (cons 1 (cons 2 (cons 3 nil))))))
  (assert (= nil (next (lazy-seq (cons 1 nil)))))
  (assert (= '(2 3) (next (lazy-seq
                             (cons 1
                               (lazy-seq
                                 (cons 2
                                   (lazy-seq (cons 3 nil)))))))))
  (assert (= nil (next (list 1))))
  (assert (= '(2 3) (next (list 1 2 3))))
  (assert (= nil (next [1])))
  (assert (= '(2 3) (next [1 2 3])))
  (assert (= nil (next (range 1 2))))
  (assert (= '(2 3) (next (range 1 4))))

  ;; UUID

  (assert (= (UUID. "550e8400-e29b-41d4-a716-446655440000")
             (UUID. "550e8400-e29b-41d4-a716-446655440000")))

  (assert (not (identical? (UUID. "550e8400-e29b-41d4-a716-446655440000")
                           (UUID. "550e8400-e29b-41d4-a716-446655440000"))))

  (assert (= 42 (get {(UUID. "550e8400-e29b-41d4-a716-446655440000") 42}
                     (UUID. "550e8400-e29b-41d4-a716-446655440000")
                     :not-at-all-found)))

  (assert (= :not-at-all-found
             (get {(UUID. "550e8400-e29b-41d4-a716-446655440000") 42}
                  (UUID. "666e8400-e29b-41d4-a716-446655440000")
                  :not-at-all-found)))

  ;; Reader literals
  (assert (= #queue [1]      (into cljs.core.PersistentQueue.EMPTY [1])))
  (assert (not= #queue [1 2] (into cljs.core.PersistentQueue.EMPTY [1])))

  (assert (= #inst "2010-11-12T18:14:15.666-00:00"
             #inst "2010-11-12T13:14:15.666-05:00"))

  (assert (= #uuid "550e8400-e29b-41d4-a716-446655440000"
             #uuid "550e8400-e29b-41d4-a716-446655440000"))

  (assert (= 42
             (get {#uuid "550e8400-e29b-41d4-a716-446655440000" 42}
                  #uuid "550e8400-e29b-41d4-a716-446655440000")))

  ;; pr-str

  (assert (= (pr-str 1) "1"))
  (assert (= (pr-str -1) "-1"))
  (assert (= (pr-str -1.5) "-1.5"))
  (assert (= (pr-str [3 4]) "[3 4]"))
  (assert (= (pr-str "foo") "\"foo\""))
  (assert (= (pr-str :hello) ":hello"))
  (assert (= (pr-str 'goodbye) "goodbye"))
  (assert (= (pr-str #{1 2 3}) "#{1 2 3}"))
  (assert (= (pr-str '(7 8 9)) "(7 8 9)"))
  (assert (= (pr-str '(deref foo)) "(deref foo)"))
  (assert (= (pr-str '(quote bar)) "(quote bar)"))
  (assert (= (pr-str 'foo/bar) "foo/bar"))
  (assert (= (pr-str \a) "\"a\""))
  (assert (= (pr-str :foo/bar) ":foo/bar"))
  (assert (= (pr-str nil) "nil"))
  (assert (= (pr-str true) "true"))
  (assert (= (pr-str false) "false"))
  (assert (= (pr-str "string") "\"string\""))
  (assert (= (pr-str ["üñîçó∂£" :ทดสอบ/你好 'こんにちは]) "[\"üñîçó∂£\" :ทดสอบ/你好 こんにちは]"))
  (assert (= (pr-str "escape chars \t \r \n \\ \" \b \f") "\"escape chars \\t \\r \\n \\\\ \\\" \\b \\f\""))

  ;;; pr-str records

  (defrecord PrintMe [a b])
  (assert (= (pr-str (PrintMe. 1 2)) "#cljs.core-test.PrintMe{:a 1, :b 2}"))

  ;;; pr-str inst

  (assert (= (pr-str (js/Date. "2010-11-12T13:14:15.666-05:00"))
             "#inst \"2010-11-12T18:14:15.666-00:00\""))

  (doseq [month (range 1 13) day (range 1 29) hour (range 1 23)]
    (let [pad (fn [n]
                (if (< n 10)
                  (str "0" n)
                  n))
          inst (str "2010-" (pad month) "-" (pad day) "T" (pad hour) ":14:15.666-00:00")]
      (assert (= (pr-str (js/Date. inst)) (str "#inst \"" inst "\"")))))

  ;;; pr-str uuid

  (let [uuid-str "550e8400-e29b-41d4-a716-446655440000"
        uuid (UUID. uuid-str)]
    (assert (= (pr-str uuid) (str "#uuid \"" uuid-str "\""))))

  ;;; pr-str PersistentQueueSeq - CLJS-800
  (assert (= (pr-str (rest (conj cljs.core.PersistentQueue.EMPTY 1 2 3))) "(2 3)"))

  ;; CLJS-405

  (defprotocol IBar (-bar [this x]))

  (defn baz [f]
    (reify
      IBar
      (-bar [_ x]
        (f x))))

  (assert (= 2 (-bar (baz inc) 1)))

  ;; CLJS-401 / CLJS-411

  (let [x "original"]
    (defn original-closure-stmt [] x))

  (let [x "overwritten"]
    (assert (= "original" (original-closure-stmt))))

  (assert (= "original" (let [x "original"
                              oce (fn [] x)
                              x "overwritten"]
                          (oce))))


  (letfn [(x [] "original")
          (y [] (x))]
    (let [x (fn [] "overwritten")]
      (assert (= "original" (y)))))

  ;; CLJS-459: reduce-kv visit order
  (assert (= (reduce-kv conj [] (sorted-map :foo 1 :bar 2))
             [:bar 2 :foo 1]))

  ;; Test builtin implementations of IKVReduce
  (letfn [(kvr-test [data expect]
            (assert (= :reduced (reduce-kv (fn [_ _ _] (reduced :reduced))
                                           [] data)))
            (assert (= expect (reduce-kv (fn [r k v] (-> r (conj k) (conj v)))
                                         [] data))))]
    (kvr-test (obj-map :k0 :v0 :k1 :v1) [:k0 :v0 :k1 :v1])
    (kvr-test (hash-map :k0 :v0 :k1 :v1) [:k0 :v0 :k1 :v1])
    (kvr-test (array-map :k0 :v0 :k1 :v1) [:k0 :v0 :k1 :v1])
    (kvr-test [:v0 :v1] [0 :v0 1 :v1]))
  (assert (= {:init :val} (reduce-kv assoc {:init :val} nil)))

  ;; data conveying exception
  (assert (= {:foo 1}
             (try (throw (ex-info "asdf" {:foo 1}))
                  (catch ExceptionInfo e
                    (ex-data e)))))
  (assert (instance? js/Error (ex-info "asdf" {:foo 1})))
  (assert (not (instance? cljs.core.ExceptionInfo (js/Error.))))

  ;; CLJS-435

  (assert (= (assoc {} 154618822656 1 261993005056 1)
             {154618822656 1 261993005056 1}))

  ;; CLJS-458

  (assert (= (get-in {:a {:b 1}} [:a :b :c] :nothing-there)
             :nothing-there))

  ;; CLJS-464

  (assert (nil? (get-in {:foo {:bar 2}} [:foo :bar :baz])))

  ;; symbol metadata

  (assert (= (meta (with-meta 'foo {:tag 'int})) {:tag 'int}))

  ;; CLJS-467

  (assert (= (reduce-kv + 0 (apply hash-map (range 1000)))
             (reduce + (range 1000))))

  ;; CLJS-477

  (assert (= [js/undefined 1 2] ((fn [& more] more) js/undefined 1 2)))
  (assert (= [js/undefined 4 5] ((fn [a b & more] more) 1 2 js/undefined 4 5)))

  ;; CLJS-493

  (assert (nil? (get 42 :anything)))
  (assert (= (get 42 :anything :not-found) :not-found))
  (assert (nil? (first (map get [42] [:anything]))))
  (assert (= (first (map get [42] [:anything] [:not-found])) :not-found))

  ;; CLJS-481

  (let [fs (atom [])]
    (doseq [x (range 4)
            :let [y (inc x)
                  f (fn [] y)]]
      (swap! fs conj f))
    (assert (= (map #(%) @fs) '(1 2 3 4))))

  ;; CLJS-495
  (assert (false? (exists? js/jQuery)))
  (def exists?-test-val 'foo)
  (assert (exists? exists?-test-val))

  ;; CLJS-496
  (assert (= (char 65) \A))
  (assert (= (char \A) \A))

  ;; compile time run symbol hash codes
  
  (assert (= (hash 'foo) (hash (symbol "foo"))))
  (assert (= (hash 'foo/bar) (hash (symbol "foo" "bar"))))

  (assert (= (lazy-cat [1] [2] [3]) '(1 2 3)))

  ;; r1798 core fn protocol regression
  (extend-type object
    ISeqable
    (-seq [coll]
      (map #(vector % (aget coll %)) (js-keys coll)))

    ILookup
    (-lookup
      ([coll k]
        (-lookup coll k nil))
      ([coll k not-found]
        (if-let [v (aget coll k)]
          v
          not-found))))

  (assert (= (seq (js-obj "foo" 1 "bar" 2)) '(["foo" 1] ["bar" 2])))
  (assert (= (get (js-obj "foo" 1) "foo") 1))
  (assert (= (get (js-obj "foo" 1) "bar" ::not-found) ::not-found))
  (assert (= (reduce (fn [s [k v]] (+ s v)) 0 (js-obj "foo" 1 "bar" 2)) 3))

  ;; CLJS-515
  (deftype PositionalFactoryTest [x])

  (assert (== 1 (.-x (->PositionalFactoryTest 1))))

  ;; CLJS-518
  (assert (nil? (:test "test")))

  ;; CLJS-541
  (letfn [(f! [x] (print \f) x)
          (g! [x] (print \g) x)]
    (assert (= "ffgfg"
               (with-out-str
                 (instance? Symbol (f! 'foo))
                 (max (f! 5) (g! 10))
                 (min (f! 5) (g! 10))))))

  ;; CLJS-582
  (assert (= #{1 2} (set [1 2 2])))
  (assert (= #{1 2} (hash-set 1 2 2)))
  (assert (= #{1 2} (apply hash-set [1 2 2])))

  ;; CLJS-585
  (assert (= (last (map identity (into [] (range 32)))) 31))
  (assert (= (into #{} (range 32))
             (set (map identity (into [] (range 32))))))

  ;; CLJS-580
  (def foo580)
  (def foo580 {:a (fn []) :b (fn [] (foo580 :a))})
  (assert (nil? (((:b foo580)))))

  ;; CLJS-587
  (assert (== (first (filter #(== % 9999) (range))) 9999))

  ;; LazySeq regressions

  ;; CLJS-604
  (assert (= () (concat nil [])))
  (assert (= () (concat [] [])))

  ;; CLJS-600
  (assert (= "foobar" (apply str (concat "foo" "bar"))))

  ;; CLJS-608
  (assert (= '("") (re-seq #"\s*" "")))

  ;; CLJS-638

  (deftype KeywordTest []
    ILookup
    (-lookup [o k] :nothing)
    (-lookup [o k not-found] not-found))

  (assert (= (:a (KeywordTest.)) :nothing))

  ;; CLJS-648 (CLJ-1285)
  (let [a (reify IHash (-hash [_] 42))
        b (reify IHash (-hash [_] 42))
        s (set (range 128))]
    (assert (= (-> (conj s a b) transient (disj! a) persistent! (conj a))
               (-> (conj s a b) transient (disj! a) persistent! (conj a)))))

  ;; CLJS-660

  (assert (= (-> 'a.b keyword ((juxt namespace name))) [nil "a.b"]))
  (assert (= (-> 'a.b/c keyword ((juxt namespace name))) ["a.b" "c"]))
  (assert (= (-> "a.b" keyword ((juxt namespace name))) [nil "a.b"]))
  (assert (= (-> "a.b/c" keyword ((juxt namespace name))) ["a.b" "c"]))

  ;; CLJS-663

  (assert (= (keyword 123) nil))
  (assert (= (keyword (js/Date.)) nil))

  ;; CLJS-647
  (let [keys #(vec (js-keys %))
        z "x"]
    (assert (= ["x"]
               (keys (js-obj "x" "y"))
               (keys (js-obj (identity "x") "y"))
               (keys (js-obj z "y")))))

  ;; CLJS-583

  (def some-x 1)
  (def some-y 1)

  (assert (= (count #{some-x some-y}) 1))

  ;; CLJS-584

  (assert (= (count {some-x :foo some-y :bar}) 1))

  ;; CLJS-717

  (assert (array? #js [1 2 3]))
  (assert (= (alength #js [1 2 3]) 3))
  (assert (= (seq #js [1 2 3]) (seq [1 2 3])))
  (assert (= (set (js-keys #js {:foo "bar" :baz "woz"})) #{"foo" "baz"}))
  (assert (= (aget #js {:foo "bar"} "foo") "bar"))
  (assert (= (aget #js {"foo" "bar"} "foo") "bar"))
  (assert (array? (aget #js {"foo" #js [1 2 3]} "foo")))
  (assert (= (seq (aget #js {"foo" #js [1 2 3]} "foo")) '(1 2 3)))

  ;; CLJS-725

  (assert (= (apply vector (drop-while (partial = 1) [1 2 3])) [2 3]))
  (assert (= (apply list (drop-while (partial = 1) [1 2 3])) '(2 3)))
  (assert (= (set (drop 1 #js [1 2 3])) #{2 3}))

  ;; CLJS-724

  (assert (nil? (first (rest (rest (rest (range 3)))))))

  ;; CLJS-730

  (assert (true? (object? #js {})))
  (assert (false? (object? nil)))

  (assert
    (== (count (hash-set [1 4] [2 4] [3 4] [0 3] [1 3] [2 3] [3 3]
                         [0 2] [1 2] [2 2] [3 2] [4 2] [0 1] [1 1]
                         [2 1] [3 1] [1 0] [2 0] [3 0]))
        (count (list [1 4] [2 4] [3 4] [0 3] [1 3] [2 3] [3 3]
                     [0 2] [1 2] [2 2] [3 2] [4 2] [0 1] [1 1]
                     [2 1] [3 1] [1 0] [2 0] [3 0]))))

  (defprotocol IWoz
    (-woz [this]))

  (def noz [])

  ;; CLJS-414

  (assert (= (specify noz IWoz (-woz [_] :boz)) noz))
  (assert (not (identical? (specify noz IWoz (-woz [_] :boz)) noz)))
  (assert (= (-woz (specify noz IWoz (-woz [this] this))) noz))
  (assert (= (-woz (specify noz IWoz (-woz [_] :boz))) :boz))

  ;; CLJS-734

  (assert (= (-> (transient []) (conj! 1 2) persistent!) [1 2]))
  (assert (= (-> (transient #{1 2 3}) (disj! 1 2) persistent!) #{3}))
  (assert (= (-> (transient {}) (assoc! :a 1 :b 2) persistent!) {:a 1 :b 2}))
  (assert (= (-> (transient {:a 1 :b 2 :c 3}) (dissoc! :a :b) persistent!) {:c 3}))

  ;; CLJS-767

  (doseq [n [nil "-1" "" "0" "1" false true (js-obj)]]
    (assert (= :fail (try (assoc [1 2] n 4)
                       (catch js/Error e :fail))))
    (assert (= :fail (try (assoc (subvec [1 2 3] 2) n 4)
                       (catch js/Error e :fail))))
    (assert (= :fail (try (assoc (range 1 3) n 4)
                       (catch js/Error e :fail)))))

  ;; CLJS-768

  (doseq [n [nil "-1" "" "0" "1" false true (js-obj)]]
    (assert (= :fail (try (assoc! (transient [1 2]) n 4)
                       (catch js/Error e :fail)))))

  ;; Namespaced destructuring
  
  (let [{:keys [:a :b]} {:a 1 :b 2}]
    (assert (= 1 a))
    (assert (= 2 b)))

  (let [{:keys [:a/b :c/d]} {:a/b 1 :c/d 2}]
    (assert (= 1 b))
    (assert (= 2 d)))

  (let [{:keys [a/b c/d]} {:a/b 1 :c/d 2}]
    (assert (= 1 b))
    (assert (= 2 d)))

  (let [{:syms [a/b c/d]} {'a/b 1 'c/d 2}]
    (assert (= 1 b))
    (assert (= 2 d)))

  (let [{:keys [::s/x ::s/y]} {:clojure.string/x 1 :clojure.string/y 2}]
    (assert (= x 1))
    (assert (= y 2)))

  ;; CLJS-739

  (defn cljs-739 [arr names]
    (let [name (first names)]
      (if name
        (recur (conj arr (fn [] (println name)))
          (rest names))
        arr)))

  (assert (= (with-out-str (doseq [fn (cljs-739 [] [:a :b :c :d])] (fn)))
             ":a\n:b\n:c\n:d\n"))

  ;; CLJS-728

  (doseq [n [nil "-1" "" "0" "1" false true (js-obj)]]
    (assert (nil? (get [1 2] n)))
    (assert (= :fail (try (nth [1 2] n) (catch js/Error e :fail))))
    (assert (= 4 (get [1 2] n 4)))
    (assert (= :fail (try (nth [1 2] n 4) (catch js/Error e :fail))))

    (assert (nil? (get (subvec [1 2] 1) n)))
    (assert (= :fail (try (nth (subvec [1 2] 1) n) (catch js/Error e :fail))))
    (assert (= 4 (get (subvec [1 2] 1) n 4)))
    (assert (= :fail (try (nth (subvec [1 2] 1) n 4) (catch js/Error e :fail))))

    (assert (nil? (get (transient [1 2]) n)))
    (assert (= :fail (try (nth (transient [1 2]) n) (catch js/Error e :fail))))
    (assert (= 4 (get (transient [1 2]) n 4)))
    (assert (= :fail (try (nth (transient [1 2]) n 4) (catch js/Error e :fail))))

    (assert (nil? (get (range 1 3) n)))
    (assert (= :fail (try (nth (range 1 3) n) (catch js/Error e :fail))))
    (assert (= 4 (get (range 1 3) n 4)))
    (assert (= :fail (try (nth (range 1 3) n 4) (catch js/Error e :fail)))))

  ;; CLJS-778
  (assert (= (-rest (rseq [0])) ()))
  (assert (nil? (-next (rseq [0]))))
  (assert (= (set (rseq [0])) #{0}))

  ;; CLJS-780
  (def cljs-780 (atom {:foo (with-meta [] {:bar '(1 2 3)})}))
  (swap! cljs-780 update-in [:foo] vary-meta update-in [:bar] vec)
  (let [x (-> @cljs-780 :foo meta :bar)]
    (assert (vector? x))
    (assert (= x [1 2 3])))

  ;; CLJS-782
  (assert (= (.toString #uuid "550e8400-e29b-41d4-a716-446655440000")
             "550e8400-e29b-41d4-a716-446655440000"))

  ;; CLJS-784
  (doseq [m [(array-map) (hash-map) (sorted-map)]]
    (assert (= :ok
               (try
                 (conj m "foo")
                 (catch js/Error _
                   :ok))))
    (assert (= {:foo 1} (conj m [:foo 1])))
    (assert (= {:foo 1} (conj m {:foo 1})))
    (assert (= {:foo 1} (conj m (list [:foo 1])))))
  
  (doseq [mt [array-map hash-map sorted-map]]
    (assert (= {:foo 1 :bar 2 :baz 3}
               (conj (mt :foo 1)
                     ((fn make-seq [from-seq]
                        ;; this tests specifically for user defined seq's, that implement the bare minimum, i.e. no INext
                        (when (seq from-seq)
                          (reify
                            ISeqable
                            (-seq [this] this)
                            ISeq
                            (-first [this] (first from-seq))
                            (-rest [this] (make-seq (rest from-seq))))))
                      [[:bar 2] [:baz 3]])))))

  ;; printing customization
  (assert (= (binding [*print-length* 1] (str [1 2 3 4 5 6 7 8 9 0]))
             "[1 ...]"))
  (assert (= (binding [*print-length* 2] (str [1 2 3 4 5 6 7 8 9 0]))
             "[1 2 ...]"))
  (assert (= (binding [*print-length* 10] (str [1 2 3 4 5 6 7 8 9 0]))
             "[1 2 3 4 5 6 7 8 9 0]"))
  ;; CLJS-804
  (assert (= (binding [*print-length* 10] (str {:foo "bar"}))
             "{:foo \"bar\"}"))
  (assert (= (binding [*print-length* 1] (str {:foo "bar" :baz "woz"}))
             "{:foo \"bar\", ...}"))
  (assert (= (binding [*print-length* 10] (str {:foo "bar" :baz "woz"}))
             "{:foo \"bar\", :baz \"woz\"}"))

  ;; case keyword
  (assert (= (let [x "a"] (case x :a 1 "a")) "a"))

  :ok
  )
