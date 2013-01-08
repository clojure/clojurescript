(ns cljs.test.quick-tests)

(defmacro quick-mac [x] x)
(defmacro quick-unless [pred a b] `(if (not ~pred) ~a ~b))

(defn run-tests []

  ;; Function reader
  (assert (= '(1) (#(list %1) 1)))
  (assert (= '(:a) (#(list %1) :a)))
  (assert (= 11 (#(+ 1 %) 10)))

  ;; Keywords
  (assert (= ':foo (keyword "foo")))
  (assert (= ':bar/foo (keyword "bar" "foo")))
  (assert (= *ns-sym* (symbol (namespace ::foo))))

  ;; symbols
  (assert (= 'mysym (symbol "mysym")))
  (assert (= 'bar/mysym (symbol "bar" "mysym")))
  (assert (= true (symbol? 'blah)))
  (assert (= true (symbol? (symbol "blah"))))
  (assert (= false (symbol? :blah)))
  (assert (= 'blah (with-meta 'blah {:a 1})))
  (assert (= {:a 1} (meta (with-meta 'blah {:a 1}))))
  (assert (= "cljs.user" (namespace (with-meta 'cljs.user/foo {:a 1}))))
  (assert (= "cljs.user" (namespace (with-meta (with-meta 'cljs.user/foo {:a 1}) {:b 2}))))

  ;; regex
  (assert (= '("/a" "/d" "/g") (re-seq #"/." "/abc/def/ghi")))
  (assert (= 1 (count (take 2 (re-seq #"^$" "")))))


  ;; arrays
  (assert (= '(3 6 9 12) (seq (let [a (array 1 2 3 4)] (amap a i ret (* 3 (aget a i)))))))
  (assert (= 25 (let [a (array 1 2 3 4 5)] (areduce a i ret 10 (+ ret (aget a i))))))


  ;; Macros
  (assert (= 5 (quick-mac 5)))
  
  (assert (= :yep (quick-unless false :yep :nope)))
  (assert (= :nope (quick-unless true :yep :nope)))


  ;; destructuring
  (assert (= [4 3 2 1] (let [[a b] [1 2] {:keys [c d]} {:c 3 :d 4}] [d c b a])))
  (assert (= [2 1] ((fn [a b] [b a]) 1 2)))
  (assert (= [2 1] ((fn [[a b]] [b a]) [1 2])))
  (assert (= [20 10] ((fn [{:keys [a b]}] [b a]) {:a 10 :b 20})))


  ;; with-out-str, time, regex, for
  (assert (= "01234" (with-out-str (doseq [n (range 5)] (print n)))))
  
  ;; try, throw
  (assert (= 3 (try (+ 1 2) (catch js/Error e :exc))))
  (assert (= :exc (try (throw (js/Error. "err")) (catch js/Error e :exc))))

  ;; ..
  (assert (= "6" (.. (array 4 5 6) (pop) (toString))))

  ;; doto
  (assert (= "b" (aget (doto (new js/Array) (.push "a") (.push "b")) 1)))

  ;; memfn
  (assert (= '(:a "1" "[object Object]") (map (memfn toString) [:a 1 (new js/Object)])))

  ;; letfn
  (assert (= [20 300] (letfn [(twice [x] (* 2 x)) (thrice [x] (* 3 x))] [(twice 10) (thrice 100)])))

  ;; for
  (assert (= '([1 2] [3 6]) (for [x [1 2 3 4] :let [y (* x 2)] :when (odd? x)] [x y])))

  ;; while
  (def w-a (atom 5))
  (assert (= "5\n4\n3\n2\n1\n" (with-out-str (while (pos? @w-a) (do (println @w-a) (swap! w-a dec))))))

  ;; doseq
  (assert (= "[1 2]\n[3 6]\n" (with-out-str (doseq [x [1 2 3 4] :let [y (* x 2)] :when (odd? x)] (println [x y])))))

  ;; dotimes
  (assert (= "01234" (with-out-str (dotimes [n 5] (print n)))))

  ;; delay
  (assert (= 50 @(delay (* 10 5))))

  ;; lazy-seq
  (defn positive-numbers ([] (positive-numbers 1)) ([n] (cons n (lazy-seq (positive-numbers (inc n))))))
  (assert (= '(1 2 3 4 5) (take 5 (positive-numbers))))

  ;; condp
  (defn tcp [v] (condp = v 1 "one" 2 "two" 3 "three" (str v " ???")))
  (assert (= "three" (tcp 3)))
  (assert (= "4 ???" (tcp 4)))

  ;; case
  (defn tc [v] (case v "" "empty" "hello" "hello world" (str (count v) " chars")))
  (assert (= "empty" (tc "")))
  (assert (= "hello world" (tc "hello")))
  (assert (= "4 chars" (tc "blah")))
  (defn tc2 [v] (case v (1 3 5 7 9) "odd" (0 2 4 6 8) "even"))
  (assert (= "odd" (tc2 1)))
  (assert (= "even" (tc2 2)))
  (assert (= :exc (try (tc2 10) (catch js/Error e :exc))))


  ;; deftype, defrecord, defprotocol
  (deftype MyType [] INext (-next [_] "mytype next"))
  (assert (= "mytype next" (next (MyType.))))
  (deftype Fn-ish [a] IFn (-invoke [_] a))
  (assert (= 1 ((Fn-ish. 1))))

  (defrecord Banana [qty])
  (defprotocol Fruit (-subtotal [item]))
  (extend-type Banana Fruit (-subtotal [item] (* 158 (:qty item))))
  (def myb (Banana. 10))
  (assert (= 10 (:qty myb)))
  (assert (= true (satisfies? Fruit myb)))
  (assert (= 1580 (-subtotal myb)))

  ;; extend-protocol
  (defprotocol Wonderful (-wonder [x]))
  (extend-protocol Wonderful
    nil (-wonder [x] "What a wonderful protocol!"))
  (assert (= "What a wonderful protocol!" (-wonder nil)))

  ;; reify
  (assert (= "foo-reified" (str (let [f "foo"] (reify Object (toString [this] (str f "-reified")))))))
  (assert (= "reified ISeqable" (seq (reify ISeqable (-seq [_] "reified ISeqable")))))


  ;; multimethods
  (defmulti bar (fn [x y] (type x)))
  (defmethod bar cljs.core/Symbol [x y] (str "got a symbol and " y))
  (defmethod bar cljs.core/PersistentVector[x y] (str "got a vector and " y))
  (assert (= "got a symbol and 1" (bar 'blah 1)))
  (assert (= "got a vector and 2" (bar [] 2)))

  ;; with-out-str, time, regex, for
  (re-find #"Elapsed time: \d* msecs" (with-out-str (time (apply str (for [n (range 100)] (with-out-str (print (format "%d/" n))))))))

  (println "All tests finished successfully"))
