(ns cljs.type-inference-tests
  (:require
    [cljs.analyzer :as ana]
    [cljs.analyzer-tests :refer [analyze test-env test-cenv]]
    [cljs.env :as env]
    [cljs.test-util :refer [unsplit-lines]]
    [clojure.test :refer [is are deftest testing]]))

(deftest basic-inference
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '1)))
        'number))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '"foo")))
        'string))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '\a)))
        'string))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(make-array 10))))
        'array))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(js-obj))))
        'object))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '[])))
        'cljs.core/IVector))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '{})))
        'cljs.core/IMap))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '#{})))
        'cljs.core/ISet))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env ())))
        'cljs.core/IList))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(fn [x] x))))
        'function))
  (is (= (env/with-compiler-env test-cenv
           (ana/no-warn
             (:tag (analyze test-env '(Foo.)))))
        'cljs.core/Foo)))

(deftest if-inference
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (analyze test-env '(if x "foo" 1)))))
        '#{number string})))

(deftest if-induced-inference
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (ana/analyze test-env '(let [x ^any []] (if (nil? x) x :kw))))))
        '#{clj-nil cljs.core/Keyword}))
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (ana/analyze test-env '(let [x ^any []] (if (boolean? x) x :kw))))))
        '#{boolean cljs.core/Keyword}))
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (ana/analyze test-env '(let [x ^any []] (if (number? x) x :kw))))))
        '#{number cljs.core/Keyword}))
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (ana/analyze test-env '(let [x ^any []] (if (double? x) x :kw))))))
        '#{number cljs.core/Keyword}))
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (ana/analyze test-env '(let [x ^any []] (if (float? x) x :kw))))))
        '#{number cljs.core/Keyword}))
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (ana/analyze test-env '(let [x ^any []] (if (integer? x) x :kw))))))
        '#{number cljs.core/Keyword}))
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (ana/analyze test-env '(let [x ^any []] (if (seq? x) x :kw))))))
        '#{seq cljs.core/Keyword}))
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (ana/analyze test-env '(let [x ^any []] (if (array? x) x :kw))))))
        '#{array cljs.core/Keyword}))
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (ana/analyze test-env '(let [x ^any []] (if (seqable? x) x :kw))))))
        '#{cljs.core/ISeqable array string cljs.core/Keyword}))
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (ana/analyze test-env '(let [x (namespace :x)] (if x x :kw))))))
        '#{string cljs.core/Keyword})))

(deftest loop-recur-inference
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (analyze test-env '(loop [x "a"] x)))))
        'string))
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (analyze test-env '(loop [x 10]
                                        (if (pos? x)
                                          (dec x)
                                          x))))))
        'number))
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (analyze test-env '((fn [p?]
                                         (loop [x nil]
                                           (if (p? x)
                                             x
                                             (recur (str x)))))
                                       11)))))
        '#{string clj-nil}))
  (is (= (ana/no-warn
           (env/with-compiler-env test-cenv
             (:tag (analyze test-env '((fn [^string x]
                                         (loop [y x]
                                           (if (= "x" y)
                                             y
                                             (recur 1))))
                                       "a")))))
        '#{number string})))

(deftest method-inference
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(.foo js/bar))))
        'js)))

(deftest fn-method-inference
  ;; should always infer 'function as tag
  (is (= 'function
        (:tag
          (env/with-compiler-env test-cenv
            (analyze test-env
              '(fn ([a] 1) ([a b] "foo") ([a b & r] ())))))))
  (is (nil?
        (:ret-tag
          (env/with-compiler-env test-cenv
            (analyze test-env
              '(fn ([a] 1) ([a b] "foo") ([a b & r] ())))))) )
  ;; methods should have inferred types
  (is (= '(number string cljs.core/IList)
        (map :tag
          (:methods
            (env/with-compiler-env test-cenv
              (analyze test-env
                '(fn ([a] 1) ([a b] "foo") ([a b & r] ())))))))))

(deftest fn-inference
  (is (= 'number
        (env/with-compiler-env test-cenv
          (:tag (analyze test-env
                  '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
                     (x :one)))))))
  (is (= 'string
        (env/with-compiler-env test-cenv
          (:tag (analyze test-env
                  '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
                     (x :one :two)))))))
  (is (= 'cljs.core/IList
        (env/with-compiler-env test-cenv
          (:tag (analyze test-env
                  '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
                     (x :one :two :three)))))))
  (is (= 'cljs.core/IList
        (env/with-compiler-env test-cenv
          (:tag (analyze test-env
                  '(let [x (fn ([a] 1) ([a b] "foo") ([a b & r] ()))]
                     (x :one :two :three :four))))))))

(deftest top-fn-inference
  (env/with-compiler-env test-cenv
    (ana/analyze-form-seq
      '[(ns test.cljs-2901)
        (defn foo
          ([a] 1)
          ([a b] "foo")
          ([a b & r] ()))
        (foo :one)]))
  (is (= '[number string cljs.core/IList]
        (map :tag
          (get-in @test-cenv [::ana/namespaces 'test.cljs-2901 :defs 'foo :methods]))))
  (is (= 'number
        (:tag
          (env/with-compiler-env test-cenv
            (ana/analyze-form-seq
              '[(ns test.cljs-2901)
                (defn foo
                  ([a] 1)
                  ([a b] "foo")
                  ([a b & r] ()))
                (foo :one)]
              nil true)))))
  (is (= 'string
        (:tag
          (env/with-compiler-env test-cenv
            (ana/analyze-form-seq
              '[(ns test.cljs-2901)
                (defn foo
                  ([a] 1)
                  ([a b] "foo")
                  ([a b & r] ()))
                (foo :one :two)]
              nil true)))))
  (is (= 'cljs.core/IList
        (:tag
          (env/with-compiler-env test-cenv
            (ana/analyze-form-seq
              '[(ns test.cljs-2901)
                (defn foo
                  ([a] 1)
                  ([a b] "foo")
                  ([a b & r] ()))
                (foo :one :two :three)]
              nil true))))))

(deftest variadic-fn-inference
  (is (= '(cljs.core/IList)
        (map :tag
          (:methods
            (env/with-compiler-env test-cenv
              (analyze test-env
                '(fn ([a b & r] ()))))))))
  (is (= 'cljs.core/IList
        (env/with-compiler-env test-cenv
          (:tag (analyze test-env
                  '(let [x (fn ([a b & r] ()))]
                     (x :one :two)))))))

  (is (= 'cljs.core/IList
        (env/with-compiler-env test-cenv
          (:tag (analyze test-env
                  '(let [x (fn ([a b & r] ()))]
                     (x :one :two :three)))))))

  (is (= 'cljs.core/IList
        (env/with-compiler-env test-cenv
          (:tag (analyze test-env
                  '(let [x (fn ([a b & r] ()))]
                     (x :one :two :three :four)))))))
  )

(deftest top-variadic-fn-inference
  (env/with-compiler-env test-cenv
    (ana/analyze-form-seq
      '[(ns test.cljs-2901-b)
        (defn foo ([a b & r] ()))
        (foo :one :two :three :four)]
      nil false))
  (is (= '[cljs.core/IList]
        (map :tag
          (get-in @test-cenv
            [::ana/namespaces 'test.cljs-2901-b :defs 'foo :methods]))))
  (is (= 'cljs.core/IList
        (:tag
          (env/with-compiler-env test-cenv
            (ana/analyze-form-seq
              '[(ns test.cljs-2901-b)
                (defn foo ([a b & r] ()))
                (foo :one :two)]
              nil true)))))
  (is (= 'cljs.core/IList
        (:tag
          (env/with-compiler-env test-cenv
            (ana/analyze-form-seq
              '[(ns test.cljs-2901-b)
                (defn foo ([a b & r] ()))
                (foo :one :two :three)]
              nil true)))))
  (is (= 'cljs.core/IList
        (:tag
          (env/with-compiler-env test-cenv
            (ana/analyze-form-seq
              '[(ns test.cljs-2901-b)
                (defn foo ([a b & r] ()))
                (foo :one :two :three :four)]
              nil true))))))

(deftest lib-inference
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(+ 1 2))))
        'number))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(alength (array)))))
        'number))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(aclone (array)))))
        'array))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(-count [1 2 3]))))
        'number))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(count [1 2 3]))))
        'number))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(into-array [1 2 3]))))
        'array))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(js-obj))))
        'object))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(-conj [] 1))))
        'clj))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(conj [] 1))))
        'clj))
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(dissoc {:foo :bar} :foo))))
        '#{clj clj-nil}))
  ;; has changed, why does this return #{clj any} ?
  ;(is (= (env/with-compiler-env test-cenv
  ;         (:tag (analyze test-env '(assoc nil :foo :bar))))
  ;      'clj))
  )

(deftest test-always-true-if
  (is (= (env/with-compiler-env test-cenv
           (:tag (analyze test-env '(if 1 2 "foo"))))
        'number)))

;; will only work if the previous test works
(deftest test-count
  (is (= (cljs.env/with-compiler-env test-cenv
           (:tag (analyze test-env '(count []))))
        'number))
  )

(deftest test-numeric
  (is (= (ana/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (analyze test-env '(dec x)))))
        'number))
  (is (= (ana/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (analyze test-env '(int x)))))
        'number))
  (is (= (ana/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (analyze test-env '(unchecked-int x)))))
        'number))
  (is (= (ana/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (analyze test-env '(mod x y)))))
        'number))
  (is (= (ana/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (analyze test-env '(quot x y)))))
        'number))
  (is (= (ana/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (analyze test-env '(rem x y)))))
        'number))
  (is (= (ana/no-warn
           (cljs.env/with-compiler-env test-cenv
             (:tag (analyze test-env '(bit-count n)))))
        'number))
  )

(deftest test-ctor-infer
  (is (= 'cljs.core/Foo
         (:tag
           (env/with-compiler-env test-cenv
             (ana/no-warn
               (analyze test-env
                 '(let [g (Foo.)]
                    g))))))))

(deftest test-goog-import-ctor-infer
  (is (= 'goog.history/Html5History
         (:tag
           (env/with-compiler-env (env/default-compiler-env)
             (ana/analyze-form-seq
               '[(ns test.foo
                   (:import [goog.history Html5History]))
                 (Html5History.)]
               {} true))))))
