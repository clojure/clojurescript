(ns cljs.externs-infer-tests
  (:require
    [cljs.analyzer :as ana]
    [cljs.analyzer-tests :refer [analyze collecting-warning-handler test-cenv]]
    [cljs.compiler :as comp]
    [cljs.closure :as closure]
    [cljs.env :as env]
    [cljs.externs :as externs]
    [cljs.test-util :refer [unsplit-lines]]
    [cljs.util :as util]
    [clojure.string :as string]
    [clojure.test :as test :refer [is are deftest testing]]))

(def externs-cenv
  (atom
    {::ana/externs
     (externs/externs-map
       (closure/load-externs
         {:externs ["src/test/externs/test.js"]}))}))

(def core-inferred
  ["var setTimeout;" "var process;" "process.hrtime;"
   "goog.isArrayLike;" "Java.type;" "Object.out;" "Object.out.println;"
   "Object.error;" "Object.error.println;"])

(deftest test-has-extern?-basic
  (let [externs (externs/externs-map
                  (closure/load-externs
                    {:externs ["src/test/externs/test.js"]
                     :use-only-custom-externs true}))]
    (is (true? (ana/has-extern? '[Foo] externs)))
    (is (true? (ana/has-extern? '[Foo wozMethod] externs)))
    (is (false? (ana/has-extern? '[foo] externs)))
    (is (false? (ana/has-extern? '[Foo gozMethod] externs)))
    (is (true? (ana/has-extern? '[baz] externs)))
    (is (false? (ana/has-extern? '[Baz] externs)))))

(deftest test-has-extern?-defaults
  (let [externs (externs/externs-map)]
    (is (true? (ana/has-extern? '[console] externs)))
    (is (true? (ana/has-extern? '[console log] externs)))
    (is (true? (ana/has-extern? '[Number isNaN] externs)))))

(deftest test-js-tag
  (let [externs (externs/externs-map
                  (closure/load-externs
                    {:externs ["src/test/externs/test.js"]}))]
    (is (= 'js/Console (ana/js-tag '[console] :tag externs)))
    (is (= 'js/Function (ana/js-tag '[console log] :tag externs)))
    (is (= 'js/Boolean (ana/js-tag '[Number isNaN] :ret-tag externs)))
    (is (= 'js/Foo (ana/js-tag '[baz] :ret-tag externs)))))

(defn infer-test-helper
  [{:keys [forms externs warnings warn js-dependency-index node-module-index with-core? opts]}]
  (let [test-cenv (atom
                    (cond->
                      (if with-core?
                        (env/default-compiler-env*
                          (closure/add-externs-sources (merge {:infer-externs true} opts)))
                        {::ana/externs
                         (externs/externs-map
                           (closure/load-externs {:externs (or externs [])}))})
                      js-dependency-index (assoc :js-dependency-index js-dependency-index)
                      node-module-index (assoc :node-module-index node-module-index)))
        wrap      (if with-core?
                    #(comp/with-core-cljs nil %)
                    #(do (%)))]
    (ana/with-warning-handlers [(collecting-warning-handler (or warnings (atom [])))]
      (binding [ana/*analyze-deps* false
                ana/*cljs-ns* ana/*cljs-ns*]
        (env/with-compiler-env test-cenv
          (wrap
            (fn []
              (binding [ana/*analyze-deps* true
                        ana/*cljs-warnings*
                        (assoc ana/*cljs-warnings*
                          :infer-warning (if (nil? warn) true warn))]
                (ana/analyze-form-seq forms))
              (with-out-str
                (comp/emit-externs
                  (reduce util/map-merge {}
                    (map (comp :externs second)
                      (get @test-cenv ::ana/namespaces))))))))))))

(deftest test-externs-infer
  (is (= 'js/Foo
        (-> (binding [ana/*cljs-ns* ana/*cljs-ns*]
              (env/with-compiler-env externs-cenv
                (analyze (ana/empty-env) 'js/baz)))
          :info :ret-tag)))
  (is (= 'js/Foo
        (-> (binding [ana/*cljs-ns* ana/*cljs-ns*]
              (env/with-compiler-env externs-cenv
                (analyze (ana/empty-env) '(js/baz))))
          :tag)))
  (is (= 'js
        (-> (binding [ana/*cljs-ns* ana/*cljs-ns*]
              (env/with-compiler-env externs-cenv
                (analyze (ana/empty-env) '(js/woz))))
          :tag)))
  (is (= 'js
        (-> (binding [ana/*cljs-ns* ana/*cljs-ns*]
              (env/with-compiler-env externs-cenv
                (analyze (ana/empty-env) '(def foo (js/woz)))))
          :tag)))
  (is (= 'js
        (-> (binding [ana/*cljs-ns* ana/*cljs-ns*]
              (env/with-compiler-env externs-cenv
                (analyze (ana/empty-env) '(def foo js/boz))))
          :tag)))
  (is (nil? (-> (binding [ana/*cljs-ns* ana/*cljs-ns*]
                  (ana/no-warn
                    (env/with-compiler-env externs-cenv
                      (analyze (ana/empty-env)
                        '(let [z (.baz ^js/Foo.Bar x)]
                           z)))))
              :tag meta :prefix))))

(deftest test-basic-infer
  (let [res (infer-test-helper
              {:forms '[(ns foo.core)
                        (defn bar [a] (js/parseInt a))
                        (def c js/React.Component)
                        (js/console.log "Hello world!")
                        (fn [& args]
                          (.apply (.-log js/console) js/console (into-array args)))
                        (js/console.log js/Number.MAX_VALUE)
                        (js/console.log js/Symbol.iterator)]})]
    (is (= (unsplit-lines ["var React;" "React.Component;"]) res))))

(deftest test-method-infer
  (let [res (infer-test-helper
              {:forms '[(defn foo [^js/React.Component c]
                          (.render c))]})]
    (is (= (unsplit-lines ["var React;" "React.Component;" "React.Component.prototype.render;"])
          res))))

(deftest test-minimal-infer
  (let [res (infer-test-helper
              {:forms '[(js/console.log (.wozMethod (js/baz)))]
               :externs ["src/test/externs/test.js"]})]
    (is (string/blank? res))))

(deftest test-type-hint-minimal-infer
  (let [res (infer-test-helper
              {:forms ''[(defn afun [^js/Foo x]
                           (.wozMethod x))]
               :externs ["src/test/externs/test.js"]})]
    (is (string/blank? res))))

(deftest test-type-hint-infer-unknown-method-in-chain
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(defn afun [^js/Foo.Bar x]
                          (let [z (.baz x)]
                            (.wozz z)))]
               :externs ["src/test/externs/test.js"]
               :warnings ws})]
    (is (= (unsplit-lines ["Foo.Boo.prototype.wozz;"]) res))
    (is (= 1 (count @ws)))
    (is (string/starts-with?
          (first @ws)
          "Cannot resolve property wozz for inferred type js/Foo.Boo"))))

(deftest test-type-hint-infer-unknown-property-in-chain
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(defn afun [^js/Foo.Bar x]
                          (let [z (.baz x)]
                            (.-wozz z)))]
               :externs ["src/test/externs/test.js"]
               :warnings ws})]
    (is (= (unsplit-lines ["Foo.Boo.prototype.wozz;"]) res))
    (is (= 1 (count @ws)))
    (is (string/starts-with?
          (first @ws)
          "Cannot resolve property wozz for inferred type js/Foo.Boo"))))

(deftest test-type-hint-infer-unknown-method
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(defn baz [^js/Foo a]
                          (.gozMethod a))]
               :externs ["src/test/externs/test.js"]
               :warnings ws})]
    (is (= (unsplit-lines ["Foo.prototype.gozMethod;"]) res))
    (is (= 1 (count @ws)))
    (is (string/starts-with?
          (first @ws)
          "Cannot resolve property gozMethod for inferred type js/Foo"))))

(deftest test-infer-unknown-method-from-externs
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(.gozMethod (js/baz))]
               :externs ["src/test/externs/test.js"]
               :warnings ws})]
    (is (= (unsplit-lines ["Foo.prototype.gozMethod;"]) res))
    (is (= 1 (count @ws)))
    (is (string/starts-with?
          (first @ws)
          "Cannot resolve property gozMethod for inferred type js/Foo"))))

(deftest test-infer-js-require
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns foo.core)
                        (def React (js/require "react"))
                        (.log js/console (.-Component React))]
               :externs ["src/test/externs/test.js"]
               :warnings ws})]
    (is (= (unsplit-lines ["var require;" "Object.Component;"]) res))
    (is (= 1 (count @ws)))
    (is (string/starts-with?
          (first @ws)
          "Adding extern to Object for property Component"))))

(deftest test-set-warn-on-infer
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns warn-on-infer-test.app)
                        (set! *warn-on-infer* true)
                        (defn wrap-baz [x]
                          (.baz x))]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :warn false
               :with-core? true})]
    (is (= 1 (count @ws)))
    (is (string/starts-with? (first @ws) "Cannot infer target type"))))

(deftest test-cljs-1970-infer-with-cljs-literals
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-1970.core)
                        (set! *warn-on-infer* true)
                        (defn foo [] (list))
                        (defn bar [] (vector))]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :with-core? true})]
    (is (zero? (count @ws)))))

(deftest test-cljs-1918-infer-with-case-keywords
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-1918.core)
                        (defn foo [x]
                          (cljs.core/case x
                            :foo 1
                            nil))]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :with-core? true})]
    (is (zero? (count @ws)))))

(deftest test-cljs-2247
  (let [ws (atom [])]
    (try
      (ana/with-warning-handlers [(collecting-warning-handler ws)]
        (env/with-compiler-env (assoc @test-cenv :repl-env {})
          (ana/analyze (ana/empty-env)
            '(defn -foo []))
          (ana/analyze (ana/empty-env)
            '(defprotocol IAlpha (-foo [this])))))
      (catch Exception _))
    (is (= ["Protocol IAlpha is overwriting function -foo"] @ws))))

(deftest test-cljs-2385-infer-priority
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-1918.core)
                        (defn thing [{:as this}]
                          (.componentDidUpdate ^js/Thing this))]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :with-core? true})]
    (is (string/includes? res "Thing.prototype.componentDidUpdate;"))
    (is (zero? (count @ws)))))

(deftest test-cljs-2392-broken-inferred-externs
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-1918.core
                          (:require [cljs.nodejs]
                                    [cljs.nodejscli]))]
               :warnings ws
               :with-core? true
               :opts {:target :nodejs}})]
    (not (string/includes? res "COMPILED"))
    (not (string/includes? res "goog"))
    (is (zero? (count @ws)))))

(deftest test-cljs-2678-global-exports-infer
  (let [ws  (atom [])
        res (infer-test-helper
              {:js-dependency-index {"react" {:global-exports '{react React}}}
               :forms '[(ns foo.core
                          (:require [react :as react]))
                        (.log js/console react/Component)]
               :warnings ws
               :warn false})]
    (is (= (unsplit-lines ["Object.Component;"]) res))))

(deftest test-cljs-2767-deftype-defrecord
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-2767.core)
                        (defrecord Foo [])]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :with-core? true})]
    (is (empty? @ws))
    (is (not (string/includes? res "cljs.core"))))
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-2767.core)
                        (deftype Foo [])]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :with-core? true})]
    (is (empty? @ws))
    (is (not (string/includes? res "cljs.core")))))

(deftest test-cljs-2790-defrecord-fields
  (let [ws  (atom [])
        res (infer-test-helper
              {:forms '[(ns cjls-2790.core)
                        (defrecord Foo [a b])]
               :externs ["src/test/externs/test.js"]
               :warnings ws
               :with-core? true})]
    (is (empty? @ws))
    (is (not (string/includes? res "cljs.core")))))

(deftest test-cljs-3181
  (let [ws  (atom [])
        res (binding [ana/*cljs-static-fns* true]
              (infer-test-helper
                {:forms '[(ns warn-on-infer-test.app)
                          (set! *warn-on-infer* true)
                          (defn f [gfn]
                            (.then ^js/Promise (gfn (inc 1)) identity))]
                 :externs ["src/test/externs/test.js"]
                 :warnings ws
                 :warn false
                 :with-core? true}))]
    (is (empty? @ws))))

(deftest test-cljs-1924
  (let [ws  (atom [])
        res (binding [ana/*cljs-static-fns* true]
              (infer-test-helper
                {:forms '[(set! *warn-on-infer* true)
                          (defrecord Foo [])]
                 :warnings ws
                 :warn true
                 :with-core? true}))]
    (is (empty? @ws))))

(deftest test-cljs-2862
  (let [ws  (atom [])
        res (binding [ana/*cljs-static-fns* true]
              (infer-test-helper
                {:forms '[(ns demo.app)
                          (set! *warn-on-infer* true)
                          (deftype Foo []
                            Object
                            (bar [this] :bar))]
                 :warnings ws
                 :warn true
                 :with-core? true}))]
    (is (empty? @ws))))

(deftest test-cljs-2957
  (let [ws  (atom [])
        res (binding [ana/*cljs-static-fns* true]
              (infer-test-helper
                {:forms '[(ns test.foo
                            (:import [goog.history Html5History]))
                          (set! *warn-on-infer* true)
                          (doto (Html5History.)
                            (.setUseFragment false))]
                 :warnings ws
                 :warn true
                 :with-core? true}))]
    (is (empty? @ws))))

(deftest test-cljs-3236
  (let [ws  (atom [])
        res (binding [ana/*cljs-static-fns* true]
              (infer-test-helper
                {:forms '[(ns test.foo)
                          (set! *warn-on-infer* true)
                          (defprotocol IFoo
                            (bar [this]))]
                 :warnings ws
                 :warn true
                 :with-core? true}))]
    (is (empty? @ws))))

(deftest test-cljs-3257
  (let [ws  (atom [])
        res (binding [ana/*cljs-static-fns* true]
              (infer-test-helper
                {:forms '[(ns app.core)
                          (set! *warn-on-infer* true)
                          (defprotocol IFoo
                            (bar [this]))
                          (defn not-ok? [v]
                            (satisfies? IFoo v))]
                 :warnings ws
                 :warn true
                 :with-core? true}))]
    (is (empty? @ws))))

(deftest test-cljs-3373
  (testing "var from foreign libraries that are invoked as fn should propagate 'js hints"
    (let [ws  (atom [])
          res (infer-test-helper
                {:js-dependency-index {"firebase" {:global-exports '{firebase Firebase}}}
                 :forms '[(ns foo.core
                            (:require [firebase :refer [getAuth]]))
                          (def auth
                            (doto (getAuth)
                              (.useDeviceLanguage)
                              (.onAuthStateChanged (fn [user]))))]
                 :warnings ws
                 :warn true
                 :with-core? false})]
      (is (= (unsplit-lines
               ["Object.getAuth;"
                "Object.useDeviceLanguage;"
                "Object.onAuthStateChanged;"])
              res)))))

(deftest test-cljs-3377
  (testing "constructors from foreign libraries that used via `new` should propagate 'js hints"
    (let [ws  (atom [])
          res (infer-test-helper
                {:js-dependency-index {"firebase" {:global-exports '{firebase Firebase}}}
                 :forms '[(ns foo.core
                            (:require [firebase :refer [GoogleAuthProvider]]))
                          (def goog-provider
                            (GoogleAuthProvider.))
                          (.someMethod goog-provider)
                          (.-someProperty goog-provider)]
                 :warnings ws
                 :warn true
                 :with-core? false})]
      (is (= (unsplit-lines
               ["Object.GoogleAuthProvider;"
                "Object.someMethod;"
                "Object.someProperty;"])
            res)))))

(deftest test-cljs-3381
  (testing "invokeable js namespaces not hinted properly"
    (let [ws  (atom [])
          res (infer-test-helper
                {:node-module-index #{"markdown-it"}
                 :forms '[(ns foo.core
                            (:require [markdown-it]))
                          (defonce mdi
                            (doto (new markdown-it
                                    (js-obj
                                      "linkify" true
                                      "typographer" true))
                              (.renderInline mdi "hi")))]
                 :warnings ws
                 :warn true
                 :with-core? false
                 :target :nodejs})]
      (is (= (unsplit-lines
               ["Object.renderInline;"])
            res)))))

(deftest test-cljs-3408
  (testing "inheritance of JS Types is inferred"
    (let [ws  (atom [])
          res (infer-test-helper
                {:forms '[(ns foo.core)
                          (.querySelectorAll js/document "div")]
                 :warnings ws
                 :warn true
                 :with-core? true})]
      (is (empty? @ws)))))

(comment
  (binding [ana/*cljs-ns* ana/*cljs-ns*]
    (ana/no-warn
      (env/with-compiler-env externs-cenv
        (analyze (ana/empty-env)
          '(let [React (js/require "react")]
             React)))))

  ;; FIXME: we don't preserve tag information
  (binding [ana/*cljs-ns* ana/*cljs-ns*]
    (ana/no-warn
      (env/with-compiler-env externs-cenv
        (let [aenv (ana/empty-env)
              _ (analyze aenv '(ns foo.core))
              aenv' (assoc-in aenv [:ns :name] 'foo.core)
              _ (ana/analyze aenv' '(def x 1))]
          (dissoc (ana/analyze-symbol (assoc-in aenv [:ns :name] 'foo.core) 'x) :env)
          ;(get-in @externs-cenv [::ana/namespaces 'foo.core])
          ))))
  )
