;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns
^{:author "Stuart Sierra, with contributions and suggestions by 
  Chas Emerick, Allen Rohner, Stuart Halloway, David Nolen, and
  Leon Grapenthin",
     :doc "A unit testing framework.

   ASSERTIONS

   The core of the library is the \"is\" macro, which lets you make
   assertions of any arbitrary expression:

   (is (= 4 (+ 2 2)))
   (is (instance? Integer 256))
   (is (.startsWith \"abcde\" \"ab\"))

   You can type an \"is\" expression directly at the REPL, which will
   print a message if it fails.

       user> (is (= 5 (+ 2 2)))

       FAIL in  (:1)
       expected: (= 5 (+ 2 2))
         actual: (not (= 5 4))
       false

   The \"expected:\" line shows you the original expression, and the
   \"actual:\" shows you what actually happened.  In this case, it
   shows that (+ 2 2) returned 4, which is not = to 5.  Finally, the
   \"false\" on the last line is the value returned from the
   expression.  The \"is\" macro always returns the result of the
   inner expression.

   There are two special assertions for testing exceptions.  The
   \"(is (thrown? c ...))\" form tests if an exception of class c is
   thrown:

   (is (thrown? ArithmeticException (/ 1 0))) 

   \"(is (thrown-with-msg? c re ...))\" does the same thing and also
   tests that the message on the exception matches the regular
   expression re:

   (is (thrown-with-msg? ArithmeticException #\"Divide by zero\"
                         (/ 1 0)))

   DOCUMENTING TESTS

   \"is\" takes an optional second argument, a string describing the
   assertion.  This message will be included in the error report.

   (is (= 5 (+ 2 2)) \"Crazy arithmetic\")

   In addition, you can document groups of assertions with the
   \"testing\" macro, which takes a string followed by any number of
   assertions.  The string will be included in failure reports.
   Calls to \"testing\" may be nested, and all of the strings will be
   joined together with spaces in the final report, in a style
   similar to RSpec <http://rspec.info/>

   (testing \"Arithmetic\"
     (testing \"with positive integers\"
       (is (= 4 (+ 2 2)))
       (is (= 7 (+ 3 4))))
     (testing \"with negative integers\"
       (is (= -4 (+ -2 -2)))
       (is (= -1 (+ 3 -4)))))

   Note that, unlike RSpec, the \"testing\" macro may only be used
   INSIDE a \"deftest\" form (see below).


   DEFINING TESTS

   (deftest addition
     (is (= 4 (+ 2 2)))
     (is (= 7 (+ 3 4))))

   (deftest subtraction
     (is (= 1 (- 4 3)))
     (is (= 3 (- 7 4))))

   This creates functions named \"addition\" and \"subtraction\", which
   can be called like any other function.  Therefore, tests can be
   grouped and composed, in a style similar to the test framework in
   Peter Seibel's \"Practical Common Lisp\"
   <http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html>

   (deftest arithmetic
     (addition)
     (subtraction))

   The names of the nested tests will be joined in a list, like
   \"(arithmetic addition)\", in failure reports.  You can use nested
   tests to set up a context shared by several tests.

   DEFINING ASYNC TESTS

   (deftest addition
     (async done
       (is (= 4 (+ 2 2)))
       (is (= 7 (+ 3 4)))
       (done)))

   Async tests are constructed with the async macro. The first argument to
   the macro is the test completion callback. The body of the async macro may
   be any series of expressions. The completion callback must be invoked when
   all assertions have run. There is no support for asynchronous coordination -
   core.async is recommended for this. Note the body of the async test must be
   truly asynchronous to avoid stack overflow.

   RUNNING TESTS

   Run tests with the function \"(run-tests namespaces...)\":

   (run-tests 'your.namespace 'some.other.namespace)

   If you don't specify any namespaces, the current namespace is
   used.  To run all tests in all namespaces, use \"(run-all-tests)\".

   By default, these functions will search for all tests defined in
   a namespace and run them in an undefined order.  However, if you
   are composing tests, as in the \"arithmetic\" example above, you
   probably do not want the \"addition\" and \"subtraction\" tests run
   separately.  In that case, you must define a special function
   named \"test-ns-hook\" that runs your tests in the correct order:

   (defn test-ns-hook []
     (arithmetic))

   \"run-tests\" also optionally takes a testing enviroment. A default
   one is supplied for you by invoking \"empty-env\".  The test
   environment contains everything needed to run tests including the
   report results map. Fixtures must be present here if you want them
   to run. Note that code that relies on \"test-ns\" will
   automatically be supplied the appropriate defined fixtures.  For
   example, this is done for you if you use \"run-tests\".

   Note: test-ns-hook prevents execution of fixtures (see below).


   OMITTING TESTS FROM PRODUCTION CODE

   You can set the ClojureScript compiler build option
   \":load-tests\" to false when loading or compiling code in
   production.  This will prevent any tests from being created by
   or \"deftest\".


   FIXTURES

   Fixtures allow you to run code before and after tests, to set up
   the context in which tests should be run.

   A fixture is a map of one or two functions that run code before and
   after tests.  It looks like this:

   {:before (fn []
              Perform setup, establish bindings, whatever.
              )
    :after (fn []
             Tear-down / clean-up code here.
             )}

   Both are optional and can be left out.

   Fixtures are attached to namespaces in one of two ways.  \"each\"
   fixtures are run repeatedly, once for each test function created
   with \"deftest\".  \"each\" fixtures are useful for
   establishing a consistent before/after state for each test, like
   clearing out database tables.

   \"each\" fixtures can be attached to the current namespace like this:
   (use-fixtures :each fixture1 fixture2 ...)
   The fixture1, fixture2 are just maps like the example above.
   They can also be passed directly, like this:
   (use-fixtures :each
     {:before (fn [] setup...), :after (fn [] cleanup...)})

   The other kind of fixture, a \"once\" fixture, is only run once,
   around ALL the tests in the namespace.  \"once\" fixtures are useful
   for tasks that only need to be performed once, like establishing
   database connections, or for time-consuming tasks.

   Attach \"once\" fixtures to the current namespace like this:
   (use-fixtures :once fixture1 fixture2 ...)

   Note: Fixtures and test-ns-hook are mutually incompatible.  If you
   are using test-ns-hook, fixture functions will *never* be run.


   WRAPPING FIXTURES

   Instead of a map, a fixture can be specified like this:

   (defn my-fixture [f]
      Perform setup, establish bindings, whatever.
     (f)  Then call the function we were passed.
      Tear-down / clean-up code here.
    )

   This style is incompatible with async tests. If an async test is
   encountered, testing will be aborted. It can't be mixed with
   fixtures specified as maps.


   EXTENDING TEST-IS (ADVANCED)

   You can extend the behavior of the \"is\" macro by defining new
   methods for the \"assert-expr\" multimethod.  These methods are
   called during expansion of the \"is\" macro, so they should return
   quoted forms to be evaluated.

   You can plug in your own test-reporting framework by specifying a
   :reporter key in the test environment. It is normally set to
   :cljs.test/default. Set this to the desired key and supply custom
   implementations of the \"report\" multimethod.

   The 'event' argument is a map.  It will always have a :type key,
   whose value will be a keyword signaling the type of event being
   reported.  Standard events with :type value of :pass, :fail, and
   :error are called when an assertion passes, fails, and throws an
   exception, respectively.  In that case, the event will also have
   the following keys:

     :expected   The form that was expected to be true
     :actual     A form representing what actually occurred
     :message    The string message given as an argument to 'is'

   The \"testing\" strings will be a list in the :testing-contexts
   property of the test environment, and the vars being tested will be
   a list in the :testing-vars property of the test environment.

   For additional event types, see the examples in the code.
"}
  cljs.test
  (:require-macros [clojure.template :as temp]
                   [cljs.test :as test])
  (:require [clojure.string :as string]
            [cljs.pprint :as pprint]))

;; =============================================================================
;; Default Reporting

(defn empty-env
  "Generates a testing environment with a reporter.
   (empty-env) - uses the :cljs.test/default reporter.
   (empty-env :cljs.test/pprint) - pretty prints all data structures. 
   (empty-env reporter) - uses a reporter of your choosing.

   To create your own reporter see cljs.test/report"
  ([] (empty-env ::default))
  ([reporter]
   (cond-> {:report-counters {:test 0 :pass 0 :fail 0 :error 0}
            :testing-vars ()
            :testing-contexts ()
            :formatter pr-str
            :reporter reporter}
     (= ::pprint reporter) (assoc :reporter ::default
                             :formatter pprint/pprint))))

(def ^:dynamic *current-env* nil)

(defn get-current-env []
  (or *current-env* (empty-env)))

(defn update-current-env! [ks f & args]
  (set! *current-env* (apply update-in (get-current-env) ks f args)))

(defn set-env! [new-env]
  (set! *current-env* new-env))

(defn clear-env! []
  (set! *current-env* nil))

(defn get-and-clear-env! []
  "Like get-current-env, but cleans env before returning."
  (let [env (cljs.test/get-current-env)]
    (clear-env!)
    env))

(defn testing-vars-str
  "Returns a string representation of the current test.  Renders names
  in *testing-vars* as a list, then the source file and line of
  current assertion."
  [m]
  (let [{:keys [file line column]} m]
    (str
      (reverse (map #(:name (meta %)) (:testing-vars (get-current-env))))
      " (" file ":" line (when column (str ":" column)) ")")))

(defn testing-contexts-str
  "Returns a string representation of the current test context. Joins
  strings in *testing-contexts* with spaces."
  []
  (apply str (interpose " " (reverse (:testing-contexts (get-current-env))))))

(defn inc-report-counter!
  "Increments the named counter in *report-counters*, a ref to a map.
  Does nothing if *report-counters* is nil."
  [name]
  (if (:report-counters (get-current-env))
    (update-current-env! [:report-counters name] (fnil inc 0))))

(defmulti
  ^{:doc "Generic reporting function, may be overridden to plug in
   different report formats (e.g., TAP, JUnit).  Assertions such as
   'is' call 'report' to indicate results.  The argument given to
   'report' will be a map with a :type key."
     :dynamic true}
  report (fn [m] [(:reporter (get-current-env)) (:type m)]))

(defmethod report :default [m])

(defmethod report [::default :pass] [m]
  (inc-report-counter! :pass))

(defn- print-comparison [m]
  (let [formatter-fn (or (:formatter (get-current-env)) pr-str)]
    (println "expected:" (formatter-fn (:expected m)))
    (println "  actual:" (formatter-fn (:actual m)))))

(defmethod report [::default :fail] [m]
  (inc-report-counter! :fail)
  (println "\nFAIL in" (testing-vars-str m))
  (when (seq (:testing-contexts (get-current-env)))
    (println (testing-contexts-str)))
  (when-let [message (:message m)] (println message))
  (print-comparison m))

(defmethod report [::default :error] [m]
  (inc-report-counter! :error)
  (println "\nERROR in" (testing-vars-str m))
  (when (seq (:testing-contexts (get-current-env)))
    (println (testing-contexts-str)))
  (when-let [message (:message m)] (println message))
  (print-comparison m))

(defmethod report [::default :summary] [m]
  (println "\nRan" (:test m) "tests containing"
    (+ (:pass m) (:fail m) (:error m)) "assertions.")
  (println (:fail m) "failures," (:error m) "errors."))

(defmethod report [::default :begin-test-ns] [m]
  (println "\nTesting" (name (:ns m))))

;; Ignore these message types:
(defmethod report [::default :end-test-ns] [m])
(defmethod report [::default :begin-test-var] [m]
  #_(println ":begin-test-var" (testing-vars-str m)))
(defmethod report [::default :end-test-var] [m])
(defmethod report [::default :end-run-tests] [m])
(defmethod report [::default :end-test-all-vars] [m])
(defmethod report [::default :end-test-vars] [m])

;; =============================================================================
;; File, Line, and Column Helpers

(defn js-line-and-column [stack-element]
  "Returns a 2-element vector containing the line and
  column encoded at the end of a stack element string.
  A line or column will be represented as NaN if not
  parsesable."
  (let [parts (.split stack-element ":")
        cnt   (count parts)]
    (if (> cnt 1)
      [(js/parseInt (nth parts (- cnt 2)) 10)
       (js/parseInt (nth parts (dec cnt)) 10)]
      [NaN NaN])))

(defn js-filename [stack-element]
  (let [output-dir (cljs.test/cljs-output-dir)
        output-dir (cond-> output-dir
                     (not (string/ends-with? output-dir "/"))
                     (str "/"))]
    (-> (.split stack-element output-dir)
      last
      (.split ":")
      first)))

(defn mapped-line-and-column [filename line column]
  (let [default [filename line column]]
    (if-let [source-map (:source-map (get-current-env))]
      ;; source maps are 0 indexed for lines
      (if-let [columns (get-in source-map [filename (dec line)])]
        (vec
          (map
            ;; source maps are 0 indexed for columns
            ;; multiple segments may exist at column
            ;; just take first
            (first
              (if-let [mapping (get columns (dec column))]
                mapping
                (second (first columns))))
            [:source :line :col]))
        default)
      default)))

(defn file-and-line [exception depth]
  ;; TODO: flesh out
  (if-let [stack-element (and (string? (.-stack exception))
                              (some-> (.-stack exception)
                                      string/split-lines
                                      (get depth)
                                      string/trim))]
    (let [fname (js-filename stack-element)
          [line column] (js-line-and-column stack-element)
          [fname line column] (mapped-line-and-column fname line column)]
      {:file fname :line line :column column})
    {:file (.-fileName exception)
     :line (.-lineNumber exception)}))

(defn do-report [m]
  (let [m (case (:type m)
            :fail (merge (file-and-line (js/Error.) 4) m)
            :error (merge (file-and-line (:actual m) 0) m)
            m)]
    (report m)))

;; =============================================================================
;; Async

(defprotocol IAsyncTest
  "Marker protocol denoting CPS function to begin asynchronous
  testing.")

(defn async?
  "Returns whether x implements IAsyncTest."
  [x]
  (satisfies? IAsyncTest x))

(defn run-block
  "Invoke all functions in fns with no arguments. A fn can optionally
  return

  an async test - is invoked with a continuation running left fns

  a seq of fns tagged per block - are invoked immediately after fn"
  [fns]
  (when-first [f fns]
    (let [obj (f)]
      (if (async? obj)
        (obj (let [d (delay (run-block (rest fns)))]
               (fn []
                 (if (realized? d)
                   (println "WARNING: Async test called done more than one time.")
                   @d))))
        (recur (cond->> (rest fns)
                 (::block? (meta obj)) (concat obj)))))))

(defn block
  "Tag a seq of fns to be picked up by run-block as injected
  continuation.  See run-block."
  [fns]
  (some-> fns
          (vary-meta assoc ::block? true)))

;; =============================================================================
;; Low-level functions

(defn- test-var-block*
  [v t]
  {:pre [(instance? Var v)]}
  [(fn []
     (update-current-env! [:testing-vars] conj v)
     (update-current-env! [:report-counters :test] inc)
     (do-report {:type :begin-test-var :var v})
     (try
       (t)
       (catch :default e
         (case e
           ::async-disabled (throw "Async tests require fixtures to be specified as maps.  Testing aborted.")
           (do-report
            {:type :error
             :message "Uncaught exception, not in assertion."
             :expected nil
             :actual e})))))
   (fn []
     (do-report {:type :end-test-var :var v})
     (update-current-env! [:testing-vars] rest))])

(defn test-var-block
  "Like test-var, but returns a block for further composition and
  later execution."
  [v]
  (if-let [t (:test (meta v))]
    (test-var-block* v t)))

(defn test-var
  "If v has a function in its :test metadata, calls that function,
  add v to :testing-vars property of env."
  [v]
  (run-block (test-var-block v)))

(defn- default-fixture
  "The default, empty, fixture function.  Just calls its argument.

  NOTE: Incompatible with map fixtures."
  [f]
  (f))

(defn compose-fixtures
  "Composes two fixture functions, creating a new fixture function
  that combines their behavior.

  NOTE: Incompatible with map fixtures."
  [f1 f2]
  (fn [g] (f1 (fn [] (f2 g)))))

(defn join-fixtures
  "Composes a collection of fixtures, in order.  Always returns a valid
  fixture function, even if the collection is empty.

  NOTE: Incompatible with map fixtures."
  [fixtures]
  (reduce compose-fixtures default-fixture fixtures))

(defn- wrap-map-fixtures
  "Wraps block in map-fixtures."
  [map-fixtures block]
  (concat (keep :before map-fixtures)
          block
          (reverse (keep :after map-fixtures))))

(defn- execution-strategy [once each]
  (letfn [(fixtures-type [coll]
            (cond
              (empty? coll) :none
              (every? map? coll) :map
              (every? fn? coll) :fn))
          (fixtures-types []
            (->> (map fixtures-type [once each])
              (remove #{:none})
              (distinct)))]
    (let [[type :as types] (fixtures-types)]
      (assert (not-any? nil? types)
        "Fixtures may not be of mixed types")
      (assert (> 2 (count types))
        "fixtures specified in :once and :each must be of the same type")
      ({:map :async :fn :sync} type :async))))

(defn- disable-async [f]
  (fn []
    (let [obj (f)]
      (when (async? obj)
        (throw ::async-disabled))
      obj)))

(defn test-vars-block
  "Like test-vars, but returns a block for further composition and
  later execution."
  [vars]
  (map
   (fn [[ns vars]]
     (fn []
       (block
        (let [env (get-current-env)
              once-fixtures (get-in env [:once-fixtures ns])
              each-fixtures (get-in env [:each-fixtures ns])]
          (case (execution-strategy once-fixtures each-fixtures)
            :async
            (->> vars
                 (filter (comp :test meta))
                 (mapcat (comp (partial wrap-map-fixtures each-fixtures)
                               test-var-block))
                 (wrap-map-fixtures once-fixtures))
            :sync
            (let [each-fixture-fn (join-fixtures each-fixtures)]
              [(fn []
                 ((join-fixtures once-fixtures)
                  (fn []
                    (doseq [v vars]
                      (when-let [t (:test (meta v))]
                        ;; (alter-meta! v update :test disable-async)
                        (each-fixture-fn
                         (fn []
                           ;; (test-var v)
                           (run-block
                            (test-var-block* v (disable-async t))))))))))]))))))
   (group-by (comp :ns meta) vars)))

(defn test-vars
  "Groups vars by their namespace and runs test-vars on them with
  appropriate fixtures assuming they are present in the current
  testing environment."
  [vars]
  (run-block (concat (test-vars-block vars)
                     [(fn []
                        (report {:type :end-test-vars :vars vars}))])))

;; =============================================================================
;; Running Tests, high level functions

(defn successful?
  "Returns true if the given test summary indicates all tests
  were successful, false otherwise."
  [summary]
  (and (zero? (:fail summary 0))
       (zero? (:error summary 0))))
