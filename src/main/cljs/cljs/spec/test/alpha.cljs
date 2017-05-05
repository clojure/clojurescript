;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.spec.test.alpha
  (:require-macros [cljs.spec.test.alpha :as m :refer [with-instrument-disabled]])
  (:require
    [goog.object :as gobj]
    [goog.userAgent.product :as product]
    [clojure.string :as string]
    [cljs.stacktrace :as st]
    [cljs.pprint :as pp]
    [cljs.spec.alpha :as s]
    [cljs.spec.gen.alpha :as gen]
    [clojure.test.check :as stc]
    [clojure.test.check.properties]))

(defn distinct-by
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[x :as xs] seen]
                     (when-let [s (seq xs)]
                       (let [v (f x)]
                         (if (contains? seen v)
                           (recur (rest s) seen)
                           (cons x (step (rest s) (conj seen v)))))))
                    xs seen)))]
     (step coll #{}))))

(defn ->sym
  [x]
  (@#'s/->sym x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; instrument ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private ^:dynamic *instrument-enabled*
  "if false, instrumented fns call straight through"
  true)

(defn get-host-port []
  (if (not= "browser" *target*)
    {}
    {:host (.. js/window -location -host)
     :port (.. js/window -location -port)}))

(defn get-ua-product []
  (if (not= "browser" *target*)
    (keyword *target*)
    (cond
      product/SAFARI :safari
      product/CHROME :chrome
      product/FIREFOX :firefox
      product/IE :ie)))

(defn get-env []
  {:ua-product (get-ua-product)})

(defn- fn-spec?
  "Fn-spec must include at least :args or :ret specs."
  [m]
  (or (:args m) (:ret m)))

;; wrap spec/explain-data until specs always return nil for ok data
(defn- explain-data*
  [spec v]
  (when-not (s/valid? spec v nil)
    (s/explain-data spec v)))

(defn- find-caller [st]
  (letfn [(search-spec-fn [frame]
            (when frame
              (let [s (:function frame)]
                (and (string? s) (not (string/blank? s))
                     (re-find #"cljs\.spec\.test\.spec_checking_fn" s)))))]
    (->> st
         (drop-while #(not (search-spec-fn %)))
         (drop-while search-spec-fn)
         first)))

;; TODO: check ::caller result in other browsers - David

(defn- spec-checking-fn
  [v f fn-spec]
  (let [fn-spec (@#'s/maybe-spec fn-spec)
        conform! (fn [v role spec data args]
                   (let [conformed (s/conform spec data)]
                     (if (= ::s/invalid conformed)
                       (let [caller (find-caller
                                      (st/parse-stacktrace
                                        (get-host-port)
                                        (.-stack (js/Error.))
                                        (get-env) nil))
                             ed (merge (assoc (s/explain-data* spec [role] [] [] data)
                                         ::s/args args
                                         ::s/failure :instrument)
                                  (when caller
                                    {::caller caller}))]
                         (throw (ex-info
                                  (str "Call to " v " did not conform to spec:\n" (with-out-str (s/explain-out ed)))
                                  ed)))
                       conformed)))]
    (doto
      (fn
        [& args]
        (if *instrument-enabled*
          (with-instrument-disabled
            (when (:args fn-spec) (conform! v :args (:args fn-spec) args args))
            (binding [*instrument-enabled* true]
              (apply f args)))
          (apply f args)))
      (gobj/extend f))))

(defn- no-fspec
  [v spec]
  (ex-info (str "Fn at " v " is not spec'ed.")
    {:var v :spec spec ::s/failure :no-fspec}))

(defonce ^:private instrumented-vars (atom {}))

(defn- instrument-choose-fn
  "Helper for instrument."
  [f spec sym {over :gen :keys [stub replace]}]
  (if (some #{sym} stub)
    (-> spec (s/gen over) gen/generate)
    (get replace sym f)))

(defn- instrument-choose-spec
  "Helper for instrument"
  [spec sym {overrides :spec}]
  (get overrides sym spec))

(defn- instrument-1*
  [s v opts]
  (let [spec (s/get-spec v)
        {:keys [raw wrapped]} (get @instrumented-vars v)
        current @v
        to-wrap (if (= wrapped current) raw current)
        ospec (or (instrument-choose-spec spec s opts)
                (throw (no-fspec v spec)))
        ofn (instrument-choose-fn to-wrap ospec s opts)
        checked (spec-checking-fn v ofn ospec)]
    (swap! instrumented-vars assoc v {:raw to-wrap :wrapped checked})
    checked))

(defn- unstrument-1*
  [s v]
  (when v
    (when-let [{:keys [raw wrapped]} (get @instrumented-vars v)]
      (swap! instrumented-vars dissoc v)
      (let [current @v]
        (when (= wrapped current)
          raw)))))

(defn- fn-spec-name?
  [s]
  (symbol? s))

(defn- collectionize
  [x]
  (if (symbol? x)
    (list x)
    x))

(defn instrumentable-syms
  "Given an opts map as per instrument, returns the set of syms
that can be instrumented."
  ([] (instrumentable-syms nil))
  ([opts]
   (assert (every? ident? (keys (:gen opts))) "instrument :gen expects ident keys")
   (reduce into #{} [(filter fn-spec-name? (keys (s/registry)))
                     (keys (:spec opts))
                     (:stub opts)
                     (keys (:replace opts))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; testing  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- explain-check
  [args spec v role]
  (ex-info
    "Specification-based check failed"
    (when-not (s/valid? spec v nil)
      (assoc (s/explain-data* spec [role] [] [] v)
        ::args args
        ::val v
        ::s/failure :check-failed))))

(defn- check-call
  "Returns true if call passes specs, otherwise *returns* an exception
with explain-data + ::s/failure."
  [f specs args]
  (let [cargs (when (:args specs) (s/conform (:args specs) args))]
    (if (= cargs ::s/invalid)
      (explain-check args (:args specs) args :args)
      (let [ret (apply f args)
            cret (when (:ret specs) (s/conform (:ret specs) ret))]
        (if (= cret ::s/invalid)
          (explain-check args (:ret specs) ret :ret)
          (if (and (:args specs) (:ret specs) (:fn specs))
            (if (s/valid? (:fn specs) {:args cargs :ret cret})
              true
              (explain-check args (:fn specs) {:args cargs :ret cret} :fn))
            true))))))

(defn- quick-check
  [f specs {gen :gen opts ::stc/opts}]
  (let [{:keys [num-tests] :or {num-tests 1000}} opts
        g (try (s/gen (:args specs) gen) (catch js/Error t t))]
    (if (instance? js/Error g)
      {:result g}
      (let [prop (gen/for-all* [g] #(check-call f specs %))]
        (apply gen/quick-check num-tests prop (mapcat identity opts))))))

(defn- make-check-result
  "Builds spec result map."
  [check-sym spec test-check-ret]
  (merge {:spec spec
          ::stc/ret test-check-ret}
    (when check-sym
      {:sym check-sym})
    (when-let [result (-> test-check-ret :result)]
      (when-not (true? result) {:failure result}))
    (when-let [shrunk (-> test-check-ret :shrunk)]
      {:failure (:result shrunk)})))

(defn- validate-check-opts
  [opts]
  (assert (every? ident? (keys (:gen opts))) "check :gen expects ident keys"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; check reporting  ;;;;;;;;;;;;;;;;;;;;;;;;

(defn- failure-type
  [x]
  (::s/failure (ex-data x)))

(defn- unwrap-failure
  [x]
  (if (failure-type x)
    (ex-data x)
    x))

(defn- result-type
  "Returns the type of the check result. This can be any of the
::s/failure keywords documented in 'check', or:

  :check-passed   all checked fn returns conformed
  :check-threw    checked fn threw an exception"
  [ret]
  (let [failure (:failure ret)]
    (cond
      (nil? failure) :check-passed
      (failure-type failure) (failure-type failure)
      :default :check-threw)))

(defn abbrev-result
  "Given a check result, returns an abbreviated version
suitable for summary use."
  [x]
  (if (:failure x)
    (-> (dissoc x ::stc/ret)
      (update :spec s/describe)
      (update :failure unwrap-failure))
    (dissoc x :spec ::stc/ret)))

(defn summarize-results
  "Given a collection of check-results, e.g. from 'check', pretty
prints the summary-result (default abbrev-result) of each.

Returns a map with :total, the total number of results, plus a
key with a count for each different :type of result."
  ([check-results] (summarize-results check-results abbrev-result))
  ([check-results summary-result]
   (reduce
     (fn [summary result]
       (pp/pprint (summary-result result))
       (-> summary
         (update :total inc)
         (update (result-type result) (fnil inc 0))))
     {:total 0}
     check-results)))

(comment
  (require
    '[cljs.pprint :as pp]
    '[cljs.spec :as s]
    '[cljs.spec.gen :as gen]
    '[cljs.test :as ctest])

  (require :reload '[cljs.spec.test :as test])

  ;; discover speced vars for your own test runner
  (s/speced-vars)

  ;; check a single var
  (test/check-var #'-)
  (test/check-var #'+)
  (test/check-var #'clojure.spec.broken-specs/throwing-fn)

  ;; old style example tests
  (ctest/run-all-tests)

  (s/speced-vars 'clojure.spec.correct-specs)
  ;; new style spec tests return same kind of map
  (test/check-var #'subs)
  (cljs.spec.test/run-tests 'clojure.core)
  (test/run-all-tests)

  ;; example evaluation
  (defn ranged-rand
    "Returns random int in range start <= rand < end"
    [start end]
    (+ start (long (rand (- end start)))))

  (s/fdef ranged-rand
    :args (s/and (s/cat :start int? :end int?)
                 #(< (:start %) (:end %)))
    :ret  int?
    :fn   (s/and #(>= (:ret %) (-> % :args :start))
                 #(< (:ret %) (-> % :args :end))))

  (instrumentable-syms)

  (m/instrument-1 `ranged-rand {})
  (m/unstrument-1 `ranged-rand)

  (m/instrument)
  (m/instrument `ranged-rand)
  (m/instrument `[ranged-rand])

  (m/unstrument)
  (m/unstrument `ranged-rand)
  (m/unstrument `[ranged-rand])

  (ranged-rand 8 5)
  (defn foo
    ([a])
    ([a b]
     (ranged-rand 8 5)))
  (foo 1 2)
  (m/unstrument-1 `ranged-rand)

  (m/check-1 `ranged-rand nil nil {})

  (m/check-fn inc
    (s/fspec
      :args (s/cat :x int?)
      :ret  int?))

  (m/checkable-syms)

  (m/check `ranged-rand)
  )





