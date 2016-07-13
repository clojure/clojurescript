;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.spec.test
  (:require
    [cljs.analyzer :as ana]
    [cljs.spec :as s]
    [cljs.spec.impl.gen :as gen]))

(defmacro with-instrument-disabled
  "Disables instrument's checking of calls, within a scope."
  [& body]
  `(binding [*instrument-enabled* nil]
     ~@body))

(defmacro run-tests
  "Like run-all-tests, but scoped to specific namespaces, or to
*ns* if no ns-sym are specified."
  ([]
   `(cljs.spec.test/run-tests '~ana/*cljs-ns*))
  ([& ns-syms]
   `(cljs.spec.test/run-var-tests
      (->> #?(:clj  ~(s/speced-vars* ns-syms)
              :cljs ~(cljs.spec$macros/speced-vars* ns-syms))
        (filter (fn [v#] (:args (cljs.spec/get-spec v#))))))))

(defmacro run-all-tests
  "Like clojure.test/run-all-tests, but runs test.check tests
for all speced vars. Prints per-test results to *out*, and
returns a map with :test,:pass,:fail, and :error counts."
  []
  `(cljs.spec.test/run-var-tests #?(:clj  ~(s/speced-vars*)
                                    :cljs ~(cljs.spec$macros/speced-vars*))))
