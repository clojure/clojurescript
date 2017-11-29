;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.predicates-test
  (:require [cljs.test :as test :refer-macros [deftest is]])
  (:import [goog.math Long Integer]))

(def pred-val-table
  (let [uuid (uuid "00000000-0000-0000-0000-000000000000")]
    [[identity    boolean? indexed? seqable?  ident?  uuid?    inst?  simple-ident? qualified-ident? simple-symbol? qualified-symbol? simple-keyword? qualified-keyword?]
     [0             false    false    false    false  false    false  false         false            false          false             false           false]
     [1             false    false    false    false  false    false  false         false            false          false             false           false]
     [-1            false    false    false    false  false    false  false         false            false          false             false           false]
     [1.0           false    false    false    false  false    false  false         false            false          false             false           false]
     [true          true     false    false    false  false    false  false         false            false          false             false           false]
     [[]            false    true     true     false  false    false  false         false            false          false             false           false]
     [nil           false    false    false    false  false    false  false         false            false          false             false           false]
     [{}            false    false    true     false  false    false  false         false            false          false             false           false]
     [:foo          false    false    false    true   false    false  true          false            false          false             true            false]
     [::foo         false    false    false    true   false    false  false         true             false          false             false           true]
     ['foo          false    false    false    true   false    false  true          false            true           false             false           false]
     ['foo/bar      false    false    false    true   false    false  false         true             false          true              false           false]
     [uuid          false    false    false    false  true     false  false         false            false          false             false           false]
     [(array)       false    false    true     false  false    false  false         false            false          false             false           false]
     ["string"      false    false    true     false  false    false  false         false            false          false             false           false]]))

(deftest test-preds
  (let [[preds & rows] pred-val-table]
    (doseq [row rows]
      (let [v (first row)]
        (dotimes [i (count row)]
          (is (= ((nth preds i) v) (nth row i))
              (pr-str (list (nth preds i) v))))))))

(def int-val-table
  (let [posint 10e10
        negint -10e10
        neg0   (/ ##-Inf)
        natl   (Long.getZero)
        posl   (Long.fromNumber posint)
        negl   (Long.fromNumber negint)
        nati   Integer.ZERO
        posi   (Integer.fromNumber posint)
        negi   (Integer.fromNumber negint)]
    [[identity  neg?  pos?   integer? int?   neg-int?  pos-int?  nat-int?]
     [0         false false  true     true   false     false     true    ]
     [neg0      false false  true     true   false     false     true    ]
     [1         false true   true     true   false     true      true    ]
     [-1        true  false  true     true   true      false     false   ]
     [1.0       false true   true     true   false     true      true    ]
     [-1.0      true  false  true     true   true      false     false   ]
     [posint    false true   true     true   false     true      true    ]
     [negint    true  false  true     true   true      false     false   ]
     [natl      false false  false    true   false     false     true    ]
     [posl      false true   false    true   false     true      true    ]
     [negl      true  false  false    true   true      false     false   ]
     [nati      false false  false    true   false     false     true    ]
     [posi      false true   false    true   false     true      true    ]
     [negi      true  false  false    true   true      false     false   ]]))

(deftest test-int-preds
  (let [[preds & rows] int-val-table]
    (doseq [row rows]
      (let [v (first row)]
        (dotimes [i (count row)]
          (is (= ((nth preds i) v) (nth row i))
              (pr-str (list (nth preds i) v))))))))