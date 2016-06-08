(ns cljs.predicates-test
  (:require [cljs.test :refer-macros [deftest is]])
  (:import [goog.math Long]))


(def pred-val-table
  (let [now (js/Date.)
        uuid (uuid nil)]
    [[identity    boolean? indexed? seqable?  ident?  uuid?    inst?  simple-ident? qualified-ident? simple-symbol? qualified-symbol? simple-keyword? qualified-keyword?]
     [0             false    false    false    false  false    false  false         false            false          false             false           false]
     [1             false    false    false    false  false    false  false         false            false          false             false           false]
     [-1            false    false    false    false  false    false  false         false            false          false             false           false]
     [1.0           false    false    false    false  false    false  false         false            false          false             false           false]
     [true          true     false    false    false  false    false  false         false            false          false             false           false]
     [[]            false    true     true     false  false    false  false         false            false          false             false           false]
     [nil           false    false    false    false  false    false  false         false            false          false             false           false]
     [{}            false    false    true     false  false    false  false         false            false          false             false           false]
     [:foo          false    false    false    true   false    false  true          nil              false          false             true            nil]
     [::foo         false    false    false    true   false    false  false         true             false          false             false           true]
     ['foo          false    false    false    true   false    false  true          nil              true           nil               false           false]
     ['foo/bar      false    false    false    true   false    false  false         true             false          true              false           false]
     [uuid          false    false    false    false  true     false  false         false            false          false             false           false]
     [now           false    false    false    false  false    true   false         false            false          false             false           false]]))

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
        natl (goog.math.Long.getZero)
        posl (goog.math.Long.fromNumber posint)
        negl (goog.math.Long.fromNumber negint)]
    [[identity  neg?  pos?   integer? long?  neg-long? pos-long? nat-long?]
     [0         false false  true     false  false     false     false    ]
     [1         false true   true     false  false     false     false    ]
     [-1        true  false  true     false  false     false     false    ]
     [1.0       false true   true     false  false     false     false    ]
     [-1.0      true  false  true     false  false     false     false    ]
     [posint    false true   true     false  false     false     false    ]
     [negint    true  false  true     false  false     false     false    ]
     [natl      false false  false    true   false     false     true     ]
     [posl      false true   false    true   false     true      true     ]
     [negl      true  false  false    true   true      false     false    ]]))

(deftest test-int-preds
  (let [[preds & rows] int-val-table]
    (doseq [row rows]
      (let [v (first row)]
        (dotimes [i (count row)]
          (is (= ((nth preds i) v) (nth row i))
              (pr-str (list (nth preds i) v))))))))