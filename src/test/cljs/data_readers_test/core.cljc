;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns data-readers-test.core)

(def custom-identity identity)

(assert (= 1 #test/custom-identity 1))

(defn custom-form-cljs 
  "a clojure and clojurescript function - in both cases targeting only cljs. 
  
  returns a clojurescript form (from :clj branch, when compiling) 
  and executes js from :cljs branch when using cljs.reader/read"
  [x]
  #?(:clj `(js/Array.of ~x)
     :cljs (js/Array.of x)))

#?(:cljs
   (def result #test/custom-form"foo"))
