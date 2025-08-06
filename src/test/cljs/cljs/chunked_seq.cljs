;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.chunked-seq
  (:refer-clojure :exclude [iter])
  (:require [cljs.test :refer-macros [deftest testing is are]]))

(deftest test-cljs-2693
  (is (chunked-seq? (range 5)))
  (is (satisfies? IChunk (chunk-first (range 5))))
  (is (nil? (chunk-next (range 32))))
  (is (not (chunked-seq? (range 2 -2 0))))
  (is (chunked-seq? (range)))
  (is (= 5 (count (chunk-first (range 5)))))
  (is (= 32 (count (chunk-first (range)))))
  (is (= 17 (nth (chunk-first (range 100)) 17)))
  (is (= 35 (nth (chunk-first (range 100)) 35)))
  (is (= 32 (count (chunk-first (range 100)))))
  (is (= 0 (first (range 5))))
  (is (= 1 (second (range 5))))
  (is (= (range 1 5) (rest (range 5))))
  (is (= (range 1 5) (next (range 5)))))