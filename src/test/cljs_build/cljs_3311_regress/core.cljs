(ns cljs-3311-regress.core
  (:require [cljs.test :refer-macros [run-tests]]
            [cljs-3311-regress.tests]))

(run-tests 'cljs-3311-regress.tests)
