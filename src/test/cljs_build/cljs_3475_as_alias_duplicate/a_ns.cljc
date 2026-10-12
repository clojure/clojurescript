(ns cljs-3475-as-alias-duplicate.a-ns
  ; This line caused:
  ;
  ;  Assert failed: Duplicate :as-alias aliased-in, already in use for lib a-ns
  #?(:cljs (:require-macros [cljs-3475-as-alias-duplicate.a-ns]))
  (:require [cljs-3475-as-alias-duplicate.aliases-in-another-ns]))