;;; pprint.clj -- Pretty printer and Common Lisp compatible format function (cl-format) for Clojure

;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Author: Tom Faulhaber
;; April 3, 2009

(ns cljs.pprint)

(defmacro ^{:private true} prlabel [prefix arg & more-args]
  "Print args to *err* in name = value format"
  `(prerr ~@(cons (list 'quote prefix) (mapcat #(list (list 'quote %) "=" %) 
                                                  (cons arg (seq more-args))))))
