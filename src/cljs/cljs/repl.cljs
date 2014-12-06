;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.repl)

(defn print-doc [m]
  (println "-------------------------")
  (println (str (when-let [ns (:ns m)] (str ns "/")) (:name m)))
  (cond
    (:forms m) (doseq [f (:forms m)]
                 (print "  ")
                 (prn f))
    (:arglists m) (prn (:arglists m)))
  (if (:special-form m)
    (do
      (println "Special Form")
      (println " " (:doc m)) 
      (if (contains? m :url)
        (when (:url m)
          (println (str "\n  Please see http://clojure.org/" (:url m))))
        (println (str "\n  Please see http://clojure.org/special_forms#"
                   (:name m)))))
    (do
      (when (:macro m)
        (println "Macro")) 
      (println " " (:doc m)))))
