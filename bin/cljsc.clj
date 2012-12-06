;   Copyright (c) Rich Hickey. All rights reserved.  The use and
;   distribution terms for this software are covered by the Eclipse
;   Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;   found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this
;   notice, or any other, from this software.

(require '[cljs.closure :as closure])

(defn transform-cl-args
  "Called from the script cljsc"
  [args]
  (let [srd-dir (first args)
        args-rest (clojure.string/split (nth args 1) #"\s")
        exclude (when (= (.indexOf args-rest ":exclude") -1)
                  (read-string (nth args-rest
                                    (inc (.indexOf args-rest ":exclude")))))
        opts-string (apply str (interpose " " (rest args)))
        options (when (> (count opts-string) 1)
                  (try (read-string opts-string)
                       (catch Exception e (println e))))]

    {:source src-dir
     :options (merge {:output-to :print} options {:exclude exclude})}))

(let [args (transform-cl-args *command-line-args*)]
  (closure/build (:source args) (:options args))
  (.flush *out*)
  (shutdown-agents)
  (System/exit 0))
