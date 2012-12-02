(ns cljs.compiler-test
  (:require [clojure.test :as t]
            [cljs.compiler :as c]))

(t/deftest test-exclude-file-names
  (t/are [x y] (= x y)
       nil (c/exclude-file-names nil nil)
       nil (c/exclude-file-names nil [])
       nil (c/exclude-file-names nil [""])
       nil (c/exclude-file-names "src/cljs" nil)
       #{} (c/exclude-file-names "src/cljs" [])
       #{} (c/exclude-file-names "src/cljs" [""])
       #{} (c/exclude-file-names "src/cljs" ["non_existent_file.cljs"])
       #{} (c/exclude-file-names "src/cljs" ["non_existent_directory"])
       #{} (c/exclude-file-names "src/cljs" ["non_existent_file.cljs" "non_existent_directory"])
       #{} (c/exclude-file-names "src/cljs" ["cljs/non_existent_file.cljs"])
       #{} (c/exclude-file-names "src/cljs" ["cljs/non_existent_directory"])
       #{} (c/exclude-file-names "src/cljs" ["cljs/non_existent_file.cljs" "cljs/non_existent_directory"])
       ))
