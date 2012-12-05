(ns cljs.compiler-test
  (:require [clojure.test :as t]
            [cljs.compiler :as c]
            [clojure.java.io :as io]))

;;; dirs seeds
(def dir-names ["__dir"
                "__dir/dir1"
                "__dir/dir2"
                "__dir/dir2/dir21"])

;;; files seeds
(def file-names ["__dir/file1.cljs"
                 "__dir/dir1/file11.cljs"
                 "__dir/dir2/file21.cljs"
                 "__dir/dir2/file22.cljs"
                 "__dir/dir2/dir21/file211.cljs"])

;;; scenario creation
(defn create-context []
  (doall (map #(.mkdir (io/file %)) @dir-names))
  (doall (map #(.createNewFile (io/file %)) @file-names)))

;;; scenario destruction
(defn clear-context []
  (doall (map #(.delete (io/file %)) @file-names))
  (doall (map #(.delete (io/file %)) (reverse @dir-names))))

(def file-paths (map #(str (.getCanonicalPath ^java.io.File (io/file ".")) java.io.File/separator %)
                     @file-names))

(defn get-file-names [coll indeces]
  (map #(nth coll %) indeces))

(t/deftest test-exclude-file-names

  ;; scenario creation

  (create-context)

  ;; declaration of assertions
  (t/are [x y] (= x y)

         ;; logical cases - all at border

         nil (c/exclude-file-names nil nil)
         nil (c/exclude-file-names nil [])
         nil (c/exclude-file-names nil [""])
         nil (c/exclude-file-names "__dir" nil)
         #{} (c/exclude-file-names "__dir" [])
         #{} (c/exclude-file-names "__dir" [""])
         #{} (c/exclude-file-names "__dir" ["non_existent_file.cljs"])
         #{} (c/exclude-file-names "__dir" ["non_existent_directory"])
         #{} (c/exclude-file-names "__dir" ["non_existent_file.cljs" "non_existent_directory"])
         #{} (c/exclude-file-names "__dir" ["dir1/non_existent_file.cljs"])
         #{} (c/exclude-file-names "__dir" ["dir1/non_existent_directory"])
         #{} (c/exclude-file-names "__dir" ["dir1/non_existent_file.cljs" "cljs/non_existent_directory"])

         ;; logical cases should they throw exceptions?
         nil (c/exclude-file-names "__dir" "")
         nil (c/exclude-file-names "__dir" "whatever.clj")
         nil (c/exclude-file-names "__dir" "dir1/whatever.clj")
         nil (c/exclude-file-names "__dir" "dir1/whatever")

         ;; real cases

         ;; exclude a single source file (standard)
         (set (get-file-names file-paths [0])) (c/exclude-file-names "__dir" ["file1.cljs"])

         ;; exclude a single directory (standard)
         (set (get-file-names file-paths [2 3 4])) (c/exclude-file-names "__dir" ["dir2"])

         ;; exclude a directory and a file already excluded (border)
         (set (get-file-names file-paths [4])) (c/exclude-file-names "__dir" ["dir2/dir21" "dir2/dir21/file211.cljs"])

         ;; exclude all files by excluding a file and two directories (border)
         (set file-paths) (c/exclude-file-names "__dir" ["file1.cljs" "dir1" "dir2"])

         ;; exclude all by using "." (border)
         (set file-paths) (c/exclude-file-names "__dir" ["."])

         ;; exclude all by using ".." (border)
         ;; (set file-paths) (c/exclude-file-names "__dir" [".."])


         )
  (clear-context)
  )
