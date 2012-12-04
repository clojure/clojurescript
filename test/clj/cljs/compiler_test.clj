(ns cljs.compiler-test
  (:require [clojure.test :as t]
            [cljs.compiler :as c]
            [clojure.java.io :as io]))

(def dir-names (atom ["__dir"
                      "__dir/dir1"
                      "__dir/dir2"
                      "__dir/dir2/dir21"]))

(def file-names (atom ["__dir/file1.cljs"
                       "__dir/dir1/file11.cljs"
                       "__dir/dir2/file21.cljs"
                       "__dir/dir2/file22.cljs"
                       "__dir/dir2/dir21/file211.cljs"]))


(defn create-context []
  (doall (map #(.mkdir (io/file %)) @dir-names))
  (doall (map #(.createNewFile (io/file %)) @file-names)))

(defn clear-context []
  (doall (map #(.delete (io/file %)) @file-names))
  (doall (map #(.delete (io/file %)) (reverse @dir-names))))

(def files (map #(io/file %) @file-names))

(def file-paths (map #(str (.getCanonicalPath ^java.io.File (io/file ".")) java.io.File/separator %)
                     @file-names))

(defn get-file-names [coll indeces]
  (map #(nth coll %) indeces))

(t/deftest test-exclude-file-names
  (create-context)
  (t/are [x y] (= x y)

         ;; border (logical cases)

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


         ;; real cases


         ;; standard
         ;; exclude a single source file
         (set (get-file-names file-paths [0])) (c/exclude-file-names "__dir" ["file1.cljs"])

         ;; standard
         ;; exclude a single directory containing a directory with a source file and two source files
         (set (get-file-names file-paths [2 3 4])) (c/exclude-file-names "__dir" ["dir2"])

         ;; border
         ;; exclude a directory and a file which happens to be included in the exclude directory
         (set (get-file-names file-paths [4])) (c/exclude-file-names "__dir" ["dir2/dir21" "dir2/dir21/file211.cljs"])

         ;; border
         ;; exclude all files by excluding a file and two directories
         (set file-paths) (c/exclude-file-names "__dir" ["file1.cljs" "dir1" "dir2"])

         ;; border
         ;; exclude all
         (set file-paths) (c/exclude-file-names "__dir" ["."])

         )
  (clear-context))
