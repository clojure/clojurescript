(ns test-exclude-files
  (:require [cljs.closure :as closure]
            [cljs.compiler :as compiler]
            [clojure.java.io :as io]
            [clojure.test :as test]
            [clojure.set :as set]))

;; In order to perform the tests, a new tree is created and then deleted at each test execution.
;; The following simple tree will be initialized.
;;
;;  dir _____ file1.cljs
;;     |_____ dir1 _____ file11.cljs
;;     |          |_____ file12.cljs
;;     |_____ dir2 _____ file22.cljs
;;                |_____ dir21 _____ file211.cljs

(def src-dir (io/file "dir"))

(def dirs ["dir"
           "dir/dir1"
           "dir/dir2"
           "dir/dir2/dir21"])

(def file-names ["dir/file1.cljs"
                 "dir/dir1/file11.cljs"
                 "dir/dir2/file21.cljs"
                 "dir/dir2/file22.cljs"
                 "dir/dir2/dir21/file211.cljs"])

(def files (do (map #(io/file %) file-names)))

(def file-paths (do (seq (map #(str (.getCanonicalPath ^java.io.File (io/file "")) java.io.File/separator %) file-names))))

(defn build-tree
  "Build a tree of files and folders for testing"
  []
  (doall (map #(.mkdir (io/file %)) dirs))
  (doall (map #(.createNewFile (io/file %)) file-names)))

(defn delete-tree
  "Remove the tree"
  []
  (doall (map #(.delete (io/file %)) files))
  (doall (map #(.delete (io/file %)) (reverse dirs))))


(defn extract-el
  "Simple function for extracting multiple elements from collections."
  [coll indices]
  (map #(nth coll %) indices))



(test/deftest test-exclude-file
  "Tests for the function exclude-file-names"
  (build-tree)
  (test/are [x y] (= x y)
            nil (compiler/exclude-file-names nil nil)
            nil (compiler/exclude-file-names nil [])
            nil (compiler/exclude-file-names nil [""])
            nil (compiler/exclude-file-names "dir" nil)
            #{} (compiler/exclude-file-names "dir" [])
            #{} (compiler/exclude-file-names "dir" [""])
            #{} (compiler/exclude-file-names "dir" ["non_existent_file.cljs"])
            #{} (compiler/exclude-file-names "dir" ["non_existent_directory"])
            #{} (compiler/exclude-file-names "dir" ["non_existent_file.cljs" "non_existent_directory"])
            #{} (compiler/exclude-file-names "dir" ["dir1/non_existent_file.cljs"])
            #{} (compiler/exclude-file-names "dir" ["dir1/non_existent_directory"])
            #{} (compiler/exclude-file-names "dir" ["dir1/non_existent_file.cljs" "dir1/non_existent_directory"])
            (set (extract-el file-paths [0])) (compiler/exclude-file-names src-dir ["file1.cljs"])
            (set (extract-el file-paths [2 3 4])) (compiler/exclude-file-names src-dir ["dir2"])
            (set (extract-el file-paths [4])) (compiler/exclude-file-names src-dir ["dir2/dir21" "dir2/dir21/file211.cljs"])
            (set file-paths) (compiler/exclude-file-names src-dir ["file1.cljs" "dir1" "dir2"])
            (set file-paths) (compiler/exclude-file-names src-dir ["."]))
  (delete-tree))

(test/deftest test-cljs-files-in
  "Tests for the function cljs-files-in"
  (build-tree)
  (test/are [x y] (= (set x) (set y))
            files (compiler/cljs-files-in (io/file "dir"))
            files (compiler/cljs-files-in (io/file "dir") :exclude nil)
            files (compiler/cljs-files-in (io/file "dir") :exclude #{})
            (extract-el files [0])  (compiler/cljs-files-in (io/file "dir") :exclude (set (extract-el file-paths [1 2 3 4])))
            (extract-el files [1 4])  (compiler/cljs-files-in (io/file "dir") :exclude (set (extract-el file-paths [0 2 3])))
            (extract-el files [0 1 2])  (compiler/cljs-files-in (io/file "dir") :exclude (set (extract-el file-paths [3 4])))
            '()  (compiler/cljs-files-in (io/file "dir") :exclude (set file-paths)))
  (delete-tree))
