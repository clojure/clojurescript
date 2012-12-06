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
  (doall (map #(.mkdir (io/file %)) dir-names))
  (doall (map #(.createNewFile (io/file %)) file-names)))

;;; scenario destruction
(defn clear-context []
  (doall (map #(.delete (io/file %)) file-names))
  (doall (map #(.delete (io/file %)) (reverse dir-names))))

(def file-paths (map #(str (.getCanonicalPath ^java.io.File (io/file ".")) java.io.File/separator %)
                     file-names))

(defn get-file-names [coll indeces]
  (map #(nth coll %) indeces))

(t/deftest test-exclude-file-names

  ;; scenario creation

  (create-context)

  ;; declaration of assertions
  (t/are [x y] (= x y)

         ;; logical cases - all at border

         nil (c/exclude-file-names nil nil)
         nil (c/exclude-file-names [] nil)
         nil (c/exclude-file-names [""] nil)
         nil (c/exclude-file-names nil "__dir")
         #{} (c/exclude-file-names [] "__dir")
         #{} (c/exclude-file-names [""] "__dir")
         #{} (c/exclude-file-names ["non_existent_file.cljs"] "__dir")
         #{} (c/exclude-file-names ["non_existent_directory"] "__dir")
         #{} (c/exclude-file-names ["non_existent_file.cljs" "non_existent_directory"] "__dir")
         #{} (c/exclude-file-names ["dir1/non_existent_file.cljs"] "__dir")
         #{} (c/exclude-file-names ["dir1/non_existent_directory"] "__dir")
         #{} (c/exclude-file-names ["dir1/non_existent_file.cljs" "cljs/non_existent_directory"] "__dir")

         ;; logical cases. should they throw exceptions or
         ;; exclude-file-names should be implemented as protocol?
         ;; now they just return nil.
         nil (c/exclude-file-names "" "__dir")
         nil (c/exclude-file-names "whatever.clj" "__dir")
         nil (c/exclude-file-names "dir1/whatever.clj" "__dir")
         nil (c/exclude-file-names "dir1/whatever" "__dir")

         ;; real cases

         ;; exclude a single source file (standard) by passing it in a
         ;; vector 
         (set (get-file-names file-paths [0]))
         (c/exclude-file-names ["file1.cljs"] "__dir")

         ;; ;; exclude a single source file (standard) by passing it as a
         ;; ;; string  
         ;; (set (get-file-names file-paths [0]))
         ;; (c/exclude-file-names "file1.cljs" "__dir")

         ;; exclude a single directory (standard) by passing it in a
         ;; vector 
         (set (get-file-names file-paths [2 3 4]))
         (c/exclude-file-names ["dir2"] "__dir")

         ;; ;; exclude a single directory (standard) by passing it as a
         ;; ;; string 
         ;; (set (get-file-names file-paths [2 3 4]))
         ;; (c/exclude-file-names "dir2" "__dir")

         ;; exclude a directory and a file already excluded (border)
         (set (get-file-names file-paths [4]))
         (c/exclude-file-names ["dir2/dir21" "dir2/dir21/file211.cljs"] "__dir")

         ;; exclude all files by excluding a file and two directories (border)
         (set file-paths)
         (c/exclude-file-names ["file1.cljs" "dir1" "dir2"] "__dir")

         ;; exclude all by using "." (border)
         (set file-paths)
         (c/exclude-file-names ["."] "__dir")

         ;; (set file-paths)
         ;; (c/exclude-file-names "." "__dir")

         ;; exclude all by using ".." (border)
         ;; (set file-paths) (c/exclude-file-names [".."] "__dir")


         )
  (clear-context)
  )
