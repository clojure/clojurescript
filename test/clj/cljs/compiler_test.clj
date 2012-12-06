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

(t/deftest test-exclude

  ;; scenario creation

  (create-context)

  ;; declaration of assertions
  (t/are [x y] (= x y)

         ;; logical cases passing nil or a vector of exclusions

         nil (c/-exclude nil nil)
         nil (c/-exclude [] nil)
         nil (c/-exclude [""] nil)
         nil (c/-exclude nil "__dir")
         #{} (c/-exclude [] "__dir")
         #{} (c/-exclude [""] "__dir")
         #{} (c/-exclude ["non_existent_file.cljs"] "__dir")
         #{} (c/-exclude ["non_existent_directory"] "__dir")
         #{} (c/-exclude ["non_existent_file.cljs" "non_existent_directory"] "__dir")
         #{} (c/-exclude ["dir1/non_existent_file.cljs"] "__dir")
         #{} (c/-exclude ["dir1/non_existent_directory"] "__dir")
         #{} (c/-exclude ["dir1/non_existent_file.cljs" "cljs/non_existent_directory"] "__dir")

         ;; logical cases passing a single exclusion as a string
         
         #{} (c/-exclude "" "__dir")
         #{} (c/-exclude "non_existent_file.clj" "__dir")
         #{} (c/-exclude "dir1/non_existing_file.clj" "__dir")
         #{} (c/-exclude "non_exsiting_dir" "__dir")

         ;; real cases

         ;; exclude a single source file by passing it inside a vector
         (set (get-file-names file-paths [0]))
         (c/-exclude ["file1.cljs"] "__dir")

         ;; exclude a single source file by passing it as a
         ;; string  
         (set (get-file-names file-paths [0]))
         (c/-exclude "file1.cljs" "__dir")

         ;; exclude a single directory by passing it inside a vector
         (set (get-file-names file-paths [2 3 4]))
         (c/-exclude ["dir2"] "__dir")

         ;; exclude a single directory by passing it as a string
         (set (get-file-names file-paths [2 3 4]))
         (c/-exclude "dir2" "__dir")

         ;; exclude a directory and a file already excluded (border)
         (set (get-file-names file-paths [4]))
         (c/-exclude ["dir2/dir21" "dir2/dir21/file211.cljs"] "__dir")

         ;; exclude all files by excluding a file and two directories (border)
         (set file-paths)
         (c/-exclude ["file1.cljs" "dir1" "dir2"] "__dir")

         ;; exclude all by passing "." inside a vector
         (set file-paths)
         (c/-exclude ["."] "__dir")

         ;; exclude all by passing "." as a string
         (set file-paths)
         (c/-exclude "__dir" ".")
        
         ;;exclude all by passing "../__dir" inside a vector
         (set file-paths) (c/-exclude ["../__dir"] "__dir")

         ;;exclude all by passing "../__dir" as a string
         (set file-paths) (c/-exclude "../__dir" "__dir")

         )
  (clear-context)
  )