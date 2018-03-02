(ns cljs-cli.test
  (:require
   [clojure.test :refer [deftest]]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell :refer [with-sh-dir]]
   [cljs-cli.util :refer [cljs-main output-is with-sources with-post-condition with-repl-env-filter]]))

(deftest eval-test
  (-> (cljs-main "-e" 3 "-e" nil "-e" 4)
    (output-is 3 4)))

(deftest init-test
  (with-sources {"src/foo/core.cljs"
                 "(ns foo.core) (def x 3)"}
    (-> (cljs-main "-i" "src/foo/core.cljs" "-e" 'foo.core/x)
      (output-is 3))))

(deftest main-test
  (with-sources {"src/foo/core.cljs"
                 "(ns foo.core) (defn -main [] (prn :hi))"}
    (-> (cljs-main "-m" "foo.core")
      (output-is :hi))))

(deftest command-line-args-test
  (with-sources {"src/foo/core.cljs"
                 "(ns foo.core) (prn *command-line-args*)"}
    (-> (cljs-main "src/foo/core.cljs" "alpha" "beta" "gamma")
      (output-is (pr-str '("alpha" "beta" "gamma"))))))

(deftest command-line-args-empty-test
  (with-sources {"src/foo/core.cljs"
                 "(ns foo.core) (prn *command-line-args*)"}
    (-> (cljs-main "src/foo/core.cljs")
      (output-is nil))))

(deftest initial-ns-test
  (-> (cljs-main "-e" "::foo")
    (output-is ":cljs.user/foo")))

(deftest source-test
  (with-sources {"src/foo/core.cljs"
                 "(ns foo.core) (prn :hi)"}
    (-> (cljs-main "src/foo/core.cljs")
      (output-is :hi))))

(deftest compile-test
  (with-sources {"src/foo/core.cljs"
                 "(ns foo.core) (defn -main [] (prn :hi))"}
    (with-post-condition (fn [dir] (.exists (io/file dir "out" "main.js")))
      (-> (cljs-main "-o" "out/main.js" "-c" "foo.core")
        (output-is)))))

(deftest run-optimized-node-test
  (with-repl-env-filter #{"node"}
    (with-sources {"src/foo/core.cljs"
                   "(ns foo.core) (prn :hello-from-node)"}
      (with-post-condition (fn [dir]
                             (= {:exit 0, :out ":hello-from-node\n", :err ""}
                               (with-sh-dir dir
                                 (shell/sh "node" (str (io/file dir "out" "main.js"))))))
        (-> (cljs-main "-t" "node" "-o" "out/main.js" "-O" "advanced" "-c" "foo.core")
          (output-is))))))
