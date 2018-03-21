(ns cljs-cli.test
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell :refer [with-sh-dir]]
   [clojure.string :as str]
   [cljs-cli.util :refer [cljs-main output-is with-sources with-in with-post-condition with-repl-env-filter repl-title]]
   [clojure.string :as string]))

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

(deftest test-cljs-2645
  (with-sources {"src/foo/core.cljs"
                 "(ns foo.core) (goog-define configurable \"default-value\") (defn -main [& args] (println configurable))"}
    (-> (cljs-main "-m" "foo.core")
      (output-is "default-value"))
    (-> (cljs-main "-co" "{:closure-defines {foo.core/configurable \"configured-value\"}}" "-m" "foo.core")
      (output-is "configured-value"))))

(deftest test-cljs-2650-loader-does-not-exists
  (doseq [optimizations [:none :advanced]]
    (let [src (io/file "src" "test" "cljs_build" "hello-modules" "src")
          opts {:output-dir "out"
                :asset-path "/out"
                :optimizations optimizations
                :modules {:foo {:entries '#{foo.core}
                                :output-to "out/foo.js"}
                          :bar {:entries '#{bar.core}
                                :output-to "out/bar.js"}}}]
      (with-sources
        {"src/foo/core.cljs" (slurp (io/file src "foo" "core.cljs"))
         "src/bar/core.cljs" (slurp (io/file src "bar" "core.cljs"))}
        (let [result (cljs-main "--compile-opts" (pr-str opts)
                                "--compile" "foo.core")]
          (is (zero? (:exit result)))
          (is (str/blank? (:err result))))))))

(deftest test-cljs-2673
  (with-repl-env-filter #{"node"}
    (-> (cljs-main
          "-e" "(require 'cljs.js)"
          "-e" "(cljs.js/eval-str (cljs.js/empty-state) \"(+ 1 2)\" nil {:eval cljs.js/js-eval :context :expr} prn)")
      (output-is
        nil
        "{:ns cljs.user, :value 3}"))))

(deftest test-cljs-2680
  (with-repl-env-filter (complement #{"node"})                    ; Exclude Node owing to CLJS-2684
    (with-sources {"src/foo/core.cljs" "(ns foo.core)"
                   "src/bar/core.clj"  "(ns bar.core) (defn watch [] (prn :watch-called))"}
      (with-in ":cljs/quit\n"
        (with-post-condition (fn [dir]
                               (some #{":watch-called"}
                                 (str/split-lines (slurp (io/file dir "out" "watch.log")))))
          (-> (cljs-main "-co" "{:watch-fn bar.core/watch}" "--watch" "src" "-c" "foo.core" "-r")
            (output-is
              "Watch compilation log available at: out/watch.log"
              (repl-title)
              "cljs.user=>")))))
    (with-sources {"src/foo/core.cljs" "(ns foo.core"
                   "src/bar/core.clj"  "(ns bar.core) (defn watch-error [e] (prn :watch-error-called (.getMessage e)))"}
      (with-in ":cljs/quit\n"
        (with-post-condition (fn [dir]
                               (let [log-contents (slurp (io/file dir "out" "watch.log"))]
                                 (and (str/includes? log-contents ":watch-error-called")
                                   (str/includes? log-contents "Unexpected EOF while reading"))))
          (-> (cljs-main "-co" "{:watch-error-fn bar.core/watch-error}" "--watch" "src" "-c" "foo.core" "-r")
            (output-is
              "Watch compilation log available at: out/watch.log"
              (repl-title)
              "cljs.user=>")))))))
