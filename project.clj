(defproject org.clojure/clojurescript "0.0-SNAPSHOT"
  :description "ClojureScript compiler and core runtime library"
  :parent [org.clojure/pom.contrib "0.1.2"]
  :url "https://github.com/clojure/clojurescript"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ^:replace ["-Xmx512m" "-server"]
  :source-paths ["src/clj"]
  :resource-paths ["src/cljs"]
  :test-paths ["test/clj"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.3"]
                 [org.clojure/tools.reader "0.8.3"]
                 [org.clojure/google-closure-library "0.0-20130212-95c19e7f0f5f"]
                 [com.google.javascript/closure-compiler "v20131014"]
                 [org.mozilla/rhino "1.7R4"]]
  :profiles {:1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0-master-SNAPSHOT"]]}}
  :aliases {"test-all" ["with-profile" "test,1.5:test,1.6" "test"]
            "check-all" ["with-profile" "1.5:1.6" "check"]}
  :min-lein-version "2.0.0")
