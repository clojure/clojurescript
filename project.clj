(defproject org.clojure/clojurescript "0.0-SNAPSHOT"
  :description "ClojureScript compiler and core runtime library"
  :parent [org.clojure/pom.contrib "0.1.2"]
  :url "https://github.com/clojure/clojurescript"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true" "-Xmx512m" "-server"]
  :source-paths ["src/main/clojure" "src/main/cljs"]
  :resource-paths ["src/main/cljs"]
  :test-paths ["src/test/clojure" "src/test/cljs" "src/test/self"]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/tools.reader "1.0.0-beta1"]
                 [org.clojure/test.check "0.9.0" :scope "test"]
                 [org.clojure/google-closure-library "0.0-20151016-61277aea"]
                 [com.google.javascript/closure-compiler "v20160315"]
                 [org.mozilla/rhino "1.7R5"]]
  :profiles {:1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :uberjar {:aot :all :main clojure.main}}
  :aliases {"test-all" ["with-profile" "test,1.5:test,1.6" "test"]
            "check-all" ["with-profile" "1.5:1.6" "check"]}
  :min-lein-version "2.0.0")
