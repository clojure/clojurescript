(defproject org.clojure/clojurescript "0.0-SNAPSHOT"
  :description "ClojureScript compiler and core runtime library"
  :parent [org.clojure/pom.contrib "0.1.2"]
  :url "https://github.com/clojure/clojurescript"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true" "-Xmx512m" "-server"]
  :source-paths ["src/main/clojure" "src/main/cljs"]
  :resource-paths ["src/main/cljs" "resources"]
  :test-paths ["src/test/clojure" "src/test/cljs" "src/test/self" "src/test/cljs_cp"]
  :dependencies [[org.clojure/clojure "1.10.0-alpha4"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/core.specs.alpha "0.1.24"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/tools.reader "1.3.0"]
                 [org.clojure/test.check "0.10.0-alpha3" :scope "test"]
                 [com.cognitect/transit-clj "0.8.309"]
                 [org.clojure/google-closure-library "0.0-20170809-b9c14c6b"]
                 [com.google.javascript/closure-compiler-unshaded "v20180610"]
                 [org.mozilla/rhino "1.7R5"]]
  :profiles {:1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :uberjar {:aot :all :main cljs.main}
             :closure-snapshot {:dependencies [[com.google.javascript/closure-compiler-unshaded "1.0-SNAPSHOT"]]}}
  :aliases {"test-all" ["with-profile" "test,1.5:test,1.6" "test"]
            "check-all" ["with-profile" "1.5:1.6" "check"]}
  :min-lein-version "2.0.0"
  :repositories {"sonatype-snapshot" {:url "https://oss.sonatype.org/content/repositories/snapshots"}})
