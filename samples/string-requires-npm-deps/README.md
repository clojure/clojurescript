# ClojureScript string-based requires demo

Running:

1. At the root of the ClojureScript repo, run `./script/bootstrap`
2. Switch into this directory: `cd samples/string-requires-npm-deps`
3. Build the project:

``` shell
$ java -cp `ls ../../lib/*.jar | paste -sd ":" -`:../../src/main/cljs:../../src/main/clojure:src clojure.main build.clj
```

4. run the generated JavaScript with `node out/main.js`
