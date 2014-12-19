## 0.0-2498

### Fixes
* Support cljs.test/use-fixtures

## 0.0-2496

### Enhancements
* cljs.test added, mirrors clojure.test
* New cljs.analyzer.api namespace for easier access to analysis info from macros
* New cljs.analyzer.api namespace for easier access to analysis info from macros
* Support :test metadata on vars
* Support static vars
* cljs.source-map for client side source mapping
* expose ClojureScript :warnings build option
* CLJS-909: Add stable api for consumers of compiler data.

### Changes
* convert all ClojureScript tests to cljs.test
* add volatile! from Clojure 1.7
* stateful transducers use volatile!
* added `js-debugger` macro, compiles to "debugger;"
* CLJS-892: Improve performance of compare-symbols/compare-keywords
* CLJS-696: remove arguments usage from defrecord constructor
* unroll `partial`, copy & pasted from Clojure core.clj
* optimize clojure.string/join

### Fixes
* fix `cljs.nodejs/enable-util-print!`, incorrectly monkey patched `cjls.core/string-print` instead of setting `cljs.core/*print-fn*`
* cljs.reader bug, '/ incorrectly read
* avoid emitting the same goog.require

## 0.0-2411

### Enhancements
* forcing source maps to load for dynamic js reloads
* All ISeqable types are now ES6 iterable
* CLJS-863: Invalid arity error when calling 0-arity multimethod
* CLJS-622: better error reporting for zero arity protocol methods
* CLJS-506: expose more Closure minification knobs

### Changes
* CLJS-807: Emitter cannot emit BigInt or BigDecimal
* CLJS-749: Ignore .repl-* given that CLJS version is appended by default.
* CLJS-749: Append CLJS version to browser repl-env
* CLJS-749: *clojurescript-version* is unbound return empty string
* implement INamed for multi-method
* revert CLJS-801
* CLJS-888: Omit redundant {} around emitted recur
* CLJS-888: Better placement of newlines in emitter
* Join preambles with newline line to catch cases with files without newlines.
* add js-in interop macro
* Add nthrest
* CLJS-510: Throw error when :output-wrapper and :optimizations :whitespace combined
* CLJS-875: bump tools.reader dep to 0.8.10
* CLJS-879: add `update` from Clojure 1.7
* CLJS-857: change deftype*/defrecord* special forms to include their inline methods decls

### Fixes
* CLJS-885: relax type inference around numbers
* fix var resolution bug pointed out by Brandon Bloom
* CLJS-853: propagate read-time metadata on fn and reify forms at runtime
* CLJS-716: support hashing of JavaScript dates
* CLJS-814: clojure.string/reverse breaks surrogate pairs
* Recursively check IEncodeClojure in js->clj
* CLJS-873: non-higher-order calls to array-map should return PAMs
* CLJS-881: check for duplicate keys in array-map
* select-keys did not preserve metadata

## 0.0-2371

### Fixes
* CLJS-862: fix inconsistent re-pattern
* CLJS-866: Faulty ns macro desugaring
* CLJS-869: When preamble is not found in source directory, compiler does not report it

## 0.0-2356

### Fixes
* fix var analysis so that some.ns/foo.bar is handled correctly
* CLJS-854: cljs.reader could not read numbers under IE8

## 0.0-2342

### Changes
* depend on tools.reader 0.8.9

## 0.0-2341

### Enhancements
* transducers

### Fixes
* CLJS-704: warn if protocol extended to type multiple times in extend-type
* CLJS-702: warn if protocol doesn't match declared
* CLJS-859: use https for the bootstrap script
* CLJS-855: combinatorial code generation under advanced
* CLJS-858: resolve-existing var does not check vars outside current ns
* CLJS-852: same group-by as Clojure
* CLJS-847: Safari toString fix
* CLJS-846: preserve namespace metadata

## 0.0-2322

### Fixes
* CLJS-839: Mobile Safari Math.imul issue
* CLJS-845: incorrect behavior of `sequence` when given multiple collections
* count check in equiv-sequential if both arguments are ICounted
* only keep the param names when storing :method-params instead of the
  entire param AST
* preserve var metadata for deftype* and defrecord*
* preserve var metadata when creating deftype/record factory fns
* CLJS-831: Extending EventType to js/Element breaks Nashorn

## 0.0-2311

### Fixes
* fix typo which broke browser REPL
* lazier seq iterators a la CLJ-1497

## 0.0-2307

### Enhancement
* Allow multi-arity anonymous fns to optimize

## 0.0-2301

### Changes
* transducers

### Fixes
* eliminate dead branches in conditionals to prevent Closure warnings
* bad var resolution if when local contained .

## 0.0-2280

### Changes
* depend on latest org.clojure/google-closure-library

### Fixes
* fix constants table bug where keywords did not include precomputed hash-code

## 0.0-2277

## Enhancements
* All IEquiv implementor now export equiv Object method

## Fixes
* CLJS-824: Unsigned hash for keywords produced via keyword fn
* CLJS-827: CLJS-827: wrap macro expansion in try/catch
* CLJS-826: fix broken closure release script
* CLJS-825: conflict between node js support files
* typo in unchecked-subtract-int

## 0.0-2268

### Changes
* Experimental support for ES6 Map/Set interface

### Fixes
* CLJS-823: use non-native imul in Safari
* CLJS-810: re-matches returns [] if string is nil

## 0.0-2261

### Changes
* Dependency on Clojure 1.6.0

### Enhancements
* Murmur3 hashing for collections

### Fixes
* CLJS-817: Warning on use of undeclared var when creating recursive definition
* CLJS-819: cljs.reader cannot handle character classes beginning with slashes in regex literals
* CLJS-820: Missing invoke without arguments in MetaFn
* CLJS-816: clojure.set/rename-keys accidentally deletes keys

## 0.0-2234

### Fixes
* CLJS-812: Recur from case statement generated invalid JavaScript
* CLJS-811: use the correct class loader in cljs.js-deps/goog-resource
* fix fns with metadata under advanced compilation
* CLJS-809: dissoc :file metadata introduced by tools.reader 0.8.4
* mark cljs.reader vars as ^:dynamic to avoid compiler warnings
