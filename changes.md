## 1.8.34

### Changes
* CLJS-1582: Type-hint extend-type first arg for primitives
* CLJS-1590: split, split-lines differs from Clojure on empty string
* CLJS-1594: NaN and both infinities cannot be elements of a set
* CLJS-1597: Redundant IPrintWithWriter test in pr-writer-impl
* CLJS-1583: (hash (symbol "/")) does not match (hash '/)
* bump tools reader
* CLJS-1492: Warn when using :optimisations instead of :optimizations
* less cryptic error if :main doesn't correspond to any file
* CLJS-744: ISequential types should implement JS indexOf, lastIndexOf
* CLJS-1411: make-array signature differs from clojure

### Fixes
* CLJS-1589: Self-host: case fail with nil
* CLJS-1596: Self-host: :load-macros and :analyze-deps don't work in cljs.js
* CLJS-1420 - get-in behavior differs from Clojure by always deferring to the 3 arity fn
* CLJS-1585: Self-host: Alias-scoped keywords
* CLJS-1577: Self-host: syntax-quote resolves on dot forms
* CLJS-1564: Self-host: cached macro *loaded* update
* CLJS-1584: Self-host: core/str error with condp
* CLJS-1521: Self-host: Macro namespaces cannot be aliased
* CLJS-1573: Self-host: Invalid UTF escaping in cljs-in-cljs
* CLJS-1570: :parallel-build causes invalid truth check in cljs.reader/read-number
* CLJS-1568: LazyTransformer doesn't implement IMeta
* CLJS-1578: Corrupted Analysis Files Break Compilation
* CLJS-1579: cljs.source-map/invert-reverse-map discards gcol
* CLJS-1580: Self-host: goog.provide offsets source-maps
* CLJS-1569: IndexedSeq doesn't implement IWithMeta / IMeta
* CLJS-1567: make-array macro missing > 2 arg arity
* CLJS-1571: Make special-symbol? true for 'var
* CLJS-1555: make-array macro missing 2 arg arity
* CLJS-970: generate assert message when compiling
* CLJS-1565: Self-host: whitespace optimization is broken
* CLJS-1541: Self-host: Cannot require 'cljs.js using cljs.jar
* CLJS-1550: Enhance docstring for extend-type wrt type-sym
* CLJS-1551: Self-host: assert-args dormant in macros
* CLJS-1552: doc for & should match fn
* CLJS-1488: cljs.repl/source Cannot read source of cljs functions that use #js reader
* CLJS-1557: Make special-symbol? return true for catch and finally
* CLJS-1542: Self-host: cljs/compile-str not handling errors properly
* CLJS-1318: Fix typo in documentation of `specify`
* CLJS-620: Warnings are generated when using a macro in argument position
* CLJS-1547: Wrong output encoding when compile with goog.LOCALE
* CLJS-1546: cljs.core/run! does not always return nil

## 1.7.228

### Enhancements
* New experimental :parallel-build compiler option

### Changes
* CLJS-1538: Type hint some cljs.core predicates
* Docstring typos
* CLJS-1463: (js-debugger) should generate nil-returning expression
* CLJS-1516: better error message when calling macros with arity
* CLJS-1514: Remove Alpha designators on *-watch and ex-*
* clojure.core/require is not thread safe, use locks
* CLJS-1505: Add tests to characterize `type` and `instance?` behavior
* CLJS-1491: Check :source-map is boolean when :optimizations :none
* split sm/encode into 2 functions so JSON generation is optional

### Fixes
* CLJS-1539: Parallel compilation fails on circular dependencies
* CLJS-1425: self-host: cljs.js/eval cb argument inconsistent with docstring
* CLJS-1425: self-host: cljs.js/eval cb argument inconsistent with docstring
* CLJS-1524: Bad hashing for Cons
* CLJS-1487: Fix handling of timestamp comparison for dependencies in JARs
* CLJS-1498: Fix parallel build logging
* CLJS-1477: Do not attempt to resolve "native" type symbols
* CLJS-1236: `constructor` needs to munged if used as namespace segment
* CLJS-1330: self-host: .toString on int needs parens
* CLJS-1512: Self-host: arithmetic form meta missing :numeric
* CLJS-1506: doc for referred fn displays alias ns
* CLJS-1504: Self-host: Pseudo-namespace for macro namespace analysis
metadata
* CLJS-1483: Minor DCE regression with advanced compilation mode

## 1.7.170

This is a breaking change for tooling libraries like lein-cljsbuild,
lein-figwheel, and boot-cljs. Refer to the corresponding documentation to
determine which version you should use.

### Enhancements
* Refactor build pipeline
* CLJS-1478: Self-host: Allow static-fns opt

### Changes
* Generate larger range of random UUIDs
* make browser REPL file reloads less chatty
* CLJS-1475: indicate that cljs.reader/read is safe
* CLJS-1470: Bump GCL Dependency
* bump Google Closure dep

### Fixes
* in system-time check that js/process.hrtime is actually a thing
* CLJS-1228: cljs.util/topo-sort is polynomial on larger dependency graphs
* check that performance.now method actually exists
* CLJS-1476: Self-host: Protocol prefixing broken for three- (or more) segment namespaces
* CLJS-1472 Patch for CLJS-1467 causes regression for nodejscli
* CLJS-1469 :modules regression
* CLJS-1445: Syntax error for var args in protocol methods
* Warn if protocol impl methods do not match its protocol
* CLJS-1451 Protocol impl do not support qualified method names
* CLJS-1422: cljs.js/eval-str fails for ns form on node.js with simple optimizations
* CLJS-1423: self-host: Requiring analyzer/compiler breaks unchecked Boolean
* CLJS-1466: Improperly munged output path for GClosure JavaScript
* CLJS-1467: Foreign Libraries not included when using :main with :simple or :advanced

## 1.7.145

### Enhancements
* CLJS-1455: high resoluting timing where available
* CLJS-1403: Add updated Windows shell scripts
* CLJS-1017: support :main for :advanced and :simple builds
* CLJS-1409: allow basic type checking of protocols
* CLJS-1404: var resolution for @param and @return
* CLJS-1395: Node.js REPL debug port support

### Changes
* CLJS-1464: docstrings for transducer arities
* Latest Google Closure Compiler dependency
* Node.js REPL sets *target*
* add cljs.analyzer.api/get-js-index
* add goog.object to list of implicit namespaces
* CLJS-1393: turn *target* into goog-define

### Fixes
* UUID hashing
* CLJS-1465: fix *main-cli-fn* doc
* CLJS-1456: bad require forms at REPL can corrupt REPL session
* CLJS-1449: self host :require-macros bug
* CLJS-1462: self host regression
* Add header bits for Node.js under :none
* CLJS-1457: unicode symbol munging
* CLJS-1442: self host, docstring typos
* CLJS-1441: portable clojure.string
* CLJS-1436: self-host, dep ns not loaded
* CLJS-1440: self-host, eval support in Web Workers
* CLJS-1400: self-host, doseq broken
* CLJS-1435: self-host, bad lexical scope
* CLJS-1434: clojure.walk no longer preseves meta
* CLJS-1432: '$ and '. symbol collision under advanced
* CLJS-1304: c.string/replace differs from Clojure
* CLJS-1430: bad code gen for self host .toString method calls
* CLJS-1353: range inconsistent with Clojure
* CLJS-1431: load-file doc output missing arglists
* CLJS-1433: cljs.js/*eval-fn* passed nil :cache
* CLJS-1299: add more support for literals to cljs.reader
* CLJS-1417: cljs.js require macros failures
* CLJS-1416: cljs.util/last-modified leaks files
* CLJS-1481: self host defprotocol regression
* CLJS-1414: only munge @param & @return if type checking
* CLJS-1401: unify runtime & compile UUID hashing
* CLJS-1395: no trailing semicolons after JS comment
* CLJS-1394: reify gensyms can clash

## 1.7.48

### Enhancements
* provide goog-define macro to support proper use of goog.define
* CLJS-1177: A compiler support for non-Closure transforms (JSX, etc)
* CLJS-1296: browser REPL should queue prints before connection then flush after connection
* add :dump-core compiler option for cljs.js config
* CLJS-1386: Symbols should be added to the constants table

### Changes
* Bump Closure Compiler dependency
* Bump Closure Library dependency

### Fixes
* CLJS-1392: cljs.repl/source regression
* CLJS-1391: Error when building for target :nodejs
* CLJS-1388: Stacktrace element handling for :output-dir w/o file/line/column
* CLJS-1311: Improve error reporting when converting JavaScript modules
* CLJS-1387: support local Closure libs that conform to classpath

## 1.7.28

### Enhancements
* New namespace cljs.js provides analysis, compilation, and eval
* CLJS-1360: Refactor JS module processing to work with recent Google Closure compiler changes
* CLJS-1282: Add a :pprint option to the default reporter in cljs.test
* CLJS-1308: :analyze-path should be extended to take a vector of paths
* CLJS-1230: ES 2015 Module Processing
* CLJS-1231: AMD Module Processing
* CLJS-1092: CommonJS Module processing

### Changes
* CLJS-1376: Printing in a tagged literal data form
* CLJS-836: Replace seq-based iterators with direct iterator for all non-seq collections that use SeqIterator
* CLJS-1367: Expose default-warning-handler and warning-enabled?
* CLJS-1267: Added the :end-test-all-vars and :end-test-vars events to have end events for all cljs.test api functions
* CLJS-1337: Move parse ns side-effects into a separate compiler pass
* CLJS-1247: Split out error printing from regular printing
* CLJS-1329: Support for reading #js tagged literals in bootstrap
* CLJS-1191: rebased patch Update clojure.walk to the current version on clojure
* CLJS-1321: remove getNamespace & getName method calls from defrecord
* CLJS-1281: Preserve test order
* CLJS-934: In the REPL return vars after defs

### Fixes
* CLJS-1316 let does not detect invalid binding vector when it contains destructuring
* CLJS-1033: take a drop accept nil as n argument
* CLJS-1324: Compiler fails to raise warning/error when invoking a keyword without arguments
* CLJS-1352: cljs.js: Allow conditional readers
* CLJS-1348: meta is printing for def at REPL
* CLJS-1342: cljs.reader/read-string should throw Error when not called with string
* CLJS-1341: Fix CommonJS conversion bug
* CLJS-1333: Analyze meta on quoted symbols
* CLJS-1210: Javascript built-in arguments replaces nil arguments locally defined by let
* CLJS-1248: alter-meta! does not work on vars
* CLJS-1276: var equality differs from Clojure
* CLJS-1310: ns libspec error message misses :import
* CLJS-428: Added step to escape docstrings with */ and associated test
* CLJS-1331: Regex literal emits invalid JS
* CLJS-1338: NPE in confirm-var-exists if suffix is ".."
* CLJS-1319: Cannot locate module namespace when filename contains dash
* CLJS-1317: Incremental compilation issues for :nodejs target
* CLJS-1227 Raise error when if form has more than 4 statements
* CLJS-1306: Browser REPL :asset-path with leading slash breaks source map support
* CLJS-1290: :refer does not work with Closure JS namespaces
* CLJS-1307: Doc for ns missing
* CLJS-1301: local :foreign-libs are not picked up the first time browser REPL is started

## 0.0-3308

### Changes
* Clojure 1.7.0-RC1 dependency
* CLJS-1292: Add IPrintWithWriter implementation for TaggedLiteral
* add cljs.core/random-uuid
* flush immediately when forwarding Node process out & err
* CLJS-1256 cache UUID hash value
* CLJS-1226: Added the :end-run-tests event to cljs.test and a dummy event handler for it

### Fixes
* CLJS-1200: compare behaves differently from Clojure
* CLJS-1293: Warning settings not conveyed via REPL
* CLJS-1291: pprint whitespace/letter checks are incomplete
* CLJS-1288: compiler doesn't emit "goog.require" for foreign library when optimization level is not set
* check that we actually read something in cjls.repl.server/read-request
* clarify cljs.test/run-tests docstring
* CLJS-1285: load-file regression
* CLJS-1284: IndexedSeq -seq implementation incorrect for i >= alength of internal array
* finish CLJS-1176, remove stray .isAlive method call
* add zero arity `newline` to match Clojure
* CLJS-1206: Images in HTML don't show up when served from localhost:9000
* CLJS-1272: :include-macros description inaccurate in require
* CLJS-1275: Corrected :test-paths in project.clj
* CLJS-1270: Docstring for delay not printed by cljs.repl/doc
* CLJS-1268: cljc support for cljs.closure/compile-file
* CLJS-1269: realized? docstring refers to promise and future
* match Clojure behavior for get on string / array. Need to coerce key into int.
* CLJS-1263: :libs regression, can no longer specify specific files
* CLJS-1209: Reduce produces additional final nil when used w/ eduction
* CLJS-1261: source fn fails for fns with conditional code

## 0.0-3269

### Fixes
* REPL support for Closure libraries that follow classpath conventions
* don't break closure libs that follow classpath conventions
* build missing .map source map & .edn caches files

## 0.0-3264

### Fixes
* Add missing JS files back to the build
* CLJS-1168: REPL fails to find .js files in :libs
* CLJS-1196: Assert failed on 3190+ while :require-ing .js file in :libs directory
* CLJS-1235: non-upstream :foreign-libs not copied to :output-dir
* CLJS-1258: stack trace mapping does not appear to work with :asset-path
* CLJS-1257: find-doc regression

## 0.0-3255

### Changes
* Update Closure Library dependency
* CLJS-1252: Update Closure Compiler Dependency to v20150505
* .clj -> .cljc for important analysis / compilation bits
* add public cljs.compiler.api namespace
* CLJS-1224: cljs.repl: Memoize stack frame mapping
* depend on tools.reader 0.9.2

### Enhancements
* add cljs.pprint/pp macro
* CLJS-710: port clojure.pprint
* CLJS-1178: Compiler does not know Math ns is not not-native
* add getBasis methods to deftype and defrecord ctors a la Clojure JVM
* support ^long and ^double type hints

### Fixes
* fix cljs-1198 async testing regression
* CLJS-1254: Update REPL browser agent detection CLJS-1253: Create/Use new Closure Library Release
* CLJS-1225: Variadic function with same name as parent function gives runtime error in advanced compile mode.
* CLJS-1246: Add cljs.core/record? predicate.
* CLJS-1239: Make eduction variadic.
* CLJS-1244: tagged-literal precondition check missing wrapping vector
* CLJS-1243: Add TaggedLiteral type & related fns
* CLJS-1240: Add cljs.core/var?
* CLJS-1214: :arglists meta has needless quoting CLJS-1232: bad arglists for doc, regression
* CLJS-1212: Error in set ctor for > 8-entry map literal
* CLJS-1218: Syntax quoting an alias created with :require-macros throws ClassCastException
* CLJS-1213: cljs.analyzer incorrectly marks all defs as tests when eliding test metadata
* CLJS-742: Compilation with :output-file option set fails

## 0.0-3211

### Changes
* CLJS-1205: Conditional reading in REPLs
* CLJS-1204: cljs.build.api/watch can now take compilation inputs
* CLJS-1203: standard way to pass multiple directories to build

### Fixes
* CLJS-1216: incorrect max fixed arity for fns both multi-arity and variadic 
* cljs.analyzer/parse-ns did not bind *cljs-file*
* CLJS-1201: compare broken for IIndexed collections
* CLJS-1202: cljs.repl/load-file is not additive
* CLJS-1199: array-map should skip dropped elements of IndexedSeq
* CLJS-1197: load-file does not reload associated macro namespace

## 0.0-3196

### Enhancements
* Conditional reading
* map clojure.core/in-ns to REPL in-ns special for existing tools
* CLJS-1171: map clojure.repl/doc, clojure.repl/source, clojure.repl/dir
* add macroexpand and macroexpand-1 macros
* CLJS-1019: REPL source map caching support
* CLJS-1154: Unmunged function names for stacktrace

### Changes
* Clojure 1.7.0-beta1 dependency
* tools.reader 0.9.1 dependency
* CLJS-1188: multi-arity fns hinder cross-module code motion
* cljs.test needs to default to sync
* CLJS-1184: log module building activity under verbose
* CLJS-1175: CLJS defmulti doesn't exhibit same defonce behavior as Clojure's defmulti, suggesting an even better reloading behavior
* CLJS-1176: redirect node REPL output through *out* and *err*, not System/out, System/err
* CLJS-1144 - expose defaul-dispatch-val and dispatch-fn multifn accessors
* CLJ-1172: supply main entry points for all standard REPLs
* less noisy REPL prompt
* add docstrings & validation to macroexpand & macroexpand-1

### Fixes
* CLJS-1192: eliminate JDK8 API dependency in cljs.repl.node
* CLJS-1158: Regression: compiler fails to see symbols defined in another namespace
* CLJS-1189: array-map will return PersistentHashMap if applied to more than (.-HASHMAP-THRESHOLD PersistentArrayMap) pairs
* CLJS-1183: load-file doesn't copy source to output directory
* CLJS-1187: var ast contains internal nodes with bad analysis :context
* CLJS-1182: semantics of load-file should be require + implicit :reload
* CLJS-1179: strange load-file behavior
* CLJS-808: Warning from `find-classpath-lib` mistakenly included in generated source
* CLJS-1169: cannot use REPL load-file on files that declare single segment namespaces
* don't use print unless printing the result of eval
* CLJS-1162: Failure to printStackTrace when REPL initialized
* CLJS-1161: actually print error stack traces to *err*, allow higher-level rebindings of *cljs-ns*
* CLJS-841: cljs.closure/build file locks
* CLJS-1156: load-file fails with :make-reader issue
* CLJS-1152: (require 'some.ns :reload) causes printing to stop working in browser REPL
* CLJS-1157: Stacktrace unmunging blindly use locals
* CLJS-1155: REPL :watch support does not play nicely with :cljs/quit
* CLJS-1137: :cljs/quit fails to actually quit in browser REPL
* CLJS-1148: ClojureScript REPL must maintain eval/print pairing
* make quit-prompt configurable
* CLJS-1149: cljs.repl/repl needs to support :compiler-env option
* CLJS-1140: typo in cljs.repl/repl, `:need-prompt prompt` instead of `:need-prompt need-prompt`

## 0.0-3126

### Fixes
* Need to wrap REPL -setup calls in cljs.compiler/with-core-cljs

## 0.0-3123

### Fixes
* CLJS-1131: cljs.closure/add-dependencies needs to be more aggressively set oriented
* CLJS-1132: compile-file analysis pass optimization broken under Closure optimization and :cache-analysis true

## 0.0-3119

### Fixes
* CLJS-1130: :foreign-libs regression under Closure optimized builds

## 0.0-3117

### Fixes
* CLJS-1126: File are not recompiled when build affecting options changes

## 0.0-3115

### Enhancements
* CLJS-806: support ^:const
* CLJS-1115: Reusable repl-bootstrap! fn

### Changes
* CLJS-667: validate extend-type and extend-protocol shape
* CLJS-1112: :repl-requires option for REPL evaluation environment
* CLJS-1111: browser REPL should have no side effects until -setup

### Fixes
* CLJS-1085: Allow to pass test environment to cljs.test/run-all-tests
* CLJS-867: extend-type with Object methods requires multi-arity style definition
* CLJS-1118: cljs.repl/doc support for protocols
* CLJS-889: re-pattern works on strings containing \u2028 or \u2029
* CLJS-109: Compiler errors/warnings should be displayed when cljs namespace 'package' names start with an unacceptable javascript symbol
* CLJS-891: Defs in "parent" namespaces clash with "child" namespaces with the same name?
* CLJS-813: Warn about reserved JS keyword usage in namespace names
* CLJS-876: merged sourcemap doesn't account for output-wrapper
* CLJS-1062: Incorrect deftype/defrecord definition leads to complex error messages
* CLJS-1120: analyze-deps does not appear to work when analyzing analysis caches
* CLJS-1119: constant table emission logic is incorrect
* CLJS-977: implement IKVReduce in Subvec
* CLJS-1117: Dependencies in JARs don't use cached analysis
* CLJS-689: js/-Infinity munges to _Infinity
* CLJS-1114: browser REPL script loading race condition
* CLJS-1110: cljs.closure/watch needs to print errors to *err*
* CLJS-1101 cljs.test might throw when trying to detect file-and-line
* CLJS-1090: macros imported from clojure.core missing docs
* CLJS-1108: :modules :output-to needs to create directories
* CLJS-1095: UUID to implement IComparable
* CLJS-1096: Update js/Date -equiv and -compare semantics based on Date.valueOf() value
* CLJS-1102 clojure.test should print column number of exception when available

## 0.0-3058

### Enhancements
* browser REPL source mapping for Firefox, Safari, Chrome
* macro support in REPL special functions
* CLJS-897: AOT core.cljs CLJS-899: AOT cache core.cljs analysis
* CLJS-1078: Nashorn REPL should use persistent code cache
* CLJS-1079: add way to execute arbitrary fn upon watch build completion
* CLJS-1034: Support REPL-defined functions in stacktrace infrastructure
* source mapping for Rhino
* CLJS-1071: support symbol keys in :closure-defines
* CLJS-1014: Support Closure Defines under :none
* CLJS-1068: node target define
* CLJS-1069: Generic :jsdoc support
* CLJS-1030: add `cljs.repl/pst`
* add `cljs.repl/apropos`, `cljs.repl/find-doc`, `cljs.repl/dir`
* fix `cljs.analyzer.api/all-ns` docstring
* add `cljs.analyzer.api/ns-publics`
* CLJS-1055: cljs.repl/doc should support namespaces and special forms
* Add ClojureScript special form doc map
* CLJS-1054: add clojure.repl/source functionality to cljs.repl
* CLJS-1053: REPLs need import special fn

### Changes
* move :init up in cljs.repl/repl
* CLJS-1087: with-out-str unexpectedly affected by *print-newline*
* CLJS-1093: Better compiler defaults
* Bump deps latest Closure Compiler, Rhino 1.7R5, data.json 0.2.6, tool.reader 0.8.16
* more sensible error if cljs.repl/repl arguments after the first incorrectly supplied
* default REPLs to :cache-analysis true
* default :output-dir for Nashorn and Node REPLs
* change ES6 Map `get` support to take additional `not-found` parameter
* deprecate clojure.reflect namespace now that REPLs are significantly enhanced, static vars, etc.

### Fixes
* stop blowing away cljs.user in browser REPL so REPL fns/macros remain available
* CLJS-1098: Browser REPL needs to support :reload and :reload-all
* CLJS-1097: source map url for AOTed cljs.core is wrong
* CLJS-1094: read option not used by cljs.repl/repl*
* CLJS-1089: AOT analysis cache has bad :file paths
* CLJS-1057: Nashorn REPL should not use EDN rep for errors
* CLJS-1086: Keyword constants should have stable names
* CLJS-964: Redefining exists? does not emit a warning like redefining array? does.
* CLJS-937: local fn name should be lexically munged
* CLJS-1082: analysis memoization bug
* CLJS-978: Analysis caching doesn't account for constants table
* CLJS-865: remove `cljs.js-deps/goog-resource` hack
* CLJS-1077: analyze-deps infinite recursive loop
* manually set *e in Rhino on JS exception
* REPL options merging needs to be more disciplined
* CLJS-1072: Calling .hasOwnProperty("source") in Clojurescript's string/replace will break with ES6
* CLJS-1064: ex-info is not printable
* Fix REPLs emitting code into .repl directory
* CLJS-1066: Rhino REPL regression
* be more disciplined about ints in murmur3 code
* Node.js REPL should work even if :output-dir not supplied
* Nashorn environment doesn't supply console, setup printing correctly

## 0.0-2913
* Support custom :output-to for :cljs-base module

## 0.0-2911

### Enhancements
* CLJS-1042: Google Closure Modules :source-map support
* CLJS-1041: Google Closure Modules :foreign-libs support
* Google Closure Modules support via :modules
* CLJS-1040: Source-mapped script stack frames for the Nashorn repl

### Changes
* CLJS-960: On carriage return REPLs should always show new REPL prompt
* CLJS-941: Warn when a symbol is defined multiple times in a file
* REPLs now support parameterization a la clojure.main/repl
* all REPLs analyze cljs.core before entering loop
* can emit :closure-source-map option for preserving JS->JS map
* REPLs can now merge new REPL/compiler options via -setup

### Fixes
* CLJS-998: Nashorn REPL does not support require special fn
* CLJS-1052: Cannot require ns from within the ns at the REPL for reloading purposes
* CLJS-975: preserve :reload & :reload-all in ns macro sugar
* CLJS-1039: Under Emacs source directory watching triggers spurious recompilation
* CLJS-1046: static vars do not respect user compile time metadata
* CLJS-989: ClojureScript REPL loops on EOF signal
* fix DCE regression for trivial programs
* CLJS-1036: use getResources not findResources in get-upstream-deps*

## 0.0-2850

### Enhancements
* CLJS-1035: REPLs should support watch recompilation

### Fixes
* CLJS-1037: cls.analyzer/ns-dependents fails for common cases

## 0.0-2843

### Enhancements
* CLJS-1032: Node.js target should support :main
* require cljs.test macro ns in cljs.test to get macro inference goodness
* include :url entries to original sources in mapped stacktraces if it can be determined   from the classpath
* support custom mapped stacktrace printing
* provide data oriented stacktrace mapping api
* CLJS-1025: make REPL source mapping infrastructure generic
* CLJS-1010: Printing hook for cljs-devtools
* CLJS-1016: make "..." marker configurable

### Changes
* CLJS-887: browser repl should serve CSS
* CLJS-1031: Get Closure Compiler over https in the bootstrap script

### Fixes
* cljs.nodejscli ns needs to set `goog.global` when `COMPILED` is true, this fixes the fundamental issues for ASYNC-110
* CLJS-967: "java.net.ConnectException: Connection refused" when running node repl
* pass relevant source map options in the incremental compile case
* add some missing source-map customization flags to optimized builds
* fix missed Rhino REPL regression, the surrounding REPL infrastructure creates cljs.user for us
* util.print has been deprecated in Node.js v0.12. Switch to console.log in Node.js REPLs.
* change `cljs.closure/watch` so it correctly watches all subdirectories do not recompile unless changed path is a file with .cljs or .js extension

## 0.0-2816

### Fixes
* CLJS-1001: reify did not elide reader metadata

## 0.0-2814

### Enhancements
* add simple source directory `cljs.closure/watch` watcher using java.nio
* CLJS-1022: Concatenate foreign dependencies safely
* CLJS-988: Support async testing in cljs.test
* CLJS-1018: Add support for cljs.core/*e Modify the JavaScript that is sent for evaluation to wrap in a try and then catch any exception thrown, assign it to *e, and then rethrow.
* CLJS-1012: Correct behavior when *print-length* is set to 0
* Added new :closure-extra-annotations compiler option allowing to define extra JSDoc annotation used by closure libraries.
* Mirrored source map support APIs on server/client
* Unified source mapping support in REPLs
* Nashorn REPL (thanks Pieter van Prooijen)

### Fixes
* CLJS-1023: regression, macro-autoload-ns? and ns-dependents need to throw on cyclic dependencies
* fix require with browser REPL, set base path to "goog/"
* CLJS-1020: off by one error in REPL source map support
* Node.js 0.12 support
* browser REPL needs to respect :output-dir
* CLJS-1006: Implicit dependency of clojure.browser.repl on cljs.repl
* CLJS-1005: Browser REPL creates 'out' directory no matter what
* CLJS-1003: fix cljs.test run-tests do-report :summary issues
* CLJS-1003: Cannot pass custom env to run-tests
* Windows Node.js REPL issues

## 0.0-2760

### Fixes
* ns spec handling regression

## 0.0-2758

### Fixes
* fix autoload macro enhancement

## 0.0-2755

### Enhancements
* CLJS-948: simplify macro usage

### Fixes
* CLJS-927: real incremental compilation
* Browser REPL regressions
* CLJS-991: Wrong inference - inconsistent behavior?
* CLJS-993: binding macro returns non-nil with empty body
* CLJS-972: Node.js REPL eats errors in required ns when using require
* CLJS-986: Add :target to the list of build options that should trigger recompilation
* CLJS-976: Node REPL breaks from uncaught exceptions

## 0.0-2740

### Changes
* local :foreign-libs can precisely override upstream :foreign-libs
* :foreign-libs :file-min is only used under :advanced optimizations
* file generated by supplying :main now idempotent
* more informative error if :main incorrectly supplied

### Fixes
* many fixes around file/resource handling for Windows users

## 0.0-2727

### Fixes
* Allow :main script imports to be configured via :asset-path

## 0.0-2725

### Fixes
* Fix Node.js support regression

## 0.0-2723

### Enhancements
* CLJS-851: simplify :none script inclusion if :main supplied
* CLJS-983: make ExceptionInfo printable

### Fixes

## 0.0-2719

### Changes
* CLJS-985: make ex-info not lose stack information
* CLJS-984: Update Node.js REPL support to use public API
* CLJS-963: do not bother computing goog/dep.js under :none

### Fixes
* CLJS-982: Var derefing should respect Clojure semantics
* CLJS-980: ClojureScript REPL stacktraces overrun prompt in many cases
* CLJS-979: ClojureScript REPL needs error handling for the special functions
* CLJS-971: :reload should work for require-macros special fn
* CLJS-936: Multi arity bitwise operators
* CLJS-962: fix inconsistent hashing of empty collections

## 0.0-2665

### Changes
* REPL -setup now must take opts
* CLJS-916: Optimize use of js-arguments in array and variadic
functions
* special case `'cljs.core/unquote`
* CLJS-945: Compile core with :static-fns true by default
* CLJS-958: Node.js REPL: Upon error, last successfully item printed

## 0.0-2657

### Changes
* Add require-macros REPL special fn

## 0.0-2655

### Changes
* add defonced cljs.core/*loaded-libs* dynamic var
* cljs.core/*print-fn* is now defonced
* throw on (var foo) when foo is not defined
* cljs.analyzer.api/resolve matches cljs.core/resolve if
  var doesn't exist return nil

### Fixes
* require needs to respect Clojure semantics, do not
  reload unless requested
* add ns/require support for :reload & :reload-all

## 0.0-2644

### Fixes
* CLJS-953: require REPL special fn can only take one argument
* CLJS-952: Bad type hinting on bit-test
* CLJS-947: REPL require of goog namespaces does not work
* CLJS-951: goog.require emitted multiple times under Node.js REPL
* CLJS-946: goog.require in REPLs will not reload recompiled libs
* CLJS-950: Revert adding compiled-by string to CLJS deps file
* CLJS-929: Minor fixes to test script
* CLJS-946: goog.require in REPLs will not reload recompiled libs

## 0.0-2629

### Enhancements
* Add Node.js REPL
* REPLs can now reuse build/analysis caching
* in-ns, require, doc support in REPLs

### Changes
* add :verbose flag to compiler to output compiler activity
* add *load-macros* to cljs.analyzer to optionally disable macro loading
* errors during ns parsing always through
* `cljs.util/compiled-by-version` needs to always return String
* pin Closure Compiler in bootstrap script
* refactor cljs.build.api namespace

### Fixes
* add cljs.test/are macro
* CLJS-931 : cljs.compiler/requires-compilation? ignores changes to build options
* CLJS-943: REPL require special fn is brittle
* CLJS-941: Warn when a symbol is defined multiple times in a file
* CLJS-942: Randomized port for Node.js REPL if port not specified
* CLJS-675: QuickStart example not working properly
* CLJS-935: script/noderepljs leaves node running after exit
* CLJS-918: preserve :arglists metadata in analysis cache
* CLJS-907: False positives from arithmetic checks
* CLJS-919 compare-and-set! relies on Atom record structure instead of protocols
* CLJS-920 add-watch/remove-watch should return reference, as in Clojure
* CLJS-921: cljs.repl/doc output includes namespace twice

## 0.0-2511

### Enhancements
* analysis caching via :cache-analysis build flag

## 0.0-2505

### Changes
* Stop generating random files for IJavaScript Strings
* added :source-map-timestamp build flag to get cache busting source
  map urls
* Enhancements to bootstrap script
* Stop warning about deps.cljs usage

### Fixes
* Fix Node.js source mapping regression introduced by commit 254e548
* CLJS-914: thrown-with-msg? is unable to get message of exception
* CLJS-915: On empty call, List and PersistentQueue do not retain meta, sorted-set/sorted map do not retain comparator

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
