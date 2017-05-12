## 1.9.542

### Enhancements
* CLJS-1572: REPL doesn't give error for expressions with too many right parentheses

### Changes
* cljs.spec -> cljs.spec.alpha
* CLJS-2013 - Add MapEntry type
* CLJS-2015: Self-host: `defmacro` should return the Var
* CLJS-2017: Upgrade Closure Compiler to latest April 2017 release

### Fixes
* CLJS-485: RegExp flags are being dropped by string/replace
* CLJS-1518: Case macro expansion evaluates expression twice
* CLJS-2024: Self-host: `find-ns-obj` broken for namespaces with 'a' as the first segment
* CLJS-2028: `realized?` throws on LazyTransformer
* CLJS-2010: refer-clojure :rename throws on valid invocations
* CLJS-2007: Whitespace optimizations should respect :main option.

## 1.9.521

### Fixes
* correct CLJS-1923 :foreign-libs regression

## 1.9.518

### Enhancements
* CLJS-1973: Add support for `:npm-deps` in upstream `deps.cljs`
* CLJS-1968: Enable calling JS modules that export a single function
* CLJS-1960: Require CommonJS modules directly from a ClojureScript namespace

### Changes
* CLJS-2006: Upgrade Closure Compiler to April 2017 release

### Fixes 
* CLJS-1497: `find` on an associative collection does not return collection key
* CLJS-1996: Support correct checking of :preloads when :optimizations not specified
* CLJS-1994: assoc on nil returns PHM (expected PAM)
* CLJS-1988: add :npm-deps to recognized compiler options
* Fix tiny bug in index-node-modules when no second argument is given
* CLJS-1985: `index-node-modules` should pass opts to `node-inputs`
* CLJS-1987: don't index node modules blindly
* CLJS-1519 Collection invoke errors report arity off by 1
* CLJS-1964: Validate that `:target :nodejs` and no optimizations requires a `:main` option to be present
* CLJS-1956: Add missing JS reserved keywords
* CLJS-1983: res -> mres in spec.cljs
* CLJS-1978: port CLJ-2035
* CLJS-1979: port CLJ-2043 (fix s/form of s/conformer)
* CLJS-1980: port CLJ-2100 (s/nilable form should retain original spec form)
* CLJS-1976: hash-map assoc stackoverflow
* CLJS-1957: Process JS modules errors and warnings don't get printed
* CLJS-1868 - Output simpler dependency rel paths when compiling with Closure libs
* CLJS-1967: Missing ^boolean for removed-leaf? in THM impl

## 1.9.494

### Fixes
* revert CLJS-1636: Mark some symbols in core macros ns as private

## 1.9.493

### Fixes
* CLJS-1948: Possible race condition in compiler w/ parallel-build true
* CLJS-1941: `cljs.compiler/cljs-files-in` shouldn't return `.cljc` files if a `.cljs` file exists for the namespace
* CLJS-1941: `cljs.compiler/cljs-files-in` shouldn't return `.cljc` files if a `.cljs` file exists for the namespace
* CLJS-1940: Undeclared var warning when invoking a protocol method on a `js` interop form
* CLJS-1951: Missing 0 and 1 arity versions of interleave
* CLJS-1952: Bump Closure Compiler to Feb 2017 release
* CLJS-1937: Self-host: undeclared cljs.core$macros/mod when compiling cljs/core.cljs
* CLJS-1936: cljs.analyzer declares vars which are only used in Clojure
* CLJS-1949: Self-host: cljs.compiler/munge doesn't preserve JVM compiler semantics
* CLJS-1950: Eliminate instances of #^
* CLJS-1943: Self-host: `cljs.pprint`'s macros can't be compiled
* CLJS-1945: cljs.spec/every-impl kind-fn kind-form dead code
* CLJS-1944: Can't spec generate non-vector collections
* CLJS-1946: Self-hosted: don't emit `goog.require` calls for foreign libs if optimizations different than `:none`
* CLJS-1636: Mark some symbols in core macros ns as private
* CLJS-1939: Fix Node load_file call for foreign-deps
* CLJS-1942: Self-host: `cljs.env.macros` and `cljs.analyzer.macros` can't be loaded
* CLJS-1935: When calling cljs.spec/valid?, subsequent predicates of cljs.spec/and are evaluated even when early predicate is unsatisfied

## 1.9.473

### Fixes
* CLJS-1931: Closure Compiler {{--generate_exports}} flag not supported
* CLJS-1934: Self-host: require-macros :reload / :reload-all fails
* CLJS-1932: Self-host: Perf regression macroexpand-check
* CLJS-1930: Master broken wrt static field: ES5_STRICT_UNCOMMON
* CLJS-1929: When expanding libs don't include Hidden files
* CLJS-1905: Self-host: Stacktraces for script/test-self-parity
* CLJS-1795: Support more options in the `:closure-warnings` compiler option
* CLJS-1922: Use :file as relative output path for foreign-libs
* CLJS-1831: Self-host: Improperly munge ns names
* CLJS-1925: Use of undeclared Var cljs.user/RegExp when extending protocol for RegExp
* CLJS-1920: cljs.build.api/node-inputs: package.json files are only added if module entries are top-leve
* CLJS-1916: __dirname and __filename are not defined when compiling for Node.js with optimizations :none
* CLJS-1915: cljs.test: Index out of bounds for stack element w/o line/column

## 1.9.456

### Enhancements
* Enhanced JavaScript module support
* Support Node resolution for CommonJS modules
* Externs inference
* Performance enhancements
* CLJS-1835: REPL load special fn
* CLJS-1194: Support for `data_readers.cljc`

### Changes
* expose :closure-module-roots option
* bump Closure Compiler dep
* Under Node.js don't need require entries in the goog.addDependency calls in cljs_deps.js
* do not throw on circular dependencies between Google Closure JS libs
* str macro should call str/1 function directly, added str benchmark
* CLJS-1718: Foreign lib files should be placed in a relative location
* CLJS-1858: Should allow `:cache-analysis true` and `cache-analysis-format nil`
* CLJS-1616: Self-host: improve documentation for compile-str
* CLJS-1643: Emit more informative error when emitting a type which has no emit multimethod case
* CLJS-1816: Basic timing info in verbose output
* add support for emitting inferred externs file
* add cljs.analyzer/analyze-form-seq
* CLJS-1666: Flag to optionally disable transit analysis cache encoding
* Provide more descriptive error message when invalid libspec detected
* CLJS-1768: cljs.spec perf tweaks
* CLJS-1842: Remove analyzer `:merge` hack for REPLs
* CLJS-1839: Relax the constraint that `new` and dot forms must be passed a symbol
* default to :ecmascript3 if :language-out not specified for :es6 module
* respect :language-out when processing ES6 modules
* default to :ecmascript3 if :language-out not specified for :es6 module
* inline some?

### Fixes
* CLJS-1911: Need to bind Node.js require
* CLJS-1909: Self-host: circular dependency when requiring cljs.reader
* CLJS-1906: Self-host: script/test-self-parity fails
* CLJS-1903: Remove anonymous vars from dir and apropos output
* CLJS-1897: Too many externs generated
* CLJS-1895: Externs inference needs to support user supplied externs
* CLJS-1873: Self-host: Unit tests fail owing to test.check dep
* CLJS-1874: Self-host: :fn-var true for macros
* CLJS-1877: :foreign-libs entries should be allowed to specify directories along with individual files
* CLJS-1890: s/form for s/nilable in cljs.spec does not match clojure.spec
* CLJS-1811: Can't compose cljs.spec.test.instrument (or cljs.spec.test.check) with cljs.spec.test.enumerate-namespace
* CLJS-1894: Unnecessary analysis of core.cljs on first compile
* CLJS-1893: Unnecessary analysis of core.cljs
* CLJS-1892: Dependencies in JARs are analyzed every time even if an analysis cache file exists
* CLJS-1887: add :watch-error-fn option
* CLJS-1883 Foreign libs can't be found on Node.js
* CLJS-1882 Fix constant table sort order when using :modules
* CLJS-1853: var metadata in compiled output
* CLJS-1878: prefer `some?` over `(not (nil? %))` in analyzer
* CLJS-1880: missing ^boolean on some hasNext calls
* CLJS-1875 Difference in seqable? between CLJ & CLJS
* CLJS-1829: get does not return not-found on negative indexes
* cljs.spec.test/unstrument shouldn't return the names of vars that weren't instrumented in the first place. Fixes CLJS-1812
* CLJS-1786: Add knob for controlling printing of namespaced maps
* CLJS-1836: nth doesn't throw for IndexedSeqs
* CLJS-1870: Quoted specs check in require macro symbols
* CLJS-1869: Regression importing goog.Uri
* Fix CLJS-1653 regression
* CLJS-1860: Resolve JS modules referred by their fully-qualified namespace
* CLJS-1861: Use usr/bin/env in build scripts for portability
* CLJS-1857: Fix self-host tests
* CLJS-1855: Subvec should implement IIterable
* CLJS-1856: Self-host: load-deps doesn't delegate to itself
* CLJS-1651: Self-host: Cannot replace core macro-function
* CLJS-1848: Analyzer can't find JS modules during macro-expansion
* CLJS-1851: Only output JS module processing time when `:compiler-stats` is true
* CLJS-1850: *unchecked-if* not declared ^:dynamic warning after commit a732f0
* CLJS-1849: Self-host: regression introduced by CLJS-1794
* CLJS-1844: port over Maria Geller's externs file parsing code
* CLJS-1845: Assoc on subvec should throw if out of bounds
* CLJS-1847: REPL should recognize `clojure.core/load`
* CLJS-1745: refer-clojure doesn't pull in previously excluded vars
* CLJS-1794: incomplete alias created for namespace cljs.spec warning under advanced compilation
* CLJS-1834: REPL regression, require of ns from the ns itself errors out in circular reference
* fix ns aliasing regression for JS namespaces
* CLJS-1837: Port halt-when over from Clojure
* CLJS-1820: "No such namespace" warning when referring to JS module namespace without using alias
* CLJS-1828: Add `:rename` to `require`'s docstring

## 1.9.293

### Enhancements
* CLJS-1346: Support require outside of ns

### Changes
* CLJS-1762: Bump Closure Compiler, refactor module support
* CLJS-1658: testing for protocol membership may return false positives
* CLJS-1536: REPL def symbol init collision
* CLJS-1805: Source map should take false
* CLJS-1804: Self-host: process namespace side-effects for new require without NS
* CLJS-1803: Use new require capability in REPLs
* CLJS-1796: Measure Google Closure specific optimization time
* CLJS-1782: Self-host: allow namespaces to require their own macros
* CLJS-1563: :source-map option to cljs.build.api/build should take nil
* CLJS-1785: Warn on reference to js/foo shadowed by local binding

### Fixes
* make String an implicit ns like Math. revert char? and clarify docstring. add unit tests for char?
* fix cljs.spec.test/check docstring
* CLJS-1826: Self-host: load-deps doesn't honor `:reload` and `reload-all`
* CLJS-1825: :source-map error when passing `false` under simple optimizations
* CLJS-1821: `add-preloads` should only touch sources if `:preloads` option specified
* CLJS-1814: Move docstrings for require, etc. from `cljs.repl` to their new definitions in `cljs.core`
* CLJS-1809: Add 0/1 arity to `into`
* CLJS-1824: transit cache feature leaks files
* CLJS-1294: Let macroexpand(-1) accept any quoted argument.
* CLJS-1818: (hash false) returns different value from Clojure
* CLJS-1817: Strange result when assoc'ing 0 to persistent hash map
* CLJS-1815: Fix failing analyzer tests
* follow-up on CLJS-460 defmulti ignores optional :hierarchy argument
* CLJS-1807: Better error messages for `ns*` calls
* CLJS-1802: Generated namespaces should be of the form `cljs.user.fileXXXX`
* CLJ-1935: Use multimethod dispatch value method lookup to take hierarchies into account in multi-spec
* CLJS-1682 :foreign-libs with module conversion does not works properly if it is used form deps.cljs
* CLJS-1710: spec/double-in not implemented
* CLJS-1787: Make cljs.spec explain pluggable
* CLJS-1781: Add cljs.hash-map-test to self-parity tests
* CLJS-1788: Port CLJ-2004: include retag in multi-spec form
* CLJS-1765: Empty iterator for hash maps with nil key
* CLJS-1784: nth doesn't throw on strings or arrays
* CLJS-1773: Self-host: Don't resolve unqualified symbols / keywords with $macros
* CLJS-1770: goog-defines broken for integers
* CLJS-1600: Destructuring defprotocol fn args causes defrecord impls to silently fail
* CLJS-1335: resolve-macro-var: information missing for macros
* CLJS-1633: Improve error associated with invalid foreign-libs :file path
* CLJS-1775: `get` with `nil` returns as if `get` with `0`
* CLJS-1780: Records without extmaps fail to iterate
* CLJS-1774: Self-host: Report filenames in warns in test-self-parity
* CLJS-1779: keyword 2-arity constructor accepts anything for both parameters which leads to different hashing

## 1.9.229

### Fixes
* CLJS-1772: Dependency index can incorrectly overwrite `.cljs` files with `.cljc` files if both are present
* pass unconform along on conformer with-gen instead of warning

## 1.9.227

### Fixes
* CLJS-1763: Defining a var that clashes with `cljs.core` throws a compiler error instead of warning

## 1.9.225

### Fixes
* CLJS-1759: Errors writing transit analysis cache if parallel build
* CLJS-1760: Self-host: test-cljs-1757 failing in test-self-parity
* CLJS-1751: port fix lost type hints in map destructuring
* CLJS-1756: Add test.check JAR to the bootstrap script
* CLJS-1757: cljs.spec/exercise-fn doesn't work when passed a quoted symbol
* CLJS-1754: Add boolean? generator
* fix REPL regression which removed warnings

## 1.9.216

### Fixes
* CLJS-1749: Missing `cljs.spec.impl.gen/double*`
* CLJS-1747: Port `clojure.spec/assert` over to ClojureScript
* fix CLJS-1663 multi-arity fn instrument regression

## 1.9.211

### Fixes
* CLJS-1746: Log the result of loading a dependency
* CLJS-1657: Self-host: Implicit macro loading with alias
* CLJS-1742: Add docstring for new refer-clojure REPL special
* CLJS-1274: Allow assignment to namespace-qualified names in current namespace
* CLJS-1744: rest produces nil for larger maps
* CLJS-1740: Self-host: Need to port more of CLJS-1733
* CLJS-1741: Add :rename to :refer-clojure in ns docstring
* CLJS-1737: Self-host: clojure alias implicit macro use regression
* invalid cljs.spec/merge res call
* CLJS-1739: seq on map literal with 9 elements leads to rest producing nil
* CLJS-1738: Self-host: need to update call to check-use-macros-inferring-missing

## 1.9.198

### Enhancements
* CLJS-1508: Extend ns form to support :rename option
* CLJS-1507: Implicit macro loading: macro var inference in :refer
* CLJS-1692: Autoalias clojure.* to exisiting cljs.* namespaces if
possible
* CLJS-1350: Compiler support for browser REPL
* CLJS-1729: Support `use` special function in REPLs
* CLJS-1730: Support `refer-clojure` special function in REPLs

### Changes
* CLJS-1515: Self-host: Allow :file key in cljs.js/*load-fn*
* add toString implementation to Vars
* Use a js array to create collections in cljs.reader
* CLJS-1640: Use the unshaded version of the closure compiler
* add :browser-repl to list of known opts
* add browser REPL preload
* parity with Clojure 1.9.0-alpha10 clojure.spec
* bump to tools.reader 1.0.0-beta3

### Fixes
* CLJS-1733: Macro inference issue for macros & runtime vars with the same name
* CLJS-1735: Self-host: cljs.spec speced-vars instance
* CLJS-1736: cljs.spec.test: checkable-syms* called with 0-arity
* CLJS-1708: Self-host: [iu]nstrument-1 needs to qualify [iu]nstrument-1*
* CLJS-1707: Self-host: with-instrument-disabled needs to qualify *instrument-enabled*
* CLJS-1732: Add docstrings for new use and use-macros REPL specials
* CLJS-1720: Qualify symbols and namespaced keywords in spec macros
* CLJS-1731: Self-host: do_template problem with script/test-self-parity
* CLJS-1556: Invalid code emit for obj literal
* CLJS-1607: bug with `specify!` in JS prototypes with `static-fns` true
* CLJS-1591 avoid analyzing invoke arguments multiple times
* CLJS-1638: :elide-asserts disables atom validators in :advanced
* CLJS-1721: 3-arity get-in fails on types which do not implement ILookup
* CLJS-1728: Update doc for ns for new :rename capability
* CLJS-1727: Regression when evaluating non-sequential forms at the REPL
* CLJS-1490: Watch macro files in cljs.build.api/watch
* CLJS-1719: Port destructuring namespaced keys and symbols
* CLJS-1653: cljs.spec: keys* causes exception
* CLJS-1700: Support clojure.* aliasing when not in vector
* CLJS-1717 remove map from equiv-map
* CLJS-1716: No longer possible to use same alias for :require-macros and :require
* Use keyword options in js->clj 1-arg impl
* Add support for regex in transit for compiler analysis cache
* Escape non-Latin1 characters before base64 encoding the source-map string
* CLJS-1698: cljs.spec: every res call needs &env
* CLJS-1695: Self-host: Port cljs / clojure namespace aliasing
* CLJS-1697: doc on inferred macros fails
* CLJS-1699: Update docstring for ns
* CLJS-1694: Self-host: Port macro var inference in :refer


## 1.9.89

### Enhancements
* CLJS-1688: :preloads compiler option for loading other entry points prior to :main
* cljs.spec - support gen overrides by name in addition to path
* cljs.spec - every and every-kv

### Changes
* added bounded-count

### Fixes
* missing cljs.spec/fn-specs -> cljs.spec/get-spec in cljs.spec.test ns
* CLJS-1687: Self-host: cljs.spec: inst-in-range? and int-in-range? need qualification
* CLJS-1668: cljs.spec: c alias needs expansion in int-in

## 1.9.76

### Enhancements
* CLJS-1648: Getting Source Info into ex-info data for Analysis Errors
* cljs.spec updated to Clojure 1.9.0-alpha7 changes

### Changes
* bump Google Closure Library dep
* AOT cljs.spec nses

### Fixes
* CLJS-1679: Self-host: Incorporate spec tests
* CLJS-1680: Self-host: Don't require items no longer provided by Closure
* CLJS-1654: cljs.spec: var name in s/fdef non-conformance
* CLJS-1655: cljs.spec: conformer docstring indicates :clojure.spec/invalid
* CLJS-1656: Self-host: cljs.spec: speced-vars* fn not resolving
* CLJS-1661: cljs.spec: non-spec'ed fn var printing
* compute read/write opts for transit if possible, handle JSValue
* CLJS-1660: cljs.spec: Always return var from instrument / unstrument
* CLJS-1671: Bad cljs.spec interactive instrumentation session 
* CLJS-1664: The filename aux.cljs is a problem on windows.
* CLJS-1667: bad describe* for and-spec-impl
* CLJS-1699: Self-host: s/fdef ns-qualify *ns* name field access

## 1.9.36

### Enhancements
* Write analysis caches as Transit if transit-clj available

### Changes
* Clojure 1f25347
* Clojure 47b8d6b
* Optimize seq (&) destructuring as per commit (0aa3467) of Clojure

### Fixes
* CLJS-1611: Function arity dispatch returns arity
* only print specs in REPL if we actually have some
* CLJS-1663: Calling instrumented multi-arity function causes exception
* CLJS-1650: `cljs.reader/read-map` now returns array-map/hash-map based on the size of the sequence.

## 1.9.14

### Enhancements
* clojure.spec ported to cljs.spec

### Fixes
* CLJS-1649: Possible issue with in cljs.reader or cljs.core/PersistentHashMap
* CLJS-1647: Rethrow exception from parallel-build
* CLJS-1642: cljs.core/reductions does not respect 'reduced'
* CLJS-1635: Var type implements IEquiv but not IHash
* CLJS-1629: Fix warning about duplicate test-pr-str
* CLJS-1637: Missing docstrings for a few vars

## 1.8.51

### Changes
* bump Closure Compiler to v20160315
* bump tools.reader to 1.0.0-beta1
* CLJS-1624: Avoid useage of JSC_HOME in test bash scripts

### Enhancements
* CLJS-1626: cljs.test for bootstrap

### Fixes
* CLJS-1588: defrecord satisfies? behavior under bootstrap
* CLJS-1632: docs / arglist consistency
* CLJS-1612: Resolve ns aliases in syntax-quote
* CLJS-1621: Foreign libs modules of different types don't compile together
* CLJS-1617: inlined `list` evaluation order
* :parallel-build race condition

## 1.8.40

### Fixes
* CLJS-1603: Only warn for misspelled comp/REPL opts
* :warning-handlers missing for known compiler options
* CLJS-1592: Self-host: Robustness for core tests

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
