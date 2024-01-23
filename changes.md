## 1.11.132

### Fixes
* CLJS-3410: JavaScript double values should not hash to the same result
* CLJS-3381: Invokable JS namespaces used as constructors not hinted properly
* CLJS-3395: `(set! a -x false)` doesn't work
* CLJS-3399: :as-alias does not work with symbols
* CLJS-3401: dedupe '+ and '_PLUS symbols with :optimize-constants
* CLJS-3400: macroexpand does not expand and and or without args correctly
* CLJS-3398: Docstring of with-redefs should mention usage of ^:dynamic in production
* CLJS-3386: defn varargs argument should be nil when no varargs are passed
* CLJS-3384: get-in should not unreduce values.

### Changes
* CLJS-3378: Change default :language-in to :ecmascript-next
* CLJS-3385: Extend empty? to counted? colls that arent seqable, such as transients
* CLJS-3327 Add :node-modules-dirs configuration
* CLJS-3391: add cljs.test/run-test
* CLJS-3369: Speed up random-uuid by generating 4 digits at a time
* CLJS-3014: Promote Error->map to be a core fn
* CLJS-3394: Add ECMASCRIPT options for 2018-2021
* CLJS-2268: Make clojure.string/escape consistent with Clojure
* Bump closure lib to 2023 release
* CLJS-3407: bump tools.reader to 1.3.7
* remove EXPERIMENTAL from ES6 iterator support
* CLJS-3406 implement reset-vals! and swap-vals! through protocol
* CLJS-3363: reduce-kv on seq of map entries
* CLJS-3393: Efficient drop, partition for persistent/algo colls
* CLJS-3408: Handle @extends in externs
* CLJS-3392: datafy support for js/Error and ExceptionInfo
* CLJS-3379: Add support for node_modules with .cjs extension
* CLJS-3387: Browser repl unable to serve wasm files

## 1.11.60

### Fixes
* broaden scope of UUID equiv to implementers of protocol rather than concrete type
* CLJS-3382: Fix regression in .apply after CLJS-3024

## 1.11.57

### Fixes
* CLJS-3377: Objects created from required JS constructor are not correctly hinted
* get-bridged-alias-map is not needed in self-hosted

## 1.11.54

### Changes
* use `require` instead of `load` for `cljs.vendor.bridge`, addresses issue
  reported by Bruce Hauman wrt. Figwheel

## 1.11.51

### Changes
* CLJS-3372: Vendorize data.json, transit-clj, and tools.reader
  data.json and transit-clj are no longer dependencies. CLJS-3375 bridges
  tools.reader for backwards compatibility
* Clojure 1.10 minimum version  
* Update Google Closure Compiler, transit-java, tools.reader dependencies to latest
* CLJS-2820 Compile cljs.loader regardless of whether :modules are used
* CLJS-3370: improved uuid regex to only accept hex characters
* Update / clarify docstrings, CLJS-3358, CLJS-3359, CLJS-3360, CLJS-3361, CLJS-3364
* CLJS-3354: map-invert should use transients and reduce-kv instead of reduce
* CLJS-3350: Update test.check dependency
* CLJS-3294: data_readers.cljc doesn't provide a way to have target-specific
  behaviour

### Fixes
* CLJS-3373: Externs Inference issue with vars invoked from foreign libs
* CLJS-3368: let binding can shadow globals, leading to strange behavior
* CLJS-3367: Backward conflict test in prefer-method causes incorrect exception
* CLJS-3371: Invalid warning on record constructor
* Fix apply of IFn for more than 20 arguments
* CLJS-3288: selfhost: *eval-fn* not bound for :js sources
* CLJS-3362: some-fn has different short-circuiting when using 3 predicates
* CLJS-3356: halt-when not usable within #'c.c/into
* CLJS-3352: Self-host negative zero emitted as positive zero
* CLJS-3319: make PersistentHashMap release inodes correctly

### Enhancements
* CLJS-3348: Implement new functions for parity with Clojure 1.11
* CLJS-3353: Add the new iteration function introduced in Clojure 1.11
* CLJS-3347: Create clojure.math namespace
* CLJS-3299: port CLJ-2603
* CLJS-3346: :as-alias
* add update-vals & update-keys

## 1.11.4

### Fixes
* CLJS-3345: package.json exports can be a dupe of main

## 1.10.914

### Fixes
* CLJS-3339: cljs.core/hash should type hint call to js/isFinite
* CLJS-3333: defonce expansion non-hygienic with respect to core names
* CLJS-3334: exists? evaluates to true for cljs.user//
* CLJS-3341: Revert dependency issue in dep order patch

### Changes
* CLJS-3332: Cannot require `@firebase/auth`
* CLJS-3335: CI, test-and-or-code-gen-pass fails on Windows
* CLJS-3440: CI, Compiler tests fail test-cljs-3235 regarding react-select
* CLJS-3342: Run CI compiler unit tests on Windows
* CLJS-3338: Missing CI test coverage
* CLJS-3343: Failing js module processing test on Windows

## 1.10.896

### Fixes
* CLJS-3336: REGRESSION: Cannot require `goog`
* CLJS-3337: REPL, Regression for :reload
* Fix Windows path issue in cljs.externs

## 1.10.891

### Changes
* Update Google Closure Compiler `v20210808`
* Update Google Closure Library `0.0-20211011-0726fdeb`
* CLJS-3330: Flag for legacy loading of goog.object & goog.array

### Fixes
* CLJS-3324: hash-map behavior differs from Clojure
* CLJS-3056: runtime namespace load order is independent from ordering in ns macro :require form
* CLJS-3074: Resolve :warning-handlers compiler option when symbols
* CLJS-3317: PersistentVector invoke does not align with Clojure

## 1.10.879

### Changes
* Revert CLJS-3276 - macros that expand to require

### Fixes
* CLJS-3096 Add :exception true to prepl errors
* CLJS-3313: Protocol implementations via metadata: ClojureScript behaves differently from Clojure

## 1.10.866

### Changes
* Google Closure v20210505

### Enhancements
* CLJS-3260: and/or optimization as compiler pass, fixes core.async go macro issue
* CLJS-3276: Support macros that expand to require statements

### Fixes
* CLJS-3309: and/or opt bug - passes to remove dropped locals from anon fns,
  missing :children keys on binding nodes
* CLJS-3300: cljs.loader regression
* CLJS-3293: Some npm packages fail to require
* CLJS-3306: subvecs must implement IAssociative -contains-key?
* CLJS-3307: Allow extending IAssociative -contains-key? to native
* CLJS-3305: defrecord must implement IAssociative -contains-key?
* CLJS-3303: checked arrays enabled in advanced
* CLJS-3304: Higher order checked arrays
* CLJS-3284: Use of private deftype by public function in another namespace when
  inside an if causes warning
* CLJS-3282: document bundle target in CLI help
* CLJS-3283: Support -contains-key? protocol check in contains?
* CLJS-3296: Update conj docstring for missing arities
* CLJS-3298: visibility diagnostic group typo
* CLJS-3302: Fixup docstring for `default-dispatch-val` and `dispatch-fn`

## 1.10.844

### Changes
* Google Closure Compiler v20210302, Google Closure Library
* Update to tools.reader 1.3.3

### Enhancements
* CLJS-3235: Support accessing a property of a library as a namespace itself

### Fixes
* CLJS-3287: selfhost: eval does not catch and return errors
* CLJS-3263: Always print #inst year with 4 digits
* CLJS-3291: Incorrect #inst parsing with respect to Julian / Gregorian calendar transition
* CLJS-3281: CLI help for target missing comma
* CLJ-3279: Error when :npm-deps is boolean and :install-deps true
* CLJS-3275: compute upstream npm-deps when :npm-deps is not set
* CLJS-3273: preserve ns-name when processing an :ns* op
* CLJS-3200: reduce code generated by destructure macro for maps
* CLJS-3271: nth on range produces nonexistent values considering floating point
* CLJS-3261: Docstring for cljs.js/eval-str specifies incorrect default for :context
* CLJS-2959: sort and sort-by should retain meta
* CLJS-3255: cljs.build.api/build doesn't work with single arity / 2-arity with nil
* CLJS-3019: Error->map should produce qualified symbols for :type
* CLJS-3130: UUID compares equal to other values
* CLJS-3257: `satisfies?` produces an inference warning when given an unhinted argument
* add `:nodejs-rt` to the list of build affecting options
* CLJS-2880: cl-format octal and Unicode character directives fail

## 1.10.764

### Fixes
* Export Google Closure Library config directly to window in brwoser
* CLI: If :main supplied to -c pass it along to -r
* Revert CLJS-2582

## 1.10.758

### Changes
* More useful functions added to cljs.analyzer.api

### Fixes
* CLJS-3242: Code Splitting Breakage
* CLJS-3244: Warnings for clojure.browser.net with :static-fns true
* Fix foreign-lib loading, was checking for `:nodejs` instead of `:nodejs-rt`
* CLJS-3239: Infinite analysis with dotted symbols
* CLJS-3238: Watch incompatible with :bundle, throws :nodejs-related exception
* CLJS-3237: compiling with --target node errors at runtime with document undefined
* CLJS-3236: defprotocol externs inference warnings
* Fix (require ... :reload) REPL pattern

## 1.10.741

### Changes
* Removed REPL/target support for Rhino, Nashorn, Graaljs
* Update Closure Compiler and Google Closure Compiler dependencies
* CLJS-1628: Make instances of js/Symbol printable

### Enhancements
* Add :target :bundle for integration with JavaScript bundlers (webpack, metro, etc.)
* Add cljs.main --install-deps flag
* Add :bundle-cmd compiler option for triggering JS bundler at end of build
* Add :nodejs-rt compiler option to diable Node.js runtime support (for bundles)
* Add :target-fn compiler option to allow users to support other targets outside of ClojureScript
* Add :npm-cmd to allow users to choose `npm` or `yarn` for their dep tool
* Make fast REPL prompt availble to 3rd party REPLs
* Transpile GCL libraries that need it
* Enhance cljs.main to be properly extensible
* repl-env providing namespaces can now be arbitrary not limited to cljs.repl.*
* CLJS-3185: Facilitate protocol static dispatch inlining
* CLJS-3199: Interop with JavaScript's iterable objects via Iterator protocol

### Fixes
* CLJS-3230: seq on empty Iterable produces an empty seq
* CLJS-2908: Don't show quick prompt if :verbose or :repl-verbose
* CLJS-2898: cljs.main: ClojureScript version should not be displayed if there are inits
* CLJS-2863: Need to reject incorrect fn with fixed arity of more params than variadic
* CLJS-3086: Switch clj-nil to not be considered a numeric type
* CLJS-3211: Widen cljs.core/defprotocol sig arguments from list to seq (to include Cons)
* CLJS-712 & CLJS-2957: resolve-var for dot still wrong, externs inference regression
* CLJS-2862: Externs inference warning when extending Object
* CLJS-3161: Include :property for :warning-type :target
* CLJS-3181: Externs inference fails, even when manual type hint is provided
* CLJS-3224: Fix cljs.loader due to usage of removed setModuleUris API
* CLJS-3223: get-js-index 0-arity should call get-js-index 1-arity
* CLJS-3220: Self-host test-import failing on asserts.assert
* CLJS-3218: NPE during Closure transpilation in self-host tests
* CLJS-3217: script/test-self-parity compilation failure stale reference to goog.text.LoremIpsum
* CLJS-3215: Travis has remnant CLI test involving GraalJS
* CLJS-3219: Problem with asserts namespace inside goog.math.Long
* CLJS-3119: get with negative ndx on string inconsistent with Clojure
* CLJS-3214: script/uberjar fails with GraalJS
* CLJS-3210: Single arity arithmetic ops don't warn on bad arguments
* CLJS-3213: Browser REPL server hang when receive unknown POST
* CLJS-3209: With clojurescript 1.10.597 HelloWorld compiled with to 94K of JS with advanced optimizations turned on
* CLJS-3191: Optimize cljs.core/re-find
* CLJS-3192: Optimize cljs.core/re-matches
* CLJS-3193: Optimize cljs.core/re-pattern
* CLJS-3202: Make :/ keyword hash consistent with =
* CLJS-3203: Overriding root path in s/cat using s/gen gives an error

## 1.10.597

### Changes
* CLJS-3120: Add :sigs to protocol var for compatibility with Clojure
* CLJS-2247: Warn when overwriting protocol method
* CLJS-3085: Types are inferred for dynamic vars
* CLJS-3097: Fix compatibility with tools.reader 1.3.1 and bump it
* CLJS-2750: tag coll in ci-reduce as not-native
* CLJS-3095: `apply vector` with array acts as `vec`
* CLJS-3093: Check subvec arguments
* CLJS-2868: Add ^string hints
* CLJS-3054: Align behavior of set/union and into with Clojure

### Enhancements
* CLJS-3077: Avoid generating unnecessary functions
* CLJS-3107: Eliminate checked ifs in TransientArrayMap
* CLJS-3164: Optimize assoc on IAssociative values
* CLJS-3147: Allow Node require from foreign lib
* CLJS-3144: NPM Modules should have all their vars marked to avoid .call invokes
* CLJS-3145: Node.js support libs cljs.nodejs and cljs.nodejscli generate random files
* CLJS-3141: Improve perf of cljs.source-map.base64/encode
* CLJS-3134: Thread predicate-induced inference through and
* CLJS-3123: analyze google closure namespaces
* CLJS-3133: simple-* / qualified-* predicate-induced inference
* CLJS-2886: count specializations for string and array
* CLJS-2950: Direct field access for keyword lookup on records

### Fixes
* CLJS-3190: Double arity warning constructing directly-accessed record
* CLJS-3137: fspec cannot be reused in clojurescript but can be in clojure
* CLJS-3124: Non-number lookup on transient vector succeeds after persistent!
* CLJS-3149: REPL load-file doesn't resolve npm requires correctly
* CLJS-3163: Skip analyzing specials in type-check-induced-tag
* CLJS-3172: Unable to import goog.async.ConditionalDelay
* CLJS-3158: Improperly widened cross-param loop/recur inference
* CLJS-3168: Self-host: externs ns used unconditionally in analyzer
* CLJS-3140: Not inferring on implements?
* CLJS-3143: assoc docstring regarding growing vector
* CLJS-3123: 'for' loop silently ignores extra forms in body
* CLJS-3017: Error->map: Map js/InternalError and js/TypeError
* CLJS-2683: Suppress compiler options in watch log
* CLJS-2881: cl-format character directive with \space fails
* CLJS-2879: Close analysis cache file
* CLJS-3051: Update to Graal RC12 in CI
* CLJS-3088: Update CI to use JavaScriptCore 4
* CLJS-3092: Peek on subvecs doesn't behave properly
* CLJS-3076: let defined variadic functions not destructuring as expected with :static-fns true
* CLJS-3067: Fix compiler crash when requiring cljs.loader w/o modules
* CLJS-3068: Compiler error with if and emit-var
* CLJS-2301: Avoid use of deprecated goog.string/isEmptySafe in clojure.string/blank?
* CLJS-3058: Remove dangling goog.date.relativeWithPlurals reference
* CLJS-3061 Fix docstring for chunked-seq?

## 1.10.520

### Changes
* Browser REPL serves more mime types

### Fixes
* CLJS-3048: Revert CLJS-3038
* CLJS-3049: Undefined fdef is still present in result of (stest/checkable-syms)

## 1.10.516

### Changes
* CLJS-3036: Provide a clojure.edn namespace for Clojure compatibility
* CLJS-2967: Make clojure.spec.alpha reloadable
* CLJS-2968: Support immutable GCC DependencyOptions
* CLJS-2693: Have Range implement IChunkedSeq
* CLJS-2971: Make cljs.spec.alpha/fn-sym private
* CLJS-2912: Reuse seq in some

### Enhancements
* CLJS-2865: Optimize string expression concatenation
* CLJS-2866: Predicate-induced type inference
* CLJS-2901: Return type inference for multi-arity & variadic fns

### Fixes
* CLJS-3043: error__GT_str not defined for cli scripts
* CLJS-3037: Self-host: Add datafy tests to self-parity tests
* CLJS-3031: loop / recur inference, warnings not suppressed on initial pass
* CLJS-3030: Regression with core.async surrounding select-keys / find on String
* CLJS-3038: Improve error message when clojure.test.check is not required
* CLJS-3034: Truthy-induced inference
* CLJS-3023: Instrumenting next gives maximum call stack size exceeded
* CLJS-3033: Maintain backward compatibility test.check keyword
* CLJS-2964: Requiring spec.test.alpha loads clojure.test.check
* CLJS-2103: clarify arg type and order constraints of keys and vals
* CLJS-3011: Port improved runtime exception printing to non-Node REPLs
* CLJS-2981: Mishandling of :npm-deps Boolean value computing upstream deps
* CLJS-3027: sorted-map can no longer be returned by a macro unless it has keyword keys
* CLJS-3028: atom docstring typo
* CLJS-2994 Ensure all prepl :vals are pr-str-ed
* CLJS-3020: cljs.main: Incorrect documentation for the --compile flag
* CLJS-2652: Line breaks in long options for compile
* CLJS-3025: Typo when updating cljs.analyzer.macros/wrapping-errors
* CLJS-2955: Self-host: spec check macro compile-time expansion
* CLJS-2999: Update datafy to use inherent support for protocols via metadata
* CLJS-2945: Print spec failure details
* CLJS-3010: Datafy does not properly check for whether the datafied value supports metadata
* CLJS-3008: Typo in error phase key placed in exception and misplaced cause
* CLJS-2956: Stack overflow when specing core =
* CLJS-2913: improvements to exception messages and printing
* CLJS-3005: empty on Cons shouldn't keep metadata
* CLJS-2958 - make symbol work on keywords and vars
* CLJS-3000: Don't pass meta to next/rest/empty of seqs
* Add support for protocols via metadata
* CLJS-3000: Don't pass meta to next/rest/empty of seqs
* CLJS-1888 - Seqs of PHMs and PAMs do not handle metadata correctly
* CLJS-2794 :Return identity when with-meta is called with identical meta
* CLJS-2980: Calling "check-fn" gives "is not public" warning
* CLJS-2977: Spec instrumentation regression with varargs / :static-fns
* CLJS-2929: Port datafy
* CLJS-2995: Instrumented self-calling multi-arity fn throws maximum call stack exceeded with optimizations advanced
* Fix source maps missing local binding names
* CLJS-2991: Need to wrap js-obj output with parens
* CLJS-2976: s/fdef docstring should refer to cljs.spec.test.alpha/check
* CLJS-2538: nth on fractional indices near array and string bounds
* CLJS-2909: clojure.walk/postwalk does not preserve MapEntry type objects
* CLJS-2537: Negative fractional index in contains? on array
* CLJS-2933: Consistent #object printing whitespace
* CLJS-2873: Improved inference for loop / recur
* CLJS-2989: Fast-path issues for predicate-induced inference based on satisfies?
* CLJS-2867: Inferred return type of namespace is string
* CLJS-2975: unstrument returns symbol of non-instrumented var
* CLJS-2974: empty for non-emptyable should return nil
* CLJS-2825: Eliminate unnecessary ^boolean annotations
* CLJS-2979: re-seq is relying on undefined behavior of subs
* remove redundant exists? check in dynaload
* fix incorrect cljs.core.MapEntry usage

## 1.10.439

### Changes
* CLJS-2904: Default :npm-deps to false
* CLJS-2878: Update Closure Compiler to v20180805
* CLJS-2827: Avoid var special in core macros for private var access
* CLJS-2819: Warn on non-dynamic earmuffed vars
* CLJS-2806: Bump test.check to 0.10.0-alpha3
* CLJS-2815: Support string keys in :global-exports
* CLJS-2812: Support for overriding object printing
* CLJS-2805: Bump tools.reader to 1.3.0
* CLJS-1702: Warning when using private vars
* Align ClojureScript AST to tools.analyzer

### Enhancements
* CLJS-2903: Support fingerprinting
* CLJS-2897: cljs.main: Display initial REPL prompt sooner
* CLJS-2884: Support for GraalJS RC6
* CLJS-2859: Graal.JS: Enable high-res timers by default, allow user-configuration
* CLJS-2831: Add a graaljs REPL environment
* CLJS-1997: Outward function type hint propagation
* CLJS-844: Optimize js->clj by switching to transients
* CLJS-2442: `set` and `vec` performance enhancements

### Fixes
* CLJS-2953: stest/with-instrument-disabled prints warning of private use
* CLJS-2728: Ability to disable macro spec checks
* CLJS-2843: s/explain of evaluated predicate yields :s/unknown
* CLJS-2951: Add a spec generator for some?
* CLJS-2940: Can't define nilable spec on undefined pred
* CLJS-2948: Stack overflow calling instrumented variadic fn with zero args
* CLJS-2793: Instrumenting breaks function with varargs
* CLJS-2934: Enhanced delay printing
* CLJS-2864: Optimize str macro for single arity case
* CLJS-1297: defrecord does not emit IKVReduce protocol
* CLJS-2937: docstring for to-array
* CLJS-2943: Update merge-with to use key / val
* CLJS-2941: seqable? should return true for nil
* CLJS-2915: Tests fail if directory has a period (.) in the path
* CLJS-2782: lein test fails if directory has hyphens
* CLJS-2911: Avoid infinite loop on infinite partitions
* CLJS-2906: cljs.main: Crash when with default REPL
* CLJS-2883: Instrumentation fails compilation with a large number of spec'd functions
* CLJS-2896: Allow parallel analysis and compilation
* CLJS-2893: seq: use .-length instead of alength for strings
* CLJS-2890: fspec role in problem path is not useful
* CLJS-2887: Improve names in core macro specs
* CLJS-2891: stop including data in ex-info message
* CLJS-2888: Printing of spec problems buries the failing predicate which should be more prominent
* CLJS-2861: Self-host: :checked-arrays not working
* CLJS-2852: Clojure imparity: ns-publics returns different arglists for macros
* CLJS-2725: Doc on spec keywords
* CLJS-2665: Port clojure.spec.test.alpha/enumerate-namespace
* CLJS-2848: Default explain printer prints root val and spec
* CLJS-2846: [spec] s/tuple explain-data :pred problem
* CLJS-2847: s/coll-of and s/every gen is very slow if :kind specified without :into
* CLJS-2841: [spec] instrument exception doesn't contain function name in ex-data
* CLJS-2842: [spec] Clarify s/every docstring for :kind
* CLJS-2845: [spec] generate random subsets of or'd required keys in map specs
* CLJS-2844: [spec] Add support for undefining a spec
* CLJS-2840: [spec] s/keys explain-data :pred problem
* CLJS-2839: [spec] s/& explain-data :pred problem
* CLJS-2838: [spec] s/& does not check preds if regex matches empty collection
* CLJS-2837: [spec] `cat` specs should verify value is sequential
* CLJS-2541: binding not made in parallel
* CLJS-2832: Bad code gen for `((not empty?) "foo")` when compiled with no optimizations
* CLJS-2855: Browser REPL prints empty string after require
* CLJS-2821: Update doto docstring to not use Java example
* CLJS-2817: Suppress private var warnings for specs on private vars
* CLJS-2822: cljs.core.specs.alpha: Map bindings should be `:kind map?`
* CLJS-2829: Fix deep object property access for :global-exports
* CLJS-2816: Skip non-string package.json browser entry values
* CLJS-2814: Fix munge-node-lib/global-export on self-host
* CLJS-2811: cljs-1537-circular-deps fail on Windows
* CLJS-2807: Macroexpand failure with set literal
* CLJS-2799: Handle nth on seqables with negative indexes
* CLJS-2798: ChunkCons -next doesn't handle nil more
* CLJS-2589: allow / as a protocol method name in cljs

## 1.10.339

### Changes
* Bump transit-clj to 0.8.309

## 1.10.329

### Changes
* add :val to :const node
* rename ast op :constant -> :const

### Fixes
* CLJS-2787: Record comparison is broken when instance is constructed from another record instance via map factory
* CLJS-2780: Async tests prematurely terminate in Node
* CLJS-2783: with-out-str conflicts with :infer-externs
* CLJS-2730: Fix docstrings in filter, filtev, remove, and take-while
* CLJS-2703: module name substitution test fails if hypen in directory path
* CLJS-2731: Failure comparing sorted sets
* CLJS-2746: Missing provides for JS modules
* CLJS-2772: Trying to run `cljs.main` repl with `:modules` results in `brepl_deps.js` being `clojure.lang.LazySeq`
* CLJS-2736: Elements returned from sets as functions are not the actual elements in the set
* CLJS-2298: REPLs should automatically load user.(cljs|cljc) files at root of Java classpath

## 1.10.312

### Enhancements
* CLJS-1871: A declare with :arglists should generate static function calls
* CLJS-2688 cljs.main: Accumulate all meaningful repeated inits modules using global-exports
* CLJS-2681: Accepting multiple paths to the --watch option for cljs.main
* CLJS-2706: Better error messages when missing namespaces contain dashes

### Changes
* CLJS-2777: Bump Closure-compiler
* validate :main
* CLJS-2771: Elide "use strict"1 from final output

### Fixes
* CLJS-2278 & CLJS-2279
* goog.global lookup must be a string
* CLJS-2775: cljs.main: Node modules not installed if -re node
* CLJS-2767: Externs inference warnings for defrecord and deftype
* CLJS-2754: Broken cli tests
* CLJS-2769: Eliminate goog.structs.AvlTree.Node in self-parity test
* CLJS-2766: Revisions to exists? fails in self-host
* CLJS-2764: exists? is not nil safe
* CLJS-2760 Make browser repl web-severs mime-type case-insensitive
* CLJS-2755: Can't generate uri instances
* CLJS-1677: Requiring [goog] breaks an :advanced build, but the compiler exits successfully
* Recompile cljs.loader in REPL
* CLJS-2733: Throw error message if too few or too many args to throw
* CLJS-2751: script/bootstrap --closure-library-head misses goog/text
* CLJS-2480: Periods at end of analyzer warnings
* CLJS-2618 Fix docstring for `remove-tap`
* CLJS-2743 Fix docstring misspelling
* CLJS-2724: Native Node modules Node (like "fs") cannot be required
* CLJS-2702: Accomodate new Closure Library dependency loading strategy
* CLJS-2741: Function invoke errors report arity off by 1
* CLJS-2745: Add GraalVM to the set of JavaScript engines we can test against
* CLJS-2739: Optimize node_modules indexing
* CLJS-2619: clojure.reflect needs exclude for macroexpand
* CLJS-2713: test-reader fails on Windows
* CLJS-2715: Have goog-define return the var at the REPL
* CLJS-2727: cljs.repl/err-out visible from cljs
* CLJS-2734: Add :arglists to defmulti
* CLJS-2721: test-cljs-2580 failing in windows CI
* CLJS-2726: test-cljs-2678-global-exports-infer failing on Windows
* CLJS-2678: Infer-externs doesn't work for JS modules using global-exports
* CLJS-2718: Setting *warn-on-infer* in REPL throws a SyntaxError
* CLJS-2385: cljs.analyzer/infer-type pass infers tag with incorrect priority
* CLJS-1918: case needs a type hint for keywords case when using *warn-on-infer*
* CLJS-1970: Cannot infer target type for list/vector expressions
* CLJS-2669: Use simple dummy package for test-cljs-2580
* CLJS-2716: Add ChakraCore to Windows CI (AppVeyor)
* CLJS-2147: apply test suit
* CLJS-2711: System newline breaking some tests on Windows
* CLJS-2712: Make Windows CI fail if a test fails
* CLJS-2708: Windows. ClojureScript fails to compile when node.js module is `require`d

## 1.10.238

### Enhancements
* cljs.main, simple command line access to Compiler & REPLs
* cljs.server.* namespaces for integration with -Dclojure.server.repl
* :aot-cache compiler to enable global AOT caching of dependencies in JARs
* :stable-names compiler flag, to support vendorization when using :modules, 
  defaults to true when using :modules.
* Add :webworker & :nashorn target
* pREPL implementation (usage requires Clojure 1.10.0-alpha)
* Add :package-json-resolution build option, allowing to choose which
  package.json entries are being used; defaults to :webpack (if no :target
  is set) or :nodejs (if the :target is :nodejs); also supports a custom
  vector of entries (e.g. ["browser", "main"]).

### Changes
* CLJS-2592: :npm-deps using ES6 modules with .mjs extensions are not detected correctly
* AOTed ClojureScript artifact is now the default, for sources only use the
  "slim" Maven classifier
* Bump Closure Compiler
* REPL now show uniform prompts
* CLJS-2660: Add cljs.core/eval which, delegates to an overridable *eval*
* CLJS-2375: Remove AMD Module Support
* CLJS-2413: Port core.specs.alpha to ClojureScript
* CLJS-2423: Allow custom :output-wrapper function
* Map entries are no longer two element vectors, now MapEntry instances
* *print-fn* automatically set
* CLJS-2561: AOT compile browser REPL client js
* CLJS-2581: Create a cljs.repl/*repl-env* dynamic var and bind it around cljs repl loops

### Fixes
* CLJS-2680: Passing :watch-fn via --compile-opts to cljs.main
* CLJS-2692: cljs.core.specs.alpha: Import list needs to require quote
* CLJS-2696: Large code size in Clojurescript 1.10.x for minimal code with optimizations advanced
* CLJS-2699: Use higher-level Closure API for module-processing
* CLJS-2691: goog.require in module-processed files shouldn't require goog.base
* CLJS-2689: Don't try to use node resolve for goog: modules
* CLJS-2676: Bad cljs.loader behavior for modules with multiple provides
* CLJS-2673: Regression: Can't require cljs.js
* CLJS-2650: Fix JAR compilation of cljs.loader
* CLJS-2671: Double analysis warning for source in JAR with AOT cache
* CLJS-2643: Socket REPL output can be directed to the wrong place
* CLJS-2670: Update cljs.compiler/warning-types
* CLJS-2491: Inference warnings are not reported
* CLJS-2653: REPL crash when mapping stacktrace in Chrome for js/blah
* CLJS-2639: Compiler crash when using aot cache with parallel compile
* CLJS-2520: Synthesize ClojureScript version if using non-built ClojureScript dep
* CLJS-2522: Handle sources that are maps in build-modules
* CLJS-2521: Only expand module graph when modules are actually used
* CLJS-2519: Module loader doesn't load :cljs-base properly
* CLJS-2493: Self host: respect :source-map-timestamp compiler option
* CLJS-2500: Call process-js-modules after compiler restart
* CLJS-2516 Build API fails targeting Node (QuickStart)
* CLJS-2462: subvec on non-integral indexes fails
* CLJS-2474: with-meta on lazy-seq causes separate realization
* CLJS-2501: Fix crash in cljs.util/compiled-by-version and build-options
* CLJS-2476: recur across try should fail compilation
* CLJS-2495: Closure compilation errors should stop Cljs compilation
* CLJS-2496 PHM seq and iter should return MapEntry on nil key case
* CLJS-2473: Infer character literals to have string type
* CLJS-2455: nth fails on eduction
* CLJS-2001: Add map-entry? predicate
* CLJS-2131: Calling empty on a ChunkedSeq should return empty list
* CLJS-1743: Transient maps should support IFn
* CLJS-2452: reverse empty vector returns nil
* CLJS-2450: Allow configuring ignored JS module extensions
* CLJS-2417: Inter-ns s/fdef expansion side effect fails when load cached source
* CLJS-2447: Ignore css JS modules
* CLJS-2397: Multi-arity function instrumentation fails with :static-fns true
  CLJS-2197: Calling instrumented var fails to check conformance
* CLJS-2443: doseq should return nil with no collections
* CLJS-2430: Fix foreign-libs with Node target
* CLJS-2414: Self-host: Macro specs are instrumented
* CLJS-2387: CLJS Analyzer does not correctly detect cache hits for analyzed spec files
* CLJS-2405: Register dumped specs fails
* CLJS-2416: Self-host: defn macro Var doesn't have :macro true meta
* CLJS-2425: Remove unnecessary zero? checks from nat-int?
* CLJS-2377: The CLJS compiled uses deprecated modules on Java 9
* Allow clj->js to preserve namespaces
* CLJS-2391: Unable to :stub a function using stest/instrument
* CLJS-2378: keep the :npm-deps-installed? to avoid to reinstall NPM deps

## 1.9.946

### Changes
* CLJS-2300: Delegate clojure.string/capitalize to goog.string/capitalize
* CLJS-2374: Print js/Infinity, js/-Infinity, js/NaN using new reader literals
* bump tools.reader (1.1.0)
* CLJS-2372: update hash to use the new infinity literals
* CLJS-2364: Bump Closure Compiler to the Sep 2017 version
* CLJS-2340: Have js-keys delegate directly to good.object/getKeys
* CLJS-2338: Support renamePrefix{Namespace} closure compiler option

### Fixes
* CLJS-1576: fix source-map string encoding by applying encodeURIComponent and fixing string/replace call
* CLJS-2294: Always use opts with implicit opts added
* CLJS-2166: Add uri? predicate
* CLJS-2368: Self-host: Never compile macro namespaces with `:optimize-constants true
* CLJS-2367: Self-host: :def-emits-var leaks into loaded namespace processing
* CLJS-2352: Emit valid JS for NaN etc. even when used w/ CLJ >= 1.9.0-alpha20
* CLJS-2339: Significant code reload slowdown with :npm-deps
* CLJS-2361: Self-host: circular dependency detection doesn't handle REPL self-require
* CLJS-2356: Self-host: circular dependency detection is not correct
* CLJS-2354: Self-host: `compile-str` doesn't handle `clojure` -> `cljs` aliasing
* CLJS-2353: use portable `node-module-dep?` function in analyze-deps
* CLJS-2345: escape paths emitted as args to cljs.core.load_file
* CLJS-2349: Port reset-vals! and swap-vals! over from Clojure
* CLJS-2336: Call alength once in areduce and amap
* CLJS-2335: Avoid alength on strings
* CLJS-2334: Also gather dependencies from foreign-libs that are modules
* CLJS-2333: module-deps.js doesn't correctly compute `main` if aliased in browser field
* CLJS-2332: module_deps.js doesn't process `export from` correctly
* CLJS-2330: Don't set `"browser"` field for Closure if target is :nodejs
* CLJS-2326: Indexing node_modules can't find `main` when it doesn't have an extension
* CLJS-2328: Args are not provided to *main-cli-fn* with optimizations advanced
* CLJS-2327: module_deps.js doesn't know about browser field advanced usage

## 1.9.908

### Enhancements
* CLJS-2323: data readers support for records

### Changes
* CLJS-2322: Require only `@cljs-oss/module-deps` to be installed to figure out Node.js dep graph
* CLJS-2321: Do not automatically call `set-loaded!` on the user's behalf
* CLJS-2316: Upgrade Closure Compiler to August release
* CLJS-2317: Upgrade Google Closure Library
* CLJS-2234: Make build scripts optionally less verbose
* CLJS-2314: Eliminate str call on literal strings in str macro
* CLJS-2291: Set up Windows CI
* CLJS-2286: Simplify JS module processing

### Fixes
* CLJS-2324: module-graph doesn't munge :requires when indexing inputs
* CLJS-2309: :module foreign-libs order not preserved
* CLJS-2318: module-deps.js doesn't respect the package.json `module` field
* CLJS-2312: Miss-compile: Uncaught SyntaxError: Unexpected token default
* CLJS-2315: module_deps.js can't resolve JSON modules
* CLJS-2313: :language-out is a build affecting option
* CLJS-2306: Provide better warning message when namespace can't be found
* CLJS-2303: Disable duplicate alias checking for self-host
* CLJS-2307: Closure warns on unreachable checked array code
* CLJS-2305 Tests: Unable to resolve symbol: opts in this context
* CLJS-2299: Failure with alias and bad require of clojure.spec
* CLJS-2302: Disable process-shim by default in Node.js targets
* CLJS-2266: Self-host: Cannot require clojure.x where clojure.x has no macros namespace
* CLJS-2304: Fix compiler infrastructure tests on Windows
* CLJS-2261: Issue using interop record constructors in macros namespaces
* CLJS-2296: Foreign libs that expose modules are not being processed under target nod
* CLJS-2293: Self-host: Can't load cljs.js owing to set alias
* CLJS-2295: `index-node-modules-dir` can't determine :main for package.json files that have `.` in the string
* CLJS-1620: In JavaScript ES2015 modules default export name is munged to default$
* CLJS-2287: Self-host: `require` prints result of loading node deps / global exports
* CLJS-2290: Node packages using require('assert') fail compilation
* CLJS-2281: module_deps.js cannot compute inputs for ES6 sources
* CLJS-2284: Fix build API tests not to pollute `out` in the current directory
* CLJS-2282: Some valid keywords are strings in JS object literals
* CLJS-2283: Regression with js-obj and gobject alias

## 1.9.854

### Enhancements
* CLJS-2280: Provide process.env :preload and auto-configure
* CLJS-2279: Infer `:module-type ` for provided `node_modules`
* CLJS-2250: Support :foreign-libs overrides via :provides
* CLJS-2243: Self-host: Add support for :global-exports
* CLJS-2232: Self-host: Add support for string-based requires
* add *print-fn-bodies* knob, set to false
* CLJS-2198: Safe array operations
* CLJS-2217: Support `:rename` for JS modules
* CLJS-2214: Support :global-exports for foreign libraries
* CLJS-1428: Add a cljs.core/*command-line-args* var
* CLJS-2061: Support ns :require for JS libs, allow strings along with symbol
* CLJS-2148: Add warnings for invalid use of aget and aset
* CLJS-2143: Add support for symbol preprocess values

### Changes
* CLJS-2273: Bump tools.reader to 1.0.3 and development dependencies
* CLJS-2235: Allow passing extra maven opts to build scripts
* CLJS-2267: Allow ^:const inlined vars to affect if emission
* CLJS-2245: Add support for using a local `node_modules` installation through a new `:node-modules` compiler flag
* CLJS-2002: Don't throw when no *print-fn* is set
* support Clojure primitive array type hints, core.async no longer
  gives warnings
* CLJS-2213: Node.js target should use node_modules index to emit platform specific require
* CLJS-2200: bump to tools.reader 1.0.2
* CLJS-2135: require macro prints last result of loaded-libs
* CLJS-2192: Add ChakraCore testing facilities
* CLJS-1800: Defer to tools.reader for cljs.reader functionality
* CLJS-2163: Clean up uses of aget / aset on objects
* CLJS-2184: Add `ns-publics` and `ns-imports`
* CLJS-2183: Assert arguments are quoted symbols in some core macros
* CLJS-2182: Assert argument to resolve is a quoted symbol
* CLJS-2186: Update docstrings for aget/aset to be consistent with Clojure
* CLJS-2180: Allow compiling `:modules` with whitespace optimizations
* CLJS-1822: Use `:file-min` when processing JS modules with advanced optimizations
* CLJS-2169: Error when compiling with :source-map and advanced optimizations
* CLJS-2037: Throw if overwriting alias in current namespace
* CLJS-2160: Add loaded? and prefetch functions to cljs.loader
* CLJS-2148: Add unsafe-get and use goog.object
* CLJS-2161: Bump Closure Compiler to June 2017 release

### Fixes
* CLJS-1854: Self-host: Reload ns with const
* CLJS-2278: JavaScript object literals are printed wth keys that cannot be read
* CLJS-2276: Self-host: Need test.check dep for CLJS-2275
* CLJS-2275: cljs.spec.alpha/fdef resolves eagerly
* CLJS-2259: Extra .cljs_node_repl directory containing cljs.core output
* CLJS-2274: Update CI script to install deps
* CLJS-2269: Warn on top level code split loads
* CLJS-2272: Tests that depended on default install deps behavior failing
* CLJS-2255: Clean up :npm-deps
* CLJS-2263: Docstring for neg-int? backwards
* CLJS-2262: Correct comment that *warn-on-infer* is file-scope
* CLJS-2258: Stack overflow regression for sequence xform applied to eduction
* CLJS-2256: Generated code doesn't add newline after sourceMappingURL comment
* CLJS-2254: Module Indexing: Provide relative paths for a package's main module
* CLJS-2248: Build API tests rely on Yarn
* CLJS-2239: Self-host: Add `:target :nodejs` to the docstrings in cljs.js
* CLJS-2251: Follow-up fix to CLJS-2249 and related commit
* CLJS-2249: Provide a test for d4b871cce73
* CLJS-2246: Revert CLJS-2245 and CLJS-2240 and fix `lein test`
* CLJS-2244: Orphaned processed JS modules breaks :modules
* CLJS-2242: Lots of undeclared Var warns in cljs.spec.gen.alpha
* CLJS-2241: Multiple requires of Node.js modules in non :nodejs target are not idempotent at the REPL
* CLJS-2229: Ensure that new modules work works correctly with REPLs
* CLJS-2238: Perf regression with node module indexing
* CLJS-2240: don't shell out to module_deps.js if `:npm-deps` not specified
* CLJS-2230: Double checked arrays
* CLJS-2227: Squelch some of the array access tests
* CLJS-2228: Port CLJS-2226 to module_deps.js
* CLJS-1955: data_readers.cljc can't reference handlers in user code
* CLJS-2225: Need to add :checked-arrays to known compiler opts
* CLJS-2226: :npm-deps can't index scoped packages
* CLJS-2224: Resolve-var is wrong wrt. module resolution
* CLJS-2223: Self-host: Undeclared Var deps/native-node-modules
* CLJS-2222: CI failing after CLJS-2217
* CLJS-2219: Enable JSC under test-simple
* CLJS-2218: Make ClojureScript aware of native node modules
* CLJS-2220: Add runtime :npm-deps tests
* CLJS-2212: Replace missing-js-modules with new index-node-modules-dir
* CLJS-2211: Add function to index a top-level node_modules installation
* CLJS-2208: module_deps.js is not compatible with older JS implementations
* CLJS-2207: cljs.test/js-filename is using non-portable .endsWith
* CLJS-1764: Double warning for undeclared Var (REPL only)
* CLJS-2204: Tests failing with respect to lodash/array namespace
* CLJS-2205: NPM deps: Correctly compute `:provides` if file ends in `index.js`
* CLJS-2203: REPL is turning on all warnings by default (including :invalid-array-access)
* CLJS-2201: Self-host: test-js-filename failing
* CLJS-2202: String requires should work from Cljs files in classpath
* CLJS-2199: String requires broken after recompile
* CLJS-2172: memfn docstring refers to Java and reflection
* CLJS-1959: under :nodejs target we should provide __dirname and __filename constants
* CLJS-1966: cljs.test assumes the output directory is '/out/' when determining the filename for a failed or errored test result.
* CLJS-2191: Clean up doc references to clojure.spec.* in favor of cljs.spec.*
* CLJS-2194: cljs.util/relative-name bug
* CLJS-2195: npm-deps tests are not idempotent
* CLJS-2179: Add test for preprocess JS module as symbol
* CLJS-2152: "is not a relative path" exception thrown when `:libs` directory is provided.
* CLJS-2193: :npm-deps dependencies are implicit
* CLJS-1797: Update aot_core to support build with MINGW on Windows
* CLJS-2189: Add test for :preloads
* CLJS-2188: Use :invalid-array-access instead of :invalid-aget / :invalid-aset
* CLJS-2181: Can't compile string sources with modules
* CLJS-2185: Self-host: Docstrings for bootstrap helpers
* CLJS-2178: Add tests for `:npm-deps`
* CLJS-2177: NPM deps & JS modules fixes for Windows
* CLJS-2175: ES6 Module processing broken with Closure v20170626
* CLJS-2175: Add test to check ES6 module processing works
* CLJS-2176: module_deps.js: fix regexes for Windows paths
* CLJS-2173: Fix `npm install` when `:npm-deps` in Windows
* CLJS-2164: Require cljs.js results in warning about new unsafe-get macro
* CLJS-1998: Printing an Object with a null prototype throws an error
* CLJS-2158: cljs_base module generates empty goog.require
* CLJS-2157: Automatically generate cljs.loader/set-loaded! call
* CLJS-2154: Provide compiler info & timing when compiling modules
* CLJS-2151: Rollback removal of dependency information for node targeted compilation
* CLJS-2141: Self-host: cljs.js is using undeclared symbol lib
* CLJS-2145: inode_find issue with hash-map
* CLJS-2142: Can't instrument a namespace containing constants

## 1.9.671

### Fixes
* CLJS-2139: Undeclared var regression in fn bodies
* CLJS-2137: Missing INext on some sequences 
* CLJS-2136: Clarify IFind contract to avoid double-lookups
* need to elide :c.a/analyzed in c.a/analyze-wrap-meta to avoid dumping unintended
  with-meta expressions
* resolve returns improperly constructed Var
* fix :fn-invoke-direct edgecase around keywords

## 1.9.660

### Changes
* CLJS-2134: Warn on variadic signatures in protocol method implementation

### Fixes
* CLJS-2133: Invalid variadic IFn implementations now fail

## 1.9.655

### Enhancements
* CLJS-2108: faster set equivalence
* CLJS-2099: Optimize apply by avoiding .apply
* CLJS-2046: Optimize expression in call position
* CLJS-1876: Faster reduce for PV, Subvec and ChunkedSeq
* CLJS-2080: Faster equiv-map
* CLJS-2066: Avoid analyzing named fn literal bodies twice
* CLJS-2065: Improve analyzer munge performance

### Changes
* CLJS-2130: Self-host: Add `:fn-invoke-direct` to public API docstrings
* CLJS-2112: Iterator based reduce path
* CLJS-2100: to-array calls seq too often
* CLJS-2041: Compiler flag to drop Function.prototype.call invokes
* CLJS-2093: inline ^:const var values
* CLJS-2042: Variadic invoke calls array_seq inefficiently
* CLJS-2003 remove redundant calls to `str` in munge/demunge
* CLJS-1907: Improve error message from cljs.reader/read-string
* CLJS-1724: Include IIterable in fast-path-protocols
* CLJS-924: Better error message for mistaken use of 'def'
* CLJS-1599: UUIDs are not equal for upper/lower case strings
* NodeJS REPL accepts a :path opt to set NODE_PATH
* CLJS-1886: RangedIterator should only be created from cljs.core.PersistentVector instances
* CLJS-2068: MapEntry, RedNode and BlackNode are IComparable
* CLJS-2073: Don't flush for every emitted line
* CLJS-2089: Warn message wrong for recur to protocol with nil
* CLJS-2085: defrecord recur method head target object
* CLJS-1977: defrecord should use murmur hashing like Clojure
* CLJS-2076: modules should support wildcard namespaces
* CLJS-2078: add resolve macro

### Fixes
* CLJS-2128: Fix regression in CLJS-1886
* CLJS-2126: Add new compiler option :fn-invoke-direct to build-affecting options
* CLJS-2054: Private core names still result in "already declared" warnings
* CLJS-2125: Duplicate HOF invoke warnings if :static-fns true
* CLJS-2119: s/form for s/& is qualified with `clojure.spec.alpha`
* CLJS-2121: Self-host: Document string as valid name arg
* CLJS-2124: Self-host: Tests failing wth Could not find tag parser for :cljs.spec.alpha
* CLJS-2122: Self-host: Non-symbol ns names dumped into env
* CLJS-2117: Self-host: Port CLJS-1989 to self-hosted
* CLJS-1989: s/fdef expansion side effect fails when load cached source
* CLJS-2116: Need to handle un-namespaced symbol when evaluating `foo.core
* CLJS-2109: incorrect syntax-quote symbol resolution (resolve-symbol 'clojure.core) -> 'clojure/core
* CLJS-2113: nth function produces different results from clojure when using a negative index on a sequence
* CLJS-2115: Pass not-found in the native-satisfies? branch of nth
* CLJS-2111: Transit analysis caching broken for JSValue or regex
* CLJS-2101: Undeclared var in do chain of defs
* CLJS-2104: Const-replaced exprs do not emit js "return"
* CLJS-1992: declare after def should have no effect
* CLJS-1251: Missing semicolons when emitting deftype and defrecord mistaken use of 'def'
* CLJS-1685: Incorrectly lazy subvec when start param is nil
* CLJS-1641: Multi-arity defn copies arguments unnecessarily for all cases
* CLJS-2092: Redundant call to equiv-map in PAM.-equiv
* Check for compilation success, and lib folder
* CLJS-2030: Case with grouped keyword test emit result-expr multiple times
* CLJS-2094: Predicates unit tests constructs a uuid with nil
* CLJS-1891: UUID.toString can return non-string
* CLJS-2072: Eliminate reflection in cljs.js-deps/build-index
* CLJS-2012: Find on PHM with nil entry always returns nil entry
* CLJS-2057: fix language-in options (es6 deprecated and add missing es2016)
* CLJS-2060: Backport CLJ-2141 Return only true/false from qualified-* predicates
* CLJS-2091: reify docstring ISeqable example needs correction
* CLJS-2088: fix caching collision between macros ns and regular ns in boostrap
* CLJS-2036: Relative path exception thrown when :preloads requires a :foreign-lib
* CLJS-2083: Test equiv-map for maps which do not impl IKVReduce
* CLJS-2081: Self-host: Regression with CLJS-2079
* CLJS-2079: Records and maps are not equal
* CLJS-2075: PersistentTreeMap.reduce-kv does not honor reduced?
* Browser REPL regression
* CLJS-2069: Self-host: automatic `clojure` -> `cljs` aliasing doesn't work when loading macro namespaces
* CLJS-2067: reduce-kv / inode-kv-reduce fails to honor reduced?
* CLJS-2056: Self-host: test-self-parity failing wrt cljs.core/fn symbol

## 1.9.562

### Enhancements
* CLJS-2027: Add language-in for ECMA 2017 and ECMA Next
* CLJS-2026: Add Compiler option for rewrite polyfills

### Changes
* CLJS-2021: subvec throws when passed non-vector
* CLJS-1884: Give a chance to MetaFn to be removed by closure under :advanced 
  optimization Replace with-meta calls by -with-meta calls where possible
* CLJS-2052: Port new spec.alpha enhancements
* Update Google Closure Compiler dependency
* Update Google Closure Library dependency
 
### Fixes
* CLJS-2053: Regression: cljs.spec.alpha/any for fdef
* CLJS-2039: remove extraneous argument from ChunkBuffer.chunk
* Fix assumption that all closure-compliant JS is goog.*
* CLJS-2035: Self-host: Add map-entry-test to self-parity
* CLJS-2033: set-validator! should check current state
* CLJS-2008: Self-host: backport fixes to threading macros
* CLJS-2005: Bad error message with duplicate arity function definitions
* CLJS-2032: Case macro expansion evaluates expression twice when no matching clause
* CLJS-2023: User supplied type hints stopped working on js/goog.DEBUG
* CLJS-2020: defmulti "miss" performance poor
* CLJS-2034: Sequence and Eduction produce infinite loop in transducer that appends to the reduction

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
