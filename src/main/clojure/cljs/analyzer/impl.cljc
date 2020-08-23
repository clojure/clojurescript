;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.analyzer.impl)

(def ANY_SYM 'any)

(def BOOLEAN_OR_SEQ '#{boolean seq})

(def BOOLEAN_SYM 'boolean)

#?(:cljs
   (def CLJ_NIL_SYM 'clj-nil))

#?(:cljs
   (def CLJS_CORE_MACROS_SYM 'cljs.core$macros))

#?(:cljs
   (def CLJS_CORE_SYM 'cljs.core))

#?(:cljs
   (def DOT_SYM '.))

(def IGNORE_SYM 'ignore)

#?(:cljs
   (def JS_STAR_SYM 'js*))

#?(:cljs
   (def NEW_SYM 'new))

(def NOT_NATIVE '#{clj not-native})

#?(:cljs
   (def NUMBER_SYM 'number))

#?(:cljs
   (def STRING_SYM 'string))

#?(:cljs
   (defn ^boolean cljs-map? [x]
        (implements? IMap x)))

#?(:cljs
   (defn ^boolean cljs-seq? [x]
     (implements? ISeq x)))

#?(:cljs
   (defn ^boolean cljs-vector? [x]
     (implements? IVector x)))

#?(:cljs
   (defn ^boolean cljs-set? [x]
     (implements? ISet x)))
