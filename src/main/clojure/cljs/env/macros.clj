;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.env.macros
  (:refer-clojure :exclude [binding ensure]))

(defmacro with-compiler-env
   "Evaluates [body] with [env] bound as the value of the `*compiler*` var in
 this namespace."
  [env & body]
   `(let [env# ~env
          env# (cond
                 (map? env#) (atom env#)
                 (and (instance? cljs.core/Atom env#) (map? @env#)) env#
                 :default
                 (throw
                   (js/Error.
                     (str "Compiler environment must be a map or atom containing a map, not "
                       (type env#)))))]
      (cljs.core/binding [cljs.env/*compiler* env#]
        ~@body)))

(defmacro ensure
  [& body]
  `(let [val# cljs.env/*compiler*]
     (when (nil? val#)
       (set! cljs.env/*compiler* (cljs.env/default-compiler-env)))
     (try
       ~@body
       (finally
         (when (nil? val#)
           (set! cljs.env/*compiler* nil))))))
