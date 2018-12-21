;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core.server
  (:refer-clojure :exclude [with-bindings resolve-fn prepl io-prepl])
  (:require [clojure.tools.reader.reader-types :as readers]
            [clojure.tools.reader :as reader]
            [cljs.env :as env]
            [cljs.closure :as closure]
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.repl :as repl]
            [cljs.compiler :as comp]
            [cljs.tagged-literals :as tags]))

(defmacro with-bindings [& body]
  `(binding [ana/*cljs-ns* ana/*cljs-ns*
             ana/*unchecked-if* ana/*unchecked-if*
             ana/*unchecked-arrays* ana/*unchecked-arrays*]
     ~@body))

(defn- resolve-fn [valf]
  (if (symbol? valf)
    (or (resolve valf)
      (when-let [nsname (namespace valf)]
        (require (symbol nsname))
        (resolve valf))
      (throw (Exception. (str "can't resolve: " valf))))
    valf))

(defn repl-quit? [v]
  (#{":repl/quit" ":cljs/quit"} v))

(defn prepl
  "A REPL with structured output (for programs)
  reads forms to eval from in-reader (a LineNumberingPushbackReader)
  Closing the input or passing the form :cljs/quit or :repl/quit will cause it
  to return

  Calls out-fn with data, one of:
  {:tag :ret
   :val string ;;eval result
   :ns ns-name-string
   :ms long ;;eval time in milliseconds
   :form string ;;iff successfully read
  }
  {:tag :out
   :val string} ;chars from during-eval *out*
  {:tag :err
   :val string} ;chars from during-eval *err*
  {:tag :tap
   :val string} ;values from tap>

  You might get more than one :out or :err per eval, but exactly one :ret
  tap output can happen at any time (i.e. between evals)
  If during eval an attempt is made to read *in* it will read from in-reader unless :stdin is supplied
"
  [repl-env {:keys [special-fns] :as opts} in-reader out-fn & {:keys [stdin]}]
  (let [repl-opts (repl/repl-options repl-env)
        opts (merge
               {:def-emits-var true}
               (closure/add-implicit-options
                 (merge-with (fn [a b] (if (nil? b) a b))
                   repl-opts opts)))
        EOF (Object.)
        tapfn #(out-fn {:tag :tap :val %1})
        env (ana-api/empty-env)
        special-fns (merge repl/default-special-fns special-fns)
        is-special-fn? (set (keys special-fns))]
    (env/ensure
      (repl/maybe-install-npm-deps opts)
      (comp/with-core-cljs opts
        (fn []
          (with-bindings
            (binding [*in*  (or stdin in-reader)
                      *out* (PrintWriter-on #(out-fn {:tag :out :val %1}) nil)
                      *err* (PrintWriter-on #(out-fn {:tag :err :val %1}) nil)
                      repl/*repl-env* repl-env]
              (let [opts (merge opts (:merge-opts (repl/setup repl-env opts)))]
                (binding [repl/*repl-opts* opts]
                  (repl/evaluate-form repl-env env "<cljs repl>"
                    (with-meta `(~'ns ~'cljs.user) {:line 1 :column 1}) identity opts)
                  (try
                    (add-tap tapfn)
                    (loop []
                      (when (try
                              (let [[form s] (binding [*ns* (create-ns ana/*cljs-ns*)
                                                       reader/resolve-symbol ana/resolve-symbol
                                                       reader/*data-readers* tags/*cljs-data-readers*
                                                       reader/*alias-map*
                                                       (apply merge
                                                         ((juxt :requires :require-macros)
                                                           (ana/get-namespace ana/*cljs-ns*)))]
                                               (reader/read+string in-reader
                                                 {:eof EOF :read-cond :allow :features #{:cljs}}))]
                                (try
                                  (when-not (identical? form EOF)
                                    (let [start (System/nanoTime)
                                          ret (if (and (seq? form) (is-special-fn? (first form)))
                                                (do
                                                  ((get special-fns (first form)) repl-env env form opts)
                                                  "nil")
                                                (repl/eval-cljs repl-env env form opts))
                                          ms (quot (- (System/nanoTime) start) 1000000)]
                                      (when-not (repl-quit? ret)
                                        (out-fn {:tag :ret
                                                 :val (if (instance? Throwable ret)
                                                        (Throwable->map ret)
                                                        ret)
                                                 :ns (name ana/*cljs-ns*)
                                                 :ms ms
                                                 :form s})
                                        true)))
                                  (catch Throwable ex
                                    (out-fn {:tag :ret :val (Throwable->map ex)
                                             :ns (name ana/*cljs-ns*) :form s})
                                    true)))
                              (catch Throwable ex
                                (out-fn {:tag :ret :val (Throwable->map ex)
                                         :ns (name ana/*cljs-ns*)})
                                true))
                        (recur)))
                    (finally
                      (remove-tap tapfn)
                      (repl/tear-down repl-env))))))))))))

(defn io-prepl
  "prepl bound to *in* and *out*, suitable for use with e.g. server/repl (socket-repl).
  :ret and :tap vals will be processed by valf, a fn of one argument
  or a symbol naming same (default identity)"
  [& {:keys [valf repl-env opts] :or {valf #(if (string? %) % (pr-str %))}}]
  (let [valf (resolve-fn valf)
        out *out*
        lock (Object.)]
    (prepl repl-env opts
      (readers/source-logging-push-back-reader *in* 1 "NO_SOURCE_FILE")
      #(binding [*out* out, *flush-on-newline* true, *print-readably* true]
         (locking lock
           (prn (cond-> %1
                  (#{:ret :tap} (:tag %1))
                  (assoc :val (valf (:val %1))))))))))

(comment

  ;; eval in order

  (defmacro clj-eval [form]
    `(quote ~(eval form)))

  (require '[cljs.repl.node :as node])

  (io-prepl :repl-env (node/repl-env))

  ;; wait a moment for Node REPL to be ready, then eval the following

  (cljs.core.server/clj-eval
    (cljs.analyzer.api/ns-resolve 'cljs.core 'first))

  (require '[clojure.string :as string])
  (string/includes? "foo" "o")

  )
