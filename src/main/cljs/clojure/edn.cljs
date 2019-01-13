;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns clojure.edn
  "edn reading.

  This namespace provides alias for cljs.reader/read and cljs.reader/read-string.
  Thus Clojure and ClojureScript source can reference these functions in the same way.
  In Clojure, read and read-string may cause evaluation,
  but clojure.edn/read and clojure.edn/read-string will not.
  In ClojureScript cljs.reader/read and cljs.reader/read-string will not cause evaluation,
  they only read edn."
  (:require [cljs.reader :as reader]))

(defn read
  "Reads the first object from an cljs.tools.reader.reader-types/IPushbackReader.
   Returns the object read. If EOF, throws if eof-error? is true otherwise returns eof.
   If no reader is provided, *in* will be used.

   Reads data in the edn format (subset of Clojure data):
   http://edn-format.org

   cljs.tools.reader.edn/read doesn't depend on dynamic Vars, all configuration
   is done by passing an opt map.

   opts is a map that can include the following keys:
   :eof - value to return on end-of-file. When not supplied, eof throws an exception.
   :readers  - a map of tag symbols to data-reader functions to be considered before default-data-readers.
              When not supplied, only the default-data-readers will be used.
   :default - A function of two args, that will, if present and no reader is found for a tag,
              be called with the tag and the value."
  ([reader]
   (reader/read reader))
  ([opts reader]
   (reader/read opts reader))
  ([reader eof-error? eof opts]
   (reader/read reader eof-error? eof opts)))

(defn read-string
  "Reads one object from the string s.
   Returns nil when s is nil or empty.

   Reads data in the edn format (subset of Clojure data):
   http://edn-format.org

   opts is a map as per cljs.tools.reader.edn/read"
  ([s]
   (reader/read-string s))
  ([opts s]
   (reader/read-string opts s)))
