;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.vendor.bridge
  (:require [cljs.vendor.clojure.tools.reader.reader-types :as vendor]
            [clojure.tools.reader.reader-types :as readers]))

(extend-protocol vendor/Reader
  clojure.tools.reader.reader_types.Reader
  (read-char [reader]
    (readers/read-char reader))
  (peek-char [reader]
    (readers/peek-char reader)))

(extend-protocol vendor/IPushbackReader
  clojure.tools.reader.reader_types.IPushbackReader
  (unread [reader ch]
    (readers/unread reader ch)))

(extend-protocol vendor/IndexingReader
  clojure.tools.reader.reader_types.IndexingReader
  (get-line-number [reader]
    (readers/get-line-number reader))
  (get-column-number [reader]
    (readers/get-column-number reader))
  (get-file-name [reader]
    (readers/get-file-name reader)))

(extend-protocol vendor/ReaderCoercer
  clojure.tools.reader.reader_types.ReaderCoercer
  (to-rdr [reader]
    (readers/to-rdr reader)))

(extend-protocol vendor/PushbackReaderCoercer
  clojure.tools.reader.reader_types.PushbackReaderCoercer
  (to-pbr [reader buflen]
    (readers/to-pbr reader buflen)))
