;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Protocols and default Reader types implementation"
      :author "Bronsa"}
    cljs.vendor.clojure.tools.reader.reader-types
  (:refer-clojure :exclude [char read-line])
  (:require [cljs.vendor.clojure.tools.reader.impl.utils :refer [char whitespace? newline? make-var]])
  (:import clojure.lang.LineNumberingPushbackReader
           (java.io InputStream BufferedReader Closeable)))

(defmacro ^:private update! [what f]
  (list 'set! what (list f what)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Reader
  (read-char [reader]
    "Returns the next char from the Reader, nil if the end of stream has been reached")
  (peek-char [reader]
    "Returns the next char from the Reader without removing it from the reader stream"))

(defprotocol IPushbackReader
  (unread [reader ch]
    "Pushes back a single character on to the stream"))

(defprotocol IndexingReader
  (get-line-number [reader]
    "Returns the line number of the next character to be read from the stream")
  (get-column-number [reader]
    "Returns the column number of the next character to be read from the stream")
  (get-file-name [reader]
    "Returns the file name the reader is reading from, or nil"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader deftypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype StringReader
    [^String s ^long s-len ^:unsynchronized-mutable ^long s-pos]
  Reader
  (read-char [reader]
    (when (> s-len s-pos)
      (let [r (nth s s-pos)]
        (update! s-pos inc)
        r)))
  (peek-char [reader]
    (when (> s-len s-pos)
      (nth s s-pos))))

(deftype InputStreamReader [^InputStream is ^:unsynchronized-mutable ^"[B" buf]
  Reader
  (read-char [reader]
    (if buf
      (let [c (aget buf 0)]
        (set! buf nil)
        (char c))
      (let [c (.read is)]
        (when (>= c 0)
          (char c)))))
  (peek-char [reader]
    (when-not buf
      (set! buf (byte-array 1))
      (when (== -1 (.read is buf))
        (set! buf nil)))
    (when buf
      (char (aget buf 0))))
  Closeable
  (close [this]
    (.close is)))

(deftype PushbackReader
    [rdr ^"[Ljava.lang.Object;" buf ^long buf-len ^:unsynchronized-mutable ^long buf-pos]
  Reader
  (read-char [reader]
    (char
     (if (< buf-pos buf-len)
       (let [r (aget buf buf-pos)]
         (update! buf-pos inc)
         r)
       (read-char rdr))))
  (peek-char [reader]
    (char
     (if (< buf-pos buf-len)
       (aget buf buf-pos)
       (peek-char rdr))))
  IPushbackReader
  (unread [reader ch]
    (when ch
      (if (zero? buf-pos) (throw (RuntimeException. "Pushback buffer is full")))
      (update! buf-pos dec)
      (aset buf buf-pos ch)))
  Closeable
  (close [this]
    (when (instance? Closeable rdr)
      (.close ^Closeable rdr))))

(deftype IndexingPushbackReader
    [rdr ^:unsynchronized-mutable ^long line ^:unsynchronized-mutable ^long column
     ^:unsynchronized-mutable line-start? ^:unsynchronized-mutable prev
     ^:unsynchronized-mutable ^long prev-column file-name
     ^:unsynchronized-mutable normalize?]
  Reader
  (read-char [reader]
    (when-let [ch (read-char rdr)]
      (let [ch (if normalize?
                 (do (set! normalize? false)
                     (if (or (identical? \newline ch)
                             (identical? \formfeed ch))
                       (read-char rdr)
                       ch))
                 ch)
            ch (if (identical? \return ch)
                 (do (set! normalize? true)
                     \newline)
                 ch)]
        (set! prev line-start?)
        (set! line-start? (newline? ch))
        (when line-start?
          (set! prev-column column)
          (set! column 0)
          (update! line inc))
        (update! column inc)
        ch)))

  (peek-char [reader]
    (peek-char rdr))

  IPushbackReader
  (unread [reader ch]
    (if line-start?
      (do (update! line dec)
          (set! column prev-column))
      (update! column dec))
    (set! line-start? prev)
    ;; This may look a bit convoluted, but it helps in the following
    ;; scenario:
    ;; + The underlying reader is about to return \return from the
    ;;   next read-char, and then \newline after that.
    ;; + read-char gets \return, sets normalize? to true, returns
    ;;   \newline instead.
    ;; + Caller calls unread on the \newline it just got.  If we
    ;;   unread the \newline to the underlying reader, now it is ready
    ;;   to return two \newline chars in a row, which will throw off
    ;;   the tracked line numbers.
    (let [ch (if normalize?
               (do (set! normalize? false)
                   (if (identical? \newline ch)
                     \return
                     ch))
               ch)]
      (unread rdr ch)))

  IndexingReader
  (get-line-number [reader] (int line))
  (get-column-number [reader] (int column))
  (get-file-name [reader] file-name)

  Closeable
  (close [this]
    (when (instance? Closeable rdr)
      (.close ^Closeable rdr))))

;; Java interop

(extend-type java.io.PushbackReader
  Reader
  (read-char [rdr]
    (let [c (.read ^java.io.PushbackReader rdr)]
      (when (>= c 0)
        (char c))))

  (peek-char [rdr]
    (when-let [c (read-char rdr)]
      (unread rdr c)
      c))

  IPushbackReader
  (unread [rdr c]
    (when c
      (.unread ^java.io.PushbackReader rdr (int c)))))

(extend LineNumberingPushbackReader
  IndexingReader
  {:get-line-number (fn [rdr] (.getLineNumber ^LineNumberingPushbackReader rdr))
   :get-column-number (fn [rdr]
                        (.getColumnNumber ^LineNumberingPushbackReader rdr))
   :get-file-name (constantly nil)})

(defprotocol ReaderCoercer
  (to-rdr [rdr]))

(declare string-reader push-back-reader)

(extend-protocol ReaderCoercer
  Object
  (to-rdr [rdr]
    (if (satisfies? Reader rdr)
      rdr
      (throw (IllegalArgumentException. (str "Argument of type: " (class rdr) " cannot be converted to Reader")))))
  cljs.vendor.clojure.tools.reader.reader_types.Reader
  (to-rdr [rdr] rdr)
  String
  (to-rdr [str] (string-reader str))
  java.io.Reader
  (to-rdr [rdr] (java.io.PushbackReader. rdr)))

(defprotocol PushbackReaderCoercer
  (to-pbr [rdr buf-len]))

(extend-protocol PushbackReaderCoercer
  Object
  (to-pbr [rdr buf-len]
    (if (satisfies? Reader rdr)
      (push-back-reader rdr buf-len)
      (throw (IllegalArgumentException. (str "Argument of type: " (class rdr) " cannot be converted to IPushbackReader")))))
  cljs.vendor.clojure.tools.reader.reader_types.Reader
  (to-pbr [rdr buf-len] (push-back-reader rdr buf-len))
  cljs.vendor.clojure.tools.reader.reader_types.PushbackReader
  (to-pbr [rdr buf-len] (push-back-reader rdr buf-len))
  String
  (to-pbr [str buf-len] (push-back-reader str buf-len))
  java.io.Reader
  (to-pbr [rdr buf-len] (java.io.PushbackReader. rdr buf-len)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source Logging support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn merge-meta
  "Returns an object of the same type and value as `obj`, with its
  metadata merged over `m`."
  [obj m]
  (let [orig-meta (meta obj)]
    (with-meta obj (merge m (dissoc orig-meta :source)))))

(defn- peek-source-log
  "Returns a string containing the contents of the top most source
  logging frame."
  [source-log-frames]
  (let [current-frame @source-log-frames]
    (.substring ^StringBuilder (:buffer current-frame) (:offset current-frame))))

(defn- log-source-char
  "Logs `char` to all currently active source logging frames."
  [source-log-frames char]
  (when-let [^StringBuilder buffer (:buffer @source-log-frames)]
    (.append buffer char)))

(defn- drop-last-logged-char
  "Removes the last logged character from all currently active source
  logging frames. Called when pushing a character back."
  [source-log-frames]
  (when-let [^StringBuilder buffer (:buffer @source-log-frames)]
    (.deleteCharAt buffer (dec (.length buffer)))))

(deftype SourceLoggingPushbackReader
    [rdr ^:unsynchronized-mutable ^long line ^:unsynchronized-mutable ^long column
     ^:unsynchronized-mutable line-start? ^:unsynchronized-mutable prev
     ^:unsynchronized-mutable ^long prev-column file-name source-log-frames
     ^:unsynchronized-mutable normalize?]
  Reader
  (read-char [reader]
    (when-let [ch (read-char rdr)]
      (let [ch (if normalize?
                 (do (set! normalize? false)
                     (if (or (identical? \newline ch)
                             (identical? \formfeed ch))
                       (read-char rdr)
                       ch))
                 ch)
            ch (if (identical? \return ch)
                 (do (set! normalize? true)
                     \newline)
                 ch)]
        (set! prev line-start?)
        (set! line-start? (newline? ch))
        (when line-start?
          (set! prev-column column)
          (set! column 0)
          (update! line inc))
        (update! column inc)
        (log-source-char source-log-frames ch)
        ch)))

  (peek-char [reader]
    (peek-char rdr))

  IPushbackReader
  (unread [reader ch]
    (if line-start?
      (do (update! line dec)
          (set! column prev-column))
      (update! column dec))
    (set! line-start? prev)
    (when ch
      (drop-last-logged-char source-log-frames))
    (unread rdr ch))

  IndexingReader
  (get-line-number [reader] (int line))
  (get-column-number [reader] (int column))
  (get-file-name [reader] file-name)

  Closeable
  (close [this]
    (when (instance? Closeable rdr)
      (.close ^Closeable rdr))))

(defn log-source*
  [reader f]
  (let [frame (.source-log-frames ^SourceLoggingPushbackReader reader)
        ^StringBuilder buffer (:buffer @frame)
        new-frame (assoc @frame :offset (.length buffer))]
    (with-bindings {frame new-frame}
      (let [ret (f)]
        (if (instance? clojure.lang.IObj ret)
          (merge-meta ret {:source (peek-source-log frame)})
          ret)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fast check for provided implementations
(defn indexing-reader?
  "Returns true if the reader satisfies IndexingReader"
  [rdr]
  (or (instance? cljs.vendor.clojure.tools.reader.reader_types.IndexingReader rdr)
      (instance? LineNumberingPushbackReader rdr)
      (and (not (instance? cljs.vendor.clojure.tools.reader.reader_types.PushbackReader rdr))
           (not (instance? cljs.vendor.clojure.tools.reader.reader_types.StringReader rdr))
           (not (instance? cljs.vendor.clojure.tools.reader.reader_types.InputStreamReader rdr))
           (get (:impls IndexingReader) (class rdr)))))

(defn string-reader
  "Creates a StringReader from a given string"
  ([^String s]
   (StringReader. s (count s) 0)))

(defn ^Closeable push-back-reader
  "Creates a PushbackReader from a given reader or string"
  ([rdr] (push-back-reader rdr 1))
  ([rdr buf-len] (PushbackReader. (to-rdr rdr) (object-array buf-len) buf-len buf-len)))

(defn ^Closeable string-push-back-reader
  "Creates a PushbackReader from a given string"
  ([s]
   (string-push-back-reader s 1))
  ([^String s buf-len]
   (push-back-reader (string-reader s) buf-len)))

(defn ^Closeable input-stream-reader
  "Creates an InputStreamReader from an InputStream"
  [is]
  (InputStreamReader. is nil))

(defn ^Closeable input-stream-push-back-reader
  "Creates a PushbackReader from a given InputStream"
  ([is]
   (input-stream-push-back-reader is 1))
  ([^InputStream is buf-len]
   (push-back-reader (input-stream-reader is) buf-len)))

(defn ^Closeable indexing-push-back-reader
  "Creates an IndexingPushbackReader from a given string or PushbackReader"
  ([s-or-rdr]
   (indexing-push-back-reader s-or-rdr 1))
  ([s-or-rdr buf-len]
   (indexing-push-back-reader s-or-rdr buf-len nil))
  ([s-or-rdr buf-len file-name]
   (IndexingPushbackReader.
    (to-pbr s-or-rdr buf-len) 1 1 true nil 0 file-name false)))

(defn ^Closeable source-logging-push-back-reader
  "Creates a SourceLoggingPushbackReader from a given string or PushbackReader"
  ([s-or-rdr]
   (source-logging-push-back-reader s-or-rdr 1))
  ([s-or-rdr buf-len]
   (source-logging-push-back-reader s-or-rdr buf-len nil))
  ([s-or-rdr buf-len file-name]
   (SourceLoggingPushbackReader.
    (to-pbr s-or-rdr buf-len)
    1
    1
    true
    nil
    0
    file-name
    (doto (make-var)
      (alter-var-root (constantly {:buffer (StringBuilder.)
                                   :offset 0})))
    false)))

(defn read-line
  "Reads a line from the reader or from *in* if no reader is specified"
  ([] (read-line *in*))
  ([rdr]
   (if (or (instance? LineNumberingPushbackReader rdr)
           (instance? BufferedReader rdr))
     (binding [*in* rdr]
       (clojure.core/read-line))
     (loop [c (read-char rdr) s (StringBuilder.)]
       (if (newline? c)
         (str s)
         (recur (read-char rdr) (.append s c)))))))

(defn source-logging-reader?
  [rdr]
  (instance? SourceLoggingPushbackReader rdr))

(defmacro log-source
  "If reader is a SourceLoggingPushbackReader, execute body in a source
  logging context. Otherwise, execute body, returning the result."
  [reader & body]
  `(if (and (source-logging-reader? ~reader)
            (not (whitespace? (peek-char ~reader))))
     (log-source* ~reader (^:once fn* [] ~@body))
     (do ~@body)))

(defn line-start?
  "Returns true if rdr is an IndexingReader and the current char starts a new line"
  [rdr]
  (when (indexing-reader? rdr)
    (== 1 (int (get-column-number rdr)))))
