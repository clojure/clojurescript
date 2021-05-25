;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.instant
  (:require [clojure.instant :as inst])
  (:import [java.time Instant OffsetDateTime ZoneOffset]
           [java.time.format DateTimeFormatter DateTimeFormatterBuilder]
           [java.util Locale Locale$Category]))

(set! *warn-on-reflection* true)

(def ^:private ^java.time.format.DateTimeFormatter utc-format
  (-> (DateTimeFormatterBuilder.)
    (.appendInstant 9)
    (.toFormatter (Locale/getDefault Locale$Category/FORMAT))))

(defn- remove-last-char ^String [s]
  (subs s 0 (dec (count s))))

(defn- print-instant
  "Print a java.time.Instant as RFC3339 timestamp, always in UTC."
  [^java.time.Instant instant, ^java.io.Writer w]
  (.write w "#inst \"")
  (.write w (remove-last-char (.format utc-format instant)))
  (.write w "-00:00\""))

(defmethod print-method java.time.Instant
  [^java.time.Instant instant, ^java.io.Writer w]
  (print-instant instant w))

(defmethod print-dup java.time.Instant
  [^java.time.Instant instant, ^java.io.Writer w]
  (print-instant instant w))

(defn- construct-instant
  "Construct a java.time.Instant, which has nanosecond precision."
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  (Instant/from
    (OffsetDateTime/of years months days hours minutes seconds nanoseconds
      (ZoneOffset/ofHoursMinutes (* offset-sign offset-hours) (* offset-sign offset-minutes)))))

(defn read-instant-instant
  "To read an instant as a java.time.Instant, bind *data-readers* to a
  map with this var as the value for the 'inst key. Instant preserves
  fractional seconds with nanosecond precision. The timezone offset will
  be used to convert into UTC."
  [^CharSequence cs]
  (inst/parse-timestamp (inst/validated construct-instant) cs))
