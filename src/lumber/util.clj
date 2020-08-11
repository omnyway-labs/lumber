(ns lumber.util
  (:import
   [java.util TimeZone]
   [java.text SimpleDateFormat]))

(defn timezone
  "Looks up a java.util.TimeZone instance by name"
  [tz]
  (if (string? tz)
    (java.util.TimeZone/getTimeZone ^String tz)
    tz))

(defn ^SimpleDateFormat simple-date-formatter
  "Create a java.text.SimpleDateFormat with `format-string` and
  optional `tz` timezone."
  ([format-string]
   (simple-date-formatter format-string "UTC"))
  ([format-string tz]
   (doto (SimpleDateFormat. format-string)
     (.setTimeZone (timezone tz)))))

(def ^:constant iso-8601-formatter
  (simple-date-formatter "yyyy-MM-dd'T'HH:mm:ss.SSSZ"))

(defn as-formatter [formatter-args]
  (let [[formatter & _] formatter-args]
    (if (instance? SimpleDateFormat formatter)
      formatter
      (apply simple-date-formatter formatter-args))))

(defn format-timestamp
  "Format a timestamp using a SimpleDateFormat, by defaults this
  function formats as ISO 8601 timestamp string, e.g.,

  (format-timestamp)
  => \"2017-04-12T17:18:37.363+0000\"

  (format-timestamp (System/currentTimeMillis))
  => \"2017-04-12T17:18:37.363+0000\"

  (format-timestamp (System/currentTimeMillis) \"yyyy/MM\")
  => \"2017/04\"
  "
  ([]
   (format-timestamp (System/currentTimeMillis)))
  ([timestamp]
   (format-timestamp timestamp iso-8601-formatter))
  ([timestamp & formatter-args]
   (.format (as-formatter formatter-args) timestamp)))

(defn safe-pr-str [o]
  (binding [*print-readably* true
            *print-meta* false
            *print-dup* false]
    (pr-str o)))
