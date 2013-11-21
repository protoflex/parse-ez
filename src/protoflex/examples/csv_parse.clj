(ns protoflex.examples.csv_parse
  (:use [protoflex.parse]))

(declare detect-sep csv-1)

(defn csv
  "Reads and returns one or more records as a vector of vector of field-values"
  ([] (csv (no-trim #(detect-sep))))
  ([sep] (multi* (fn [] (no-trim #(csv-1 sep))))))

(defn csv-1
  "Reads and returns the fields of one record (line)"
  [sep] (sep-by #(any-string sep) #(chr sep)))

(defn detect-sep
  "Detects the separator used in a csv file (a comma or a tab)"
  [] (let [m (mark-pos)
           s (attempt #(any dq-str sq-str))
           s (if s s (no-trim #(read-to-re #",|\t")))
           sep (read-ch)]
       (back-to-mark m)
       sep))
