(in-package #:cl-percent-coding-tests)

(def-test start-end ()
  "Typical START/END parameters work as expected."
  (for-all ((string (gen-string)))
    (for-all ((start (gen-integer :min 0 :max (length string))))
      (for-all ((end (gen-integer :min start :max (length string))))
        (is (string= (subseq string start end)
            (url-decode string :start start :end end)))))))

(def-test too-short ()
  "Truncated encoded values signal."
  (signals error (url-decode "%"))
  (signals error (url-decode "%2")))

(def-test invalid ()
  "Invalid encodings signal."
  (signals error (url-decode "%%")))
