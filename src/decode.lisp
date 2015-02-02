(in-package #:cl-percent-coding)

(defvar *default-external-format*
  (flexi-streams:make-external-format :utf-8 :eol-style :lf)
  "Default external format to en- and decode strings.")

;; http://stackoverflow.com/questions/600070/how-to-convert-byte-array-to-string-in-common-lisp

(defun url-decode (string &key (start 0)
                               (end (length string))
                               (external-format *default-external-format*)
                               (output-element-type 'character))
  "Decodes STRING as a percent encoded URL.  Returns a new STRING as
result if OUTPUT-ELEMENT-TYPE is set to CHARACTER.  Decoding to characters
is done via the given EXTERNAL-FORMAT."
  (iterate
    (with octet-stream = (flexi-streams:make-in-memory-output-stream))
    (for index from start below end)
    (for character = (char string index))
    (cond
      ((char= character #\%)
       (let ((code (parse-integer string :start (1+ index) :end (+ index 3) :radix 16)))
         (write-byte code octet-stream)
         (incf index 2)))
      (T
       (write-sequence
        (flexi-streams:string-to-octets
         (string character)
         :external-format external-format)
        octet-stream)))
    (finally
     (return
       (cond
        ((eq output-element-type 'character)
         (flexi-streams:octets-to-string
          (flexi-streams:get-output-stream-sequence octet-stream)
          :external-format external-format))
        ((equal output-element-type '(unsigned-byte 8))
         (flexi-streams:get-output-stream-sequence octet-stream))
        (T (error "unknown OUTPUT-ELEMENT-TYPE ~A" output-element-type)))))))

(defun url-encode (string &key (start 0)
                               (end (length string))
                               (external-format *default-external-format*)
                               (output-element-type 'character))
  (let ((result
         (with-output-to-string (stream)
           (iterate
             (for byte in-vector (flexi-streams:string-to-octets string :external-format external-format :start start :end end))
             (format stream "%~2,'0x" byte)))))
    (cond
     ((eq output-element-type 'character)
      result)
     ((equal output-element-type '(unsigned-byte 8))
      (flexi-streams:string-to-octets result :external-format external-format))
     (T (error "unknown OUTPUT-ELEMENT-TYPE ~A" output-element-type)))))

;; TODO: allow sequence (of characters or bytes) as source
;; TODO: allow stream (of characters or bytes) as source
