;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-percent-coding; -*-

;; Copyright (c) 2013-2015, Olof-Joachim Frahm
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:cl-percent-coding)

(defvar *default-external-format*
  (flexi-streams:make-external-format :utf-8 :eol-style :lf)
  "Default external format to en- and decode strings.")

;; http://stackoverflow.com/questions/600070/how-to-convert-byte-array-to-string-in-common-lisp

(defun url-decode (string &key (start 0)
                               (end (length string))
                               (external-format *default-external-format*)
                               (output-element-type 'character)
                               x-www-form-urlencoded-p)
  "Decodes STRING as a percent encoded URL.  Returns a new STRING as
result if OUTPUT-ELEMENT-TYPE is set to CHARACTER.  Decoding to characters
is done via the given EXTERNAL-FORMAT."
  (iterate
    (with octet-stream = (flexi-streams:make-in-memory-output-stream))
    (for index from start below end)
    (for character = (char string index))
    (cond
      ((char= character #\%)
       (when (> (+ index 3) end)
         (error "Out of bounds access."))
       (let ((code (parse-integer string :start (1+ index) :end (+ index 3) :radix 16)))
         (write-byte code octet-stream)
         (incf index 2)))
      ((and x-www-form-urlencoded-p
            (char= character #\+))
       (write-byte #.(char-code #\Space) octet-stream))
      (T
       (write-sequence
        (flexi-streams:string-to-octets
         (string character)
         :external-format external-format)
        octet-stream)))
    (finally
     (return
       (let ((is-character-p (eq output-element-type 'character)))
         (cond
           ((or is-character-p
                (eq output-element-type 'base-char))
            (let ((result
                    (flexi-streams:octets-to-string
                     (flexi-streams:get-output-stream-sequence octet-stream)
                     :external-format external-format)))
              (if is-character-p
                  result
                  (coerce result 'base-string))))
           ((equal output-element-type '(unsigned-byte 8))
            (flexi-streams:get-output-stream-sequence octet-stream))
           (T (error "Unknown ~A ~A." 'output-element-type output-element-type))))))))

(defun url-encode (input &key (start 0)
                              (end (length input))
                              (external-format *default-external-format*)
                              (output-element-type 'character)
                              x-www-form-urlencoded-p)
  (let ((result
          (with-output-to-string (stream)
            (cond
              ((stringp input)
               (iterate
                 (for index from start below end)
                 (for char = (char input index))
                 ;; TODO: make these a bit configurable
                 (cond
                   ((or (char<= #\a char #\z)
                        (char<= #\A char #\Z)
                        (char<= #\0 char #\9)
                        (find char "-_.~"))
                    (write-char char stream))
                   ((and x-www-form-urlencoded-p
                         (char= char #\Space))
                    (write-char #\+ stream))
                   (T
                    (iterate
                      ;; TODO: oh god
                      (for byte in-vector (flexi-streams:string-to-octets (string char) :external-format external-format :end 1))
                      (format stream "%~2,'0x" byte))))))
              ((vectorp input)
               (iterate
                 (for byte in-vector input)
                 (format stream "%~2,'0x" byte)))))))
    (let ((is-character-p (eq output-element-type 'character)))
      (cond
        ((or is-character-p
             (eq output-element-type 'base-char))
         (if is-character-p
             result
             (coerce result 'base-string)))
        ((equal output-element-type '(unsigned-byte 8))
         (flexi-streams:string-to-octets result :external-format external-format))
        (T (error "Unknown ~A ~A." 'output-element-type output-element-type))))))

;; TODO: allow sequence (of characters or bytes) as source
;; TODO: allow stream (of characters or bytes) as source
