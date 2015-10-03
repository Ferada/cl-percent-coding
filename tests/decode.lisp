;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-percent-coding-tests; -*-

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

(in-package #:cl-percent-coding-tests)

(in-suite cl-percent-coding)

(def-test start-end ()
  "Typical START/END parameters work as expected."
  (let ((string "hello, world"))
    (for-all ((start (gen-integer :min 0 :max (length string))))
      (for-all ((end (gen-integer :min start :max (length string))))
        (is (string= (subseq string start end)
            (url-decode string :start start :end end)))))))

(def-test too-short ()
  "Truncated encoded values signal."
  (signals error (url-decode "%"))
  (signals error (url-decode "%2"))
  (signals error (url-decode "%42" :end 1))
  (signals error (url-decode "%42" :end 2)))

(def-test invalid ()
  "Invalid encodings signal."
  (signals error (url-decode "%%")))

(def-test output-types ()
  "Output types are recognised."
  (is (typep (url-encode "foo") 'string))
  (is (typep (url-encode "foo" :output-element-type 'base-char) 'base-string))
  (is (typep (url-encode "foo" :output-element-type '(unsigned-byte 8)) '(vector (unsigned-byte 8))))
  (is (typep (url-decode "foo") 'string))
  (is (typep (url-decode "foo" :output-element-type 'base-char) 'base-string))
  (is (typep (url-decode "foo" :output-element-type '(unsigned-byte 8)) '(vector (unsigned-byte 8)))))
