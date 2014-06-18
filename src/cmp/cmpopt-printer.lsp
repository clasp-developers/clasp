;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  CMPOPT-PRINTER  -- Optimize output operations
;;;;

(in-package "COMPILER")

(defun printer-default-stream (stream env)
  (if (constantp stream env)
      (let ((value (ext:constant-form-value stream env)))
        (case value
          ((nil) '*standard-output*)
          ((t) '*terminal-io*)
          (otherwise (cmpwarn
                      (if (streamp value)
                          "Found~%~A~%as expression for a stream, but it cannot be externalized."
                          "Found~%~A~%where a stream was expected.")
                      stream)
                     stream)))
      `(ffi:c-inline (,stream) (:object) :object
                     "_ecl_stream_or_default_output(#0)"
                     :one-liner t)))

(define-compiler-macro princ (expression &optional stream &environment env)
  (if (constantp expression env)
      (let ((value (ext:constant-form-value expression env)))
        (cond ((eql value #\Newline)
               `(terpri ,stream))
              ((characterp value)
               `(ffi:c-inline ,(list value stream) (:wchar :object) :wchar
                              "ecl_princ_char(#0,#1)"
                              :one-liner t))
              ((and (stringp value)
                    (= (length value) 1))
               `(ffi:c-inline ,(list (aref value 0) stream) (:wchar :object) :wchar
                              "ecl_princ_char(#0,#1)"
                              :one-liner t))
              ((and (typep value 'base-string)
                    (< (length value) 80))
               `(progn
                  (ffi:c-inline ,(list stream) (:object) :void
                                ,(concatenate 'string
                                              "ecl_princ_str("
                                              (c-inline-safe-string value)
                                              ",#0)")
                                :one-liner t)
                  ,value))
              (t
               `(ffi:c-inline ,(list expression stream) (:object :object) :object
                              "ecl_princ(#0,#1)"
                              :one-liner t))))
      `(ffi:c-inline ,(list expression stream) (:object :object) :object
                     "ecl_princ(#0,#1)"
                     :one-liner t)))

(define-compiler-macro terpri (&optional stream &environment env)
  `(ffi:c-inline (,stream)
                 (:object) :object
                 "ecl_terpri(#0)"
                 :one-liner t))

(define-compiler-macro print (value &optional stream &environment env)
  `(ffi:c-inline (,value ,stream)
                 (:object :object) :object
                 "ecl_print(#0,#1)"
                 :one-liner t))

(define-compiler-macro prin1 (value &optional stream &environment env)
  `(ffi:c-inline (,value ,stream)
                 (:object :object) :object
                 "ecl_prin1(#0,#1)"
                 :one-liner t))

