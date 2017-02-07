;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; clasp-ffi-tests.asd --- ASDF system definition for CLASP-FFI unit tests.
;;;
;;; Copyright (C) 2016, Frank Goenninger  <frank.goenninger@goenninger.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(defsystem "clasp-ffi.tests"
    :description "CLASP unit tests for CFFI."
    :depends-on ("rt" "cffi")
    :components
    ((:module "fli"
              :components
              ((:file "defpackage")
               (:file "bindings" :depends-on ("defpackage"))
               (:file "funcall" :depends-on ("bindings"))
               (:file "memory" :depends-on ("bindings"))
               (:file "execute-tests" :depends-on ("funcall"))
               #|
               (:file "callbacks" :depends-on ("bindings"))
               (:file "foreign-globals" :depends-on ("defpackage"))
               (:file "strings" :depends-on ("defpackage"))
               (:file "arrays" :depends-on ("defpackage"))
               (:file "struct" :depends-on ("defpackage"))
               (:file "union" :depends-on ("defpackage"))
               (:file "enum" :depends-on ("defpackage"))
               (:file "fsbv" :depends-on ("bindings" "enum"))
               (:file "misc-types" :depends-on ("bindings"))
               (:file "misc" :depends-on ("bindings"))
               |#
               ))))

;;; vim: ft=lisp et
