;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; bindings.lisp --- Setup CFFI bindings for libtest.
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

(in-package #:clasp-ffi.tests)

;;; Return the directory containing the source when compiling or
;;; loading this file.  We don't use *LOAD-TRUENAME* because the fasl
;;; file may be in a different directory than the source with certain
;;; ASDF extensions loaded.
(defun load-directory ()
  (let ((here #.(or *compile-file-truename* *load-truename*)))
    (make-pathname :name nil :type nil :version nil
                   :defaults here)))

(defun make-lib-pathname (name)
  (merge-pathnames (make-pathname :host nil
                                  :device nil
                                  :version nil
                                  :name (string-downcase (format nil "~a" name))
                                  :type "dylib"
                                  :directory nil)
                   (load-directory)))

(defun load-test-libraries ()
  (%load-foreign-library 'libtest (make-lib-pathname 'libtest))
  (%load-foreign-library 'libtest2 (make-lib-pathname 'libtest2))
  (%load-foreign-library 'libfsbv (make-lib-pathname 'libfsbv)))

#-(:and :ecl (:not :dffi))
(load-test-libraries)

(defun run-clasp-ffi-tests (&key (compiled nil))
  (let ((regression-test::*compile-tests* compiled)
        (*package* (find-package '#:clasp-ffi-tests)))
    (format t "~&;;; running tests (~Acompiled)" (if compiled "" "un"))
    (do-tests)
    (set-difference (regression-test:pending-tests)
                    regression-test::*expected-failures*)))

(defmacro expecting-error (&body body)
  `(handler-case (progn ,@body :no-error)
     (error () :error)))
