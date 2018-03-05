;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Christian Schafmeister June 1 2015
;;;
;;; This file figures out which Cleavir source files are required to
;;; construct the ASDF system that one gets with (require :clasp-cleavir)
;;;
;;; This code is derived from code written by Mark Cox - see below


;; Copyright (c) 2011, Mark Cox
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.

;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(require :asdf)

(defpackage #:asdf-system-groveler
  (:use #:common-lisp)
  (:export #:determine-complete-set-of-asdf-source-files
           #:determine-complete-set-of-asdf-source-files-absolute-path))


(in-package :asdf-system-groveler)

(defvar *all-systems* nil)
(defvar *all-source-files* nil)
(defclass sticky-beak-op (#-asdf3 asdf:operation
				  #+asdf3 asdf:downward-operation)
  ())

(defmethod asdf:component-depends-on ((op sticky-beak-op) component)
  (append (list (cons 'sticky-beak-op (asdf::component-sideway-dependencies component)))
	  (call-next-method)))

(defmethod asdf:perform ((op sticky-beak-op) (component t))
  (declare (ignore op component)))

(defmethod asdf:perform ((op sticky-beak-op) (component asdf:system))
  (declare (ignore op))
  (pushnew component *all-systems*))




(defmethod asdf:perform ((op sticky-beak-op) (c asdf/component:source-file))
  (pushnew c *all-source-files*))


(defun determine-complete-set-of-asdf-systems (systems)
  (let ((*all-systems* nil))
    (map nil #'(lambda (system)
		 (asdf:oos 'sticky-beak-op system :force t))
	 systems)
    *all-systems*))


(defun determine-complete-set-of-asdf-source-files (systems)
  (let ((*all-source-files* nil))
    (map nil #'(lambda (system)
		 (asdf:oos 'sticky-beak-op system :force t))
	 systems)
    (let (source)
      (mapc
       (lambda (x)
         (when (typep x 'asdf/lisp-action:cl-source-file)
           (let ((file (let* ((part-name
                                (enough-namestring
                                 (asdf/component:component-pathname x)
                                 (translate-logical-pathname #P"SOURCE-DIR:")))
                              (no-type
                                (make-pathname :directory (pathname-directory part-name)
                                               :name (pathname-name part-name))))
                         (enough-namestring (pathname (namestring no-type)) (translate-logical-pathname "source-dir:")))))
             (format t "x -> ~a   file -> ~a~%" x file)
             (push file source))))
       *all-source-files*)
      source)))

(defun determine-complete-set-of-asdf-source-files-absolute-path (systems)
  (let ((*all-source-files* nil))
    (map nil #'(lambda (system)
		 (asdf:oos 'sticky-beak-op system :force t))
	 systems)
    (mapcar (lambda (x)
              (asdf/component:component-pathname x))
            *all-source-files*)))



