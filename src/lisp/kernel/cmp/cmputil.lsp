;;;
;;;    File: cmputil.lsp
;;;

;; Copyright (c) 2014, Christian E. Schafmeister
;;
;; CLASP is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; See directory 'clasp/licenses' for full details.
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; -^-

(in-package :cmp)


(defconstant +note-format+  "    %s\n")
(defconstant +warn-format+  "!   %s\n")
(defconstant +error-format+ "*   %s\n")
(defconstant +fatal-format+ "**  %s\n")


(defstruct (compiler-message (:type vector))
  (prefix "Note")
  (format +note-format+)
  message
  source-dir
  source-filename
  lineno
  column
  top-level-form
  form)

(defstruct (compiler-note (:include compiler-message)
                          (:type vector)))

(defstruct (compiler-warning
             (:type vector)
             (:include compiler-message
                       (prefix "Warning")
                       (format +warn-format+))))

(defstruct (compiler-error
             (:type vector)
             (:include compiler-message
                       (prefix "Error")
                       (format +error-format+))))

(defstruct (compiler-fatal-error
             (:type vector)
             (:include compiler-error
                       (prefix "Fatal Error")
                       (format +fatal-format+))))

(defstruct (compiler-style-warning
             (:type vector)
             (:include compiler-warning
                       (prefix "Style Warning")
                       (format +warn-format+))))

(defun compiler-error (form message &rest args)
  (let* ((cspi (ext:current-source-location))
         (source-path (source-file-info-pathname (source-file-info cspi)))
         (source-dir (directory-namestring source-path))
         (source-file (file-namestring source-path)))
    (let ((err (make-compiler-error :message (apply #'core:bformat nil message args)
                                    :lineno lineno
                                    :source-dir source-dir
                                    :source-filename source-file)))
      (push err *compilation-messages*))))


#||
(defstruct (
    ((prefix :initform "Warning")
     (format :initform +warn-format+)))

  (define-condition compiler-error (compiler-message)
    ((prefix :initform "Error")
     (format :initform +error-format+)))



  (define-condition compiler-message (simple-condition)
    ((prefix :initform "Note" :accessor compiler-message-prefix)
     (format :initform +note-format+ :accessor compiler-message-format)
     (file :initarg :file :initform *compile-file-pathname*
           :accessor compiler-message-file)
     (position :initarg :file :initform *compile-file-position*
               :accessor compiler-message-file-position)
     (toplevel-form :initarg :form :initform *current-toplevel-form*
                    :accessor compiler-message-toplevel-form)
     (form :initarg :form :initform *current-form*
           :accessor compiler-message-form))
    (:report (lambda (c stream)
               (apply #'compiler-message-report stream c
                      (simple-condition-format-control c)
                      (simple-condition-format-arguments c)))))

  (define-condition compiler-note (compiler-message) ())

  (define-condition compiler-debug-note (compiler-note) ())

  (define-condition compiler-warning (compiler-message style-warning)
    ((prefix :initform "Warning")
     (format :initform +warn-format+)))

  (define-condition compiler-macro-expansion-failed (compiler-warning)
    ())

  (define-condition compiler-error (compiler-message)
    ((prefix :initform "Error")
     (format :initform +error-format+)))

  (define-condition compiler-fatal-error (compiler-error)
    ((format :initform +fatal-format+)))

  (define-condition compiler-internal-error (compiler-fatal-error)
    ((prefix :initform "Internal error")))

  (define-condition compiler-style-warning (compiler-message style-warning)
    ((prefix :initform "Style warning")
     (format :initform +warn-format+)))

  (define-condition compiler-undefined-variable (compiler-style-warning)
    ((variable :initarg :name :initform nil))
    (:report
     (lambda (c stream)
       (compiler-message-report stream c
                                "Variable ~A was undefined. ~
                               Compiler assumes it is a global."
                                (slot-value c 'variable)))))
  )

||#

(defun print-compiler-message (c stream)
  (bformat stream ";;; %s\n" c))

(defvar *compilation-messages* nil)
(defmacro with-compiler-env ( (conditions &rest options) &rest body )
  "Initialize the environment to protect nested compilations from each other"
  `(let ((*the-module* nil)
	 (*irbuilder-ltv-function-alloca* nil)
	 (*irbuilder-ltv-function-body* nil)
	 (*ltv-function-landing-pad-block* nil)
	 (*irbuilder-function-alloca* nil)
	 (*irbuilder-function-body* nil)
	 (*generate-compile-file-load-time-values* nil)
	 (*next-load-time-value-index* nil)
	 (*load-time-value-holder-global-var* nil)
	 (*load-time-value-coalesce* nil)
	 (*load-time-initializer-environment* nil)
	 (*the-module-dibuilder* nil)
	 (*readtable* *readtable*)
	 (*package* *package*)
         (*compilation-messages* nil)
	 )
     (unwind-protect (progn ,@body)
       (setq conditions *compilation-messages*))))
