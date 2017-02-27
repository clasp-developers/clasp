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

(defvar *compile-defs*)
(defvar *compilation-messages*)

(defconstant +note-format+  "Note:        %s")
(defconstant +warn-format+  "Warning:     %s")
(defconstant +error-format+ "Error:       %s")
(defconstant +fatal-format+ "Fatal-error: %s")

(defstruct (compiler-message (:type vector))
  (prefix "Note")
  (format +note-format+)
  message
  source-pos-info
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
  (let* ((cspi (ext:current-source-location)))
    (let ((err (make-compiler-error :message (apply #'core:bformat nil message args))))
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

(defun describe-source-location (cspi)
  (let* ((lineno (source-pos-info-lineno cspi))
         (filepos (source-pos-info-filepos cspi))
         (source-file-info (source-file-info cspi))
         (pathname (source-file-info-pathname source-file-info)))
  (core:bformat nil "%s filepos: %d  lineno: %d" (namestring pathname) filepos lineno)))

(defun print-compiler-message (c stream)
  (let ((msg (core:bformat nil (compiler-message-format c) (compiler-message-message c))))
    (bformat stream ";;; %s" msg)
    (bformat stream ";;;     at %s \n" (describe-source-location (compiler-message-source-pos-info c)))))

(defmacro with-compiler-env ( (conditions &rest options) &rest body )
  "Initialize the environment to protect nested compilations from each other"
  `(let ((*the-module* nil)
	 (*irbuilder-ltv-function-alloca* nil)
	 (*irbuilder-ltv-function-body* nil)
	 (*ltv-function-landing-pad-block* nil)
	 (*irbuilder-function-alloca* nil)
	 (*irbuilder-function-body* nil)
	 (*generate-compile-file-load-time-values* nil)
	 (*table-index* nil)
	 (*load-time-value-holder-global-var* nil)
	 (*load-time-value-coalesce* nil)
	 (*load-time-initializer-environment* nil)
	 (*the-module-dibuilder* nil)
	 (*readtable* *readtable*)
	 (*package* *package*)
	 )
     (with-compilation-unit ()
       (unwind-protect (progn ,@body)
         (setq conditions *compilation-messages*)))))





(defstruct (compile-file-def (:type vector) :named)
  type
  name
  source-pos-info)

(defun register-compile-file-def (type name)
  (when (boundp '*compile-file-defs*)
    (let ((existing (gethash name *compile-file-defs*)))
      (if existing
          (let ((warning (make-compiler-warning
                          :message (core:bformat nil "The %s %s was previously defined as a %s at %s\n"
                                                 type
                                                 name
                                                 (compile-file-def-type existing)
                                                 (describe-source-location (compile-file-def-source-pos-info existing)))
                          :source-pos-info (ext:current-source-location))))
            (push warning *compilation-messages*))
          (setf (gethash name *compile-file-defs*) 
                (make-compile-file-def :type type
                                       :name name
                                       :source-pos-info (ext:current-source-location)))))))

(defun function-info (env func)
  (let ((info (classify-function-lookup env func)))
;;;    (core:bformat t "function-info: %s   @ %s\n" info (describe-source-location (ext:current-source-location)))
    info))

(defun variable-info (env var)
  "Lookup the variable in the lexical environment - if not found then check if it is a special"
  (let ((info (classify-variable env var)))
    (unless info
      (setq info (cons 'ext:special-var var)))
;;;    (core:bformat t "variable-info: %s   @ %s\n" info (describe-source-location (ext:current-source-location)))
    info))

(defun analyze-top-level-form (form)
  (cond
    ((eq (car form) 'core:fset)
     (let ((type (if (eq (fourth form) t)
                     'defmacro
                     'defun))
           (name (second form)))
       (register-compile-file-def type name)))
    ((and (consp form)
          (eq (first form) 'cl:let)
          (and (let ((x (third form)))
                 (and (consp x) (eq 'cl:quote (first x))
                      (eq core::*special-defun-symbol* (second x))))))
     (let ((name (second (fourth form))))
       (register-compile-file-def 'defun name)))
    #+(or)(t (core:bformat t "Unknown: %s\n" form))))

(defun compilation-unit-finished (messages)
  (when messages
    (bformat t "Compilation-unit finished \n\n")
    (dolist (m (reverse messages))
      (print-compiler-message m *debug-io*))))

